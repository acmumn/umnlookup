{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative

import Network.HTTP

import Data.String.Utils (strip, replace)
import Data.List (intersperse, partition)
import Data.Semigroup ((<>))

import Text.Printf (printf)

import System.Console.Terminal.Size
import System.IO

import Text.HTML.Onama
import Text.Parsec (parse, try, many1, ParsecT)
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = execParser opts >>= lookupPeople
  where lookupPeople lookup = do
          tags <-     (drop 4)
                  <$> (dropWhile notFormClose)
                  <$> parseTags
                  <$> urlSlurp (lookupURL lookup)
          case parse umnLookup "" tags of
            Left error -> hPutStrLn stderr (show error)
            Right (Left error) -> hPutStrLn stderr error
            Right (Right people) -> stdoutIO
                                      (printPeopleTTY people)
                                      (printPeopleNoTTY people)
        notFormClose (TagClose "form" _) = False
        notFormClose _other              = True

urlSlurp :: String -> IO String
urlSlurp x = getResponseBody =<< simpleHTTP (getRequest x)


-- Parsing our command-line options.

opts :: ParserInfo Lookup
opts = info (helper <*> lookupCLI)
         ( fullDesc
        <> progDesc "Lookup people from UMN's student/faculty database. Outputs in a human-readable format at the command line, or to CSV if called as part of a pipeline."
        <> header "umnlookup - look up university peoples"
         )

data Lookup = Lookup
  { campus :: Campus
  , role   :: Role
  , x500   :: Bool
  , search :: String
  }
  deriving (Eq, Show)

lookupCLI :: Parser Lookup
lookupCLI =
  Lookup
  <$> option verifyCampus
       ( long "campus"
      <> short 'c'
      <> metavar "CAMPUS"
      <> value AnyCampus
      <> help "Restrict search to a specific campus. Choices: any, crookston, duluth, morris, rochester, twincities, other. Default: any"
       )
  <*> option verifyRole
       ( long "role"
      <> short 'r'
      <> metavar "ROLE"
      <> value AnyRole
      <> help "Restrict search to certain kinds of University people. Choices: any, staff, student, alumni, retired. Default: any"
       )
  <*> switch
       ( long "id-only"
      <> short 'i'
      <> help "Search only by x500. Default is to search as if the search term could be either a name or an x500."
       )
  <*> argument str (metavar "NAME/X500")

-- | Each Lookup maps to a URL which we request in order to get the actual people.
lookupURL :: Lookup -> String
lookupURL lookup =
  "http://myaccount.umn.edu/lookup?SET_INSTITUTION=&" ++ vars lookup
  where vars (Lookup campus role x500 search) =
          let baseVars = zip ["campus", "role", "CN"]
                             [toParamString campus, toParamString role, search]
          in urlEncodeVars $ ("type", if x500 then "Internet ID" else "name") : baseVars

data Campus
  = AnyCampus
  | Crookston
  | Duluth
  | Morris
  | Rochester
  | TwinCities
  | OtherCampus
  deriving (Eq, Show)

data Role
  = AnyRole
  | Staff
  | Student
  | Alumni
  | Retired
  deriving (Eq, Show)

verifyCampus :: ReadM Campus
verifyCampus = eitherReader $ \str ->
  case str of
    "any"        -> Right AnyCampus
    "crookston"  -> Right Crookston
    "duluth"     -> Right Duluth
    "morris"     -> Right Morris
    "rochester"  -> Right Rochester
    "twincities" -> Right TwinCities
    "other"      -> Right OtherCampus
    _otherwise   -> Left $ printf "invalid campus name: %s" str

verifyRole :: ReadM Role
verifyRole = eitherReader $ \str ->
  case str of
    "any"      -> Right AnyRole
    "staff"    -> Right Staff
    "student"  -> Right Student
    "alumni"   -> Right Alumni
    "retired"  -> Right Retired
    _otherwise -> Left $ printf "invalid role: %s" str

class HTMLParam param where
  toParamString :: param -> String

instance HTMLParam Campus where
  toParamString AnyCampus   = "a"
  toParamString Crookston   = "c"
  toParamString Duluth      = "d"
  toParamString Morris      = "m"
  toParamString Rochester   = "r"
  toParamString TwinCities  = "t"
  toParamString OtherCampus = "o"

instance HTMLParam Role where
  toParamString AnyRole = "any"
  toParamString Staff   = "sta"
  toParamString Student = "stu"
  toParamString Alumni  = "alu"
  toParamString Retired = "ret"


-- Parsing our HTML into a list of people.

umnLookup :: Monad m => ParsecT [Tag String] u m LookupResult
umnLookup =
      try singlePersonLookup
  <|> try multiPersonLookup
  <|> try errorLookup

type LookupResult = Either String [Person]

errorLookup :: Monad m => ParsecT [Tag String] u m LookupResult
errorLookup = do
  error <- innerText <$> balancedTags "b"
  return $ Left (strip error)

deassoc :: Eq a => a -> [(a, b)] -> ([(a, b)], Maybe b)
deassoc k l =
  let (l', l'') = partition ((/= k) . fst) l
  in (l', lookup k l'')

singlePersonLookup :: Monad m => ParsecT [Tag String] u m LookupResult
singlePersonLookup = do
  optional $ tagOpen "img" >> tagOpen "br"
  name <- innerText <$> balancedTags "h2"
  fields <- tableFields
  let (fields', email) = deassoc "Email Address" fields
  case email of
    Nothing -> fail "no email found"
    Just e  -> return $ Right [Person name e fields']
  where tableFields = do
          tagOpen "table"
          fields <- many1 tableField
          tagClose "table"
          return fields

        tableField = do
          tagOpen "tr"
          fieldName <- (replace ":" "") <$> innerText <$> balancedTags "th"
          fieldValue <- strip <$> innerText <$> balancedTags "td"
          tagClose "tr"
          return (fieldName, fieldValue)

multiPersonLookup :: Monad m => ParsecT [Tag String] u m LookupResult
multiPersonLookup = do
  tagOpen "table"
  fieldNames <- fieldNames
  people <- many1 fieldValues
  tagClose "table"
  case generatePeople fieldNames people of
    Nothing     -> fail "not a properly-formatted multi-result page (maybe the layout changed?)"
    Just people -> return $ Right people
  where fieldNames = do
          tagOpen "tr"
          f <- many1 $ (try $ strip <$> innerText <$> balancedTags "th")
          tagClose "tr"
          return f

        fieldValues = do
          tagOpen "tr"
          f <- many1 $ (try $  strip
                           <$> (replace "\xA0" " ")
                           <$> innerText
                           <$> balancedTags "td")
          tagClose "tr"
          return f

        generatePeople fieldNames people = mapM (generatePerson fieldNames) people

        generatePerson fieldNames person =
          let fields = zip fieldNames person
              (fields', name) = deassoc "Name" fields
              (fields'', email) = deassoc "Email" fields'
          in case (name, email) of
               (Just n, Just e) -> Just $ Person (normalize n) e fields''
               _other           -> Nothing
               where normalize n = subRegex (mkRegex "(.+?), (.+?)") n "\\2 \\1"


-- Printing the people we've found to the terminal.

defaultWindow :: Integral a => Window a
defaultWindow = Window 24 80

-- | Produce an IO which calls the first IO if connected to a TTY, and
--   the second IO otherwise.
stdoutIO :: (Window Int -> IO a) -> IO a -> IO a
stdoutIO f noTTY = do
  isTTY <- hIsTerminalDevice stdout
  if isTTY
    then (maybe defaultWindow id) <$> size >>= f
    else noTTY

data Person = Person
  { name   :: String
  , email  :: String
  , fields :: [(String, String)]
  }

putLn :: IO ()
putLn = putStrLn ""

printPeopleTTY :: [Person] -> Window Int -> IO ()
printPeopleTTY people window =
  sequence_ $ intersperse (printf "\n* * * * *\n\n")
                          (fmap (printPersonTTY window) people)

filterMaybes :: [Maybe a] -> [a]
filterMaybes = fmap (maybe undefined id)
               . (filter $ \obj -> case obj of { Nothing -> False; Just _x  -> True} )

printPersonTTY :: Window Int -> Person -> IO ()
printPersonTTY window (Person name email fields) = do
  putStrLn name
  putStrLn email
  putLn
  sequence_ $ intersperse putLn (filterMaybes (fmap (printFieldTTY window) fields))

printFieldTTY :: Window Int -> (String, String) -> Maybe (IO ())
printFieldTTY _window (fieldName, value) =
  if null value
    then Nothing
    else Just $ do printf "### %s ###\n" fieldName
                   putStrLn $ value

printPeopleNoTTY :: [Person] -> IO ()
printPeopleNoTTY = mapM_ printPersonNoTTY

printPersonNoTTY :: Person -> IO ()
printPersonNoTTY (Person name email fields) = do
  printf "%s,%s" name email
  putChar ','
  printFieldNamesNoTTY $ fmap fst fields
  putChar ','
  printFieldValuesNoTTY $ fmap snd fields
  putLn

printFieldNamesNoTTY :: [String] -> IO ()
printFieldNamesNoTTY = mapM_ putStr . intersperse ";" . fmap strip

printFieldValuesNoTTY :: [String] -> IO ()
printFieldValuesNoTTY strs = mapM_ putStr
                               (intersperse ";" $ fmap (escapeCSV . strip) strs)

escapeCSV :: String -> String
escapeCSV = mconcat . fmap escape
  where escape '\n' = "\\n"
        escape ';'  = "\\;"
        escape ','  = "\\,"
        escape c    = [c]
