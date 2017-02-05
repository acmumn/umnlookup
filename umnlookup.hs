import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (urlEncodeVars)
import Text.HTML.TagSoup (Tag, parseTags, (~==), canonicalizeTags, isTagCloseName)
import Text.HTML.Scalpel (scrape, chroot, chroots, text, texts, (@:), (@=))
import Text.Printf (printf)

import Data.Text (dropAround, pack, unpack)
import Data.Char (isSpace, isControl, isPunctuation)
import Data.List (partition)

import System.Environment (getArgs)
import System.IO (stdout, hIsTerminalDevice)

import Control.Applicative ((<$>))

urlSlurp :: String -> IO String
urlSlurp x = getResponseBody =<< simpleHTTP (getRequest x)

mix :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
mix combine f g = \x -> f x `combine` g x

(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<&&>) = mix (&&)
infixl 8 <&&>

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = mix (||)
infixl 7 <||>

deassoc :: Eq a => a -> [(a, b)] -> ([(a, b)], Maybe b)
deassoc k l =
  let (l', l'') = partition ((/= k) . fst) l
  in (l', lookup k l'')

-- UMN-specific stuff.

directoryURL :: String
directoryURL = "http://myaccount.umn.edu/lookup?"

directoryEnd :: Tag String -> Bool
directoryEnd = fmap not $ (~== "<b>") <||> (~== "<p>") <||> (~== "<hr>")

data URLArgs =
  URLArgs { searchTerm :: String,
            campus :: String,
            role :: String }
  deriving (Show, Eq)

defaultArgs :: URLArgs
defaultArgs = URLArgs { searchTerm = "", campus = "any", role = "any" }

main :: IO ()
main = getArgs >>= parseArgs >>= perform

data Action = Usage
            | Lookup String
            | Invalid [(String, String)]
  deriving (Show)

(+/+) :: String -> String -> String
(+/+) = (++) <$> (++ "\n")

perform :: Action -> IO ()
perform Usage =
  putStrLn $
  "usage: umnlookup [(--help|-h)] [OPTIONS] <NAME/X500>" +/+
  "" +/+
  "    options:" +/+
  "      --campus, -c <CAMPUS>    restrict search to certain campus" +/+
  "          choices: any, crookston, duluth, morris, rochester, twincities, other" +/+
  "      --role, -r <ROLE>        restrict search to certain kinds of university people" +/+
  "          choices: any, staff, student, alumni, retired"
perform (Lookup url) = urlSlurp url >>= directoryTags >>= parsePeople >>= printPeople
perform (Invalid invalids) =
  foldl (>>) (return ()) ((putStrLn . invalidMsg) <$> invalids)
    where invalidMsg (flag, val) = "Invalid value for " ++ flag ++ ": `" ++ val ++ "'"

data Person = Person { name :: String,
                       email :: String,
                       fields :: [(String, String)] }
  deriving (Show, Eq)

directoryTags :: String -> IO [Tag String]
directoryTags = return
  . takeWhile (not <$> isEnd)
  . drop 4
  . dropWhile (not <$> isTagCloseName "form")
  . canonicalizeTags
  . parseTags
  where isEnd = (~== "<b>") <||> (~== "<p>") <||> (~== "<hr>")

parsePeople :: [Tag String] -> IO [Person]
parsePeople tags =
  case scrape singleName tags of
    Just name ->
      let (Just fields) = scrape singleFields tags
          (fields', Just email) = deassoc "Email Address" fields
      in return [Person { name = name, email = email, fields = fields' }]
    Nothing ->
      case scrape multiFieldNames tags of
        Just fields ->
          let (Just fvals) = scrape multiFields tags
          in return $ fmap (extractInfo . zip fields) fvals
        Nothing -> return []
        
      where extractInfo :: [(String, String)] -> Person
            extractInfo f =
              let (f', Just email) = deassoc "Email" f
                  (f'', Just name) = deassoc "Name" f'
              in Person { name = name, email = email, fields = f'' }

  where singleName = text "h2"
        singleFields = chroots "tr" (do
          key <- text "th"
          val <- text "td"
          return (strip key, strip val))

        multiFieldNames = chroot ("tr" @: ["align" @= "CENTER"]) (fmap strip <$> texts "th")
        multiFields = chroots ("tr" @: ["align" @= "LEFT"]) (fmap strip <$> texts "td")

        strip = unpack . dropAround (isSpace <||> (== ':') <||> isControl) . pack

printPeople :: [Person] -> IO ()
printPeople people =
  if null people
    then return ()
    else let output = fmap printPerson people
         in foldl ((>>) <$> (>> putPersonDivider)) (head output) (tail output)
            >> putStr "\n"
  where printPerson :: Person -> IO ()
        printPerson (Person { name = name, email = email, fields = fields }) =
          putNameAndEmail
          >> putFieldDivider
          >> putFields

          where
            putNameAndEmail = do
              isTTY <- hIsTerminalDevice stdout
              if isTTY
                then printf "%s\t\t\t%s\n" name email
                else putStr name
                     >> putFieldDivider
                     >> putStr email
            putFieldDivider = do
              isTTY <- hIsTerminalDevice stdout
              if isTTY then putStr "\n" else putStr ","
            putFields = do
              isTTY <- hIsTerminalDevice stdout
              if null fields
                then return ()
                else if isTTY
                     then let output = fmap (\(k, v) -> printf "%s\t\t\t%s" k v) fields
                          in foldl ((>>) <$> (>> putStr "\n")) (head output) (tail output)
                     else let keys = fmap (putStr . fst) fields
                              vals = fmap (putStr . snd) fields
                          in foldl ((>>) <$> (>> putStr ";")) (head keys) (tail keys)
                             >> putFieldDivider
                             >> foldl ((>>) <$> (>> putStr ";")) (head vals) (tail vals)
                             
        putPersonDivider = do
          isTTY <- hIsTerminalDevice stdout
          if isTTY then putStr "\n\n" else putStr "\n"
              
parseArgs :: [String] -> IO Action
parseArgs flags = parse' flags defaultArgs
  where parse' ("--help":_) _ = return Usage
        parse' ("-h":_) _ = return Usage
        parse' ("--campus":campus:flags) args =
          parse' flags (args { campus = campus })
        parse' ("-c":campus:flags) args =
          parse' flags (args { campus = campus })
        parse' ("--role":role:flags) args =
          parse' flags (args { role = role })
        parse' ("-r":role:flags) args =
          parse' flags (args { role = role })
        parse' [searchTerm] args = return (validate $ args { searchTerm = searchTerm })
        parse' _ _ = return Usage

        validate args =
          case (convertCampus (campus args), convertRole (role args)) of
            (Just campus', Just role') -> Lookup $ convertToURL (args { campus = campus', role = role' })
            (Nothing, Just _) -> Invalid [("campus", campus args)]
            (Just _, Nothing) -> Invalid [("role", role args)]
            (Nothing, Nothing) -> Invalid [("campus", campus args), ("role", role args)]

        convertToURL :: URLArgs -> String
        convertToURL (URLArgs { searchTerm = searchTerm, campus = campus, role = role }) =
          directoryURL ++ urlEncodeVars [("CN", searchTerm), ("campus", campus), ("role", role)]
        
        convertCampus :: String -> Maybe String
        convertCampus "any" = Just "a"
        convertCampus "crookston" = Just "c"
        convertCampus "duluth" = Just "d"
        convertCampus "morris" = Just "m"
        convertCampus "rochester" = Just "r"
        convertCampus "twincites" = Just "t"
        convertCampus "other" = Just "o"
        convertCampus _ = Nothing

        convertRole :: String -> Maybe String
        convertRole "any" = Just "any"
        convertRole "staff" = Just "sta"
        convertRole "student" = Just "stu"
        convertRole "alumni" = Just "alu"
        convertRole "retired" = Just "ret"
        convertRole _ = Nothing
