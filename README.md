# umnlookup [![Release](http://github-release-version.herokuapp.com/github/acmumn/umnlookup/release.svg)](https://github.com/acmumn/umnlookup/releases) [![License](https://img.shields.io/badge/license-BSD--3-ff69b4.svg)](https://github.com/acmumn/umnlookup/blob/master/LICENSE)

Command-line program to look up people from the UMN student directory.

## Installation

Install [Haskell](https://www.haskell.org/)
and [Cabal](https://www.haskell.org/cabal/). Use Cabal to install the
following packages:

```bash
cabal install tagsoup http scalpel-0.3.1
```

Then clone this repo and run the following commands from the repo
directory:

```bash
cabal configure
cabal install
```

This will install a command `umnlookup` that you can then go run.

If you're having trouble installing this, go ask Prof. Eric Van Wyk
for help. He seems to know his way around Haskell.

## Notes on output

If run at the command line, `umnlookup` produces a human-readable listing
of all the people it finds. However, if you need to use it more
programmatically, then pipe its output somewhere, like another program
or a file. It'll instead emit a CSV-formatted list of people, looking
like this:

```
<NAME>,<EMAIL>,<LIST OF FIELD NAMES>,<LIST OF FIELDS VALUES>
...
```

The first two fields are name and email address, respectively, while
the next two are **semicolon** separated lists of fields. This is
necessary because the amount of field-value pairs can vary based on the
person you're looking up.

Literal newlines in field values get escaped as `\n`, while literal commas
and semicolons get escaped as `\,` and `\;`.
