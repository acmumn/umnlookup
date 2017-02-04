# umnlookup [![Release](http://github-release-version.herokuapp.com/github/acmumn/umnlookup/release.svg)](https://github.com/acmumn/umnlookup/releases) [![Build Status](https://travis-ci.org/acmumn/umnlookup.svg?branch=master)](https://travis-ci.org/acmumn/umnlookup)

Command-line program to look up people from the UMN student
directory.

## Installation

Install [Haskell](https://www.haskell.org/) and [Cabal](https://www.haskell.org/cabal/).
Use Cabal to install the following packages:

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
