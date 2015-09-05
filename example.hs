#!/usr/bin/env runhaskell

import Gfilter

rules = List "somemailinglist.googlegroups.com" ==> [Archive, Label "somelist"]
      : From  "boss@mycompany.com" ==> [Star, Important]
      : toOrCc "me@mycompany.com" ==> [Important]
      : []

main = do putStr $ compile rules
