# Gfilter
## Embedded DSL for Gmail Filters

Gfilter provides an embedded DSL for writing Gmail mail filters, which can then
be exported as XML which can be imported into Gmail.

This is heavily inspired by [Britta](https://github.com/antifuchs/gmail-britta),
which is basically the same thing but in Ruby.

Why Haskell? Haskell is pretty good for embedded DSLs, I guess.

Note that Gmail queries have a maximum (string) length; to make it easier to
write very long filters, Gfilter splits rules with long conditions into
multiple rules by splitting on logical "or"s.

## Example

```
#!/usr/bin/env runhaskell

rules = [ List "somemailinglist.googlegroups.com" ==> [Archive, Label "somelist"]
        , From  "boss@mycompany.com" ==> [Star, Important]
        , toOrCc "me@mycompany.com" ==> [Important]
        ]

main = putStr $ compile rules
```

Run this example like ./example.hs > rules.xml. You can then go to the Filter
settings in Gmail and use the "Import filters" option.

More powerful rules can be written by reusing condition clauses, like so:

```
toMe = To "me@gmail.com" `Or` To "me@hotmail.com"

rules = [ toMe ==> [Important]
        , List "somelist" `And` (Not toMe) ==> [Archive]
        ]
```

You get the point.
