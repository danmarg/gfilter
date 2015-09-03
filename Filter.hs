module Filter where

import Data.List

-- Dunno.
maxQueryLength = 512

-- Filter primitives
data Cond = List String |
            To String |
            Cc String |
            From String |
            Sender String |
            Subject String |
            Has String |
            Or Cond Cond |
            And Cond Cond |
            Not Cond |
            All [Cond] |
            Any [Cond]

-- Actions
data Action = Archive |
              Label String |
              Star |
              Delete |
              MarkAsRead |
              Important |
              Unimportant

data Rule = Rule Cond [Action]

-- Make a rule
(==>) :: Cond -> [Action] -> Rule
(==>) c as = Rule c as

-- Convenience functions
-- toOrCc does what it says.
toOrCc x = (To x) `Or` (Cc x)
-- from does Sender or From.
from x = (Sender x) `Or` (From x)

-- Expand rules on ORs to avoid size limits.
expand :: Rule -> [Rule]
expand (Rule c as) = map (\x -> Rule x as) (maybeExpand c)
expandCond :: Cond -> [Cond]
expandCond (x `Or` y) = maybeExpand x ++ maybeExpand y
expandCond (Any (x:xs)) =
  -- Only expand Anys for as long as the tail is over maxQueryLength.
  if (length $ toQuery (Any (x:xs))) > maxQueryLength then
  maybeExpand x ++ (maybeExpand (Any xs)) else [Any (x:xs)]
expandCond (x `And` y) =
  let xs = expandCond x in
  let ys = expandCond y in
  let pairs = [(x,y) | x <- xs, y <- ys] in
  map (\(x, y) -> x `And` y) pairs
expandCond x = [x]
maybeExpand x =
  if (length $ toQuery x) > maxQueryLength then
  expandCond x else [x]

-- Compile a bunch of rules. Is there any reasonable error checking to do here?
compile :: [Rule] -> String
compile rs =
   "<feed xmlns:apps='http://schemas.google.com/apps/2006' " ++
   "xmlns='http://www.w3.org/2005/Atom'><title>Mail Filters</title>" ++
   "<id>tag:mail.google.com,2008:filters:</id>" ++
   compile' (foldr (\r rs -> expand r ++ rs) [] rs) ++
   "</feed>"
compile' :: [Rule] -> String
compile' [] = ""
compile' ((Rule c a):rs) =
  "<entry><category term='filter'/><apps:property name='hasTheWord' value='" ++
  (toQuery c) ++
  "'/>" ++ (foldl (\x y -> x ++ (toAction y)) "" a) ++ "</entry>\n"
  ++ compile' rs
  
--- Concatenate two strings, quoting the second.
(+++) :: String -> String -> String
(+++) x y  = x ++ "\"" ++ y ++ "\""

-- Convert a Cond to a Gmail query string.
toQuery :: Cond -> String
toQuery (List x) = "list:" +++ x
toQuery (To x) = "to:" +++  x
toQuery (Cc x) = "cc:" +++ x
toQuery (From x) = "from:" +++ x
toQuery (Sender x) = "sender:" +++ x
toQuery (Subject x) = "subject:" +++ x
toQuery (Has x) = "\"" ++ x ++ "\""
toQuery (Or x y) = (toQuery x) ++ " OR " ++ (toQuery y)
toQuery (And x y) = "((" ++ (toQuery x) ++ ") AND (" ++ (toQuery y) ++ "))"
toQuery (Not x) = "-(" ++ (toQuery x) ++ ")"
toQuery (All (x:xs)) = toQuery (foldl (\x y -> And x y) x xs)
toQuery (Any (x:xs)) = toQuery (foldl (\x y -> Or x y) x xs)

-- Convert an Action to a Gmail action XML.
toAction :: Action -> String
toAction Archive = "<apps:property name='shouldArchive' value='true'/>"
toAction (Label x) = "<apps:property name='label' value='" ++ x ++ "'/>"
toAction Star = "<apps:property name='shouldStar' value='true'/>"
toAction Delete = "<apps:property name='shouldDelete' value='true'/>"
toAction MarkAsRead = "<apps:property name='shouldMarkAsRead' value='true'/>"
toAction Important = "<apps:property name='shouldMarkAsImportant' value='true'/>"
toAction Unimportant = "<apps:property name='shouldNeverMarkAsImportant' value='true'/>"

instance Show Cond where show x = toQuery x
instance Eq Cond where
  x == y = (show x == show y)
