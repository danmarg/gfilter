-- |
-- Module      : Gfilter
-- Description : Gfilter provides a simple eDSL for generating Gmail mail filters.
-- Copyright   : (c) Daniel Margolis, 2015
-- License     : MIT
-- Maintainer  : dan@af0.net
-- Stability   : experimental
-- Portability : portable
--
-- Gfilter provides an embedded DSL for writing Gmail mail filters, which can then
-- be exported as XML which can be imported into Gmail.
--
-- Note that Gmail queries have a maximum (string) length; to make it easier to
-- write very long filters, Gfilter splits rules with long conditions into
-- multiple rules by splitting on logical "or"s.
--
-- --* Example
--
-- @
-- import Gfilter
--
-- rules = List \"somemailinglist.googlegroups.com\" ==> [Archive, Label \"somelist\"] : []
-- main = putStr $ compile rules
-- @
module Gfilter (
  Cond(List,
       To,
       Cc,
       From,
       Subject,
       Has,
       DeliveredTo,
       Attachment,
       Or,
       And,
       Not,
       All,
       Any),
  Action(Archive, Label, Star, Delete, MarkAsRead, Important, Unimportant),
  (==>), toOrCc, compile
) where

import Data.List

-- Dunno.
maxQueryLength = 512

-- | Cond represents a filter condition.
data Cond = List String  -- ^ List matches mail to a mailing list.
          | To String  -- ^ To matches mail with the given To:.
          | Cc String  -- ^ Cc matches mail with the given CC:.
          | From String  -- ^ From matches mail with the given From:.
          | Subject String  -- ^ Subject matches mail with the given Subject:.
          | Has String  -- ^ Has matches arbitrary string patterns.
          | DeliveredTo String  -- ^ DeliveredTo matches on Delivered-To:.
          | Attachment  -- ^ Attachment matches messages with attachments.
          | Or Cond Cond  -- ^ Or combines two conditions in a logical "or".
          | And Cond Cond  -- ^ And combines two conditions in a logical "and".
          | Not Cond  -- ^ Not inverts a condition.
          | All [Cond]  -- ^ All matches if all of the conditions are true.
          | Any [Cond]  -- ^ Any matches if any of the conditions are true.

-- | Action represents the actions of a filter.
data Action = Archive  -- ^ Archive a message (skip the inbox).
            | Label String  -- ^ Label a message.
            | Star  -- ^ Star a message.
            | Delete  -- ^ Delete a message.
            | MarkAsRead  -- ^ Mark a message read.
            | Important  -- ^ Mark a message important.
            | Unimportant  -- ^ Mark a message unimportant.
            | NeverSpam -- ^ Never mark as spam.
            | ForwardTo String -- ^ Forward a message.

-- | Rule represents a complete filter rule.
data Rule = Rule Cond [Action]

-- | The ==> creates a filter from a condition and a list of actions.
(==>) :: Cond -> [Action] -> Rule
(==>) c as = Rule c as

-- | toOrCc does what it says.
toOrCc x = (To x) `Or` (Cc x)

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

-- | compile compiles a list of rules into an string XML file. The result can
-- be imported into Gmail.
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
(+-+) :: String -> String -> String
(+-+) x y  = x ++ "\"" ++ y ++ "\""

-- Convert a Cond to a Gmail query string.
toQuery :: Cond -> String
toQuery (List x) = "list:" +-+ x
toQuery (To x) = "to:" +-+  x
toQuery (Cc x) = "cc:" +-+ x
toQuery (From x) = "from:" +-+ x
toQuery (Subject x) = "subject:" +-+ x
toQuery (Has x) = "\"" ++ x ++ "\""
toQuery (DeliveredTo x) = "deliveredto:" +-+ x
toQuery Attachment = "has:attachment"
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
toAction NeverSpam = "<apps:property name='shouldNeverSpam' value='true'/>"
toAction (ForwardTo  x) = "<apps:property name='forwardTo' value='" ++ x ++ "'/>"

instance Show Cond where show x = toQuery x
instance Eq Cond where
  x == y = (show x == show y)
