module Wordlist ( Wordlist
                , Wordtree(Wnode, Wleaf)
                , parseWordtree
                , readWordList) where

import Data.Char

import Debug.Hood.Observe
import Debug.Trace

data Wordtree = Wempty | Wnode [Wordnode] | Wleaf String deriving (Show, Eq)
type Wordnode = (Int, Wordtree) 

type Wordlist = [Wordline]
type Wordline = ([Int],String)

{-
instance Read Wordtree where
  reads = undefined
-}


instance Observable Wordtree where
  observer = observeBase


parseWordtree:: String -> Maybe Wordtree
parseWordtree str = (readWordList str
                    -- >>= return.(observe "Wordlist: ")
                    ) >>= (return.list2Tree)


list2Tree:: Wordlist -> Wordtree
list2Tree = foldl insertToTree Wempty
  where
    insertToTree:: Wordtree -> Wordline -> Wordtree
    insertToTree node ([], str) | node == Wempty  = Wleaf str
                                | otherwise  = error ("Trying to insert " ++ str
                                               ++ " at " ++ (show node))
    insertToTree Wempty ((i:is), str) = Wnode [(i, insertToTree Wempty (is, str))]
    insertToTree (Wnode list) x = Wnode $ insertToNodeList list x 
    insertToTree (Wleaf lstr) wlist = error $ "Trying to insert " ++ show wlist ++ " into leaf " ++ (show $ Wleaf lstr)

    insertToNodeList:: [Wordnode] -> Wordline -> [Wordnode] 
    insertToNodeList [] ((i:is) , str) = (i, insertToTree Wempty (is,str)):[]
    insertToNodeList (entry:es) l@((i:is), str) | fst entry == i = ( i
                                                                   , insertToTree (snd entry) (is,str)):es
                                                | otherwise      = entry:(insertToNodeList es l)
                                                                   
                                                                     
 
    
    
readWordList:: String -> Maybe Wordlist
readWordList = sequence.(map (parseWordLine')).lines
               


parseWordLine':: String -> Maybe Wordline
parseWordLine' l = do
  let (dicevals, wordstr) = parseDiceNums' l
  let word = parseWordStr wordstr
  return $ (dicevals, word)
  where
    parseDiceNums:: String -> Maybe ([Int], String)
    parseDiceNums = psequence pInt
    parseDiceNums':: String -> ([Int], String)
    parseDiceNums' str = ( map digitToInt $ takeWhile isDigit str, dropWhile (isDigit) str)
    parseWordStr:: String ->  String
    parseWordStr = tail



parseWordLine:: String -> Maybe Wordline
parseWordLine l = case ( (psequence pInt) # (pSpace -# pWord) ) l of
                   Nothing     -> Nothing
                   Just (a, _) ->  Just a



type Parser a = String  -> Maybe (a, String)

-- to test a condition on a
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
  case m cs of
    Nothing -> Nothing
    Just(a,cs) -> if p a then Just(a,cs) else Nothing


-- alternatives in the parsing
infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs =
  case m cs of
    Nothing -> n cs
    mcs -> mcs

-- apply two parsers in sequence
infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
    case m cs of
      Nothing -> Nothing
      Just(p, cs') ->
             case n cs' of
               Nothing -> Nothing
               Just(q, cs'') -> Just((p,q), cs'')


-- apply two parsers and throw away the result of the left parser
infixl 6 -#               
(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = case (m # n) cs of
                    Nothing -> Nothing
                    Just((p,q), cs') -> Just(q, cs')

-- apply two parsers and throw away the result of the right parser
infixl 6 #-               
(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = case (m # n) cs of
                   Nothing -> Nothing 
                   Just((p,q), cs') -> Just(p, cs')    



-- transform Parsed item to different type using a function
infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs = case m cs of
                 Just(a,cs') -> Just(k a, cs')
                 Nothing -> Nothing


-- concatenate results of sequentially applied parsers to list
cons:: (a, [a]) -> [a]
cons (a, bs) = a:bs


-- return a parser (used for empty parser below
preturn:: a -> Parser a
preturn a cs = Just(a,cs)


psequence:: Parser a -> Parser [a]
psequence p =  p # psequence p >-> cons ! preturn []

               
base10AccmulateInt:: [Int] -> Int
base10AccmulateInt = foldl (\accu i -> 10*accu + i) 0   


pSpace:: Parser Char
pSpace = char ? isSpace

pWord:: Parser String
pWord = psequence ( char ? not.isSpace)

pInt:: Parser Int
pInt = psequence digitVal >-> base10AccmulateInt ! preturn (-1)


char:: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit:: Parser Char
digit  = char ? isDigit

digitVal:: Real a => Parser a
digitVal = digit >-> (fromIntegral.digitToInt)

               
