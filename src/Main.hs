module Main where

import System.Random
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Either
import Control.Monad

import Debug.Hood.Observe

import Wordlist

  
diceword:: Wordtree -> IO String
diceword wt = randomFindWord [] wt
              >>= (\word -> case word of
                    (Left errpath) -> error $ "Missing Word in Wordtree at " ++ (show errpath)
                    (Right word) -> return $ observe "word found: "  word
                  )
  where
    randomFindWord:: [Int] -> Wordtree -> IO (Either [Int] String)
    randomFindWord _ (Wleaf word) = return $ Right word
    randomFindWord searchpath (Wnode wt) = randomRIO (1,6)
                                           >>= (\r -> case selectSubList wt r of
                                                 (Left err) -> return $ observe "Path missing" (Left  (err:searchpath))
                                                 (Right subwt) -> randomFindWord (observe "found entry for path " (r:searchpath))   subwt
                                               )
    selectSubList:: [(Int, Wordtree)] ->  Int -> Either (Int) Wordtree
    selectSubList [] rand = Left rand --  "Empty Wordtree"
    selectSubList ((i,wl):xs) rand | i == rand = Right wl
                                   | otherwise = case selectSubList xs rand of
                                                  (Left err) -> Left err
                                                  (Right tree) -> Right tree
 
  
dicewordArray:: Wordtree -> Int -> IO [String]
dicewordArray wordtree num = sequence (take num $ repeat $ diceword wordtree)


------------- CLI ----------------

data Options = Options {
  numGenerated:: Int
  } deriving Show
               
defaultOptions:: Options
defaultOptions = Options {
  numGenerated = 5
  }


options:: [OptDescr (Options -> Options)]
options= [Option ['n'] []
          (ReqArg (\arg o ->  o {numGenerated = read arg }) "number")
          "Number of words to generate"]
                              
main :: IO ()
main = -- runO $
       do
          args <- getArgs
          let (opts, nonOpts) = case getOpt RequireOrder options args of
                                 (_,[],_)   -> error ("No Wordfile given")
                                 (o,n,[])   -> ( foldl (flip id) defaultOptions o, n)
                                 (_,_,errs) -> error ("Error "
                                                      ++ (concat errs ++ usageInfo "Usage: " options))
          wordtree <- readFile (head nonOpts) >>= return.parseWordtree
          case wordtree of
           (Just wt) -> do
            dicewordArray wt (numGenerated opts) 
                         >>= (\dwa -> (putStrLn.show) dwa >> (return dwa))
                         >>= putStrLn.concat
           Nothing -> error "Corrupt Wordtree"
          --putStrLn $ show $ numGenerated $ foldl (flip id) defaultOptions opts 

