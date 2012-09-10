{-- Simple KIT mensa page parser. It doesn't rely on the stack-based structure of html, don't get your hopes up. It mainly uses regexes and the functional equivalent of the sledgehammer to get to something that looks like it might be a menu.
The tweets are sent using a perl script called TTYtter, because I couldn't find a twitter/oath-library that would install in cabal.
As such, this requires ttytter.pl to be present in the current directory. It must already be set up to tweet. --}
import Data.Maybe
import System.Environment
import Network.Curl
import Data.List.Split
import Text.Regex
import Text.Regex.Posix
import Data.List
import System.Process
import Control.Concurrent

-- Get arguments, expand into default if needed, and parse the data.
main :: IO ()
main = do
    args <- getArgs
    let safeArgs = fromMaybe "http://www.studentenwerk-karlsruhe.de/de/essen/?view=ok&STYLE=popup_plain&c=adenauerring&p=1" $ listToMaybe args
    input <- getRaw safeArgs
    let action = case "--dry-run" `elem` args of
                      True  -> putListLn $ tweets input
                      False -> tweetList $ tweets input
    action


-- Tweets a list of strings
tweetList :: [String] -> IO ()
tweetList [] = do
    return ()
tweetList (x:xs) = do
    pid <- tweet x
    waitForProcess pid
    -- We can't tweet too fast, twitter doesn't like it when we do
    threadDelay 2
    tweetList xs

-- Use external perl script to tweet a status
tweet :: String -> IO ProcessHandle
tweet tw = do
    -- TTYtter can't handle some encodings, so we need to convert this to utf8
    safetw <- encodec tw 
    pid <- runCommand $ "./ttytter.pl -status=\"" ++ safetw ++ "\""
    return pid

-- convert String into utf8 string
encodec :: String -> IO String
endocec [] = []
encodec = readProcess "/usr/bin/iconv" ["-t","utf8","-f","latin1"]

-- Print content of String list one line at a time
putListLn :: [String] -> IO ()
putListLn [] = do
   return () 
putListLn (x:xs) = do
    putStrLn x
    putListLn xs

-- Get raw input data from web page (prefix http://) or file
getRaw :: String -> IO String
getRaw x
    | isPrefixOf "http://" x = do
        curled <- curlGetString x []
        return $ snd curled
    | otherwise = do
        input <- readFile x
        return input

-- Drop odd elements from list
dropOdd :: [a] -> [a]
dropOdd [] = []
dropOdd (x:xs) = (take 1 xs) ++ (dropOdd $ drop 1 xs)

-- Drop even elements from list
dropEven :: [a] -> [a]
dropEven [] = []
dropEven (x:xs) = x : (dropEven $ drop 1 xs)

-- flattens list hierachy by merging elements pairwise
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- Parse raw data and return list of tweets
-- This got messier than anticipated. :/
tweets :: String -> [String]
tweets rawHTML =
    map ((take 140).unwords) filtered 
    where
        filtered = map (filter (\x -> length x > 1)) blasted
        blasted = map (splitRegex (mkRegex "<[^>]*>")) $ zipList lineNames lineFood
        lineNames = flatten $ map (take 1) splitDaily 
        lineFood = map (intercalate ";\n") $ map dropOdd splitDaily
        splitDaily = map (splitRegex (mkRegex "<[/]*b>")) rawDaily
        -- rawDaily is one full day's menu as full line menu tables
        rawDaily = splitOn "</table>" $ weekMenu !! 1 -- second element as first one is header stuff
        weekMenu = splitRegex (mkRegex "(Mo|Di|Mi|Do|Fr) [0-9]*.[0-9]*") $ filterWhitespace rawHTML

-- zip two lists, merging instead of creating tuples
zipList :: [[a]] -> [[a]] -> [[a]]
zipList [] xs = xs
zipList xs [] = xs
zipList (x:xs) (y:ys) = (x ++ y):(zipList xs ys)

-- Get rid of whitespace within the HTML data
-- I could have used the whitespace as delimiters, splitting at line ends, but I'd
-- rather rely on HTML structure than whitespace structure
filterWhitespace :: String -> String
filterWhitespace xs = unwords $ words xs
