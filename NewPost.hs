module Main where

import Control.Monad
import Data.Time.Clock      (getCurrentTime, utctDay)
import Data.Time.Calendar   (Day, toGregorian)
import Data.Char            (toLower)
import Data.List            (find)
import Data.Maybe           (fromJust)
import System.Environment   (getArgs) 
import System.IO            (IOMode(..), hPutStrLn, withFile)


-- | This is a very basic program that accepts a single argument (post title)
-- and creates a new post file with the appropriate name and header 
-- information.
main :: IO ()
main = do
    args <- getArgs
    date <- today
    let rawTitle = concat args
        fileName = newPostFileName date rawTitle
    withFile fileName WriteMode $ \handle ->
        hPutStrLn handle $ header date rawTitle
    

-- | today returns the current day.
-- `show day` has format YYYY-MM-DD which is perfect for a Jekyll blog post!
today :: IO Day
today = liftM utctDay getCurrentTime


-- | newPostFileName takes the Day and a title for the post and returns a
-- string suitable for a filename.
newPostFileName :: Day -> String -> String
newPostFileName time title = concat ["_posts/", show time, "-", slugify title, ".markdown"]


-- | Slugify downcases a string and replaces space characters with underscores.
slugify :: String -> String
slugify = escape escapeChars . map toLower
      where
          escapeChars = [(' ', "_"), ('!', "")]


-- | header takes a Day and a Title and returns a header for the post.
header :: Day -> String -> String
header day title = unlines [ "---"
                           , "title: \"" ++ title ++ "\""
                           , "date: " ++ show day
                           , "layout: post"
                           , "categories: programming"
                           , "---"
                           ] 


-- | `escape` takes a list of pairs of chars and strings, and replaces the
-- instances of the chars with their accompanying strings.
escape :: [(Char, String)] -> String -> String
escape crs str = concat $ foldr ff [] str
             where
                 ff x acc | x `elem` cs = r x : acc
                          | otherwise   = [x] : acc
                 cs = map fst crs
                 r cx = snd $ fromJust $ find ((==cx) . fst) crs
