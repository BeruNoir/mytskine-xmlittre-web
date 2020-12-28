{-# LANGUAGE OverloadedStrings #-}
module LinksTest
    ( linksSpecs
    , getLinks
    ) where

import TestImport
import Filesystem as FS (readFile)
import qualified Filesystem.Path.CurrentOS as FP (fromText)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Text.Regex.Posix
import Data.Text.Encoding (decodeUtf8)

readTemplate :: T.Text -> IO BS.ByteString
readTemplate filename = FS.readFile $ FP.fromText $ T.append "./templates/pages/" filename

findLinks :: BS.ByteString -> [T.Text]
findLinks t = map (\x -> decodeUtf8 $ x !! 1) (t =~ regexp)
    where
        regexp :: BS.ByteString
        regexp = "DefinitionR \"([^\"]+)\""

checkLink :: T.Text -> YesodSpec App
checkLink l =  yit ("Check link " ++ s) $ do
    -- get $ DefinitionR l
    get (T.append "/definition/" l)
    statusIs 200
    htmlAllContain "h1" s
  where s = T.unpack l

getLinks :: T.Text -> IO [T.Text] 
getLinks filename = do
    t <- readTemplate filename
    return $ findLinks t

linksSpecs :: [T.Text] -> Spec
linksSpecs links =
    ydescribe "Check page links" $
        -- Expected: YesodSpec site
        runAll $ map checkLink links
        -- liftIO $ fmap (mconcat . (map checkLink)) links
  where
    -- runAll :: [YesodSpec site] -> YesodSpec site
    runAll (x:xs) = x >> runAll xs
    runAll [] = checkLink "bonjour"
