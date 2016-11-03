{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.IO (stdout, stderr)
import Control.Lens ((&), (.~), (<&>), (?~), (^?))
import Control.Applicative ((<|>))
import Control.Monad (join)
import qualified Data.Text as T
import Text.InterpolatedString.Perl6

import System.Environment (setEnv, lookupEnv, getArgs)
import Data.Time

import Network.Google
import Network.Google.YouTube
import Network.Google.YouTube.Types
import Network.Google.Resource.YouTube.Search.List
import Network.Google.Resource.YouTube.Videos.List
import Network.Google.Auth.ApplicationDefault (defaultCredentialsPath)
import Network.Google.Prelude

import Conduit
import qualified Data.Conduit.Binary as CB

type YouTubeReadOnlyScope = '["https://www.googleapis.com/auth/youtube.readonly"]

type PublishedPeriod = (Maybe UTCTime, Maybe UTCTime) 

prepareEnv :: IO (Env YouTubeReadOnlyScope)
prepareEnv = do
  -- l <- newLogger Debug stderr
  setEnv "GOOGLE_APPLICATION_CREDENTIALS" "/path/to/your/json/credentials"
  -- e <- newEnv <&> (envLogger .~ l) . (envScopes .~ youTubeReadOnlyScope)
  e <- newEnv <&> (envScopes .~ youTubeReadOnlyScope)
  return e
  
mkSearchQuery :: Text -> PublishedPeriod -> SearchList
mkSearchQuery q (after, before) = searchList "id,snippet"
                      & slQ .~ Just q
                      & slVideoDuration .~ Just Short
                      & slType .~ "video"
                      & slRelevanceLanguage .~ Just "ru"
                      & slOrder .~ Date
                      & slMaxResults .~ 50
                      & slPublishedAfter .~ after
                      & slPublishedBefore .~ before

mkVideosQuery :: Text -> VideosList                      
mkVideosQuery idString = videosList "contentDetails"
                       & vlId .~ Just idString 
                       & vlMaxResults .~ 50
  
-- e.g. sendQuery :: SearchList -> Env YouTubeReadOnlyScope -> IO SearchListResponse
sendQuery :: ( GoogleRequest r
            , (HasScope YouTubeReadOnlyScope r))
            => r 
            -> Env YouTubeReadOnlyScope
            -> IO (Rs r)
sendQuery searchQuery env = do
  runResourceT . runGoogle env $ do { send searchQuery }

getVideoId :: SearchResult -> Text
getVideoId x = x ^. srId <&> (^. riVideoId) & join & fromJust
                  
-- parseTimeM True defaultTimeLocale "PT%-HH%-MM%-SS" "PT1H1M1S" :: Maybe UTCTime

parseIso8601Duration :: Text -> Maybe NominalDiffTime
parseIso8601Duration str = diffUTCTime <$> parsed <*> zeroTime
  where
    withPattern pat = parseTimeM True defaultTimeLocale pat (T.unpack str)
    zeroTime = parseTimeM True defaultTimeLocale "" ""
    parsed :: Maybe UTCTime
    parsed = withPattern "PT%-HH%-MM%-SS" 
         <|> withPattern "PT%-MM%-SS"
         <|> withPattern "PT%-SS"
         
zeroDuration = parseIso8601Duration "PT0H0M0S"

videoDuration :: Video -> Maybe NominalDiffTime
videoDuration v = parseIso8601Duration duration
  where
    duration = v ^. vContentDetails <&> (^. vcdDuration) & join & fromJust

parseDate :: String -> Maybe UTCTime
parseDate str = withPattern "%-Y-%-m-%" <|> withPattern "%-Y-%-m"
  where withPattern pat = parseTimeM True defaultTimeLocale pat str

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b

extractJust getter record = record <&> (^. getter) & join & fromJust

data SearchParams = SP { spMaxRes :: Int
                       -- , spMaxRequestRes :: Int -- 50
                       , spDurationMin :: Maybe NominalDiffTime
                       , spDurationMax :: Maybe NominalDiffTime
                       -- , spPublishedAfter :: Maybe UTCTime
                       -- , spPublishedBefore :: Maybe UTCTime
                       } deriving Show

searchParams :: Int -> Text -> Text -> SearchParams
searchParams maxRes durMin durMax = SP maxRes dmin dmax
 where
   dmin = parseIso8601Duration durMin
   dmax = parseIso8601Duration durMax

videoSearchSource :: MonadIO m => SearchList -> Env YouTubeReadOnlyScope -> SearchParams -> ConduitM i Video m ()
videoSearchSource initQuery env SP{..} = loop spMaxRes initQuery 
  where
    loop limit query | limit <= 0 = return ()
                     | otherwise = do
      videosInfo <- liftIO $ (sendQuery query env) <&> (^. slrItems)

      let videoIds = map getVideoId videosInfo
      let !lastVideo = last videosInfo
      let lastVideoUploadDate = lastVideo ^. srSnippet <&> (^. srsPublishedAt) & join & fromJust

      let idString = T.intercalate "," videoIds
      let videoListQuery = mkVideosQuery idString
      videos <- liftIO $ (sendQuery videoListQuery env) <&> (^. vlrItems)

      -- Will be extracted into separate conduit,
      -- rather interesting to see benchmarks on this vs. extracted version
      let filteredVideos = filter (between spDurationMin spDurationMax . videoDuration) videos
      liftIO $ putStrLn $ " -> " ++ (show $ length filteredVideos)
      -- let filteredIds = map getVideoId filteredVideos
      -- yieldMany filteredIds
      yieldMany filteredVideos
      loop (limit-50) (query & slPublishedBefore .~ Just lastVideoUploadDate)

videoIdToLink :: Text -> Text
videoIdToLink vid = [qc|https://youtube.com/watch?v={vid}|] `T.append` "\n"

main :: IO () 
main = do
  env <- prepareEnv
  let after = parseDate ""
  -- let before = parseDate "2016-10"
  [smaxres, dmin, dmax, sbefore, searchString] <- getArgs
  let maxres = read smaxres :: Int
  let before = parseDate sbefore
  let searchQuery = mkSearchQuery (T.pack searchString) (after, before)
  let sp = searchParams maxres (T.pack dmin) (T.pack dmax)
  runConduitRes $ (videoSearchSource searchQuery env sp)
            .| mapC (\v -> (v ^. vId) & fromJust)
            .| mapC videoIdToLink
            -- .| stdoutC
            .| sinkFile "youtube-search-results.txt"
