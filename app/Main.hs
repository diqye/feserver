{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Main where

import Lib
import System.Directory
import qualified Happstack.Server as S
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad(msum,join)
import Control.Concurrent.MVar(readMVar)
import System.IO
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.Monoid((<>))
import Data.String(fromString)
import Network.HTTP.Types(statusCode)
import Data.List(intercalate)
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Yaml((.:))
import Control.Applicative((<|>))



data ServerRouter = ServerRouter { serverPath :: String
                                 , serverRewrite :: String
                                 , locationPath :: String
                                 }
data ServerConfig = ServerConfig { serverPort :: Int
                                 , serverRouters :: [ServerRouter]
                                 }

instance Y.FromJSON ServerRouter where
  parseJSON  = Y.withObject "routers" $ \v ->
    pure ServerRouter
    <*>
    v .: "path"
    <*>
    (v .: "rewrite" <|> return "none")
    <*>
    (v .: "locationPath" <|> return "none")

instance Y.FromJSON ServerConfig where
  parseJSON  = Y.withObject "config" $ \v ->
    pure ServerConfig
    <*>
    v .: "port"
    <*>
    v .: "routers"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "==== v0.0.2 ==="
  config <- parsingConfig
  let sport = serverPort config
  putStrLn $ "==== 服务启动 port:" ++ show sport ++ " ===="
  let config = S.nullConf {S.port=sport}
  S.simpleHTTP config $ enterRouter

parsingConfig :: IO ServerConfig
parsingConfig = do
  currentPath <- getHomeDirectory
  let configFile = currentPath ++ "/feserver.yaml"
  config <- (Y.decodeFileEither configFile) :: IO (Either Y.ParseException ServerConfig)
  case config of Left e -> error $ show e
                 Right r -> return r




enterRouter = do
  config <- liftIO parsingConfig
  msum $ map buildRouter $ serverRouters config

buildRouter :: ServerRouter -> S.ServerPart S.Response
buildRouter (ServerRouter path "none" location) =
  if path == "/" then logic else S.dirs path logic
  where logic = S.serveDirectory S.EnableBrowsing ["index.html","diqye.html"] location
buildRouter (ServerRouter path host "none") = 
  if path == "/" then logic else S.dirs path logic
  where logic = selfproxy host

{-
enterRouter = msum
 [ S.dir "mobile" $ selfproxy "https://m.vipfengxiao.com/mobile"
 , S.nullDir >> (S.ok $ S.toResponse ("nullDir"::String))
 , S.dir "api" $ S.dir "gw" $ selfproxy "https://www.vipfengxiao.com/api/gw"
 , S.dir "vipbclass" $ selfproxy "https://www.vipfengxiao.com/vipbclass"
 , S.dirs "/a" $ (S.ok $ S.toResponse ("abc"::String))

 ]
 -}

selfproxy :: String -> S.ServerPart S.Response
selfproxy uri = do
  serverRq <- S.askRq
  maybeV <- S.getHeaderM "Authorization"
  let auth = maybe [] (pure . ((,) "Authorization")) maybeV
  contentType' <- S.getHeaderM "Content-Type"
  let contentType = maybe [] (pure . ((,) "Content-Type")) contentType'
  acceptMaybe <- S.getHeaderM "Accept"
  let accept = maybe [] (pure . ((,) "Accept")) acceptMaybe
  originMaybe <- S.getHeaderM "origin"
  let origin = maybe [] (pure. ((,) "origin")) originMaybe
  res <- liftIO $ do 
    initRq <- parseRequest uri
    manager <- newManager tlsManagerSettings
    serverRequestBody <- readMVar $ S.rqBody serverRq
    let clientRq = initRq { path = path initRq <> "/" <> fromString (intercalate "/" $ S.rqPaths serverRq)
                          , queryString = fromString $ S.rqQuery serverRq
                          , method = fromString $ show $ S.rqMethod serverRq
                          , requestBody = RequestBodyLBS $ S.unBody serverRequestBody
                          , requestHeaders = [] ++ contentType ++ auth ++ accept ++ origin
                          }    
    httpLbs clientRq manager

  let type' = maybe "" id $ lookup "Content-Type" $ responseHeaders res
  S.setHeaderM "Content-Type" $ T.unpack $ E.decodeUtf8 type'
  S.resp (statusCode $ responseStatus res) $ S.toResponse $ responseBody res

{-
main = do
  req <- parseRequest "https://m.vipfengxiao.com"
  manager <- newManager tlsManagerSettings
  res <- httpLbs req manager
  putStrLn $ show $ responseHeaders res
-}

