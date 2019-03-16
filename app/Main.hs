{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}

module Main where

import Lib
import System.Directory
import qualified Happstack.Server as S
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad(msum,join,guard)
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
                                 , originHeader :: String
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
    <*>
    (v .: "originHeader" <|> return "none")

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
  putStrLn "==== v1.0.0 ==="
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
buildRouter (ServerRouter path "none" location _) =
  if path == "/" then logic else S.dirs path logic
  where logic = S.serveDirectory S.EnableBrowsing ["index.html","mock.json"] location
buildRouter (ServerRouter path host "none" origin) = 
  if path == "/" then logic else S.dirs path logic
  where logic = selfproxy host origin  

{-
enterRouter = msum
 [ S.dir "mobile" $ selfproxy "https://m.vipfengxiao.com/mobile"
 , S.nullDir >> (S.ok $ S.toResponse ("nullDir"::String))
 , S.dir "api" $ S.dir "gw" $ selfproxy "https://www.vipfengxiao.com/api/gw"
 , S.dir "vipbclass" $ selfproxy "https://www.vipfengxiao.com/vipbclass"
 , S.dirs "/a" $ (S.ok $ S.toResponse ("abc"::String))

 ]
 -}

selfproxy :: String -> String -> S.ServerPart S.Response
selfproxy uri originHeader = do
  liftIO $ putStrLn uri
  serverRq <- S.askRq
  maybeV <- S.getHeaderM "Authorization"
  let auth = maybe [] (pure . ((,) "Authorization")) maybeV
  contentType' <- S.getHeaderM "Content-Type"
  let contentType = maybe [] (pure . ((,) "Content-Type")) contentType'
  acceptMaybe <- S.getHeaderM "Accept"
  let accept = maybe [] (pure . ((,) "Accept")) acceptMaybe
  originMaybe <- S.getHeaderM "origin"
  let origin = maybe [] (pure . ((,) "origin")) originMaybe
  let rorigin = if originHeader == "none" then origin else [("origin", fromString originHeader)]
  res <- liftIO $ do 
    initRq <- parseRequest uri
    manager <- newManager tlsManagerSettings
    serverRequestBody <- readMVar $ S.rqBody serverRq
    let clientRq = initRq { path = path initRq <> "/" <> fromString (intercalate "/" $ S.rqPaths serverRq)
                          , queryString = fromString $ S.rqQuery serverRq
                          , method = fromString $ show $ S.rqMethod serverRq
                          , requestBody = RequestBodyLBS $ S.unBody serverRequestBody
                          , requestHeaders = [] ++ contentType ++ auth ++ accept ++ rorigin
                          }    
    putStrLn $ show $ clientRq
    httpLbs clientRq manager

  let type' = maybe "" id $ lookup "Content-Type" $ responseHeaders res
  S.setHeaderM "Content-Type" $ T.unpack $ E.decodeUtf8 type'
  guard $ (statusCode $ responseStatus res) /= 404
  S.resp (statusCode $ responseStatus res) $ S.toResponse $ responseBody res

{-
main = do
  req <- parseRequest "https://m.vipfengxiao.com"
  manager <- newManager tlsManagerSettings
  res <- httpLbs req manager
  putStrLn $ show $ responseHeaders res
-}

