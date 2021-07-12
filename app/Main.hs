{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.AppM
import Web.Static.Static
import System.Directory
import qualified Data.ByteString as B
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as CT
import Control.Monad(msum,join,guard)
import System.IO
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.Monoid((<>))
import Data.String(fromString)
import qualified Data.Yaml as Y
import Data.Yaml((.:))
import Control.Applicative((<|>),empty)
import Control.Exception
import Data.String.Conversions(cs)
import System.FilePath.Posix((</>))
import Control.Concurrent
import Data.List
import Data.String.Conversions
import Data.Binary.Builder(fromByteString)
import qualified Network.Wai.Handler.WarpTLS as TLS

data ServerRouter = ServerRouter { serverPath :: String
                                 , serverRewrite :: String
                                 , locationPath :: String
                                 , hostHeader :: String
                                 }
data ServerConfig = ServerConfig { serverPort :: Int
                                 , serverRouters :: [ServerRouter]
                                 , serverHTTPSPort :: Int
                                 , serverHTTPSCertificate :: FilePath
                                 , serverHTTPSKey :: FilePath
                                 }

instance Y.FromJSON ServerRouter where
  parseJSON  = Y.withObject "routers" $ \v ->
    pure ServerRouter
    <*>
    (v .: "startWith" <|> v .: "path")
    <*>
    (v .: "rewrite" <|> return "none")
    <*>
    (v .: "locationPath" <|> return "none")
    <*>
    (v .: "hostHeader" <|> return "none")

instance Y.FromJSON ServerConfig where
  parseJSON  = Y.withObject "config" $ \v ->
    pure ServerConfig
    <*>
    v .: "port"
    <*>
    v .: "routers"
    <*>
    (v .: "HTTPSPort" <|> pure 0)
    <*>
    (v .: "HTTPSCertificate" <|> pure "")
    <*>
    (v .: "HTTPSKey" <|> pure "")

myOnException :: Maybe Request -> SomeException -> IO ()
myOnException (Just req) e = do
  putStrLn "**onException"
  putStrLn $ show req
  putStrLn $ displayException e
myOnException _ _ = pure ()
setting = setPort 7777
  $ setOnException myOnException
  $ setOnExceptionResponse exceptionResponseForDebug
  $ setTimeout (30*60*60)
  $ defaultSettings

-- | 增加守护进程
main :: IO ()
main = mainRun

mainRun = do
  hSetBuffering stdout LineBuffering
  putStrLn "==== v4.0.1 ==="
  config <- parsingConfig
  let sport = serverPort config
  putStrLn $ "==== 服务启动 port:" ++ show sport ++ " ===="
  forkIO $ do
    let httpsPort = serverHTTPSPort config
    let httpsserver = do
          putStrLn $ "==== tls服务启动 port:" ++ show httpsPort ++ " ===="
          TLS.runTLS
             (TLS.tlsSettings
               (serverHTTPSCertificate config)
               (serverHTTPSKey config))
             (setPort httpsPort setting)
             (toApplication createApp)
    if httpsPort == 0 then pure () else httpsserver
  runSettings (setPort sport $ setting)  $ toApplication $ createApp
  

  
createApp :: AppIO
createApp = do
  config <- liftIO $ parsingConfig
  msum $ map toApp $ serverRouters config

toApp :: ServerRouter -> AppIO
toApp router = do
  req <- getRequest
  let matchPath = serverPath router
  let matchPathDirs = toDirs matchPath
  let len = length matchPathDirs
  let reqDirs = map cs $ pathInfo req
  guard $ matchPathDirs == take len reqDirs
  -- 消费前缀
  mapM_ (consum . cs) matchPathDirs
  let tailDirs = drop len reqDirs
  going tailDirs router req
    -- 最终失败，将消费的前缀返还
    <|> (unconsum matchPathDirs >> empty)

unconsum :: [String] -> AppM ()
unconsum matchPathDirs  = do
  req <- getRequest
  let req' = req {pathInfo = map cs matchPathDirs ++ pathInfo req}
  putRequest req'

toDirs :: String -> [String]
toDirs "" = []
toDirs "/" = []
toDirs ('/':xs@('/':_)) = toDirs xs
toDirs ('/':xs) = takeWhile (/='/') xs: toDirs (dropWhile (/='/') xs)
toDirs xs = takeWhile (/='/') xs: toDirs (dropWhile (/='/') xs)

getRequestBody :: Request -> IO B.ByteString
getRequestBody req = do
  chunk <- getRequestBodyChunk req
  if chunk == "" then pure chunk else do
    rest <- getRequestBody req
    pure $ chunk <> rest
going :: [String] -> ServerRouter -> Request -> AppIO
going tailDirs router req | serverRewrite router /= "none" = do
  let uri = intercalate "/" tailDirs
  let reuri = serverRewrite router
  liftIO $ putStrLn $ "Request:"  <> reuri </> uri
  initRq <- HC.parseRequest (reuri </> cs uri)
  reqBody <- liftIO $ getRequestBody req
  let host =  hostHeader router
  let hheaders = if host /= "none" then [("host",cs host),("origin",cs host)] else []
  let ignoreFilter = not . (`elem`
        [ "host"
        , "Content-Length"
        ]) . fst
  let clientRq = initRq { HC.method=requestMethod req
                        -- , HC.secure = isSecure req
                        , HC.queryString = rawQueryString req
                        , HC.requestHeaders = (filter ignoreFilter $ requestHeaders req) ++ hheaders
                        , HC.requestBody = HC.RequestBodyBS $ cs reqBody
                        -- , HC.requestVersion = httpVersion req
                        }
  mvar <- liftIO $ newEmptyMVar
  liftIO $ forkIO $ do
    manager <- HC.newManager CT.tlsManagerSettings
    let sendMsg resp = do
          let body = HC.responseBody resp
          let headers = HC.responseHeaders resp
          putMVar mvar (HC.responseStatus resp, headers, "")
          loopMsg body
          where loopMsg body = do
                  bsBody <- HC.brRead body
                  putMVar mvar (status200,[],bsBody)
                  if bsBody == "" then pure () else loopMsg body
    HC.withResponse clientRq manager sendMsg
  (status,headers,_) <- liftIO $ takeMVar mvar
  let notIn = ["Content-Encoding","Transfer-Encoding"]
  mapM_ (\ (a,b) -> putHeader a b) $ filter (not . (`elem` notIn) . fst) $ headers

  guard $ status /= status404
  let bodyfn write flush = do
        let loop' = do
              (_,_,bs) <- takeMVar mvar
              write $ fromByteString bs
              flush
              if bs == "" then do
                putStrLn $ reuri </> uri <> "=>" <> show status
                flush
                pure () 
              else loop'
        loop'
  respStream status bodyfn
                     | otherwise = do
  let location = locationPath router
  let uri = intercalate "/" tailDirs
  liftIO $ putStrLn $ "fileServe:" ++ show (location, tailDirs)
  dirServe location ["index.html","mock.json"] <|> dirBrowse location


-- | 解析配置文件
parsingConfig :: IO ServerConfig
parsingConfig = do
  currentPath <- getHomeDirectory
  let configFile = currentPath ++ "/feserver.yaml"
  config <- (Y.decodeFileEither configFile) :: IO (Either Y.ParseException ServerConfig)
  case config of Left e -> error $ show e
                 Right r -> return r


