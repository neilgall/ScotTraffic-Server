{-# LANGUAGE OverloadedStrings #-}

module Utils.HTTP (openURL, openURI, maybeOpenURL, maybeOpenURI, downloadURL) where
    
import           Control.Concurrent          (threadDelay)
import           Control.Exception           (catch, Exception)
import           Control.Monad
import qualified Data.ByteString.Lazy as BS
import           Data.Either.Combinators     (rightToMaybe)
import           Debug.Trace
import           Network.HTTP
import           Network.Stream              (Result, ConnError(..))
import           Network.URI
import           System.FilePath.Posix

import           Utils.AtomicFile
    
-- Open an HTTP connection to a URL and return the response body as a ByteString
-- (deprecated - use openURI)
openURL :: String -> IO (Result BS.ByteString)
openURL url = case parseURI url of
    Nothing ->
         return $ Left . ErrorMisc $ "can't parse URL " ++ url
         
    Just uri  -> do
        openURI uri

-- Open an HTTP connection to a URI and return the response body as a ByteString
openURI :: URI -> IO (Result BS.ByteString)
openURI uri = repeatOpenURL uri 10
    
repeatOpenURL :: URI -> Int -> IO (Result BS.ByteString)
repeatOpenURL uri 0 = do
    putStrLn ("GET " ++ (show uri) ++ " too many retries")
    return $ Left . ErrorMisc $ "failed to open " ++ (show uri)

repeatOpenURL uri n = do
    let request = mkRequest GET uri
    response <- catch (simpleHTTP request) catchIOError
    case response of
        Left e -> do
            tryAgain $ show e
            
        Right _ -> do
            body <- getResponseBody response
            if BS.null body then do
                tryAgain "empty"
                
            else do
                putStrLn ("GET " ++ (show uri) ++ " OK")
                return $ Right body
  where
      catchIOError :: IOError -> IO (Result a)
      catchIOError = return . Left . ErrorMisc . show
      
      tryAgain err = do
          putStrLn ("GET " ++ (show uri) ++ " " ++ err)
          threadDelay 500000 -- half a second
          repeatOpenURL uri (n-1)

maybeOpenURL :: String -> IO (Maybe BS.ByteString)
maybeOpenURL = (liftM rightToMaybe) . openURL

maybeOpenURI :: URI -> IO (Maybe BS.ByteString)
maybeOpenURI = (liftM rightToMaybe) . openURI

-- Open an HTTP connection to a URL and download the contents to a local file
downloadURL :: String -> FilePath -> IO (Result BS.ByteString)
downloadURL url filepath = do
    let encodedURL = concatMap (\c -> if c == ' ' then "%20" else [c]) url
    response <- openURL encodedURL
    case response of
        Left e ->
            return $ Left e
        
        Right content -> do
            atomicFile filepath (flip BS.writeFile content)
            return $ Right content
