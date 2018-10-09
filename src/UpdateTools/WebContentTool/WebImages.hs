{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module WebImages (downloadImages) where


import qualified Data.Digest.Pure.SHA       as SHA
import           Data.Maybe                             (catMaybes)
import qualified Data.Text                  as T
import           System.Directory
import           System.FilePath.Posix

import           Types.TrafficCamera
import           Utils.AtomicFile
import           Utils.HTTP

downloadImages :: [TrafficCamera] -> FilePath -> IO [TrafficCameraWithImageDigest] 
downloadImages cameras directory = do
    results <- sequence $ map (downloadImage directory) cameras
    return $ catMaybes results
        
downloadImage :: FilePath -> TrafficCamera -> IO (Maybe TrafficCameraWithImageDigest)
downloadImage path camera = do
    createDirectoryIfMissing True (dropFileName filepath)
    response <- downloadURL url filepath
    return $ case response of
        Left _ ->
            Nothing
            
        Right imageData ->
            Just (camera, imageDigest imageData)

    where
        url = T.unpack . tcImage $ camera
        filename = T.unpack . tcFilename $ camera
        filepath = combine path filename
        imageDigest = T.pack . SHA.showDigest . SHA.sha1


