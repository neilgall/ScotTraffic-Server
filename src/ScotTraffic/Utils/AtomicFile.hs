module Utils.AtomicFile (atomicFile) where

import System.Directory
import System.FilePath.Posix

-- Atomically write a file. Any existing file is atomically replaced.
atomicFile :: FilePath -> (FilePath -> IO ()) -> IO ()
atomicFile path write = do
    tmpDir <- getTemporaryDirectory
    let tmp = combine tmpDir (takeFileName path)
    write tmp
    renameFile tmp path
