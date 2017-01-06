module WebOutput (toTheBrowser, multiToTheBrowser) where

import System.IO.Temp (openTempFile, createTempDirectory)
import Web.Browser (openBrowser)
import System.IO (hPutStr, hFlush)
import System.Directory (getTemporaryDirectory)
import System.FilePath.Posix (combine)
import qualified Data.Text.IO as T

toTheBrowser content = do
  dir <- getTemporaryDirectory
  path <- writeContentToTempFile dir content "output.html"
  openBrowser path
    where
      writeContentToTempFile dir content fileName = do
        (path, handle) <- openTempFile dir "output.html"
        hPutStr handle content
        hFlush handle
        return path


-- this will open the browser on the first content
multiToTheBrowser namesAndContents = do
  dir <- getTemporaryDirectory
  containing <- createTempDirectory dir "output"
  mapM (uncurry T.writeFile) (combined containing)
  (openBrowser . fst . head) (combined containing)
    where
      combined dir = map (applyFst (combine dir)) namesAndContents
      applyFst f (a, b) = (f a, b)
