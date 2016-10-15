module WebOutput (toTheBrowser) where

import System.IO.Temp (openTempFile)
import Web.Browser (openBrowser)
import System.IO (hPutStr, hFlush)
import System.Directory (getTemporaryDirectory)

toTheBrowser content = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir "output.html"
  hPutStr handle content
  hFlush handle
  openBrowser path
