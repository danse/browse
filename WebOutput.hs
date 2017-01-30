module WebOutput where

import System.IO.Temp (openTempFile, createTempDirectory)
import Web.Browser (openBrowser)
import System.IO (hPutStr, hFlush)
import System.Directory (getTemporaryDirectory)
import System.FilePath.Posix (combine, FilePath)
import qualified Data.Text as T
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

data Resource = Resource {
  location :: FilePath,
  content :: T.Text
}

writeResource (Resource loc con) = T.writeFile loc con

manyToTheBrowser resources = do
  dir <- getTemporaryDirectory
  containing <- createTempDirectory dir "output"
  mapM writeResource (combined containing)
  (openBrowser . location . head) (combined containing)
  where
    combined dir = map (applyLocation (combine dir)) resources
    applyLocation f (Resource a b) = Resource (f a) b
