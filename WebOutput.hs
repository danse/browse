{-|

This module provides simple utility functions useful to present web
content to the user via their default browser, creating temporary
files. Use 'toTheBrowser' in order to simply show some markup (HTML)
to the user, and use 'manyToTheBrowser' with an array of 'Resource' in
order to show a page linking to multiple files, for example a page
linking to a local style file and a local Javascript file

-}
module WebOutput (
  toTheBrowser,
  Resource(..),
  manyToTheBrowser,
  ) where

import System.IO.Temp (openTempFile, createTempDirectory)
import Web.Browser (openBrowser)
import System.IO (hPutStr, hFlush)
import System.Directory (getTemporaryDirectory)
import System.FilePath.Posix (combine, FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Writes its argument to a temporary file and opens the user's browser 
-- to show that file. The returned boolean can be ignored
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

-- | Use the Resource type to create resources that are linking to
-- each other, and you want to show to the user all together. For
-- example one resource could contain some HTML linking to another
-- resource containing Javascript or other. 
data Resource = Resource {
  location :: FilePath, -- ^ 'location' should contain a relative URL where the resource will be placed. For example this could be @"script.js"@ for a Javascript file
  content :: T.Text
}

writeResource (Resource loc con) = T.writeFile loc con

-- | Present multiple files via the user's browser, so that they can
-- reference each other. A temporary directory is created, the files
-- are written there, and the browser is opened on the first resource
-- passed as an argument to this function. The returned boolean can be
-- ignored
manyToTheBrowser resources = do
  dir <- getTemporaryDirectory
  containing <- createTempDirectory dir "output"
  mapM writeResource (combined containing)
  (openBrowser . location . head) (combined containing)
  where
    combined dir = map (applyLocation (combine dir)) resources
    applyLocation f (Resource a b) = Resource (f a) b
