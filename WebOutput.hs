{-|

This module provides simple utility functions useful to present web
content to the user via their default browser. Use 'toTheBrowser' in
order to simply show some markup (HTML) to the user, and use
'manyToTheBrowser' with an array of 'Resource' in order to show a page
linking to multiple files, for example a page linking to a local style
file and a local Javascript file

-}
module WebOutput (
  toTheBrowser,
  Resource(..),
  manyToTheBrowser,
  ) where

import Control.Exception
import Web.Browser (openBrowser)
import System.IO
import System.Directory (createDirectory)
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Writes its argument to a file and opens the user's browser 
-- to show that file
toTheBrowser content = do
  let path = "browse.html"
  writeContentToTempFile content path
  openBrowser path
  pure ()

writeContentToTempFile content fileName = do
  handle <- openFile fileName ReadWriteMode
  hPutStr handle content
  hFlush handle

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
-- reference each other. A directory is created, the files
-- are written there, and the browser is opened on the first resource
-- passed as an argument to this function
manyToTheBrowser resources = do
  let containing = "browse"
  try $ createDirectory containing :: IO (Either SomeException ())
  mapM_ writeResource (combined containing)
  (openBrowser . location . head) (combined containing)
  pure ()
  where
    combined dir = map (applyLocation (combine dir)) resources
    applyLocation f (Resource a b) = Resource (f a) b
