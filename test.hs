{-# LANGUAGE OverloadedStrings #-}
import Browse

s = Asset "script.js" "window.onload = alert('it works')"
i = Asset "index.html" "<script src=\"script.js\"></script>"

main = browseLinked [i, s]
