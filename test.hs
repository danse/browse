{-# LANGUAGE OverloadedStrings #-}
import WebOutput

s = Resource "script.js" "window.onload = alert('it works')"
i = Resource "index.html" "<script src=\"script.js\"></script>"

main = manyToTheBrowser [i, s]
