{-# LANGUAGE OverloadedStrings #-}
import WebOutput

s = ("script.js", "window.onload = alert('it works')")
i = ("index.html", "<script src=\"script.js\"></script>")

main = multiToTheBrowser [i, s]
