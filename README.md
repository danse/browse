This library exposes a simple function:

    toTheBrowser :: String -> IO ()

It will accept a string, create an HTML page containing it and open
the user web browser on that page. This is convenient for example in
order to show the output of a script
