### Output a single file to the browser

This library exposes a simple function:

    toTheBrowser :: String -> IO ()

It will accept a string, create an HTML page containing it and open
the user web browser on that page. This is convenient for example in
order to show the output of a script

### Output multiple files to the browser

    multiToTheBrowser :: [(String, Text)] -> IO ()

Provided with file names and contents, the library will create a
directory, write the files with the corresponding names and open the
user browser pointing to the first file in the list. Useful to show
pages which require assets divided in multiple files. The user will be
pointed to a location showing the path of the directory in their
filesystem, so that they could easily copy the contents and
publish. Supports only Posix file systems like in a Mac or a Linux.
