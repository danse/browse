
This library exposes two simple functions

### Browse some markup

    browse :: String -> IO ()

It will accept a string, create an HTML page containing it and open
the user web browser on that page. This is convenient for example in
order to show the output of a script

### Browse linked assets

    Asset :: FilePath -> T.Text -> Asset
    browseLinked :: [Asset] -> IO ()

Provided with file names and contents, the function will create a
directory, write the files with the corresponding names and open the
user browser pointing to the first file in the list.

Useful to show pages which require assets divided in multiple files.

Supports only Posix file systems like in a Mac or a Linux. Tested with
flat file names, for instance "file.css" rather than "style/file.css".
