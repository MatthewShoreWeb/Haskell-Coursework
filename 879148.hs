-- MATHFUN
-- Haskell Coursework
-- UP879148

import Data.List

-- Types:
-- Definition of the Album type:

type Album = (String, String, Int, Int)

testData :: [Album]
testData = [("Greatest Hits", "Queen", 1981, 6300000),("Gold: Greatest Hits", "ABBA", 1992, 5400000),
  ("Sgt. Pepper's Lonely Hearts Club Band", "The Beatles", 1967, 5340000),("21", "Adele", 2011, 5110000),
  ("(What's the Story) Morning Glory?", "Oasis", 1995, 4940000),("Thriller", "Michael Jackson", 1982, 4470000),
  ("The Dark Side of the Moon", "The Pink Floyd", 1973, 4470000),("Brothers in Arms", "Dire Straits", 1985, 4350000),
  ("Bad", "Michael Jackson", 1987, 4140000),("Rumours", "Fleetwood Mac", 1977, 4090000),
  ("Greatest Hits II", "Queen", 1991, 3990000),("Back to Black", "Amy Winehouse", 2006, 3940000),
  ("The Immaculate Collection", "Madonna", 1990, 3700000),("25", "Adele", 2015, 3500000),
  ("Stars", "Simply Red", 1991, 3450000),("Come On Over", "Shania Twain", 1998, 3430000),
  ("x", "Ed Sheeran", 2014, 3380000),("Legend", "Bob Marley", 1984, 3380000),
  ("Bat Out of Hell", "Meat Loaf", 1977, 3370000),("Back to Bedlam", "James Blunt", 2004, 3360000),
  ("Urban Hymns", "The Verve", 1997, 3340000),("Bridge over Troubled Water", "Simon & Garfunkel", 1970, 3260000),
  ("1", "The Beatles", 2000, 3230000),("Spirit", "Leona Lewis", 2007, 3170000),("Crazy Love", "Michael Bublé", 2009, 3130000),
  ("No Angel", "Dido", 2000, 3090000),("White Ladder", "David Gray", 1998, 3020000),("The Fame", "Lady Gaga", 2009, 2990000),
  ("Only by the Night", "Kings of Leon", 2008, 2980000),("A Rush of Blood to the Head", "Coldplay", 2002, 2960000),
  ("Talk on Corners", "The Corrs", 1997, 2960000),("Spice", "Spice Girls", 1996, 2960000),
  ("Life for Rent", "Dido", 2003, 2900000),("Beautiful World", "Take That", 2006, 2880000),
  ("The Joshua Tree", "U2", 1987, 2880000),("Hopes and Fears", "Keane", 2004, 2860000),
  ("The War of the Worlds", "Jeff Wayne", 1978, 2800000),("X&Y", "Coldplay", 2005, 2790000),
  ("Jagged Little Pill", "Alanis Morissette", 1995, 2780000),("Tubular Bells", "Mike Oldfield", 1973, 2760000),
  ("Scissor Sisters", "Scissor Sisters", 2004, 2760000),("...But Seriously", "Phil Collins", 1989, 2750000),
  ("Tracy Chapman", "Tracy Chapman", 1988, 2710000),("Parachutes", "Coldplay", 2000, 2710000),
  ("The Man Who", "Travis", 1999, 2687500),("Greatest Hits", "ABBA", 1975, 2606000),
  ("I've Been Expecting You", "Robbie Williams", 1998, 2586500),("Come Away with Me", "Norah Jones", 2002, 2556650),
  ("Graceland", "Paul Simon", 1986, 2500000),("Ladies & Gentlemen: The Best of", "George Michael", 1998, 2500000)]

-- I
--Converts a single album tuple to a string:
showAlbums :: (String, String, Int, Int) -> String
showAlbums (name, artist, date, sales) = name ++ ", " ++ artist ++ ", " ++ show(date) ++ ", " ++ show(sales)

albumsToString :: [Album] -> String
albumsToString list = unlines(map showAlbums list)

-- II
--Function to show the top 10 albums:
top10 :: [Album] -> [Album]
top10 list = take 10 list

-- III
--Function to get albums between two years:
betweenTwoYears :: Int -> Int -> [Album] -> String
betweenTwoYears year1 year2 list = unlines(map showAlbums (filter (\(_,_, year,_) -> year >= year1 && year <= year2) list))

-- IV
--Function for showing albums with a given prefix:
displayPrefix :: String -> [Album] -> String
displayPrefix prefix list = unlines(map showAlbums (filter (\(name,_,_,_) -> isPrefixOf prefix name) list))

-- V
--Total sales:
artistSales :: String -> [Album] -> String
artistSales artistName list = "Artist Sales: " ++  show(sum [sales | (name,artist,year,sales) <- list, artist == artistName])

-- VI
--Total appearances:
totalAppearances :: [Album] -> String
totalAppearances list = unlines(map getAppearance (nub[artist | (_,artist,_,_) <- list]))

--Gets total appearances for an individual artist:
getAppearance :: String -> String
getAppearance artistName = artistName ++ ": " ++ show(length(filter (\(_,artist,_,_) -> artist == artistName) testData))

--VII
-- Remove the lowest selling album then add the new album:
changeList :: Album -> [Album] -> [Album]
changeList newAlbum list =  init(take (findLocation newAlbum list) list ++ [newAlbum] ++ reverse(take(50 - (findLocation newAlbum list)) (reverse list)))

--Finds the list location where the new album should be inserted:
findLocation :: Album -> [Album] -> Int
findLocation (name,artist,date,newSales) list = length(filter (\(_,_, _,sales) -> newSales < sales) list)


--VIII
--Increase the sales of a single album:
addSales :: String -> String -> Int -> [Album] -> [Album]
addSales title artist sales list = take (findLocation(increaseSales (getAlbum title artist list) sales) list) list ++ [increaseSales(getAlbum title artist list)sales] ++  reverse(take(50 - (findLocation (increaseSales(getAlbum title artist list)sales) list)) (reverse list))

--removeDuplicate :: String -> [Album] -> [Album]
--removeDuplicate albumTitle [(title, artist, date, sales)] = nubBy (title [(title, artist, date, sales)] where title == albumTitle)

getAlbum :: String -> String -> [Album] -> Album
getAlbum title artist list = head(filter (\(name,artistName,_,_) -> isPrefixOf title name && isPrefixOf artist artistName) list)

increaseSales :: Album -> Int -> Album
increaseSales (name,artist,date,sales) salesAdd = (name,artist,date,sales + salesAdd)


--Demos
demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (betweenTwoYears 2000 2010 testData)
demo 4  = putStrLn (displayPrefix "Th" testData)
demo 5  = putStrLn (artistSales "Queen" testData)
demo 6  = putStrLn (totalAppearances testData)
demo 7  = putStrLn (albumsToString(changeList ("Progress", "Take That", 2010, 2700000) testData))
demo 8  = putStrLn (albumsToString(addSales "21" "Adele" 400000 testData))


main :: IO ()
main = do
  fileText <- readFile "albums.txt"
  let list = read fileText :: [Album]
  putStrLn (albumsToString list)
  selection list


displayOptions :: IO ()
displayOptions = do
  putStrLn("Please select an option:")
  putStrLn("Option 1, print the list of albums.")
  putStrLn("Option 2, print the top 10 albums.")
  putStrLn("Option 3, get a list of albums between two years.")
  putStrLn("Option 4, get a list of albums with a given prefix.")
  putStrLn("Option 5, get the amount of sales for an artist.")
  putStrLn("Option 6, get the total amount of appearances for an artist.")
  putStrLn("Option 7, insert an album into the list.")
  putStrLn("Option 8, increase sales for a specific album.")
  putStrLn("Save & Exit: Terminates the program.")
  putStrLn("")



selection :: [Album] -> IO ()
selection list = do
  displayOptions
  putStrLn("Select an option (Example: 1):")
  select <- getLine

  if select == "save & exit"
    then do
     putStrLn("Saving and Exiting...")
     writeFile "albums.txt" (show(list))
    else if read(select) < 0 || read(select) > 8
    then putStrLn("Value out of range 1 -> 8.")
    else if read(select) == 1
      then do
        putStrLn(albumsToString list)
        selection list
      else if read(select) == 2
        then do
          putStrLn (albumsToString (top10 testData))
          selection list
        else if read(select) == 3
          then do
            putStrLn("Enter first year:")
            year1 <- getLine
            putStrLn("Enter second year:")
            year2 <- getLine
            let yearOne = read(year1)
            let yearTwo = read(year2)
            if yearOne < yearTwo
              then do
                putStrLn(betweenTwoYears yearOne yearTwo list)
                selection list
              else do
                putStrLn(betweenTwoYears yearTwo yearOne list)
                selection list
          else if read(select) == 4
            then do
              putStrLn("Enter prefix:")
              prefix <- getLine
              putStrLn (displayPrefix prefix list)
              selection list
            else if read(select) == 5
              then do
                putStrLn("Enter artist:")
                artist <- getLine
                putStrLn (artistSales artist list)
                selection list
            else if read(select) == 6
              then do
                putStrLn (totalAppearances list)
                selection list
            else if read(select) == 7
              then do
                putStrLn("Enter album title:")
                title <- getLine
                putStrLn("Enter album artist:")
                artist <- getLine
                putStrLn("Enter album date:")
                date <- getLine
                if read(date) > 2020 || read(date) < 1950
                  then do
                    putStrLn("Date out of range 1950 -> 2020.")
                    selection list
                  else putStrLn("Enter album sales:")
                sales <- getLine
                putStrLn(albumsToString(changeList (title, artist, read(date), read(sales)) list))
                selection list
            else if read(select) == 8
              then do
                putStrLn("Enter album title:")
                title <- getLine
                putStrLn("Enter album artist:")
                artist <- getLine
                putStrLn("Enter album sales increase:")
                salesIncrease <- getLine
                if read(salesIncrease) <= 0
                  then do
                    putStrLn("Sales increase cannot be equal to or less than 0.")
                    selection list
                  else do
                    putStrLn (albumsToString (addSales title artist 400000 list ))
                    selection list
              else putStrLn("Invalid Input")
