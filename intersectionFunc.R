source("functions.R")
source("Get.Unique.NameID.R")

# name.input <- c("Ameeta", "Nasir Hussain")
# title.input <- c("Rishte Naahte","Koi Tujh Sa Kahan")
# 
# # All tests ----
# 
# # Name to NameID
# # run the below for testing
# name.input = c("Ameeta", "Nasir Hussain")
# get.nameID(name.input)
# 
# # NameID to Name 
# # this is run to get the names for the final test
# n <- c("nm0024526", "nm0403827")
# n.nonunique <- c("nm0024526", "nm3550751")
# test.names <- get.name(c("nm0196409", "nm0762507"))
# 
# # Function: Use Name and Title ID
# # testing
# titleID <- name.title.id("nm0196409", "nm0762507")
# 
# # TitleID to Title Name
# get.title(titleID)
# 
# # Try these
# # One test Unique Names ----
# n <- c("nm0557630", "nm0694890")
# get.name(n)
# 
# names <- c("Homi Master", "Prabhashankar")
# titlesKnown <- c("Fankdo Fituri", "Bhedi Trishul")

# Two test: two double -----
# colnames(staff_movie) # pick random nm
# 
# n <- c("nm0403827", "nm0474801")
# tt <- c("tt0047990", "tt0054910")
# 
# names <- c("Nasir Hussain", "Dilip Kumar")
# titlesKnown <- c("Devdas", "Gunga Jumna")
# # 
# name.title.id(n[1],n[2]) # check for intersection

# nm <- name.basic[which(name.basic$primaryName == names[2]), 1]
# 
# # Look at all the titles for these first names 
# knownForTitles <- sapply(1:length(nm), function(x) strsplit(as.character(name.basic[name.basic$nconst == nm[x], 6]), ','))
# 
# # Omit the elements where there are null values
# nm <- nm[which(knownForTitles != "\\N")]
# knownForTitles <- knownForTitles[knownForTitles != "\\N"]
# listOfTitles <- sapply(1:length(knownForTitles), function(x) get.title(knownForTitles[[x]]))

# A final function that does all 3 ----------------------------------------

name.title.characters <- function(names, titlesKnown) { # n1 and n2 come from the two character name inputs
  
  # get nameID from input names and specified titles
  if (length(specNameID(names, titlesKnown)) == 1){
    nameID <- get.nameID(names)
  } else {
    nameID <- specNameID(names, titlesKnown)
  }
  
  # get the titleIDs that are the same between two names
  titleID <- name.title.id(nameID[1], nameID[2])
  
  # get title name (use primaryName)
  return(get.title(titleID))
  
}

# names <- c("Hrishikesh Mukherjee", "Sachin Bhowmick")
# titlesKnown <- c("Anand", "Jhooth Bole Kauwa Kaate")
# 
# name.title.characters(names, titlesKnown)


