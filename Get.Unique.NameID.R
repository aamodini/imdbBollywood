# source("../functions.R")

# Tests -----------------------------------------------------------
# get the nameID where its primaryProfession is specified. 
# Specify only if there 2 people are known for the same movie.
# profession <- sapply(1:length(nm) , function (x) name.basic[name.basic$nconst == nm[x], 5])

# Check if its all working right: ------
# indianNames <- colnames(staff_movie)
# nameBasicSubset <- head(name.basic[name.basic$nconst %in% indianNames, ])


# Specify NameID ----------------------------------------------------------

specNameID <- function(name.input, title.input) {
  
  nameID <- list()
  for (name in 1:length(name.input)) {
    # NameID for one name
    nm <- name.basic[which(name.basic$primaryName == name.input[name]), 1]
    
    # Look at all the titles for these first names 
    knownForTitles <- sapply(1:length(nm), function(x) strsplit(as.character(name.basic[name.basic$nconst == nm[x], 6]), ','))
    
    # Omit the elements where there are null values
    nm <- nm[which(knownForTitles != "\\N")]
    knownForTitles <- knownForTitles[knownForTitles != "\\N"]
    listOfTitles <- sapply(1:length(knownForTitles), function(x) get.title(knownForTitles[[x]]))
    
    # Find the index for which the titles match up
    
    for (i in 1:length(listOfTitles)) {
      if (sum(listOfTitles[[i]] == title.input[name]) == 1) {
        nameID[name] <- nm[i]
      }
    }
  }
  return(unlist(nameID))
}

# sapply(1:length(knownForTitles[[5]]), function(x) get.title(knownForTitles[[5]][x]))
# length(name.basic[name.basic$knownForTitles == "\\N",1])
# length(name.basic[which(name.basic$knownForTitles == "\\N" & name.basic$primaryProfession == ""),1])


