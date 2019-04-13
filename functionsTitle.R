# Load libraries and relevant data ---------------------------------------------------------------
library(dplyr)
library(readr)
library(data.table)

title.basic <- as.data.table(readRDS("../IMDbRDS/title.basic.rds"))
name.basic <- as.data.table(readRDS("../IMDbRDS/name.basic.rds"))
staff_movie <- as.data.table(readRDS("../IMDbRDS/staff_movie2.rds"))

# Name to NameID ----------------------------------------------------------

get.nameID <- function (name.input) {
  # filter and keep the primaryNames that match name.inputs
  name.list <- name.basic %>% select(nconst, primaryName) %>% 
    filter(primaryName %in% name.input) %>% select(nconst)
  
  # outputs all possible unique nameIDs that belong to a possibly non-unique string of names
  return(name.list) 
}


# NameID to Name ----------------------------------------------------------

# Test function

get.name <- function (n) {
  nameID.list <- name.basic %>% select(nconst, primaryName) %>% 
    filter(nconst %in% n) %>% select(primaryName)
  
  return(nameID.list) # outputs a name for every nameID
}

# Function: Use Name and Title ID -----------------------------------------

name.title.id <- function (n) {
  # keep just the columns in staff_movie that match the nameIDs
  df <- staff_movie[,n, with = FALSE]
  int = intersect(na.omit(df[,n[1],with=F][[1]]),na.omit(df[,n[2],with=F][[1]]))
  
  # int is the intersection of the two columns
  return(int) 
}

# TitleID to Title Name --------------------------------------------------------

get.title <- function(titleID){
  # filter and keep the primaryTitles that match the titleIDs
  title.name <- title.basic %>% select(tconst, primaryTitle) %>% 
    filter(tconst %in% titleID) %>% select(primaryTitle)
  
  # outputs a string of characters (name of the movie)
  return(title.name)
}


# Get Unique NameID -------------------------------------------------------

specNameID <- function(name.input, title.input) {
  
  output <- c(NA,NA)
  for (i in 1:length(name.input)){
    # filter and keep the primaryNames that matches the i-th name.input
    name.list <- name.basic %>% select(nconst, primaryName, knownForTitles) %>% 
      filter(primaryName == name.input[i]) %>% arrange(primaryName)
    # count the number of nameIDs that is a result of a non-unique string
    count <- name.list %>% summarise(n()) 
    
    # a list of knownForTitles for each unique nameID
    listOfTitles <- lapply(1:count[[1]], function(i) get.title(strsplit(name.list[,3][i],",")[[1]]))
    
    # get the index of the list of knownForTitles that has the i-th title.input in it
    # the index is in line with the nameIDs as well
    # the index will indicate the unique nameID the user is looking for
    idx <- grep(title.input[i],listOfTitles)
    output[i] <- name.list[idx,1]
  }
  # outputs a list of unique nameID
  return(output)
}

# Intersection Function ---------------------------------------------------

name.title.characters <- function(names, titlesKnown) {
  
  # get nameID from input names and specified titles
  # checking if the string of names is unique or not
  if (dim(get.nameID(names))[1] == 2){
    nameID <- get.nameID(names)
  } else {
    nameID <- specNameID(names, titlesKnown)
  }
  
  # get the titleIDs that are the same between two names
  titleID <- name.title.id(nameID[,1])
  
  # get title name (use primaryName)
  return(get.title(titleID))
  
}






