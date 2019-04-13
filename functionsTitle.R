# Load libraries and relevant data ---------------------------------------------------------------
library(dplyr)
library(readr)
library(data.table)

title.basic <- as.data.table(readRDS("../IMDbRDS/title.basic.rds"))
name.basic <- as.data.table(readRDS("../IMDbRDS/name.basic.rds"))
staff_movie <- as.data.table(readRDS("../IMDbRDS/staff_movie2.rds"))

# Name to NameID ----------------------------------------------------------

get.nameID <- function (name.input) {
  name.list <- name.basic %>% select(nconst, primaryName) %>% 
    filter(primaryName %in% name.input) %>% select(nconst)
  return(name.list) # outputs all possible nconsts
}


# NameID to Name ----------------------------------------------------------

# Test function

get.name <- function (n) {
  nameID.list <- name.basic %>% select(nconst, primaryName) %>% 
    filter(nconst %in% n) %>% select(primaryName)
  return(nameID.list) # outputs a name for every nconsts 
}

# Function: Use Name and Title ID -----------------------------------------

name.title.id <- function (n) {
  df <- staff_movie[,n, with = FALSE]
  int = intersect(na.omit(df[,n[1],with=F][[1]]),na.omit(df[,n[2],with=F][[1]]))
  return(int)
}

n <-  c("nm0760044", "nm0665381")
titleID <- name.title.id(n)

# TitleID to Title Name --------------------------------------------------------

get.title <- function(titleID){
  title.name <- title.basic %>% select(tconst, primaryTitle) %>% 
    filter(tconst %in% titleID) %>% select(primaryTitle)
  return(title.name)
}









