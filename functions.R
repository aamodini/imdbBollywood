# Libraries and Data ---------------------------------------------------------------
library(sqldf)
library(readr)
library(data.table)
# setwd("427project")
# principal<-as.data.frame(fread("title.principals.tsv"))
# crew <- as.data.frame(fread("rawData/title.crew.tsv"))

# akas <- as.data.frame(fread("title.akas.tsv"))
# title.basic <- as.data.frame(fread("title.basics.tsv"))
# name.basic <- as.data.frame(fread("name.basics.tsv"))
# staff_movie <- as.data.frame(fread("staff_movie.tsv"))

# Specify for 2 nameIDs 

# Alternate Method --------------------------------------------------------

# colnames(df_interest) <- c("n1", "n2")

# intersec.sql <- sqldf(" 
#   SELECT n1
#   FROM df_interest
#   INTERSECT
#   SELECT n2
#   FROM df_interest
#   ")
# outputs the name id 
# name.id <- colnames(staff_movie[name.index[1]]) 

# Without SQL

# Map(intersect, staff_movie[,1], staff_movie[,2])

# This second one will omit na values completely. This might be easier to use to generalize a function.
# Can reproduce similar results in SQL using paste0() into a query

# Map(intersect, staff_movie[complete.cases(staff_movie[,1]),1], staff_movie[complete.cases(staff_movie[,2]),2]) 


# Name to NameID ----------------------------------------------------------

get.nameID <- function (name.input) {
  name.matrix <- matrix(NA, 1, length(name.input)) # matrix for storage
  
  # Name in characters to nameID 
  
  for (i in 1:length(name.input)){
    name.matrix[1,i] <- name.basic[which(name.basic$primaryName == name.input[i]), 1]
  }
  
  return(name.matrix) # outputs 2 corresponding name IDs 
}

# NameID to Name ----------------------------------------------------------

# had to create this function to test.

get.name <- function (n) {
  nameID.matrix <- matrix(NA, 1, length(n)) # matrix for storage
  
  # Name in characters to nameID 
  
  for (i in 1:length(n)){
    nameID.matrix[1,i] <- name.basic[which(name.basic$nconst == n[i]), 2]
  }
  
  return(nameID.matrix) # outputs 2 corresponding name IDs 
}

# Function: Use Name and Title ID -----------------------------------------

name.title.id <- function (n1, n2) {
  # nameID <- c(n1, n2) # this will be the input converted to the name ID
  # name.index <- sapply(1:length(nameID), function (x) which(colnames(staff_movie) == nameID[x])) #gives name indices 
  # df_interest <- data.frame(staff_movie[,name.index])
  # 
  # # get nameID to recognize in SQL syntax
  # getQuote <- names(sqldf("SELECT * FROM df_interest WHERE 1 = 0"))
  # 
  # # generalize the query
  # query <- paste0("SELECT ", getQuote[1]," FROM df_interest INTERSECT SELECT ", getQuote[2], " FROM df_interest")
  # intersect.sql <- sqldf(query)
  # return(intersect.sql)
  
  x <- staff_movie[,colnames(staff_movie) == n1]
  y <- staff_movie[,colnames(staff_movie) == n2]
  return(data.frame(int=intersect(x,y)))
}

# n <-  c("nm0760044", "nm0665381")


# TitleID to Title Name --------------------------------------------------------

get.title <- function(titleID){
  
  if (sum(is.na(titleID)) == 0){
    titleID <- titleID
  } else {
    titleID <- titleID[complete.cases(titleID),]
  }
  
  titles <- matrix(NA, 1, 1000)
  
  for (i in 1:length(titleID)){
    titles[1,i] <- title.basic[which(title.basic$tconst == titleID[i]), 3]
    title.name <- titles[!is.na(titles)]
  }
  
  return(title.name)
}









