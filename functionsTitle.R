# Load libraries and relevant data ---------------------------------------------------------------
library(dplyr)
library(readr)
library(data.table)

library(tidyverse)
library(igraph)
library(RColorBrewer)

title.basic <- as.data.table(readRDS("../IMDbRDS/title.basic.rds"))
name.basic <- as.data.table(readRDS("../IMDbRDS/name.basic.rds"))
staff_movie <- as.data.table(readRDS("../IMDbRDS/staff_movie2.rds"))
rating <- as.data.table(readRDS("../IMDbRDS/title.ratings.rds")) 

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


# Visualizations ----------------------------------------------------------

### --------------- Network between two staff ------------------- 


#######################
## Fig. of Time Line ##
#######################

get.sim.movie <- function(names,titlesKnown){
  if (dim(get.nameID(names))[1] == 2){
    nameID <- get.nameID(names)
  } else {
    nameID <- specNameID(names, titlesKnown)
  }
  
  id1 <- nameID[[1]][1]
  id2 <- nameID[[1]][2]
  
  ### find similar movies
  link <- name.title.id(c(id1,id2))
  
  sim.movie <- as.data.table(link)
  sim.movie$from <- id1
  sim.movie$to <- id2
  sim.movie$link <- as.character(sim.movie$link)
  
  ### find individual movies
  movie1 <- na.omit(staff_movie[,id1, with = FALSE])
  names(movie1)[1] <- "link"
  movie1$link <- as.character(movie1$link)
  
  movie2 <- na.omit(staff_movie[,id2, with = FALSE])
  names(movie2)[1] <- "link"
  movie2$link <- as.character(movie2$link)
  
  ### add info of movies' releasing year and ratings
  basics <- title.basic
  names(basics)[1] <- "link"
  names(rating)[1] <- "link"
  
  # match all by movie ids
  junk <- left_join(sim.movie, basics, by = "link")
  junk <- left_join(junk, rating, by = "link")
  sim.movie <- select(junk, c(from, to, link, startYear, primaryTitle, averageRating))
  
  junk <- left_join(movie1, rating, by = "link")
  movie1 <- select(junk, c(link, averageRating))
  
  junk <- left_join(movie2, rating, by = "link")
  movie2 <- select(junk, c(link, averageRating))
  
  # create a factor identifying decades 
  sim.movie$decades <- cut(as.numeric(sim.movie$startYear), 
                           breaks = seq(1920, 2020, 10),
                           labels = paste(seq(1920, 2010, 10),"s",sep = ""))
  return(sim.movie)
}

timeLine.df <- function(names, titlesKnown){
  
  sim.movie <- get.sim.movie(names, titlesKnown)
  # count number of movies in each decades
  plot.df <- as.data.frame(table(sim.movie$decades))
  
  # first time co-op
  first <- head(which(plot.df$Freq > 0), n = 1)
  
  # last time co-op
  last <- tail(which(plot.df$Freq > 0), n = 1)
  
  # filter the data to
  plot.df <- plot.df[first:last,]
  
  return(plot.df)
}



#######################
### Fig. of Ratings ###
#######################

# function for creating the rate categorys
rate_cat <- function(movie.df) {
  # input is a data frame containing a column named "averageRating"
  # output is the same data frame with extra column named "rate"
  movie.df$rate <- cut(as.numeric(movie.df$averageRating),
                       breaks = seq(0, 10, 2),
                       labels = c("Terrible (IMDB 0~2)",
                                  "Bad (IMDB 2~4)",
                                  "Mediocre (IMDB 4~6)",
                                  "Good (IMDB 6~8)",
                                  "Excellent (IMDB 8~10)"))
  return(movie.df)
}

pieCharts.df <- function(names, titlesKnown){
  
  sim.movie <- get.sim.movie(names, titlesKnown)
  sim.movie <- rate_cat(sim.movie)
  movie1 <- rate_cat(movie1)
  movie2 <- rate_cat(movie2)
  
  # create plot df
  plot.df <- as.data.frame(table(sim.movie$rate))
  plot.df$type <- "Together [unit: %]"
  
  temp.df <- as.data.frame(table(movie1$rate))
  temp.df$type <- paste(names[1],"[unit:%]")
  plot.df <- rbind(plot.df, temp.df)
  
  temp.df <- as.data.frame(table(movie2$rate))
  temp.df$type <- paste(names[2],"[unit:%]")
  plot.df <- rbind(plot.df, temp.df)
  
  plot.df <- plot.df %>% group_by(type) %>%
    mutate(pct = round((Freq / sum(Freq)*100),0))
  plot.df$pct[plot.df$Freq == 0] <- NA
  
  # plot a pie chart
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  
  return(plot.df)
}






