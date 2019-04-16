library(visNetwork)
library(igraph)
library(data.table)

library(ForceAtlas2)
library(igraph)
library(RColorBrewer)

library(dplyr)

# setwd("/Users/gupta88/427project/iGraph")
all.name.basic <- as.data.frame(readRDS("../IMDbRDS/name.basic.rds")) # for nameID to name labels
IC <- as.data.frame(readRDS("../IMDbRDS/IC-nodes-file.rds")) # the nodes with prominent last names


# Pick Edgelist -----------------------------------------------------------

pick.df <- function(family){
  edgelist <- switch(family,
                     Kapoor = as.data.frame(readRDS("../IMDbRDS/edgelist_kapoor.rds")),
                     Anand = as.data.frame(readRDS("../IMDbRDS/edgelist_anand.rds")))
  return(edgelist)
}
  


# First Degree ----------------------------------------------------------------

get_first_degree <- function(family){
  edgelist <- pick.df(family)
  fam_name <-  unique(IC$id[IC$last_name == family])
  fam_name <- fam_name[grep("nm.*", fam_name)] 
  fam_name <- droplevels(fam_name) # eliminate nf IDs
  
  fam_name_edge <- rbind(edgelist[edgelist$from %in% fam_name,], edgelist[edgelist$to %in% fam_name,])
  return(fam_name_edge)
}

# Smaller name.basic ------------------------------------------------------

sm.name.basic <- function(family){
  fam_name_edge <- get_first_degree(family)
  n <- unique(union(fam_name_edge$from, fam_name_edge$to))
  final <-  all.name.basic[all.name.basic$nconst %in% n, c(1,2)]
  return(final)
}

# Name Labels Function ----------------------------------------------------

get.name <- function (n, family) {
  name.basic <- sm.name.basic(family)
  nameID <- sapply(1:length(n), function(i) name.basic[which(name.basic$nconst == n[i]), 2])
  return(nameID) 
}

# Edges and Nodes ---------------------------------------------------------

get_edges <- function(family){
  fam_edge <- get_first_degree(family)
  edges <- fam_edge[fam_edge$weight >= 4,]
  return(edges)
}

get_nodes <- function(family){
  edges <- get_edges(family)
  ids <- union(edges$from, edges$to)
  names <- get.name(ids, family)
  nodes <- data.table(id = ids, label = names)
}


# Community Detection -----------------------------------------------------

communityDetection <- function(family){
  source("http://michael.hahsler.net/SMU/ScientificCompR/code/map.R")
  fam_edge <- get_first_degree(family)
  fam_edge_net <- fam_edge[fam_edge$weight >= 3,]
  fam_net <- graph_from_data_frame(d=fam_edge_net, 
                                   vertices = unique(union(fam_edge_net$from, fam_edge_net$to)),
                                    directed= F)
  layout <- layout.forceatlas2(fam_net, k=500, gravity=1, iterations=300, directed = FALSE, plotlabels = FALSE)
  
  community <- cluster_louvain(fam_net)
  pr <- page.rank(fam_net)$vector
  btw <- betweenness(fam_net)
  
  
  colors <- brewer.pal(8,"Set1")
  vertex.col <- colors[community$membership]
  
  edge.start <- ends(fam_net,es=E(fam_net),names= F)[,1]
  el.col <- vertex.col[edge.start]
  igraph.options(plot.layout= layout,
                 rescale = FALSE,
                 vertex.label.family="Helvetica",
                 vertex.label.font=1,
                 vertex.frame.color="gray70",
                 vertex.label.cex=map(btw,c(1,4))/3.5,
                 vertex.size=map(pr,c(5,16)),
                 vertex.label = NA,
                 vertex.label.color="black",
                 vertex.color= vertex.col,
                 edge.width=E(fam_net)$Weight/6,
                 edge.curved=0.4, edge.color= el.col)
  
  output <- list(fam_net, layout, unique(vertex.col), unique(community$membership))
  return(output)
}

# Community Members -------------------------------------------------------

# All names

allcommunityMembers <- function(family){
  fam_edge <- get_first_degree(family)
  fam_edge_net <- fam_edge[fam_edge$weight >= 3,]
  fam_net <- graph_from_data_frame(d=fam_edge_net, 
                                   vertices = unique(union(fam_edge_net$from, fam_edge_net$to)),
                                   directed= F)
  
  community <- cluster_louvain(fam_net)
  pr <- page.rank(fam_net)$vector
  
  members <- membership(community)
  comData <- data.frame(labels = labels(members), 
                        com = sapply(1:length(members), function(i) members[[i]]))
  allComMembers <- data.frame(comData, 
                              pr = sapply(1:length(members), 
                                          function(i) pr[which(comData$labels[i] == labels(pr))][[1]]))
  
  arrangedMembers <- allComMembers %>% 
                        arrange(com) %>% 
                        mutate(names = get.name(labels, family))
  return(arrangedMembers)
}

# Highest page rank names

topcommunityMembers <- function(family){
  
  all.df <- allcommunityMembers(family)
  
  arrangedMembers <- all.df %>% 
    group_by(com) %>%
    filter(pr == max(pr)) %>% 
    arrange(desc(pr))
  
  return(arrangedMembers)
}

# Data frame of communities

by.community <- function(family){
  
  every.name <- allcommunityMembers(family)
  mainmem <- topcommunityMembers(family)
  
  # get the size of the desired data frame
  n <- every.name %>% 
        group_by(com) %>% 
        summarise(maxtotal = n()) %>% 
        summarise(max(maxtotal))
  n <- unlist(n[[1]])
  
  full_df <- matrix(NA, nrow = n, ncol = length(mainmem[[2]]))
  
  # data frame of the communities 
  # first to last column is arranged largest to smallest communities (page rank)
  # each name is arranged accordingly within each community 
  
  for (i in mainmem[[2]]){
    display.names <- every.name %>% 
                      arrange(desc(pr)) %>% 
                      filter(com == i) %>% select(names)
    
    listofnames <- display.names[[1]]
    length(listofnames) <- n
    
    j <- which(mainmem[[2]] %in% i)
    full_df[,j] <- listofnames
  }
  
  return(full_df)
}







