

setwd("/Users/gupta88/427project/iGraph")
edgelist <- read.csv("Edgelist.csv", header = TRUE, as.is = T) # assume all recorded edges
IC <- read.csv("IC-nodes-file.csv", header = TRUE) # the nodes with prominent last names

# First Degree of connection -----------------------------------------------------

kapoor <- unique(IC$id[IC$last_name == "Kapoor"])
set.seed(123)
index <- sample(1:35, 5) # unknown index only appears this once
kapoor <- kapoor[grep("nm.*", kapoor)] 
kapoor <- droplevels(kapoor) # eliminate nf IDs

# get the first degree connections 
kapoor_edge <- rbind(edgelist[edgelist$from %in% kapoor,], edgelist[edgelist$to %in% kapoor,])

## iGraph ------------------------------------------------------
# This section will give a nice net work graph
# it is not necessary for community detection
# this graph looks kinda useless so i can remove this for now
library(visNetwork)
library(igraph)
library(data.table)
edges <- kapoor_edge[kapoor_edge$weight >= 4,]
nodes <- data.table(id = union(edges$from, edges$to))

visNetwork(nodes, edges, height = "800px") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T),
             nodesIdSelection = T)

# visNetwork(nodes, edges, height = "500px", width = "100%") %>%
#   visInteraction(hideEdgesOnDrag = TRUE) %>%
#   visLayout(randomSeed = 123)

## Community Detection ---------------------------------------------

source("http://michael.hahsler.net/SMU/ScientificCompR/code/map.R")
install.packages("devtools")
devtools::install_github("analyxcompany/ForceAtlas2")

kapoor_edge_net <- kapoor_edge[kapoor_edge$weight >= 3,]

library(igraph)
kapoor_net<-graph_from_data_frame(d=kapoor_edge_net, vertices = unique(union(kapoor_edge_net$from, kapoor_edge_net$to)),
                                  directed= F)

library(ForceAtlas2)
# The layout has some parameters need to be tuned
# basicly, "k" gives the repel power --> the greater the constant k the stronger the repulsion force between points
# "gravity" controls the closeness of different clusters, I set it to be 1 because I want the clusters to be more spread out
# the more "iteration", the clearer the graph, 300 is pretty good for now
layout <- layout.forceatlas2(kapoor_net, k=500, gravity=1, iterations=300, directed = FALSE, plotlabels = FALSE)


community <- cluster_louvain(kapoor_net)
pr<-page.rank(kapoor_net)$vector
btw<-betweenness(kapoor_net)

library(RColorBrewer)
colors<-brewer.pal(8,"Set1")
vertex.col<-colors[community$membership]

# 3 is green 
# 4 is purple
# 1 is red
# 6 is yellow
# 7 is brown
# 5 is orange
# 2 is blue

edge.start<-ends(kapoor_net,es=E(kapoor_net),names= F)[,1]
el.col<-vertex.col[edge.start]
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
               edge.width=E(kapoor_net)$Weight/6,
               edge.curved=0.4, edge.color= el.col)

# plot(kapoor_net, layout = layout)



# Get Community Members ---------------------------------------------------

# Create a data frame - try to use dplyr 

library(dplyr)
library(tidyverse)
# First create the dataset 
members <- membership(community)

comData <- data.frame(labels = labels(members), 
                      com = sapply(1:length(members), function(i) members[[i]]))
allComMembers <- data.frame(comData, 
                            pr = sapply(1:length(members), 
                                        function(i) pr[which(comData$labels[i] == labels(pr))][[1]]))
# sort to get it by page rank 

arrangedMembers <- allComMembers %>% arrange(com, desc(pr))

# Can select by the top few.
comData %>% 
  arrange(com) %>% 
  filter(com == "1")




