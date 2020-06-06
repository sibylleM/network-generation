#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to simulate:                                                                                # 
# - "timeordered" small-world networks where only long-range connections are rewired                 #
# - random network of same size                                                                      #
# - label long-range connections  and randomly assign type (school,work,office)                      #
# - fixed ocal links are labelled "home"                                                             #
# Purpose: Input contact lists for new COVID-19 model                                                #
# Author: Sibylle Mohr                                                                               #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Here I generate a small-world networks using igraph, but not using the "rewiring" as implemented in the watts.strogatz.game(). 
# The applied method is the Newman–Watts variant (as described in Newman (2003)). 
# The Newman–Watts variant of a Watts–Strogatz network adds "shortcut" edges (i.e. the long-range connections)
# between pairs of nodes in the same way as in a Watts-Strogatz network but without removing edges from the underlying lattice. 
# More precisely, all local links are fixed but a fixed number of random links are lifted and dropped randomly on the network at a fixed rate. 
# To do this, I simply generate a lattice graph and then put a classical random graph on top of that.
# I use graph.lattice to generate a lattice, then erdos.renyi.game with the same number of vertices and a fixed probability to generate a random graph. 
# Then I combine the two graphs using the %u% (union) operator. 

rm(list=ls())

library(igraph)
require(data.table)
require(dplyr)

# rewire function
rewireF <- function(g) {
  #g <- g
  g<- graph.lattice(length=N, dim=1, circular=TRUE, directed=FALSE)
  g2 <- sample_gnp(N, 1/N, directed = FALSE) %>% set_edge_attr("label", value ="longrange")
  g3 <- g %u% g2
  # plot.igraph(g3, vertex.size = 3,vertex.label = NA, edge.label = NA, layout=layout_in_circle)
  dat <- igraph::as_data_frame(g3)
  #dat$weight <- rlnorm(nrow(dat), meanlog = log(6), sdlog =log(2)) # weight is sampled from log-normal distribution
  #g <- g3
  return(dat)
}


res <- list();
time <- (1:90);
N = 10000;

for(i in 1:length(time)){
  
  res[[i]] <- (rewireF(g))
}

# add identity to list elements
names(res) <- paste0(1:90)

# unlist into dataframe

# unlist nested list with id
contacts <- rbindlist(res, fill = T, idcol = "id")

# remove 
rm(res,i,time)

colnames(contacts)[1] <- "time"
## to avoid exponential notation when we simulate networks that are large (> 1,000,000 nodes), I need to convert to integer
contacts$to <- as.integer(contacts$to)
contacts$from <- as.integer(contacts$from)
contacts$time <- as.integer(contacts$time)



## recode long-range links as "home, "supermarket", "work"

contacts <- contacts %>%
  mutate(
    label = case_when(
      label == "longrange" ~ sample(c("work", "school", "supermarket"),
                                    nrow(contacts),
                             replace = TRUE,
                             prob = c(0.4, 0.4, 0.2)),
      TRUE ~ NA_character_
    )
  )
contacts$label <- ifelse(is.na(contacts$label), "home", contacts$label)

# length(which(contacts$label != "home"))

x <- rlnorm(length(which(contacts$label == "home")), meanlog = log(6), sdlog =log(2)) # weight "home contacts
y <- rlnorm(length(which(contacts$label != "home")), meanlog = log(16), sdlog =log(2)) # weight anything else

contacts$weight <- ifelse(contacts$label=="home", sample(x,length(which(contacts$label == "home")),replace=TRUE), sample(y,length(which(contacts$label != "home")),replace=TRUE))
# data.table::fwrite(contacts, file = "/Users/sibyllemohr/data/RAMP/simulated_contact_data/contacts.csv", row.names = FALSE)

ids <- data.frame(1:N)
colnames(ids) <- "id"

ids$age = sample(0:90,N,replace=TRUE) # assign age; should be replaced to sample from age distribution for geographic area in question

#fwrite(ids, file = "/simulated_contact_data/ids.csv", row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Generate homogeneous contact network                     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(igraph)
generate_random <- function(g) {
  g <- sample_gnp(N, 1/N, directed = FALSE)
  dat <- igraph::as_data_frame(g)
  #dat$weight <- rlnorm(nrow(dat), meanlog = log(6), sdlog =log(2))
  return(dat)
}

res.2 <- list();
time <- (1:90);
N <- 10000;

for(i in 1:length(time)){
  res.2[[i]] <- generate_random(g)
  
}

# add identity to list elements
names(res.2) <- paste0(1:90)

# unlist into dataframe
require(data.table)
require(dplyr)

# unlist nested list with id
contacts_homogeneous <- rbindlist(res.2, fill = T, idcol = "id")

rm(res.2,i,time)

colnames(contacts_homogeneous)[1] <- "time"
contacts_homogeneous$to <- as.integer(contacts_homogeneous$to)
contacts_homogeneous$from <- as.integer(contacts_homogeneous$from)
contacts_homogeneous$time <- as.integer(contacts_homogeneous$time)


## add some labels (maybe this should be completely random instead of the proportions that I use)
contacts_homogeneous$label <- ""

contacts_homogeneous <- contacts_homogeneous %>%
  mutate(
    label = case_when(
      label == "" ~ sample(c("work", "school", "supermarket", "home"),
                           nrow(contacts_homogeneous),
                                    replace = TRUE,
                                    prob = c(0.25, 0.25, 0.25, 0.25)),
      TRUE ~ NA_character_
    )
  )

contacts_homogeneous$weight <- rlnorm(nrow(contacts_homogeneous), meanlog = log(16), sdlog =log(2))

# fwrite(contacts_homogeneous, file ="homogeneous_contacts.csv", row.names = FALSE)
