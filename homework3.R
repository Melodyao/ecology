rm(list = ls())

# Read in the doubs and delete the site 8 
# find which is the most species site 
# and the most widespread species

library("ade4")
library("tidyverse")

data(doubs)
fish <- doubs$fish[-8, ]
site_species <- apply(fish > 0, 1, sum) 
sort(site_species) #29
species_site <- apply(fish > 0, 2, sum) 
sort(species_site) #Lece 

# indentify groups of species 
# Which groups of species are related to these groups of sites?

library("permute")
library("lattice")
library("vegan")

fish.t <- t(fish)
fish.t.chi <- decostand(fish.t, "chi.square")
fish.t.d <- dist(fish.t.chi)
fish.chi.single <- hclust(fish.t.d, method = "single")     
plot(fish.chi.single)
rect.hclust(fish.chi.single, k=5) 

# cluster species by site
fish.chi <- decostand(fish, "chi.square")
fish.d <- dist(fish.chi)
fish.d.complete <- hclust(fish.d, method = "complete")
plot(fish.d.complete)
rect.hclust(fish.d.complete, k=3) 

# cluster env by site
env <- doubs$env[-8, ]
env.chi <- decostand(env, "chi.square")
env.d <- dist(env.chi)
env.d.complete <- hclust(env.d, method = "complete")
plot(env.d.complete)
rect.hclust(env.d.complete, k=3) 

# Do RDA analysis
# find which environmental variables cause a community to vary across a landscape?

decorana(fish)
fish.h <- decostand(fish, "hellinger") 
fish.rda <- rda(fish.h ~ ., env)
summary(fish.rda)
