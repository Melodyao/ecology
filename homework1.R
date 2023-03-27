#########  homework 1 ###########
#Loading libraries of tidyverse and ade4, as well as the doubs data into R, and checking what the data looks like and the class of the data.
library("ade4")
library("tidyverse")
data(doubs,package = "ade4") #Loading the doubs data
doubs #checking what doubs looks like
class(doubs) #checking the class of doubs

#Turning the row names into a column called site, then convert the data frame to a tibble, named it env_tb.
library(dplyr)
a <- tibble::rownames_to_column(doubs$env, "site") #Turning the row names into a column called site
a
env_tb <- as_tibble(a) #convert the data frame to a tibble, named it env_tb.
env_tb

#Concatenating several steps with %>% pipe, and name the final variable as env_final.
 #One of the columns is dfs. It indicates the distance from sources. Extract and remain the data of the dfs with more than 1000 km.
 #Only interested in these columns: site, dfs, slo, flo, pH, nit, oxy. Select these columns for further analysis.
 #Some column names are not intuitive. Rename them as follows: dfs to distsour, slo to slope, flo to flowrate, nit to nitrogen, oxy to oxygen.
 #Order the data. Arrange the data first by slope in ascending order, and then by pH in descending order.
env_final <- env_tb %>% 
  filter(dfs>1000) %>% 
  select(site, dfs, slo, flo, pH, nit, oxy) %>% 
  rename(distsour=dfs, slope=slo, flowrate=flo, nitrogen=nit, oxygen=oxy) %>% 
  arrange(slope, desc(pH))
env_final