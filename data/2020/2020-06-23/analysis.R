# Title:    Caribou Location Tracking
# File:     analysis.R
# DATE:       June 23 2020
# Project:  tidytuesday/data/2020/2020-06-23

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(pacman, rio, tidyverse,DataExplorer,lobstr,maps,leaflet,esquisse,ggthemes,ggraph,tidytext,patchwork,wordcloud2,janitor,glue,scales,here)




# GET THE DATA################################


individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# LOOK AT THE DATA################################

View(individuals)
View(locations)

summary(locations)

# WRANGLE THE DATA ################################

### Lets see the top study sites ###

locations %>% 
  
  group_by(study_site) %>% 
  count(study_site,sort = TRUE)

### lets look just 2010 ###
locations_2010 <- locations %>% 
  filter(grepl("2010",timestamp) )

  
   



 
# VISUALIZE ################################
library(ggplot2)

ggplot(locations_2010) +
 aes(x = study_site, fill = season) +
 geom_bar() +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(season))

ggplot(locations_2010) +
 aes(x = timestamp, fill = study_site) +
 geom_histogram(bins = 30L) +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(study_site))



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base


