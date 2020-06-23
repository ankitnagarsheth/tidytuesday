# Title:    American Slavery and Juneteenth
# File:     analysis.R
# DATE:       June 16 2020
# Project:  tidytuesday/data/2020/2020-06-16

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(pacman, rio, tidyverse,DataExplorer,esquisse,ggthemes,ggraph,tidytext,patchwork,wordcloud2,janitor,glue,scales,here)
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons



# GET THE DATA################################


blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')




# LOOK AT THE DATA################################

View(blackpast)
View(census)
View(slave_routes)
View(african_names)


# WRANGLE THE DATA ################################


slave_routes <- slave_routes %>% 
  mutate(port_arrival = as.factor(port_arrival))%>% 
   clean_names() %>% 
  remove_empty() %>% 
  as_tibble()

###Lets see the top arrival ports#####################
top_ports <- slave_routes %>% 
  mutate(port_arrival = str_to_lower(port_arrival)) %>% 
  group_by(port_arrival) %>% 
  summarise(sum_slaves = sum(n_slaves_arrived)) %>% 
arrange(desc(sum_slaves)) %>% 
  top_n(10)


###Lets see the top origin ports###################


top_origin_ports <- slave_routes %>% 
  mutate(port_origin = str_to_lower(port_origin)) %>% 
  group_by(port_origin) %>% 
  summarise(sum_slaves = sum(n_slaves_arrived)) %>% 
  arrange(desc(sum_slaves)) %>% 
  top_n(10)
# filter(str_ends(port_arrival,"ton"))


 
# VISUALIZE ################################


p1 <- ggplot(top_ports) +
  aes(x = port_arrival, fill = port_arrival, weight = sum_slaves) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  theme_minimal()

p1


p2 <- ggplot(top_origin_ports) +
  aes(x = port_origin, fill = port_origin, weight = sum_slaves) +
  geom_bar() +
  labs(x = "Origin Port", y = "Counts", title = "Top Origin Ports") +
  theme_minimal() +
  theme(legend.position = "top")



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base


