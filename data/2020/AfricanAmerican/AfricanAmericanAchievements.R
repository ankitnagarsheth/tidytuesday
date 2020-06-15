# Title:    African American Achievements
# File:     African American Achievements.R
# Project:  tidytuesday/data/2020/2020-06-09/

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(pacman, rio, tidyverse,DataExplorer,tidytext,wordcloud2,htmlwidgets,ggwordcloud)
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# GET THE DATA################################


firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


View(firsts)

glimpse(firsts)
library(DataExplorer)
?DataExplorer

create_report(firsts)

View(science)
glimpse(science)
create_report(science)
glimpse(firsts)


# WRANGLE THE DATA ################################

##Lets clean the accomplishment shwich has lot of first african american verbiage
firsts <- firsts %>% as_tibble() %>% 
  mutate(year = as.factor(year)) 
firsts <- firsts %>% mutate(accomplishment = str_replace(accomplishment,"First free African-American" , " ")) %>% 
print()
firsts <- firsts %>% mutate(accomplishment = str_replace(accomplishment,"First known African-American" , " ")) %>% 
  print()
firsts <- firsts %>% mutate(accomplishment = str_replace(accomplishment,"First African-American" , " ")) %>% 
  print()
firsts <- firsts %>% mutate(accomplishment = str_replace(accomplishment,"First" , " ")) %>% 
  print()

##Lets groupby and see how is the data looking

firsts %>% group_by(gender) %>% count()

firsts %>% group_by(category,gender) %>% count(sort=TRUE) 

firsts %>% group_by(category) %>% add_count() %>% 
  arrange(desc(n))

firsts %>% colnames()

firsts %>% pull(category) %>% 
unique() %>%
print()

glimpse(firsts)

firsts %>% nest(gender)



## Lets replace the Gender verbiage to male and female

firsts <- firsts %>% mutate(gender = ifelse(gender == "African-American Firsts", "Male", "Female")) %>% 
  
print(n=10)


View(firsts)

##Lets make inventions same in both the data frames and then join

glimpse(science)

colnames(science)[5] <- "accomplishment"
  
  print()


# VISUALIZE ################################

##Top Categories by Gender in the Firsts dataset
ggplot(firsts) +
  aes(x = category, fill = gender) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  theme_bw()
  
##Lets build a wordcloud and see for the accomplishments
  
  unique_stopwords <- data.frame(word = c("american","african"))
  
accomplishments <-  firsts %>% select(accomplishment) %>% 
  unnest_tokens(word,accomplishment) %>% 
  anti_join(stop_words) %>% 
  count(word,sort = TRUE) %>% 
  print()

acc1 <- accomplishments %>%  anti_join(unique_stopwords, by = "word") %>% 
count(word,sort = TRUE) %>% 
  print()

mywordcloud <- wordcloud2(acc1,size =0.5, color = "random-light", backgroundColor = "black")


# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
