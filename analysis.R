## LOAD PACKAGES ####
library(tidyverse)  # Need for dplyr and ggplot2
library(maps)       # Need for 48 states map 
library(mapproj)    # Need for update pro
library(albersusa)  # Need for 50 states map <-- devtools::install_github("hrbrmstr/albersusa")
library(maptools)   # Need for foritifying 50 states map


## READ IN DATA AND ORGANIZE ####
# Data for analysis
data_elections = read.table("data/data_us_presidential_elections.txt", header=T, sep="\t")

data_electoral = read.table("data/data_electoral_votes.txt", header=T, sep="\t")

# Get map - continental 48 states version
# states = map_data("state") %>%
#   # Change name of column so it matches other data
#   rename(state = region)

# Get map - all 50 states version
states_map = usa_composite()

states = fortify(states_map, region="name") %>%
  # Change name of column so it matches other data
  rename(state = id) %>%
  # Make state names lower case to match other data
  mutate(state = tolower(state))

# Organize final data for plot, combine specific data frames
data_plot = inner_join(data_elections, states)

data_result = inner_join(data_elections, data_electoral)


## MAKE MAP ####
ggplot(subset(data_plot, year == 2012),
       aes(x = long, y = lat, group = group, fill = party_winner)) +
  # Draw states as polygons with white borders between states
  geom_polygon(color = "white") +
  # Set manual colors so D and R are blue and red
  scale_fill_manual(values = c("blue", "red", "yellow", "green")) +
  # Update map projection to match most maps
  coord_map(projection = "polyconic") +
  # Remove axes and background
  theme_void() +
  # Move legend position and increase text size
  theme(legend.position = "top", text = element_text(size = 40))  


## MAKE TABLE SUMMARY OF ELECTION RESULT ####
data_sum = data_result %>%
  # Focus on one specific year
  filter(year == 2012) %>%
  # Aggregate by each party in that election year
  group_by(party_winner) %>%
  # Compute total number of votes for each party
  summarise(total_votes = sum(num_electoral_votes, na.rm = T)) %>%
  # Ungroup for any future analyses
  ungroup() %>%
  # Drop any NAs for party_winner
  filter(!is.na(party_winner))


## MAKE SENTENCE SUMMARY OF ELECTION RESULT ####
paste("The winner of the election was the",
      filter(data_sum, total_votes == max(total_votes))$party_winner,
      "party with", filter(data_sum, total_votes == max(total_votes))$total_votes,
      "electoral college votes.")