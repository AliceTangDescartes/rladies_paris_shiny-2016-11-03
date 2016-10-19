library(dplyr)
library(purrr)

data_election_results = list.files(path = "data/elections", full.names = T) %>%
  # Run read.table call on all files
  map(read.table, header = T, sep = "\t") %>%
  # Combine all data frames into a single data frame by row
  reduce(rbind)

data_elections = read.table("data/rcourse_lesson5_data_elections.txt", header=T, sep="\t")

data = inner_join(data_election_results, data_elections) %>%
  mutate(winner = ifelse(incumbent_party == "democrat" & perc_votes_incumbent > perc_votes_challenger,
                         "democrat", ifelse(incumbent_party == "republican" & perc_votes_incumbent < perc_votes_challenger,
                                            "demorat", "republican"))) %>%
  mutate(state = tolower(state)) %>%
  mutate(year = factor(year))

all_states <- map_data("state") %>%
  rename(state = region)

data_plot = inner_join(data, all_states)