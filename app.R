## LOAD PACKAGES ####
library(shiny)
library(tidyverse)
library(maps)
library(mapproj)


## READ IN DATA ####
data = read.table("data/data_us_presidential_elections.txt", header=T, sep="\t") %>%
  mutate(year = factor(year))

data_electoral = read.table("data/data_electoral_votes.txt", header=T, sep="\t") %>%
  mutate(year = factor(year))

states = map_data("state") %>%
  rename(state = region)

data_plot = inner_join(data, states)

data_result = inner_join(data, data_electoral)


## MAKE UI INPUTS ####
ui <- fluidPage(
  # Add space for year selector
  selectInput(inputId = "year", label = "Election Year",
              choices = c(levels(data$year))),
  
  # Add space for election summary sentence
  textOutput("result_sent"),
  
  # Add space for election summary table
  tableOutput("result_tab"),
  
  # Add space for map 
  plotOutput("map")
)


## MAKE SERVER OUTPUTS
server <- function(input, output) {
  
  output$map = renderPlot({
    data_plot %>%
      filter(year == input$year) %>%
      ggplot() +
      geom_polygon(aes(x=long, y=lat, group = group, fill = party_winner),
                    colour="white") +
      scale_fill_manual(values = c("blue", "red", "yellow", "green", "purple")) +
      coord_map(projection = "polyconic") +
      theme_void() +
      theme(legend.position = "top", text = element_text(size = 40))
  })
  
  data_sum = reactive({
    data_result %>%
      filter(year == input$year) %>%
      group_by(party_winner) %>%
      summarise(total_votes = sum(num_electoral_votes, na.rm = T)) %>%
      ungroup() %>%
      filter(!is.na(party_winner))
  })
  
  output$result_sent = renderText({
    paste("The winner of the election was the",
          filter(data_sum(), total_votes == max(total_votes))$party_winner,
          "party with", filter(data_sum(), total_votes == max(total_votes))$total_votes,
          "votes.")
  })
  
  output$result_tab = renderTable({
    data_sum()
  })

}


## RENDER APP ####
shinyApp(ui = ui, server = server)