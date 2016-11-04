## LOAD PACKAGES ####
library(shiny)
library(tidyverse)
library(maps)
library(mapproj)
library(albersusa)
library(maptools)


## READ IN DATA AND ORGANIZE ####
# Data for analysis
data_elections = read.table("data/data_us_presidential_elections.txt", header=T, sep="\t")

data_electoral = read.table("data/data_electoral_votes.txt", header=T, sep="\t")

# Get map - continental 48 states version
# states = map_data("state") %>%
#   rename(state = region)

# Get map - all 50 states version
states_map = usa_composite()

states = fortify(states_map, region="name") %>%
  rename(state = id) %>%
  mutate(state = tolower(state))

# Organize final data for plot, combine specific data frames
data_plot = inner_join(data_elections, states)

data_result = inner_join(data_elections, data_electoral)


## MAKE UI INPUTS ####
ui <- fluidPage(
  # Add CSS template
  theme = "bootswatch-cerulean.css",
  
  # Add webpage title
  title = "R-Ladies Paris: Shiny Tutorial",
  
  # Add top descriptor inforamtion information
  tags$h1("Historical United States Presidential Election Results"),
  tags$h4("Data from", tags$a(href = "https://en.wikipedia.org/wiki/List_of_United_States_presidential_election_results_by_state",
         "Wikipedia: List of United States presidential election results by state"), "article."),
  
  # Set-up layout of main part of page
  sidebarLayout(
    
    # Add a sidebar panel
    sidebarPanel(
      
      # Add space for election year input
      selectInput(inputId = "year", label = "Election Year",
                  choices = c(levels(factor(data_result$year))))
    ),
    
    # Add main panel
    mainPanel(
      
      # Add space for election summary table
      tableOutput("result_tab"),
      
      # Add space for election summary sentence
      textOutput("result_sent"),

      # Add space for map 
      plotOutput("result_map")
      
    )
  )
)


## MAKE SERVER OUTPUTS
server <- function(input, output) {
  
  # Make map
  output$result_map = renderPlot({
    ggplot(subset(data_plot, year == input$year),
           aes(x = long, y = lat, group = group, fill = party_winner)) +
      geom_polygon(color = "white") +
      scale_fill_manual(values = c("blue", "red", "yellow", "green")) +
      coord_map(projection = "polyconic") +
      theme_void() +
      theme(legend.position = "top", text = element_text(size = 40))  
  })
  
  # Save summary information as a reactive variable 
  data_sum = reactive({
    data_result %>%
      filter(year == input$year) %>%
      group_by(party_winner) %>%
      summarise(total_votes = sum(num_electoral_votes, na.rm = T)) %>%
      ungroup() %>%
      filter(!is.na(party_winner))
  })
  
  # Make table
  output$result_tab = renderTable({
    data_sum() %>%
      # Clean up column header names to be prettier
      rename(Party = party_winner) %>%
      rename("Electoral College Votes" = total_votes)
  },
  #include.rownames=FALSE
  )
  
  # Make sentence
  output$result_sent = renderText({
    paste("The winner of the election was the",
          filter(data_sum(), total_votes == max(total_votes))$party_winner,
          "party with", filter(data_sum(), total_votes == max(total_votes))$total_votes,
          "electoral college votes.")
  })

}


## RENDER APP ####
shinyApp(ui = ui, server = server)