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
              #choices = c("1789", "1792", "1796", "1800", "1804", "1808", "1812", "1816", "1820", "1824", "1828",
              #              "1832", "1836", "1840", "1844", "1848", "1852", "1856", "1860", "1864", "1868", "1872",
              #              "1876", "1880", "1884", "1888", "1892", "1896", "1900", "1904", "1908", "1912", "1916",
              #              "1920", "1924", "1928", "1932", "1936", "1940", "1944", "1948", "1952", "1956", "1960",
              #              "1964", "1968", "1972", "1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004",
              #              "2008", "2012")
              choices = c(levels(data$year))),
  
  # Add space for map 
  plotOutput("map"),
  
  # Add space for election summary
  verbatimTextOutput("result")
  
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
  
  output$result = renderPrint({
    data_result %>%
      filter(year == input$year) %>%
      group_by(party_winner) %>%
      summarise(total_votes = sum(num_electoral_votes, na.rm = T)) %>%
      ungroup() %>%
      filter(!is.na(party_winner))
  })
    

}


## RENDER APP ####
shinyApp(ui = ui, server = server)