## LOAD SHINY PACKAGE ####
library(shiny)


## MAKE UI INPUTS ####
ui <- fluidPage(
  selectInput(inputId = "year", label = "Election Year",
              choices = c("1789", "1792", "1796", "1800", "1804", "1808", "1812", "1816", "1820", "1824", "1828",
                            "1832", "1836", "1840", "1844", "1848", "1852", "1856", "1860", "1864", "1868", "1872",
                            "1876", "1880", "1884", "1888", "1892", "1896", "1900", "1904", "1908", "1912", "1916",
                            "1920", "1924", "1928", "1932", "1936", "1940", "1944", "1948", "1952", "1956", "1960",
                            "1964", "1968", "1972", "1976", "1980", "1984", "1988", "1992", "1996", "2000", "2004",
                            "2008", "2012")),
  
  plotOutput("map")
)


## MAKE SERVER OUTPUTS
server <- function(input, output) {
  library(tidyverse)
  library(maps)
  library(mapproj)
  
  data = read.table("data/data_us_presidential_elections.txt", header=T, sep="\t") %>%
    mutate(year = factor(year)) %>%
    mutate(state = tolower(state))

  states = map_data("state") %>%
    rename(state = region)
  
  data_plot = inner_join(data, states)
  
  output$map = renderPlot({
    filter(data_plot, year == input$year) %>%
    ggplot() +
      geom_polygon(aes(x=long, y=lat, group = group, fill = party_winner),
                    colour="white") +
      scale_fill_manual(values = c("blue", "red")) +
      coord_map(projection = "polyconic") +
      theme_void() +
      theme(legend.position = "top", text = element_text(size = 40))
  })

}


## RENDER APP ####
shinyApp(ui = ui, server = server)