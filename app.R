## LOAD SHINY PACKAGE ####
library(shiny)


## MAKE UI INPUTS ####
ui <- fluidPage(
  selectInput(inputId = "year", label = "Election Year",
              choices = c("1964", "1972", "1980", "1984", "1992", "1996", "2004", "2012")),
  
  plotOutput("map")
)


## MAKE SERVER OUTPUTS
server <- function(input, output) {
  library(dplyr)
  library(ggplot2)
  library(maps)
  library(mapproj)
  
  data = read.table("data/data.txt", header=T, sep="\t") %>%
     mutate(year = factor(year))

  states = map_data("state") %>%
    rename(state = region)
  
  data_plot = inner_join(data, states)
  
  output$map = renderPlot({
    filter(data_plot, year == input$year) %>%
    ggplot() +
      geom_polygon(aes(x=long, y=lat, group = group, fill = winner),
                    colour="white") +
      scale_fill_manual(values = c("blue", "red")) +
      coord_map(projection = "polyconic") +
      theme_void() +
      theme(legend.position = "top", text = element_text(size = 40))
  })

}


## RENDER APP ####
shinyApp(ui = ui, server = server)