library(shiny)
library(ggplot2)
source("sourcing.R")

function(input, output) {
  
  studies <- get_studies()
  
  output$plot <- renderPlot({
    
    studies %>% group_by(StartMonth) %>%
      summarise(count = n()) %>% 
      ggplot() +
      geom_bar(aes(x = StartMonth,
                   y = count), stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}