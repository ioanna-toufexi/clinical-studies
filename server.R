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
                   y = count), fill="blue", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y= "number of studies", x = "month (2020)", title = "Number of clinical studies related to COVID-19 per starting month (2020)")
  })
  
}