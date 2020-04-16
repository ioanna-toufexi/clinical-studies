library(shiny)
library(ggplot2)
source("sourcing.R")

function(input, output) {
  
  studies <- get_studies()
  
  type <- reactive({
    input$type
  })
  
  output$plot <- renderPlot({
    
    if(type()=="StartDate") {
      studies %>% group_by_at(vars(type())) %>%
        summarise(count = n()) %>% 
        ggplot() +
        geom_bar(aes_string(x = type(),
                            y = "count"), fill="blue", stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y= "number of studies", x = "month (2020)", title = "Number of clinical studies related to COVID-19 per starting month (from Jan 2020)")
    }
    else {
      
      #Some observations have multiple countries. 
      #Data manipulations to separate them.
      countries_as_string = paste(studies$Country, collapse = ',,')
      countries <- data.frame(Country = unlist(strsplit(countries_as_string, split=",,")))
      
      countries %>% 
        group_by(Country) %>% 
        summarise(count = n()) %>% 
        top_n(20, count) %>% 
        mutate(highlight_flag = ifelse(Country != 'No data', T, F)) %>% 
      ggplot() +
        geom_bar(aes(x = reorder(Country, -count),
                     y = count, fill=highlight_flag), stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
        labs(y= "number of studies", x = "Country", title = "Top 20 countries per number of clinical studies related to COVID-19 (from Jan 2020). \nIncludes appearances in multi-country collaborations.")}
  })
  
}