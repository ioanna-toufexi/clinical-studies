library(shiny)
library(ggplot2)
source("sourcing.R")

function(input, output) {
  
  raw_studies <- get_studies_from_api()
  studies <- wrangle_studies(raw_studies)
  
  initial_number <- nrow(raw_studies)
  final_number <- nrow(studies)
  ommited_number <- initial_number - final_number
  
  type <- reactive({
    input$type
  })
  
  output$warning <- renderText(str_c("Total: ", initial_number, ", <font color=\"#FF0000\"><b>Had no start date: ", ommited_number, "</b></font>"))
  
  output$plot <- renderPlot({
    
    #TODO add empty months
    if(type()=="StartMonth") {
      studies %>% group_by(StartMonth) %>%
        summarise(count = n()) %>% 
        ggplot() +
        geom_bar(aes(x = as.factor(StartMonth),
                            y = count), fill="blue", stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y= "Number of studies", x = "Start month", title = "Number of clinical studies related to COVID-19 per starting month (from Nov 2019)")
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
        labs(y= "Number of studies", x = "Country", title = "Top 20 countries per number of clinical studies related to COVID-19 (from Nov 2019). \nIncludes appearances in multi-country collaborations.")}
  })
  
}