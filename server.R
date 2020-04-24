library(shiny)
library(ggplot2)
library(lubridate)
library(zoo)
source("sourcing.R")

function(input, output) {
  
  raw_studies <- get_raw_studies()
  studies <- wrangle_studies(raw_studies)
  
  initial_number <- nrow(raw_studies)
  final_number <- nrow(studies)
  ommited_number <- initial_number - final_number
  
  studies_per_month <- studies %>% 
    group_by(StartMonth) %>%
    summarise(count = n())
  
  top_month <- (studies_per_month %>% 
                filter(count == max(count)))$StartMonth
  
  #Some observations have multiple countries. 
  #Data manipulations to separate them.
  countries_as_string = paste(studies$Country, collapse = ',,')
  countries <- data.frame(Country = unlist(strsplit(countries_as_string, split=",,")))

  studies_per_country <- countries %>% 
    group_by(Country) %>% 
    summarise(count = n()) %>% 
    top_n(20, count) %>% 
    mutate(highlight_flag = ifelse(Country != 'No data', T, F))
  
  top_country <- (studies_per_country %>% 
                  filter(highlight_flag) %>% 
                  filter(count == max(count)))$Country
  
  no_data_number <-   (studies_per_country %>% 
                                        filter(!highlight_flag))$count

  type <- reactive({
    input$type
  })
  
  output$warning <- reactive({
    if (input$type=="StartMonth") {
      str_c("<font color=\"#FF0000\"><b>There was no starting date for ", ommited_number, " out of ", initial_number, " studies.</b></font>")
    }
    else {
      str_c("<font color=\"#FF0000\"><b>There was no country for ", no_data_number, " out of ", initial_number, " studies.</b></font>")
    }
  })
  
  output$plot <- renderPlot({
    
    #TODO add empty months
    if(type()=="StartMonth") {
      studies_per_month %>% 
        ggplot() +
        geom_bar(aes(x = as.factor(StartMonth),
                            y = count), fill="blue", stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y= "Number of studies", 
             x = "Start month", 
             title = str_c(lubridate::month(as.Date(top_month), label=TRUE, abbr = FALSE), 
                           " ", 
                           year(as.Date(top_month)), 
                           " is the month with the most new COVID-19 clinical trials"),
             caption = "Data from clinicaltrials.gov"
             
             )
    }
    else {
      
      studies_per_country %>% 
      ggplot() +
        geom_bar(aes(x = reorder(Country, -count),
                     y = count, fill=highlight_flag), stat = "identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") +
        labs(y= "Number of studies", 
             x = "Country", 
             title = str_c(top_country," is the country involved in the most COVID-19 clinical trials"),
             subtitle = "Includes appearances in multi-country collaborations",
             caption = "Data from clinicaltrials.gov")
      }
  })
  
}