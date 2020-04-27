library(shiny)
library(ggplot2)
library(lubridate)
library(zoo)
source("sourcing.R")
source("helper.R")

# Main Shiny server function
function(input, output) {
  
  # Get studies from API and process them
  raw_studies <- get_raw_studies()
  studies <- wrangle_studies(raw_studies)
  
  # Calculate studies that were ommited because they had no start date specified
  initial_number <- nrow(raw_studies)
  final_number <- nrow(studies)
  ommited_number <- initial_number - final_number
  
  # Count studies per month
  studies_per_month <- studies %>% 
    group_by(StartMonth) %>%
    summarise(count = n())
  
  # Add observations for months that had 0 studies
  # So that they also show in the plot
  studies_per_month <- addMissingMonths(studies_per_month)
  
  # Find month with most studies
  top_month <- (studies_per_month %>% 
                filter(count == max(count)))$StartMonth
  
  # Some observations have multiple countries. 
  # Data manipulations to separate them.
  countries_as_string = paste(studies$Country, collapse = ',,')
  countries <- data.frame(Country = unlist(strsplit(countries_as_string, split=",,")))

  # Count studies per country
  studies_per_country <- countries %>% 
    group_by(Country) %>% 
    summarise(count = n()) %>% 
    top_n(20, count) %>% 
    mutate(highlight_flag = ifelse(Country != 'No data', T, F))
  
  # Find country with most studies
  top_country <- (studies_per_country %>% 
                  filter(highlight_flag) %>% 
                  filter(count == max(count)))$Country
  
  # Get number of studies that had no country specified
  no_data_number <-   (studies_per_country %>% 
                                        filter(!highlight_flag))$count

  # Variable to display (Start month or Country), selected by the UI
  type <- reactive({
    input$type
  })
  
  # Dynamic text output
  output$warning <- reactive({
    if (input$type=="StartMonth") {
      str_c("<font color=\"#FF0000\"><b>There was no starting date for ", ommited_number, " out of ", initial_number, " studies.</b></font>")
    }
    else {
      str_c("<font color=\"#FF0000\"><b>There was no country for ", no_data_number, " out of ", initial_number, " studies.</b></font>")
    }
  })
  
  # Depending on the UI input, the appropriate plot is sent to the output
  output$plot <- renderPlot({
    
    if(type()=="StartMonth") {
      studies_per_month %>% 
        ggplot(aes(x = as.factor(StartMonth),
                   y = count)) +
        geom_bar(fill="blue", stat = "identity") +
        geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y= "Number of studies", 
             x = "Start month", 
             title = str_c(lubridate::month(as.Date(top_month), label=TRUE, abbr = FALSE), 
                           " ", 
                           year(as.Date(top_month)), 
                           " is the month with the most new COVID-19 clinical trials"),
             subtitle = "Checking the month a trial started or is set to start",
             caption = "Data from clinicaltrials.gov"
             
             )
    }
    else {
      
      studies_per_country %>% 
      ggplot(aes(x = reorder(Country, -count),
                 y = count)) +
        geom_bar(aes(fill=highlight_flag), stat = "identity") +
        geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
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