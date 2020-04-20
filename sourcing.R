library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(purrr)

base_url <- "https://www.clinicalTrials.gov/api/query/study_fields?fmt=JSON&expr=${expr}&min_rnk=${min_rnk}&max_rnk=${max_rnk}&fields=${fields}"

# Gets data from API and returns them after wrangling
get_studies <- function() {
  
  expr <- "COVID-19"
  min_rnk <- 1
  max_rnk <- 1000 #TODO more than 1000 ranks
  fields <- "OrgFullName,OrgClass,OfficialTitle,OverallStatus,StartDate,Condition,LocationCountry"
  
  get_studies_url <- str_interp(base_url)
  
  study_fields_response <-  GET(get_studies_url) %>% 
                            content(as="text") %>% 
                            str_remove_all("\n") %>% 
                            fromJSON(flatten = TRUE)
  
  # Getting the dataframe which is nested in the response
  study_fields_df <- study_fields_response$StudyFieldsResponse$StudyFields
  
  # Data wrangling
  # Values that are vectors are flattened to strings
  # All columns need to be unlisted
  # Dates of the type month-year are converted to 1-month-year
  studies <- study_fields_df %>% 
                  rename(Country = LocationCountry) %>% 
                  mutate(Country = map(Country,unique)) %>% 
                  mutate(Country = map(Country,conc_or_repl), Condition = map(Condition,conc_or_repl), StartDate = map(StartDate,conc_or_repl)) %>% 
                  run_on_df(unlist) %>% 
                  mutate(StartDate = ifelse(str_detect(StartDate, ","),StartDate,str_replace(StartDate, " ", " 1, "))) %>% 
                  mutate(StartMonth = as.yearmon(as.Date(StartDate, format="%B %d, %Y"))) %>% 
                  filter(StartMonth > as.yearmon("2019-10"))
}

# Runs a given function on all columns of a dataframe
run_on_df <- function(df,func) {
  for (i in seq_along(df)) {
    df[[i]] <- func(df[[i]])
  }
  df
}

# Concatenates vector data
conc_or_repl <- function(x) {
  if(identical(x,character(0))) {
    "No data"
  }
  else {
    str_c(x,collapse=", ")
  }
}

