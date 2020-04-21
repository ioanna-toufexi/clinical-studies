library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(zoo)
library(purrr)
source("api.R")

get_raw_studies <- function() {
  
  study_fields_response <- get_studies_from_api(1,1000)
  
  #Repeat 
  study_fields_response$NStudiesFound
  
  get_dataframe_from_response(study_fields_response)
}

get_dataframe_from_response <- function(study_fields_response) {
  # Getting the dataframe which is nested in the response
  study_fields_df <- study_fields_response$StudyFieldsResponse$StudyFields
}

# Gets data from API and returns them after wrangling
wrangle_studies <- function(study_fields_df) {
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

