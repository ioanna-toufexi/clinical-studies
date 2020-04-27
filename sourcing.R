library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(zoo)
library(purrr)
library(lgr)
source("api.R")

# Gets studies from API and saves them to a data frame
get_raw_studies <- function() {
  
  # Max number of rows the API can return is 1000
  response <- get_studies_from_api(1,1000)
  
  # Check if more calls are needed
  raw_df <- extract_df(response)
  number_of_studies <- response$StudyFieldsResponse$NStudiesFound
  repeat_times <- number_of_studies %/% 1000
  lgr$info(str_c("Number of studies: ", number_of_studies, ", will repeat API call ", repeat_times, " times."))
  
  # Repeat in batches of 1000
  if (repeat_times>0) {
    for (i in 1:repeat_times) {
      raw_df <- rbind(raw_df, extract_df(get_studies_from_api(1+1000*i,1000+1000*i)))
    }
  }
  
  raw_df
}

# Gets the dataframe nested in the response
extract_df <- function(study_fields_response) {
  study_fields_df <- study_fields_response$StudyFieldsResponse$StudyFields
}

# Data wrangling
# Values that are vectors are flattened to strings
# All columns need to be unlisted
# Dates of the type month-year are converted to 1-month-year
wrangle_studies <- function(study_fields_df) {

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

