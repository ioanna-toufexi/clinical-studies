library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

base_url <- "https://www.clinicalTrials.gov/api/query/study_fields?fmt=JSON&expr=${expr}&min_rnk=${min_rnk}&max_rnk=${max_rnk}&fields=${fields}"

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
  
  study_fields_df <- study_fields_response$StudyFieldsResponse$StudyFields
  
  studies <- study_fields_df %>% 
                  rename(Country = LocationCountry) %>% 
                  mutate(Country = map(Country,unique)) %>% 
                  mutate(Country = map(Country,conc_or_repl), Condition = map(Condition,conc_or_repl), StartDate = map(StartDate,conc_or_repl)) %>% 
                  run_on_df(unlist) %>% 
                  mutate(StartDate = ifelse(str_detect(StartDate, ","),StartDate,str_replace(StartDate, " ", " 1, "))) %>% 
                  mutate(StartDate = as.Date(StartDate, format="%B %d, %Y")) %>%
                  filter(StartDate > as.Date("31/12/2019", format="%d/%m/%Y")) %>% 
                  mutate(StartMonth = lubridate::month(as_date(StartDate),label = TRUE)) %>% 
                  run_on_df(unlist)
}

run_on_df <- function(df,func) {
  for (i in seq_along(df)) {
    df[[i]] <- func(df[[i]])
  }
  df
}

conc_or_repl <- function(x) {
  if(identical(x,character(0))) {
    "No data"
  }
  else {
    str_c(x,collapse=", ")
  }
}
