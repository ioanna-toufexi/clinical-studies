library(httr)
library(jsonlite)
library(stringr)
library(lgr)

base_url <- "https://www.clinicalTrials.gov/api/query/study_fields?fmt=JSON&expr=${expr}&min_rnk=${min_rnk}&max_rnk=${max_rnk}&fields=${fields}"

get_studies_from_api <- function(min_rnk, max_rnk) {
  
  expr <- "COVID-19"
  fields <- "OrgFullName,OrgClass,OfficialTitle,OverallStatus,StartDate,Condition,LocationCountry"
  
  lgr$info(str_c("Calling study_fields API with min_rnk = ", min_rnk, ", max_rnk = ", max_rnk))
  get_studies_url <- str_interp(base_url)
  
  study_fields_response <-  GET(get_studies_url) %>% 
    content(as="text") %>% 
    str_remove_all("\n") %>% 
    fromJSON(flatten = TRUE)
}
