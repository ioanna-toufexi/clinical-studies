# Adds months to the dataframe that have no studies 
# and are between months with studies
addMissingMonths <- function(studies_per_month) {
  
  all_dates = seq(as.Date(min(studies_per_month$StartMonth)), as.Date(max(studies_per_month$StartMonth)), by="month")
  
  studies_per_month <- studies_per_month %>% mutate(StartMonth = as.Date(StartMonth))
  
  studies_per_month <- base::merge(data.frame(StartMonth = all_dates),
                                   studies_per_month,
                                   by.x='StartMonth',
                                   by.y='StartMonth',
                                   all.x=T,
                                   all.y=T)
  
  studies_per_month <- studies_per_month %>% 
    mutate(StartMonth = as.yearmon(StartMonth)) %>% 
    mutate(count = ifelse(is.na(count),0,count))
  
}