library(shiny)
library(ggplot2)
library(stringr)
library(shinycssloaders)

attribution <- str_c("Data fetched from ClinicalTrials.gov API on ", format(Sys.time(), "%a %d %b %X "), Sys.timezone())

fluidPage(
  
  titlePanel("Demo Shiny app"),

    p(attribution),
    p("Disclaimer:"),
    p("1) This is for demo purposes only."),
    p("2) \"COVID-19\" is used as a generic search keyword, 
    thus the number is slightly higher than the condition-specific search on ClinicalTrials.gov (to be fixed)."),
    p("3) This dataset only contains studies listed on ClinicalTrials.gov. For additional studies refer to https://www.who.int/ictrp/en/"),

    sidebarPanel(
      radioButtons(
        inputId='type', label='Choose variable', choiceNames=list("Start month", "Country"), choiceValues=list("StartMonth", "Country")
      )
    ),
  mainPanel(
    plotOutput('plot') %>% withSpinner(color="#0dc5c1"),
    htmlOutput('warning')
  )
)