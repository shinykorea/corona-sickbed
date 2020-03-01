# library ----
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(shiny.info)

library(plotly)
library(ggthemes)
library(ggrepel)

library(DT)
library(formattable)
library(knitr)

library(lubridate)
library(purrr)
library(zip)

## header ----
header <- dashboardHeader(
  title = "COVID-SickBed",
  titleWidth = 270
)


## sidebar ----
sidebar <- dashboardSidebar(
  width = 270,
  sidebarMenu(
    
    fileInput(
      "selFile",
      "병실현황을 업로드 해주세요",
      buttonLabel = "파일선택"
    ),
    
    menuItem("대시보드", tabName = "dashboard", icon = icon("dashboard")),
    
    menuItem(
      "병원별",
      tabName = "analysis",
      icon = icon("chart-line")
    ),
    
    menuItem(
      "환자별",
      tabName = "indi-analysis",
      icon = icon("ellipsis-h")
    ),
    
    menuItem(
      "병상 시뮬레이션",
      tabName = "modeling",
      icon = icon("brain")
    ),
    
    menuItem("환자유입", tabName = "calving", icon = icon("calendar")),
    menuItem("데이터", tabName = "data", icon = icon("database")),
    menuItem("About", tabName = "link", icon = icon("star"))
  )
)

## body ----
body <- dashboardBody(
  
  ### style ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css")
  ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  ### Last Update ----
  shiny.info::version(paste("Last Update:", ymd(Sys.Date())), position = "bottom right")
  
)

# ui ----
ui <- dashboardPage(header, sidebar, body)

# server ----

server <- function(input, output, session) {
  
}

# run app  --------------
shinyApp(ui, server)
