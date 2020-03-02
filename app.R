# library ----
library(tidyverse)

library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(shiny.info)

library(plotly)
library(leaflet)
library(highcharter)
library(ggthemes)
library(ggrepel)

library(DT)
library(formattable)
library(knitr)

library(lubridate)
library(purrr)
library(zip)

## Make login DB
library(shinymanager) 

#credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

#create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

## header ----
header <- dashboardHeader(
  title = "COVID-SickBed",
  titleWidth = 270
)

## sidebar ----
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    fileInput(
      "selFile",
      "병실현황을 업로드 해주세요",
      buttonLabel = "파일선택"
    ),

    menuItem("대시보드", tabName = "dashboard", icon = icon("dashboard")),

    menuItem(
      "병원별",
      tabName = "by_hospital",
      icon = icon("hospital")
    ),


    menuItem(
      "환자별",
      tabName = "by_patient",
      icon = icon("procedures")
    ),

    menuItem("환자유입", tabName = "enter_patient", icon = icon("ambulance")),

    menuItem(
      "시뮬레이션",
      tabName = "simulation_sickbed",
      icon = icon("calculator")
    ),

    menuItem("데이터", tabName = "data", icon = icon("database")),
    menuItem("About", tabName = "about", icon = icon("star"))
  )
)

## body ----
body <- dashboardBody(

  ### style ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css")
  ),

  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),

  ### Last Update ----
  shiny.info::version(paste("Last Update:", ymd(Sys.Date())), position = "bottom right"),

  ### Dashboard ----
  tabItem(
    tabName = "dashboard",
    fluidPage(
      valueBoxOutput("db_bed") %>% withLoader(type = "html", loader = "loader7"),
      valueBoxOutput("db_use") %>% withLoader(type = "html", loader = "loader7"),
      valueBoxOutput("db_empty") %>% withLoader(type = "html", loader = "loader7")
    ),
    fluidPage(
      valueBoxOutput("db_definite") %>% withLoader(type = "html", loader = "loader7"),
      valueBoxOutput("db_percent") %>% withLoader(type = "html", loader = "loader7"),
      valueBoxOutput("db_serious") %>% withLoader(type = "html", loader = "loader7")
    ),
    fluidPage(
      ### map ----
      column(12, leafletOutput("dashboard_map")),
      ### summary table ----
      column(12, dataTableOutput("data_sickbed"))
    )
  ),

  ## raw data ----
  tabItem(
    tabName = "data",
    fluidPage(
      # dataTableOutput("data_sickbed")
    )
  )

  ## endpoint
)

# ui ----
ui <- dashboardPage(header, sidebar, body) 

# login 기능은 개발과정에서는 생략하고 배포시에만 적용
# %>% secure_app(enable_admin = T) 


# server ----

server <- function(input, output, session) {
  
  ## Apply login DB ----
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  ## data ----
  hospital <- reactive({
    hospital <- readxl::read_excel("hospital.xlsx")
    return(hospital)
  })

  sickbed <- reactive({
    sickbed <- readxl::read_excel("data-example/2020-02-23.xlsx")
    sickbed <- sickbed %>%
      mutate(date = ymd(date)) %>%
      mutate(bed = ifelse(is.na(이름) == TRUE, 0, 1))
    return(sickbed)
  })
  
  summary <- reactive({
    summary <- sickbed() %>%
      group_by(hospital) %>%
      summarise(전체 = n(), 사용 = sum(bed)) %>%
      mutate(
        여유 = 전체-사용,
        사용율 = round(사용/전체*100, 1)
      ) 
    return(summary)
  })
  
  join <- reactive({
    join <- hospital() %>%
      left_join(summary()) %>%
      mutate(capacity = ifelse(사용율 >= 75, "low", ifelse(사용율 <= 50, "high", "mid")))
    return(join)
  })

  ## value box ----
  output$db_bed <- renderValueBox({
    value <- summary()$전체 %>% sum()
    
    valueBox(
      paste0(value),
      "전체 병상수",
      color = "green",
      icon = icon("bed")
    )
  })
  
  output$db_use <- renderValueBox({
    value <- summary()$사용 %>% sum()
    
    valueBox(
      paste0(value),
      "사용",
      color = "red",
      icon = icon("procedures")
    )
  })
  
  output$db_empty <- renderValueBox({
    value <- summary()$여유 %>% sum()
    
    valueBox(
      paste0(value),
      "여유",
      color = "aqua",
      icon = icon("smile")
    )
  })
  
  output$db_definite <- renderValueBox({
    value <- sickbed() %>%
      filter(의사.확진 == "확진") %>%
      nrow()
    
    valueBox(
      paste0(value),
      "확진자수",
      color = "green",
      icon = icon("tired")
    )
  })
  
  output$db_percent <- renderValueBox({
    value <- round(sum(summary()$사용)/sum(summary()$전체)*100, 1)
    
    valueBox(
      paste0(value, "%"),
      "사용율",
      color = "red",
      icon = icon("procedures")
    )
  })
  
  output$db_serious <- renderValueBox({
    value <- sickbed() %>%
      filter(중증도변화 == "중증") %>%
      nrow()
      
    valueBox(
      paste0(value),
      "중증환자수",
      color = "aqua",
      icon = icon("dizzy")
    )
  })
  
  
  ## map ----
  output$dashboard_map <- renderLeaflet({
    
    join() %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(label = ~hospital) 
  })

  ## data ----
  output$data_sickbed <- renderDataTable({
      datatable(
        summary(),
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 50, 100, 200, 300),
          dom = "Blfrtip",
          buttons = c("copy", "excel", "print")
        )
      ) %>%
      formatStyle("사용율",
                  color = "white",
                  background = styleColorBar(c(0, 100), "#ce0f3d"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      )
      
  })
}

# run app  --------------
shinyApp(ui, server)
