# library ----
library(tidyverse)

library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(shiny.info)

library(plotly)
library(leaflet)
library(leaflet.minicharts)
library(highcharter)
library(ggthemes)
library(ggrepel)
library(waffle) # devtools::install_github("hrbrmstr/waffle")

library(DT)
library(formattable)
library(knitr)

library(lubridate)
library(purrr)
library(zip)

## Make login DB
library(shinymanager)

# credentials <- data.frame(
#  user = c("admin", "user1"),
#  password = c("admin", "user1"),
#  admin = c(T, F),
#  stringsAsFactors = FALSE)

# create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

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
      tags$a(href="https://github.com/shinykorea/corona-sickbed/blob/master/data-example/2020-02-23.xlsx?raw=true", tags$div(HTML(paste("병실현황을", tags$span(style="color:black", "업로드"), "해주세요", sep = "")))),
      buttonLabel = "파일선택"
    ),

    menuItem("대시보드", tabName = "dashboard", icon = icon("dashboard")),

    menuItem("환자분포", tabName = "distribution", icon = icon("chart-pie")),

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
    menuItem("About", tabName = "about", icon = icon("star")),
    
    uiOutput("ui_checkbox")
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

  tabItems(
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
        column(8, leafletOutput("dashboard_map")),
        column(4, plotOutput("dashboard_waffle"))
      ),
      fluidPage(
        ### summary table ----
        column(12, dataTableOutput("data_sickbed"))
      )
    ),

    ## 환자분포 ----
    tabItem(
      tabName = "distribution",
      fluidPage(
        column(6, plotlyOutput("distribution_serious_age")),
        column(6, plotlyOutput("distribution_etc"))
      )
    ),

    ## raw data ----
    tabItem(
      tabName = "data",
      fluidPage(
        dataTableOutput("data_raw")
      )
    )
  )
)

# ui ----
ui <- dashboardPage(header, sidebar, body)
# %>% secure_app(enable_admin = T) # login 기능은 개발과정에서는 생략하고 실제 애플리케이션 배포시에만 적용


# server ----

server <- function(input, output, session) {

  ## Apply login DB ----
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite")
  )

  ## data ----
  ### import
  data <- reactive({
    req(input$selFile$datapath)
    
    if (grepl(".zip", input$selFile$datapath) == TRUE) {
      dir.create(tmp <- tempfile())
      zip::unzip(input$selFile$datapath, exdir = tmp)
      nm <- list.files(file.path(tmp))
      nm <- subset(nm, grepl(".xls", nm) == TRUE | grepl(".xlsx", nm) == TRUE | grepl(".csv", nm) == TRUE | grepl(".txt", nm) == TRUE)
      data <- do.call(rbind, lapply(nm, FUN = function(x) readxl::read_excel(file.path(tmp, x))))
      data
    } else {
      data <- readxl::read_excel(input$selFile$datapath)
    }
  })
  
  hospital <- reactive({
    hospital <- readxl::read_excel("hospital.xlsx")
    return(hospital)
  })

  sickbed <- reactive({
    # sickbed <- readxl::read_excel("data-example/2020-02-23.xlsx")
    sickbed <- data() 
    sickbed <- sickbed %>%
      filter(hospital %in% input$checkbox) %>%
      mutate(date = ymd(date)) %>%
      mutate(중증도변화 = factor(중증도변화, level = c("경증", "중증도", "중증", "최중증"))) %>%
      mutate(bed = ifelse(is.na(이름) == TRUE, 0, 1))
    return(sickbed)
  })

  summary <- reactive({
    summary <- sickbed() %>%
      group_by(hospital) %>%
      summarise(전체 = n(), 사용 = sum(bed)) %>%
      mutate(
        여유 = 전체 - 사용,
        사용율 = round(사용 / 전체 * 100, 1)
      )
    return(summary)
  })

  join <- reactive({
    join <- hospital() %>%
      left_join(summary())
    return(join)
  })
  
  ## renderUI ----
  output$ui_checkbox <- renderUI({
    checkboxGroupInput(
      "checkbox",
      "병원선택",
      c(hospital()$hospital),
      selected = c(hospital()$hospital)
    )
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
    value <- round(sum(summary()$사용) / sum(summary()$전체) * 100, 1)

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
    leaflet() %>%
      addTiles() %>%
      addMinicharts(
        join()$long, join()$lat,
        type = "pie",
        chartdata = join()[, c("사용", "여유")],
        colorPalette = c("#fe346e", "#cccccc")
      )
  })

  ## waffle ----
  output$dashboard_waffle <- renderPlot({
    sickbed() %>%
      group_by(bed) %>%
      summarise(n = n()) %>%
      mutate(bed = ifelse(bed == 1, "In Use", "Empty")) %>%
      mutate(bed = factor(bed)) %>%
      ggplot(aes(fill = bed, values = n)) +
      geom_waffle(n_rows = 8, color = "white", flip = TRUE) +
      scale_fill_manual(
        name = NULL,
        values = c("#fe346e", "#5b8c5a"),
        labels = c("In Use", "Empty")
      ) +
      coord_equal() +
      theme_enhance_waffle() +
      theme_void() +
      theme(legend.position = "bottom")
  })

  ## data ----
  output$data_sickbed <- renderDataTable({
    summary() %>%
      arrange(desc(사용율)) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 50, 100, 200, 300),
          dom = "Blfrtip",
          buttons = c("copy", "excel", "print")
        )
      ) %>%
      formatStyle("사용율",
        color = "#f4dada",
        background = styleColorBar(c(0, 100), "#ce0f3d"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  ## 환자분포 ----
  output$distribution_serious_age <- renderPlotly({
    p <- sickbed() %>%
      ggplot(aes(중증도변화, 나이)) +
      geom_boxplot() +
      geom_jitter(aes(color = 성별)) +
      scale_color_manual(values = c("#fe346e", "#381460")) +
      ggthemes::theme_few() +
      labs(
        title = "중증도별 나이분포"
      )
    plotly::ggplotly(p)
  })
  
  output$distribution_etc <- renderPlotly({
    p <- sickbed() %>%
        group_by(특이사항) %>%
        summarise(n = n()) %>%
        filter(is.na(특이사항) == FALSE) %>%
        ggplot(aes(reorder(특이사항, n), n)) +
        geom_col(aes(fill = 특이사항)) +
        theme(legend.position = "top") +
        scale_fill_brewer(palette = 7) +
        ggthemes::theme_few() +
        labs(
          title = "특이사항",
          x = "특이사항", 
          y = "Count"
        )
    ggplotly(p)
  })
  
  ## data raw ----
  output$data_raw <- renderDataTable({
    sickbed() %>%
      arrange(desc(나이)) %>%
      datatable(
        extensions = "Buttons",
        options = list(
          pageLength = 50,
          lengthMenu = c(10, 50, 100, 200, 300),
          dom = "Blfrtip",
          buttons = c("copy", "excel", "print")
        )
      ) %>%
      formatStyle("나이",
                  color = "#f4dada",
                  background = styleColorBar(c(0, 100), "#ce0f3d"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      )
  })
  
  
}

# run app  --------------
shinyApp(ui, server)
