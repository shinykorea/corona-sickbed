# library ----
library(tidyverse)

library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(shiny.info)
library(shinyjs)

library(leaflet)
library(leaflet.minicharts)
library(highcharter)

library(DT)
library(knitr)

library(lubridate)
library(purrr)

## Make login DB
library(shinymanager)

### import from dropbox ----
library(rdrop2)
token <- readRDS("token.rds")

### Report
library(rmarkdown)



## header ----
header <- dashboardHeader(
  title = "G-CoMS 병상운용현황",
  titleWidth = 350,
  tags$li(a(
    href = "https://www.gg.go.kr/",
    img(
      src = "logo2.png",
      title = "경기도청홈페이지", height = "30px"
    ),
    style = "padding-top:10px; padding-bottom:10px;"
  ),
  class = "dropdown"
  )
)

## sidebar ----
sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    menuItem("대시보드", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("데이터", tabName = "data", icon = icon("database")),
    menuItem("Map", tabName = "map", icon = icon("map-marked-alt")),
    radioButtons("reporttype", "Report type", c("pdf", "docx"), inline = T),
    shinyWidgets::downloadBttn(
      outputId = "report_sickbed",
      label = "Download report",
      style = "bordered",
      color = "default"
    )
  )
)

## body ----
body <- dashboardBody(
  
  ### style
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css")
  ),
  
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  useShinyjs(),
  tags$head(tags$style(type = "text/css", "table.dataTable tr.selected td, table.dataTable td.selected {background-color: #d1c4e9 !important;}")),
  tags$head(tags$style(type = "text/css", "th, td {text-align:center !important;}")),
  tags$head(tags$style(type = "text/css", "html, body {height:100% !important;}")),
  tags$head(includeCSS("www/includefont.css")),
  
  ## Change Font Here ---------------------------------------------------
  ## Possible Options :
  ## GGTitle_B, GGTitle_L, GGTitle_M
  ## GGBatang_B, GGBatang_R
  tags$head(tags$style(type = "text/css", "*{font-family:GGTitle_L}")),
  tags$head(tags$style(type = "text/css", ".indicator {height:0.3em;}")),
  tags$head(tags$style(type = "text/css", "table {table-layout:fixed; width:100%;}")),
  tags$head(tags$style(type = "text/css", ".btn.btn-default.action-button.buttons-fab.shiny-bound-input { background-color: #b388ff;}")),
  tags$head(tags$style(type = "text/css", ".tabs .tab a, .tabs .tab a:hover, .tabs .tab a.active {font-size:1.5em; color : #311b92;}")),
  tags$head(tags$style(type = "text/css", ".skin-blue .main-header .logo .logo:hover {font-size:1.5em; color : #311b92;}")),
  tags$head(tags$style(HTML('.main-header .logo {font-family: "GGTitle_L", serif; font-weight: bold; font-size: 20px;}'))),
  
  
  ### Last Update
  uiOutput("shinyinfo"),
  
  tabItems(
    ### Dashboard ----
    tabItem(
      tabName = "dashboard",
      fluidPage(
        column(width = 4, highchartOutput(outputId = "pie1")),
        column(width = 4, highchartOutput(outputId = "pie2")),
        column(width = 4, highchartOutput(outputId = "pie4"))
      ),
      fluidPage(
        column(8, highchartOutput("병원별") %>% withLoader(type = "html", loader = "loader7")),
        column(4, highchartOutput("중환자실") %>% withLoader(type = "html", loader = "loader7"))
      ),
      fluidPage(
        column(12, leafletOutput("map"))
      )
    ),
    
    ## raw data ----
    tabItem(
      tabName = "data",
      checkboxGroupInput(
        "hospital1",
        "분류1",
        choices = c("중환자실", "음압", "비음압"),
        selected = c("중환자실", "음압", "비음압"),
        inline = TRUE
      ),
      fluidPage(
        dataTableOutput("data_raw")
      )
    ),
    
    tabItem(
      tabName = "map",
      tags$style(type = "text/css", "#map2 {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map2")
    )
  )
)

# ui ----
ui <- dashboardPage(header, sidebar, body)
ui <- secure_app(ui, enable_admin = T)

# server ----

server <- function(input, output, session) {
  
  
  res_auth <- secure_server(
    check_credentials = check_credentials("database.sqlite"),
    timeout = 60 * 24 * 30,  ## 30 days
  )
  
  ## data ----
  
  file_info <- reactive({
    file <- drop_dir("corona19", dtoken = token) %>%
      select(name, client_modified) %>%
      arrange(desc(client_modified))
    
    file_name <- file %>%
      pull(name) %>%
      .[1]
    
    file_date <- file %>%
      pull(client_modified) %>%
      .[1] %>%
      lubridate::ymd_hms() + 60*60*9
    
    if (!dir.exists("data")){
      dir.create("data")
    }
    
    if (length(list.files("data/")) > 0){
      file.remove(paste0("data/", list.files("data/")))
    }
    
    paste0("corona19/", file_name) %>% drop_download(local_path = "data", overwrite = TRUE, dtoken = token)
    return(list(file_name, file_date))
  })
  
  
  data <- reactive({
    data <- readxl::read_excel(paste0("data/", file_info()[[1]]))
    # data <- readxl::read_excel("sample-data.xlsx")
    data <- data %>% select(1:6)
    colnames(data) <- c("병원명", "분류1", "분류2", "총병상", "사용병상", "가용병상")
    return(data)
  })
  
  data2 <- reactive({
    data <- readxl::read_excel(paste0("data/", file_info()[[1]]), sheet = 2)
    # data <- readxl::read_excel("sample-data.xlsx", sheet = 2)
    data <- data %>% select(1:3)
    colnames(data) <- c("센터명", "총객실", "사용객실")
    return(data)
  })
  
  
  
  hospital <- reactive({
    hospital <- readxl::read_excel("hospital.xlsx")
    return(hospital)
  })
  
  join <- reactive({
    join <- data() %>%
      filter(분류1 == "합계")
    
    join <- hospital() %>%
      left_join(join)
    
    return(join)
  })
  
  ################### data preprocess 4 piechart #############################3
  data_smmry <- reactive({
    data() %>%
      group_by(`분류1`) %>%
      summarise(사용병상 = sum(사용병상), 가용병상 = sum(가용병상))
  })
  
  pie1_data <- reactive({
    data_smmry() %>%
      filter(`분류1` == "합계") %>%
      gather(type, n, ends_with("병상")) %>%
      mutate(sum_bed = sum(n))
  })
  
  pie1_2_data <- reactive({
    data_smmry() %>%
      filter(`분류1` != "합계") %>%
      gather(type, n, ends_with("병상"))
  })
  
  pie2_data <- reactive({
    data_smmry() %>%
      filter(`분류1` == "중환자실") %>%
      gather(type, n, ends_with("병상")) %>%
      mutate(sum_bed = sum(n))
  })
  
  pie3_data <- reactive({
    data_smmry() %>%
      filter(`분류1` == "음압") %>%
      gather(type, n, ends_with("병상")) %>%
      mutate(sum_bed = sum(n))
  })
  
  pie4_data <- reactive({
    data2() %>%
      mutate(가용객실 = 총객실 - 사용객실) %>%
      summarise(
        총객실 = sum(총객실),
        사용객실 = sum(사용객실),
        가용객실 = sum(가용객실)
      ) %>%
      mutate(사용률 = round(사용객실 / 총객실 * 100, 1))
  })
  
  ## dashboard ----
  #### pie 1
  pie1 <- reactive({
    highchart() %>%
      hc_title(text = "<b>전체 병상 가동률</b>") %>%
      hc_add_series(
        type = "pie", size = "100%", innerSize = "50%",
        data = list(
          list(
            y = pie1_data() %>% filter(type == "사용병상") %>% select(n) %>% unlist() %>% as.vector(),
            z = pie1_data() %>% filter(type == "사용병상") %>% select(sum_bed) %>% unlist() %>% as.vector(),
            name = "사용병상",
            color = c("#e5dfdf"),
            drilldown = "사용병상",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -40,
              style = list(textAlign = "center", fontSize = "1.2em * 1vw", color = "black", textDecoration = "none")
            )
          ),
          list(
            y = pie1_data() %>% filter(type == "가용병상") %>% select(n) %>% unlist() %>% as.vector(),
            z = pie1_data() %>% filter(type == "가용병상") %>% select(sum_bed) %>% unlist() %>% as.vector(),
            name = "가용병상",
            color = c("#4d80e4"),
            drilldown = "가용병상",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -40,
              style = list(textAlign = "center", fontSize = "1.2em * 1vw", color = "black", textDecoration = "none")
            )
          )
        )
      ) %>%
      hc_tooltip(headerFormat = "", pointFormat = "{point.name}: {point.y}") %>%
      hc_add_series(pie1_data() %>% filter(type == "사용병상"),
                    type = "pie",
                    hcaes(x = n, y = sum_bed, z = round(n / sum_bed * 100, 1)),
                    innerSize = "100%", size = "100%",
                    dataLabels = list(
                      format = "<span style='font-size:30px'>{point.z}%</span>",
                      useHTML = T,
                      style = list(textAlign = "center", fontSize = "1.2em * 2vw", color = "black"),
                      align = "center",
                      distance = -170
                    ),
                    showInLegend = F,
                    enableMouseTracking = FALSE
      ) %>%
      hc_colors(c("#381460", "#b21f66", "#fe346e"))
  })
  
  pie1_drill_1 <- reactive({
    pie1_2_data() %>%
      filter(type == "사용병상") %>%
      select(`분류1`, n)
  })
  
  pie1_drill_2 <- reactive({
    pie1_2_data() %>%
      filter(type == "가용병상") %>%
      select(`분류1`, n)
  })
  
  
  ## renderUI ----
  out.pie1 <- reactive({
    pie1() %>% hc_drilldown(series = list(
      list(id = "사용병상", type = "pie", data = list_parse2(pie1_drill_1())),
      list(id = "가용병상", type = "pie", data = list_parse2(pie1_drill_2()))
    ))
  })
  
  output$pie1 <- renderHighchart({
    out.pie1() %>% 
      hc_exporting(enabled = TRUE,
                   filename = "plot")
  })
  
  ### pie2
  out.pie2 <- reactive({
    highchart() %>%
      hc_title(text = "<b>중환자실 가동률</b>") %>%
      hc_plotOptions(pie = list(center = c("50%", "50%"), dataLabels = list(useHTML = T, align = "center"))) %>%
      hc_add_series(
        type = "pie", size = "100%", innerSize = "50%",
        data = list(
          list(
            y = pie2_data() %>% filter(type == "사용병상") %>% select(n) %>% unlist() %>% as.vector(),
            z = pie2_data() %>% filter(type == "사용병상") %>% select(sum_bed) %>% unlist() %>% as.vector(),
            name = "사용병상",
            color = "#e5dfdf",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -40,
              style = list(textAlign = "center", fontSize = "1.2em * 1vw", color = "black", textDecoration = "none")
            )
          ),
          list(
            y = pie2_data() %>% filter(type == "가용병상") %>% select(n) %>% unlist() %>% as.vector(),
            z = pie2_data() %>% filter(type == "가용병상") %>% select(sum_bed) %>% unlist() %>% as.vector(),
            color = "#db4455",
            name = "가용병상",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -40,
              style = list(textAlign = "center", fontSize = "1.2em * 1vw", color = "black", textDecoration = "none")
            )
          )
        )
      ) %>%
      hc_tooltip(headerFormat = "", pointFormat = "{point.name}: {point.y}") %>%
      hc_add_series(pie2_data() %>% filter(type == "사용병상"),
                    type = "pie",
                    hcaes(x = n, y = sum_bed, z = round(n / sum_bed * 100, 1)),
                    innerSize = "100%", size = "100%",
                    dataLabels = list(
                      format = "<span style='font-size:30px'>{point.z}%</span>",
                      useHTML = T,
                      style = list(textAlign = "center", fontSize = "1.2em * 2vw"),
                      align = "center",
                      distance = -170
                    ),
                    showInLegend = F,
                    enableMouseTracking = FALSE
      )
  })
  
  output$pie2 <- renderHighchart({
    out.pie2() %>% 
      hc_exporting(enabled = TRUE,
                   filename = "plot")
  })
  
  ## pie4
  out.pie4 <- reactive({
    highchart() %>%
      hc_title(text = "<b>생활치료센터 가동률</b>") %>%
      hc_plotOptions(pie = list(center = c("50%", "50%"), dataLabels = list(useHTML = T, align = "center"))) %>%
      hc_add_series(
        type = "pie", size = "100%", innerSize = "50%",
        data = list(
          list(
            y = pie4_data() %>% select(사용객실) %>% unlist() %>% as.vector(),
            z = pie4_data() %>% select(총객실) %>% unlist() %>% as.vector(),
            name = "사용객실",
            color = "#e5dfdf",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -45,
              style = list(textAlign = "center", fontSize = "1.2em * 1vw", color = "black", textDecoration = "none")
            )
          ),
          list(
            y = pie4_data() %>% select(가용객실) %>% unlist() %>% as.vector(),
            z = pie4_data() %>% select(총객실) %>% unlist() %>% as.vector(),
            name = "가용객실",
            color = "#02a8a8",
            dataLabels = list(
              useHTML = TRUE,
              format = "<span style='font-size:12px'>{point.name}<br>{point.y}/{point.z}</span>",
              distance = -40,
              style = list(
                textAlign = "center",
                fontSize = "1.2em * 1vw",
                color = "black",
                textDecoration = "none"
              )
            )
          )
        )
      ) %>%
      hc_tooltip(headerFormat = "", pointFormat = "{point.name}: {point.y}") %>%
      hc_add_series(
        type = "pie", innerSize = "100%", size = "100%",
        data = list(
          list(
            y = pie4_data() %>% select(사용률) %>% unlist() %>% as.vector(),
            name = "사용률",
            dataLabels = list(
              format = "<span style='font-size:30px'>{point.y}%</span>",
              useHTML = T,
              style = list(textAlign = "center", fontSize = "1.2em * 2vw"),
              align = "center",
              distance = -170
            )
          )
        ),
        showInLegend = F,
        enableMouseTracking = FALSE
      )
  })
  
  output$pie4 <- renderHighchart({
    out.pie4() %>% 
      hc_exporting(enabled = TRUE,
                   filename = "plot")
  })
  ####################################### end piechart ###################################
  
  out.perhospital <- reactive({
    data() %>%
      filter(분류1 == "합계") %>%
      gather("가용", "병상수", 사용병상:가용병상) %>%
      mutate(가용 = factor(가용, level = c("사용병상", "가용병상"), ordered = TRUE)) %>%
      hchart(
        type = "column",
        hcaes("병원명", "병상수", group = "가용"),
        stacking = "normal",
        dataLabels = list(enabled = TRUE, color = "#323232", format = '<span style="font-size: 10px">{point.y}</span>')
      ) %>%
      hc_xAxis(
        title = "",
        labels = list(style = list(fontSize = "13px", color = "#323232", fontWeight = "bold"))
      ) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_colors(c("#e5dfdf", "#4d80e4")) %>%
      hc_legend(
        align = "inner", verticalAlign = "top",
        layout = "vertical", x = 0, y = 30
      ) %>%
      hc_title(text = "<b>병원별 병상운용 현황</b>") 
  })
  
  output$병원별 <- renderHighchart({
    out.perhospital() %>% 
      hc_exporting(enabled = TRUE,
                   filename = "plot")
  })
  
  out.icu <- reactive({
    data() %>%
      filter(분류1 == "중환자실") %>%
      filter(총병상 != 0) %>%
      select(병원명, 사용병상:가용병상) %>%
      gather("가용", "병상수", 사용병상:가용병상) %>%
      mutate(병원명 = factor(병원명, levels = c("성남의료원", "분당서울대", "명지병원", "고려대안산", "순천향부천", "아주대병원", "한림대성심", "일산병원", "동탄성심"))) %>%
      mutate(가용 = factor(가용, level = c("사용병상", "가용병상"), ordered = TRUE)) %>%
      arrange(병원명) %>%
      hchart(
        type = "column",
        hcaes("병원명", "병상수", group = "가용"),
        stacking = "normal",
        dataLabels = list(
          enabled = TRUE,
          color = "#323232",
          format = '<span style="font-size: 13px">{point.y}</span>'
        )
      ) %>%
      hc_colors(c("#e5dfdf", "#db4455")) %>%
      # hc_colors(c("#db4455")) %>%
      hc_yAxis(visible = FALSE) %>%
      hc_xAxis(
        title = "",
        labels = list(style = list(fontSize = "13px", color = "#323232", fontWeight = "bold"))
      ) %>%
      # hc_chart(inverted = TRUE) %>%
      hc_legend(
        align = "inner", verticalAlign = "top",
        layout = "vertical", x = 0, y = 30
      ) %>%
      hc_tooltip(pointFormat = "{point.y}병상") %>%
      hc_title(text = "<b>중환자실 운용 현황</b>")
  })
  
  output$중환자실 <- renderHighchart({
    out.icu() %>% 
      hc_exporting(enabled = TRUE,
                   filename = "plot")
  })
  
  ## map ----
  out.map <- reactive({
    join <- join() %>%
      select(병원명, long, lat, 사용병상, 가용병상) %>%
      rename("사용" = "사용병상", "가용" = "가용병상")
    
    leaflet() %>%
      addTiles() %>%
      # addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      setView(lng = 127.06, lat = 37.36, zoom = 9) %>%
      addMinicharts(
        join$long, join$lat,
        type = "pie",
        opacity = 0.8,
        layerId = join$병원명,
        chartdata = join[, c("사용", "가용")],
        colorPalette = c("#cccccc", "#4d80e4")
        # width = 45, height = 45
      )
  })
  
  output$map <- renderLeaflet({
    out.map()
  })
  
  ## map ----
  out.map2 <- reactive({
    join <- join() %>%
      select(병원명, long, lat, 사용병상, 가용병상) %>%
      rename("사용" = "사용병상", "가용" = "가용병상")
    
    leaflet() %>%
      addTiles() %>%
      # addProviderTiles("CartoDB.DarkMatter") %>%
      # setView(lng = 127.06, lat = 37.36, zoom = 9) %>%
      addMinicharts(
        join$long, join$lat,
        # type = "pie",
        layerId = join$병원명,
        # opacity = 0.8,
        chartdata = join[, c("사용", "가용")],
        colorPalette = c("darkred", "darkblue"),
        width = 70, height = 100,
        popup = popupArgs(
          labels = c("사용", "가용")
        ),
        showLabels = TRUE
      )
  })
  output$map2 <- renderLeaflet({
    out.map2()
  })
  
  ## data ----
  out.data_raw <- reactive({
    data() %>%
      filter(분류1 %in% input$hospital1)
    
  })
  
  output$data_raw <- renderDataTable({
    datatable(out.data_raw(),
              extensions = "Buttons",
              options = list(
                pageLength = 100,
                lengthMenu = c(10, 50, 100, 200, 300),
                dom = "Blfrtip",
                buttons = c("copy", "excel", "print")
              )
    )
  })
  
  output$shinyinfo <- renderUI(shiny.info::version(paste("Last Update:", file_info()[[2]]), position = "bottom right"))
  
  
  ## Report ----
  output$report_sickbed <- downloadHandler(
    filename = function() {
      paste0(paste0('Report_sickbed_', file_info()[[2]]), '.', input$reporttype)
    },
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:5) {
                       incProgress(1/5)
                       Sys.sleep(0.2)
                     }
                     
                     src <- normalizePath("www/report.Rmd")
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'report_sickbed.Rmd', overwrite = TRUE)
                     
                     out <- NULL
                     if (input$reporttype == "docx"){
                       out <- render('report_sickbed.Rmd', 
                                     word_document(toc=F, reference_docx= "/home/js/ShinyApps/corona-sickbed/www/style-ref.docx"),
                                     params=list(data = data(),
                                                 data2 = data2()),
                                     
                                     envir = new.env()
                       )
                     } else{
                       out <- render('report_sickbed.Rmd', 
                                     pdf_document(toc=F),
                                     params=list(data = data(),
                                                 data2 = data2()),
                                     
                                     envir = new.env()
                       )
                     }
                     file.rename(out, file)
                     
                   })
      
    })
}

# run app  --------------
shinyApp(ui, server)