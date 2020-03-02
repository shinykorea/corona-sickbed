library(tidyverse)
library(leaflet)

hospital <- readxl::read_excel("hospital.xlsx")

sickbed <- readxl::read_excel("data-example/2020-02-23.xlsx")
sickbed <- sickbed %>%
  mutate(date = ymd(date)) %>%
  mutate(bed = ifelse(is.na(이름) == TRUE, 0, 1))

summary <- sickbed %>%
  group_by(hospital) %>%
  summarise(전체 = n(), 사용 = sum(bed), 퍼센트 = round(사용/전체*100, 1))


join <- hospital %>%
  left_join(summary) %>%
  mutate(level = ifelse(퍼센트 <= 50, "low", ifelse(퍼센트 <= 75, "mid", "high")))
  
pal <- colorFactor(c("green", "yellow", "red"), domain = c("low", "mid", "high"))

join %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = ~퍼센트/10,
    color = ~pal(level),
    stroke = FALSE,
    fillOpacity = 0.7
  )
  # addMarkers(~long, ~lat, popup = ~hospital, label = ~hospital)
