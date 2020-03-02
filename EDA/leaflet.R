library(tidyverse)
library(leaflet)
library(lubridate)
library(leaflet.minicharts)

hospital <- readxl::read_excel("hospital.xlsx")

sickbed <- readxl::read_excel("data-example/2020-02-23.xlsx")
sickbed <- sickbed %>%
  mutate(date = ymd(date)) %>%
  mutate(중증도변화 = factor(중증도변화, level = c("경증", "중증도", "중증", "최중증"))) %>%
  mutate(bed = ifelse(is.na(이름) == TRUE, 0, 1))

summary <- sickbed %>%
  group_by(hospital) %>%
  summarise(전체 = n(), 사용 = sum(bed), 미사용 = 전체-사용, 퍼센트 = round(사용/전체*100, 1))

join <- hospital %>%
  left_join(summary)
  # mutate(level = ifelse(퍼센트 <= 50, "low", ifelse(퍼센트 <= 75, "mid", "high")))
  
leaflet() %>%
  addTiles() %>%
  addMinicharts(
    join$long, join$lat,
    type = "pie",
    chartdata = join[, c("사용", "미사용")],
    colorPalette = c("#4fc13c", "#cccccc")
  )

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

sickbed %>%
  group_by(bed) %>%
  summarise(n = n()) %>%
  mutate(bed = ifelse(bed == 1, "사용", "미사용")) %>%
  mutate(bed = factor(bed)) %>%
  ggplot(aes(fill = bed, values = n)) +
  geom_waffle(n_rows = 8, color = "white", flip = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("#fe346e", "#5b8c5a"),
    labels = c("사용", "미사용")
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  theme_void(base_family = "NanumGothic") +
  theme(legend.position = "bottom") +
  labs(title = "전체병상 사용율")

plotly::ggplotly(
sickbed %>%
  ggplot(aes(나이, fill = 중증도변화)) +
  geom_histogram(binwidth = 10, color = "black", position = "dodge") +
  scale_fill_manual(values = c("green", "yellow", "orange", "red", "grey"))
)

plotly::ggplotly(
sickbed %>%
  ggplot(aes(중증도변화, 나이)) +
  geom_boxplot() +
  geom_jitter(aes(color = 성별)) +
  scale_color_manual(values = c("#fe346e", "#381460")) +
  ggthemes::theme_few() +
  labs(
    title = "중증도별 나이분포"
  )
)

