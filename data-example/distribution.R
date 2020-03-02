plotly::ggplotly(
sickbed %>%
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
)
