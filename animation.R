
library(ggplot2)
library(gganimate)

covid <- raw_1 %>% arrange(CountryName,nuorongay ) %>%  group_by( CountryName) %>% 
  mutate(date=row_number(), max_cum=  max(CumulativeConfirmed)) %>% ungroup() %>%  filter(max_cum > 10000) %>% 
  select(CountryName, date,CumulativeConfirmed )
# Make a ggplot, but add frame=year: one image per year
ggplot(covid, aes(date, CumulativeConfirmed, size = CumulativeConfirmed, color = CountryName)) +
  geom_point() +
  # scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(date) +
  ease_aes('linear')