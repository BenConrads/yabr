library(jsonlite)
library(purrr)
library(dplyr)
speeches_list<- read_json('data.json')
speeches_list <- speeches_list[[1]]
dates <- speeches_list %>%
  map('date')
speeches_df <- speeches_list %>%
  map('speech_list') %>%
  map(purrr::transpose) %>%
  map(~map(.,unlist)) %>%
  map(as.data.frame) %>%
  map2(dates, ~mutate(.x, date=.y)) %>%
  bind_rows()

all_topics <- speeches_df %>%
  group_by(topic) %>%
  summarize(count=n()) %>%
  arrange(topic)

all_speakers <- speeches_df %>%
  distinct(speaker, house) %>%
  arrange(house, speaker)
