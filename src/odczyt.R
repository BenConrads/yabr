library(jsonlite)
library(purrr)
library(dplyr)
speeches_list<- read_json('data/data.json')
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
  arrange(house, speaker) %>%
  group_by(speaker, house) %>%
  summarize(count=n())

speakers_dict <- read.csv('data/Przypisanie.csv',stringsAsFactors = F)
speakers_dict <- speakers_dict %>%
  mutate(speaker_key = paste(speaker,house))
speeches_df <- speeches_df %>%
  mutate(speaker_key=paste(speaker,house)) %>%
  left_join(speakers_dict, by = "speaker_key") %>%
  select(-speaker.y, -house.y, -speaker_key) %>%
  rename(speaker=speaker.x, house=house.x)