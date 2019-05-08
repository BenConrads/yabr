rm(list=ls())
setwd('C:\\Users\\Ben\\Documents\\SGH\\MAGISTERSKIE\\Praca')
source('src/odczyt.R')
source('src/text_library.R')

library(tm)
library(qdap)

speeches_corpus <- VCorpus(VectorSource(speeches_df$speech))

speeches_corpus <- speeches_corpus %>%
  tm_map(content_transformer(deabbreviate)) %>%
  tm_map(content_transformer(num2word)) %>%
  tm_map(content_transformer(rm_punct)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace)

corpus_list <- unclass(speeches_corpus)
speech_vec <- corpus_list$content %>%
  map(`[`, 'content') %>%
  map_chr(unlist)
rm(corpus_list)

# load("data/POS.RData")
speeches_pos <- pos(speech_vec, parallel = T)
save(speeches_pos, file="data/POS.RData")
rm(speech_vec)

#write.csv(all_speakers, "speakers.csv")