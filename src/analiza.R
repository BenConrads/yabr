rm(list=ls())
setwd('C:\\Users\\Ben\\Documents\\SGH\\MAGISTERSKIE\\Praca')
source('odczyt.R')
source('text_library.R')

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

speeches_pos <- pos(speech_vec, parallel = T)
save(speeches_pos, file="POS.RData")
rm(speech_vec)

