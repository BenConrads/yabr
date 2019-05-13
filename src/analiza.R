rm(list=ls())
setwd('C:\\Users\\Ben\\Documents\\SGH\\MAGISTERSKIE\\Praca')

# install.packages('jsonlite')
# install.packages('purrr')
# install.packages('dplyr')
# install.packages('tm')
# install.packages('qdap')
# install.packages('stringr')
# install.packages('scales')
# install.packages('SnowballC')

source('src/odczyt.R')
source('src/text_library.R')
source('src/wykresy_wstępne.R', echo=T, encoding = 'UTF-8')

library(tm)
library(qdap)
library(SnowballC)


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

load("data/POS.RData")
# speeches_pos <- pos(speech_vec, parallel = T)
# save(speeches_pos, file="data/POS.RData")
rm(speech_vec)

speeches_corpus <- speeches_corpus %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords('en'))

###
speeches_dtm <- DocumentTermMatrix(speeches_corpus, control = list(weighting = weightTfIdf))
speeches_dtm
speeches_dtm <- removeSparseTerms(speeches_dtm, 0.8)
speeches_dtm_mat <- as.matrix(speeches_dtm)
speeches_dtm_df <- as.data.frame(speeches_dtm_mat)
speeches_train_df <- cbind.data.frame(speeches_dtm_df, speeches_pos$POSprop)
speeches_train_df <- cbind.data.frame(speeches_train_df, club=speeches_df$club)

###
log_model <- glm(formula = club~., family = binomial(), data = speeches_train_df)
log_model2 <- step(log_model, direction = 'both')
