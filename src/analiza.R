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
# install.packages('keras')
# install.packages('rAPACK')
# install.packages('lsa')
# install.packages('LSAfun')

source('src/odczyt.R')
source('src/text_library.R')
source('src/modele.R')
# source('src/wykresy_wstêpne.R', echo=T, encoding = 'UTF-8')

library(tm)
library(qdap)
library(SnowballC)
library(keras)
library(rARPACK)
library(lsa)
library(LSAfun)
library(parallel)

my_seed <- 60709
num_cores <- detectCores()
cl <- makeCluster(num_cores-1)

speeches_df$speech <- speeches_df$speech %>%
  deabbreviate %>%
  num2word %>%
  rm_punct %>%
  tolower %>%
  stripWhitespace %>%
  trimws

speeches_df <- speeches_df %>%
  filter(!(topic %in% c("Honoring", "Formal")))
speeches_df2 <- strip_sentences(speeches_df, 'speech')

speeches_corpus <- VCorpus(VectorSource(speeches_df2$speech))
  
## Rozró¿nienie czêœci mowy
#
# corpus_list <- unclass(speeches_corpus)
# speech_vec <- corpus_list$content %>%
#   map(`[`, 'content') %>%
#   map_chr(unlist)
# rm(corpus_list)
# 
# load("data/POS.RData")
# # speeches_pos <- pos(speech_vec, parallel = T)
# # save(speeches_pos, file="data/POS.RData")
# rm(speech_vec)

speeches_corpus <- speeches_corpus %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords, stopwords('en'))

rm(list=c("all_speakers", "all_topics", "dates", "speakers_dict", "speeches_list"))


### Podejœcie freq
speeches_dtm_frq <- DocumentTermMatrix(speeches_corpus)
X_mat <- as.matrix(speeches_dtm_frq)
y_vec <- ifelse(speeches_df2$club=='D',1,0)
rm(speeches_dtm_frq)

# Normalizacja
X_mat <- parApply(cl, X_mat, 2, function(x) {
  max_x <- max(x)
  min_x <- min(x)
  return((x-min_x)/(max_x-min_x))
})

set.seed(my_seed)
train_ind <- sample(1:nrow(speeches_df2), floor(nrow(speeches_df2)*0.8))
X_tr <- X_mat[train_ind,]
y_tr <- y_vec[train_ind]
X_test <- X_mat[-train_ind,]
y_test <- y_vec[-train_ind]
rm(X_mat)

# freq_model <- load_model_hdf5("data/modele/freq_model.h5")

freq_model <- dropout_model(X = X_tr, y = y_tr,
                            drop_in = 0.5, drop_out=0.2,
                            lay1 = 2048, lay2 = 1024, lay3 = 256,
                            decay=0.025, momentum=0.95, epochs = 50, verb = 1)

freq_model %>%
  evaluate(X_test, y_test, batch_size=16)


# freq_model %>%
#   save_model_hdf5("data/modele/freq_model.h5")

freq_model %>%
  callback

rm(list=c("X_tr", "X_test"))

### Podejœcie tf-idf
speeches_dtm_tfidf <- DocumentTermMatrix(speeches_corpus, control = list(weighting = weightTfIdf))
X_mat <- as.matrix(speeches_dtm_tfidf)
y_vec <- ifelse(speeches_df2$club=='D',1,0)
rm(speeches_dtm_tfidf)


#Normalizacja
X_mat <- parApply(cl, X_mat, 2, function(x) {
  max_x <- max(x)
  min_x <- min(x)
  return((x-min_x)/(max_x-min_x))
})
set.seed(my_seed)
train_ind <- sample(1:nrow(speeches_df2), floor(nrow(speeches_df2)*0.8))
X_tr <- X_mat[train_ind,]
y_tr <- y_vec[train_ind]
X_test <- X_mat[-train_ind,]
y_test <- y_vec[-train_ind]
rm(X_mat)

tfidf_model <- load_model_hdf5("data/modele/tfidf_model.h5")


# tfidf_model <- dropout_model(X = X_tr, y = y_tr,
#                              drop_in = 0.5, drop_out=0.2,
#                              lay1 = 2048, lay2 = 1024, lay3 = 256,
#                              decay=0.025, momentum=0.95, epochs = 50, verb = 1)

tfidf_model %>%
  evaluate(X_test, y_test, batch_size=16)

# tfidf_model %>%
#   save_model_hdf5("data/modele/tfidf_model.h5")

rm(list=c("X_tr", "X_test"))

## Podejœcie lsa
speeches_tdm_tfidf <- TermDocumentMatrix(speeches_corpus, control = list(weighting = weightTfIdf))
# lsa_decomp <- lsa(speeches_tdm_tfidf, dims=dimcalc_share())
# save(lsa_decomp, file="data/lsa.RData")
load("data/lsa.RData")
X_mat <- lsa_decomp$dk
y_vec <- ifelse(speeches_df2$club=='D',1,0)

set.seed(my_seed)
train_ind <- sample(1:nrow(speeches_df2), floor(nrow(speeches_df2)*0.8))
X_tr <- X_mat[train_ind,]
y_tr <- y_vec[train_ind]
X_test <- X_mat[-train_ind,]
y_test <- y_vec[-train_ind]
rm(X_mat)

lsa_model <- load_model_hdf5("data/modele/lsa_model.h5")

# lsa_model <- dropout_model(X = X_tr, y = y_tr,
#                            drop_in = 0.5, drop_out=0.2,
#                            lay1 = 1024, lay2 = 512, lay3 = 128,
#                            decay=0, momentum=0.95, epochs = 50, verb = 1)

lsa_model %>%
  evaluate(X_test, y_test, batch_size=16)

# lsa_model %>%
#   save_model_hdf5("data/modele/lsa_model.h5")

# lsa_lstm_model <- lstm_model(X = X_tr, y = y_tr,
#                            drop_in = 0.5, drop_out=0.2,
#                            lay1 = 2048, lay2 = 1024, lay3 = 256,
#                            decay=0, momentum=0.95, epochs = 50, verb = 1)
# 
# lsa_lstm_model %>%
#   evaluate(X_test, y_test, batch_size=16)

rm(list=c("X_tr", "X_test"))

