library(stringr)
library(purrr)
library(dplyr)

rm_punct <- function(x) {
  return(gsub("[^[:alnum:]\\.]", " ", x))
}

num2word <- function(x) {
  ones <- c("one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine ")
  teens <- c("eleven ", "twelve ", "thirteen ", "fourteen ", "fifteen ", "sixteen ", "seventeen ","eighteen ", "nineteen ")
  tens <- c("ten ", "twenty ", "thirty ", "fourty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety ")
  aboves <- c("","thousand", "milion", "billion", "trillion", "quadrillion", "quintillion")
  
  proc_text <- str_replace_all(x, "(?<=\\d{1,3}),(?=\\d{1,3})", "")
  proc_text <- str_replace_all(proc_text, "\\$([\\d\\.]+)", "\\1 dollars")
  proc_text <- str_replace_all(proc_text, "(?<=\\d)\\.(?=\\d)", " point ")
  for(i in 6:1) {
    proc_text <- str_replace_all(proc_text,
                                sprintf("0{3}(?=\\d{%i})",i*3),
                                "")
    proc_text <- str_replace_all(proc_text,
                                 sprintf("(\\d{1,3})(\\d{%i})",i*3),
                                 sprintf("\\1%s \\2", aboves[i+1]))
      }
  proc_text <- str_replace_all(proc_text, "0(?=\\d{2})","")
  for (i in 1:9) {
    proc_text <- str_replace_all(proc_text, sprintf("%i(?=\\d{2})",i), sprintf("%s hundred ", ones[i]))
    proc_text <- str_replace_all(proc_text, sprintf("%i",i+10), teens[i])
  }
  for (i in 1:9) {
    proc_text <- str_replace_all(proc_text, sprintf("%i(?=\\d)",i), tens[i])
  }
  for (i in 1:9) {
    proc_text <- str_replace_all(proc_text, sprintf("%i",i), ones[i])
  }
  proc_text <- str_replace_all(proc_text, "0", "")
  proc_text
}

deabbreviate <- function(x) {
  proc_text <- str_replace_all(x,"Mr[ \\.]", "Mister ")
  proc_text <- str_replace_all(proc_text, "Ms[ \\.]", "Miss ")
  proc_text <- str_replace_all(proc_text, "\\.com", "_dot_com")
  proc_text <- str_replace_all(proc_text, "Mrs[ \\.]", "Mistress ")
  proc_text <- str_replace_all(proc_text, "n't", "_not")
  proc_text <- str_replace_all(proc_text, "'ll", "_will")
  proc_text <- str_replace_all(proc_text, "let's", "let us")
  proc_text <- str_replace_all(proc_text, "Let's", "Let us")  
  proc_text <- str_replace_all(proc_text, "(Dr[ \\.]|Ph\\.D[ \\.])", "Doctor ")
  proc_text <- str_replace_all(proc_text, "Gov[ \\.]", "Governor ")
  proc_text <- str_replace_all(proc_text, "Hon[ \\.]", "Honorable ")
  proc_text <- str_replace_all(proc_text, "Jr", "Junior")
  proc_text <- str_replace_all(proc_text, "Res\\.", "Resolution")
  proc_text <- str_replace_all(proc_text, "(H\\.R\\.|H\\.B\\.)", "House Bill ")
  proc_text <- str_replace_all(proc_text, "U\\.\\S\\.C\\.", "United States Court ")
  proc_text <- str_replace_all(proc_text, "U\\.\\S\\.", "United States")
  proc_text <- str_replace_all(proc_text, "USS", "United States Ship")
  proc_text <- str_replace_all(proc_text, "\\s[[:alpha:]]\\.", " ")
  proc_text <- str_replace_all(proc_text, "\\.\\s\\.\\s\\.\\s", " ")
  proc_text
}

strip_sentences <- function(df, x, n_sent=3, min_len=50) {
  speech_vec <- str_replace_all(df[[x]], "\\.$", "")
  rest_df <- df[,!names(df)==x]
  stripped <- str_split(speech_vec, "\\.")
  temp_list <- list()
  rest_list <- list()
  for (i in seq_along(stripped)) {
    temp_list[[i]] <- list()
    rest_list[[i]] <- list()
    for (col in names(rest_df)) {
      rest_list[[i]][col]<-rest_df[i,col]
    }
    pack <- list()
    pack_len <- length(stripped[[i]])
    pack_seq <- 1:pack_len
    iter_seq <- 1:ceiling(pack_len/n_sent)
    for (j in iter_seq) {
      sent_vec <- stripped[[i]][ceiling(pack_seq/n_sent)==j]
      pack[[j]] <- paste0(sent_vec, collapse=" ")
      if (str_length(pack[[j]]) < min_len) {
        pack[[j-1]] <- paste0(pack[[j-1]], pack[[j]], collapse=" ")
        pack <- pack[-j]
      }
    }
    temp_list[[i]]$sentences <- pack
  }
  temp2 <- temp_list %>%
    map('sentences') %>%
    map(~unlist(.)) %>%
    map(~data.frame(speech=., stringsAsFactors=F)) %>%
    map2(rest_list, ~mutate(.x,
                            house=.y$house,
                            speaker=.y$speaker,
                            topic=.y$topic,
                            date=.y$date,
                            state=.y$state,
                            club=.y$club,
    )) %>%
    bind_rows()
  temp2
}
