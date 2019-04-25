library(stringr)

rm_punct <- function(x) {
  return(gsub("[[:punct:]]", " ", x))
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
  proc_text <- str_replace_all(proc_text, "Mrs[ \\.]", "Mistress ")
  proc_text <- str_replace_all(proc_text, "n't", " not")
  proc_text <- str_replace_all(proc_text, "(Dr[ \\.]|Ph\\.D[ \\.])", "Doctor ")
  proc_text <- str_replace_all(proc_text, "Gov[ \\.]", "Governor ")
  proc_text <- str_replace_all(proc_text, "Hon[ \\.]", "Honorable ")
  proc_text <- str_replace_all(proc_text, "Jr", "Junior")
  proc_text <- str_replace_all(proc_text, "(H\\.R\\.|H\\.B\\.)", "House Bill ")
  proc_text <- str_replace_all(proc_text, "U\\.\\S\\.C\\.", "United States Court ")
  proc_text <- str_replace_all(proc_text, "U\\.\\S\\.", "United States")
  proc_text <- str_replace_all(proc_text, "USS", "United States Ship")
  proc_text
}