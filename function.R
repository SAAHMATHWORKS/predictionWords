library(textclean)
#library(tm)
library(data.table)


load("data.RData")

# function to return highly probable previous word given two successive words
triWords <- function(w1, w2, n = 5) {
  pwords <- tri_words[.(w1, w2)][order(-Prob)]
  if (any(is.na(pwords)))
    return(biWords(w2, n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_3])
  count <- nrow(pwords)
  bwords <- biWords(w2, n)[1:(n - count)]
  return(c(pwords[, word_3], bwords))
}

# function to return highly probable previous word given a word
biWords <- function(w1, n = 5) {
  pwords <- bi_words[w1][order(-Prob)]
  if (any(is.na(pwords)))
    return(uniWords(n))
  if (nrow(pwords) > n)
    return(pwords[1:n, word_2])
  count <- nrow(pwords)
  unWords <- uniWords(n)[1:(n - count)]
  return(c(pwords[, word_2], unWords))
}

uniWords <- function(n = 5) {  
  return(sample(uni_words[, word_1], size = n))
}
cleanInput <-function(input) {
  # 1. Separate words connected with - or /
  input <- gsub("-", " ", input)
  input <- gsub("/", " ", input)
  
  # 2. Establish end of sentence, abbr, number, email, html
  input <- gsub("\\? |\\?$|\\! |\\!$", " EEOSS ", input)
  input <- gsub("[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\. ", " AABRR ", input)
  input <- gsub("\\. |\\.$", " EEOSS ", input)
  input <- gsub("[0-9]+"," NNUMM ",input)
  input <- gsub("\\S+@\\S+","EEMAILL",input) 
  input <- gsub("[Hh}ttp([^ ]+)","HHTMLL",input) 
  input <- gsub("RT | via"," RTVIA ",input) # retweets
  input <- gsub("@([^ ]+)","ATPPLE",input) # @people
  input <- gsub("[@][a - zA - Z0 - 9_]{1,15}","UUSRNMSS",input) # usernames
  
  # 3. to lower
  input <- tolower(input)
  
  # 4. Remove/replace &, @, 'm, 's, 'are, 'll, etc...
  input <- gsub(" & ", " and ", input)
  input <- gsub(" @ ", " at ", input)
  input <- replace_contraction(input)
  input <- gsub("'s", "", input) 
  input <- gsub("haven't", "have not", input)
  input <- gsub("hadn't", "had not", input)
  
  # 5. Remove emoji's, emoticons
  input <- gsub("[^\x01-\x7F]", "", input)
  
  # 6. Remove g, mg, lbs etc; removes all single letters except "a" and "i"
  
  input <- gsub(" [1-9]+g ", " ", input) # grams
  input <- gsub(" [1-9]+mg ", " ", input) # miligrams, etc
  input <- gsub(" [1-9]+kg ", " ", input)
  input <- gsub(" [1-9]+lbs ", " ", input)
  input <- gsub(" [1-9]+s ", " ", input) # seconds, etc
  input <- gsub(" [1-9]+m ", " ", input)
  input <- gsub(" [1-9]+h ", " ", input)
  input <- gsub(" +g ", " ", input) # grams
  input <- gsub(" +mg ", " ", input) # miligrams, etc
  input <- gsub(" +kg ", " ", input)
  input <- gsub(" +lbs ", " ", input)
  input <- gsub(" +s ", " ", input) # seconds, etc
  input <- gsub(" +m ", " ", input)
  input <- gsub(" +h ", " ", input)
  input <- gsub(" +lbs ", " ", input)
  input <- gsub(" +kg ", " ", input)
  

  
  # 8. remove all single letters eccept i and a
  input <- gsub(" u ", " you ", input)
  input <- gsub(" [b-hj-z] ", " ", input)
  
  # 9. remove profanity
  #input <- removeWords(input, profanity[,1])
  
  # 10. remove extra spaces
  # input <- gsub("^[ ]{1,10}","",input)
  # input <- gsub("[ ]{2,10}"," ",input)
  input <- stripWhitespace(input)
  # remove space at end of phrase
  input <- gsub(" $", "", input)
  return(input)
}
# The prediction app
getWords <- function(str, n){
  require(quanteda)
  str <- cleanInput(str)
  tokens <- tokens(x = char_tolower(str))
  tokens <- rev(rev(tokens[[1]])[1:2])
  
  words <- triWords(tokens[1], tokens[2], n)
  chain_1 <- paste(tokens[1], tokens[2], words[1], sep = " ")
  
  print(words)
}
