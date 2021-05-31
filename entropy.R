library(readr) # needed to read compressed tsv directly
library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)
french_words <- as.data.frame(read.csv(file = "fr_FR_NewOS_log10_3000.txt"))
french_word_column <- french_words$word
french_singles <- vector()
for (f in french_word_column){if (!(f %in% french_singles)){french_singles<-append(french_singles, f)}} 
#helper functions
#input a word, return a filtered word that represents a possible view of word when fixating at position 2
#using a 0.25 per letter drop rate
filter_word_2 <- function(word) {
  r_1 <- sample(1:4,1)
  if (r_1 == 1){p_1 <- substr(word,1,1)}
  else if (r_1 == 2){p_1 <- substr(word,1,1)}
  else if (r_1 == 3){p_1 <- substr(word,1,1)}
  else {p_1 <- "_"}
  p_2 <- substr(word,2,2)
  r_3 <- sample(1:4,1)
  if (r_3 == 1){p_3 <- substr(word,3,3)}
  else if (r_3 == 2){p_3 <- substr(word,3,3)}
  else if (r_3 == 3){p_3 <- substr(word,3,3)}
  else {p_3 <- "_"}
  r_4 <- sample(1:2,1)
  if (r_4 == 1){p_4 <- substr(word,4,4)}
  else {p_4 <- "_"}
  r_5 <- sample(1:4, 1)
  if (r_5 == 1){p_5 <- "_"}
  else if (r_5 == 2){p_5 <- "_"}
  else if (r_5 == 3){p_5 <- "_"}
  else {p_5 <- substr(word, 5, 5)}
  p_6 <- "_"
  p_7 <- "_"
  return(c(p_1, p_2, p_3, p_4, p_5, p_6, p_7))
 

}
#input a word, return a character vector that represents a possible view of word when fixating at position 6
#using a 0.25 per letter drop rate
filter_word_6 <- function(word) {
  p_1 <- "_"
  p_2 <- "_"
  r_3 <- sample(1:4, 1)
  if (r_3 == 1){p_3 <- "_"}
  else if (r_3 == 2){p_3 <- "_"}
  else if (r_3 == 3){p_3 <- "_"}
  else {p_3 <- substr(word, 3, 3)}
  r_4 <- sample(1:2,1)
  if (r_4 == 1){p_4 <- substr(word,4,4)}
  else {p_4 <- "_"}
  r_5 <- sample(1:4,1)
  if (r_5 == 1){p_5 <- substr(word,5,5)}
  else if (r_5 == 2){p_5 <- substr(word,5,5)}
  else if (r_5 == 3){p_5 <- substr(word,5,5)}
  else {p_5 <- "_"}
  p_6 <- substr(word,6,6)
  r_7 <- sample(1:4,1)
  if (r_7 == 1){p_7 <- substr(word,7,7)}
  else if (r_7 == 2){p_7 <- substr(word,7,7)}
  else if (r_7 == 3){p_7 <- substr(word,7,7)}
  else {p_7 <- "_"}
  return(c(p_1, p_2, p_3, p_4, p_5, p_6, p_7))}
  
#check if two words match
is_match <- function(word_1, word_2){
  i <- 1
  for (l in word_1){
    if (!(l == "_")){if(!(l == substr(word_2, i, i) )){return(FALSE)}}
    i <- i + 1
  }
  return(TRUE)
}
  
 

#input a word, return vector of words in french_word_column that word could be 
#with each possible word listed the number of times it occurs in french_word_column
find_matching_words <- function(word){
  match_list <- vector()
  for (w in french_word_column){
    if(is_match(word,w)){match_list <- append(match_list,w)}
   
  }
  return(match_list)

}

#get_pw (input list of words that are matches, return a data frame
#with columns of the words (non redundant) and the corresponding pw with column names
#word and pw

get_pw <- function(word_list){
  freq_all <- french_words$wpm
  freq_match <- vector()
 for(i in word_list){
   index_of_word <- which(french_word_column == i)
   freq_word <- freq_all[index_of_word]
   freq_match <-append(freq_match, freq_word)} 
 total_freq <- sum(freq_match)
 pw_match <- vector()
 for(i in freq_match){
   pw_match <- append(pw_match,i/total_freq)}
 return(data.frame(word = word_list, pw = pw_match))
 }





  





# makes lists of entropy values at fixation 6 and 2
list_of_entropies_6 <- vector()
for(w in french_singles){
  ent_sum <- 0
  for(i in 1:3){
    filtered <- filter_word_6(w)
    matches <- find_matching_words(filtered)
    if(is.null(matches[1])){ent <- 0}
    else{
    pw_matches <- get_pw(matches)
    pw_list <- pw_matches$pw
    words <- pw_matches$word
    ent_so_far <- 0
    for (f in words){
      pw_1 <- pw_list[which(words == f)]
      new_ent <- -1 * pw_1 * log(pw_1,base = 2)
      ent_so_far_ <- ent_so_far + new_ent}
    
    ent <- ent_so_far}
    ent_sum <- ent_sum + ent
    
  }

  avg_ent_6 <- ent_sum / 50
  list_of_entropies_6 <- append(list_of_entropies_6, avg_ent_6)
}

list_of_entropies_2 <- vector()
for(w in french_singles){
  ent_sum <- 0
  for (i in 1:3){
    filtered <- filter_word_2(w)
    matches <- find_matching_words(filtered)
    if(is.null(matches[1])){ent <- 0}
    else{
    pw_matches <- get_pw(matches)
    pw_list <- pw_matches$pw
    words <- pw_matches$word
    ent_so_far <- 0
    for (f in words){
      pw_1 <- pw_list[which(words == f)]
      new_ent <- -1*pw_1 * log(pw_1,base = 2)
      ent_so_far_ <- ent_so_far + new_ent}
    
    ent <- ent_so_far}
    ent_sum <- ent_sum + ent
    
  }
  
  avg_ent_2 <- ent_sum / 50
  list_of_entropies_2 <- append(list_of_entropies_2, avg_ent_2)}
#make list of entropy differences
ent_dif <- vector()
p <- 1
for(i in list_of_entropies_6){
  ent_dif <- append(ent_dif,(i - list_of_entropies_2[p]))
  p <- p + 1}

 hist(ent_dif)

#map each each unique entropy difference from list of entropies to number of times that entropy occurs
# plot this data 

  
  
  
                        
               