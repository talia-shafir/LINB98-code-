
# This script pre-processes the open subtitles and wiki corpora
# in each language to generate statistics and tables suitable
# for input to the neural network pattern generation script.

library(readr) # needed to read compressed tsv directly
library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)

#
en_letters_only <- function(x) !grepl("[^a-z]", x)
hb_letters_only <- function(x) !grepl("[^\u05D0-\u05EA]", x)
fr_letters_only <- function(x) !grepl("[^abcdefghijklmnopqrstuvwxyzéâêîôûçäëïöüàèìòùœæÿ]",x)

lang_letters_only <- function(x,lang) {
  if (lang == "en") return(en_letters_only(x))
  else if (lang == "hb") return(hb_letters_only(x))
  else if (lang == "fr") return(fr_letters_only)
  else  stop("invalid language")
}


descs = function(f, d,d7) {
  
  sink(f)
  print("")
  print("FOR ALL WORDS")
  print("Number of words")
  print(nrow(d))
  
  #print out relevant statistics
  print("Min word frequency")
  print(min(d$wpm))
  
  print("Likelihood a frequency-weighted randomly sampled word in top k")
  print(max(d$cum_p))
  
  
  print("")
  print("FOR 7-LETTER WORDS")
  
  print("Number of words")
  print(nrow(d7))
  
  print("Likelihood a frequency-weighted randomly sampled word in top k that are 7-letters")
  print(max(d7$cum_p7))
  
  cnt= sum(d7$exp,na.rm=TRUE)
  
  print("Number of experimental items in restricted set")
  print(cnt)
  print("Percent coverage of experimental items")
  print(cnt/400*100)
  
  print("Sum of word frequency column in file")
  print(sum(d7$wpm))
  
  print("Sum of log10(wf+1) in file")
  print(sum(log10(d7$wpm+1)*7))
  
  print("Ratio of wf/log10(wf+1)")
  print((sum(d7$wpm))/(sum(log10(d7$wpm+1)*7)))
  
  sink();
  
  print("")
}

#############################################################################

#files retrieved from https://github.com/jvparidon/subs2vec March 5, 2021
files = list(c("en","dedup.enopensub.words.unigrams.tsv.zip","EN_NewOS"),
             c("en","dedup.enwiki-meta.words.unigrams.tsv.zip","EN_NewWiki"),
             c("hb","dedup.heopensub.words.unigrams.tsv.zip","HB_NewOS"),
             c("hb","dedup.hewiki-meta.words.unigrams.tsv.zip","HB_NewWiki"),
             c("fr","dedup.fr.words.unigrams.tsv.zip","FR_NewOS"),
             c("fr","dedup.frwiki-meta.words.unigrams.tsv.zip","FR_NewWiki"))
        



for (f in files) {
  fn = f[[2]]
  print(fn)
  
  lang = f[[1]][1]
  
  if (lang == "fr"){df = read_tsv(fn)}
  
  else {df = read_tsv(paste("./freq_tables/",fn,sep=""))}
  
  #load in the corresponding language experimental items
 
  
  if (lang == "hb") {
    Sys.setlocale(locale = "Hebrew")
    df$lettersonly = hb_letters_only(df$unigram)
    raw = nrow(df)
    df = subset(df,lettersonly == TRUE)
    df$lettersonly = NULL;
    clean = nrow(df)
    
    per_invalid = 100 - clean/raw*100}
    
  else if (lang =="en") {
    Sys.setlocale(locale = "English")
    df$lettersonly = en_letters_only(df$unigram)
    raw = nrow(df)
    df = subset(df,lettersonly == TRUE)
    df$lettersonly = NULL;
    clean = nrow(df)
    
    per_invalid = 100 - clean/raw*100}
    
  else if (lang == "fr"){
      Sys.setlocale(locale = "English")
      df$lettersonly = fr_letters_only(df$unigram)
      raw = nrow(df)
      df = subset(df,lettersonly == TRUE)
      df$lettersonly = NULL;
      clean = nrow(df)
      per_invalid = 100 - clean/raw*100 }
    
    
   else {
    stop("invalid language")
  }
  locale("en")
  exp = as.data.frame(read.table(paste("./exp/",lang,".txt",sep=""), header=TRUE))
  exp$exp = 1;
  
  #this lets the Hebrew load in correctly for display, but is not 
  # a data frame.  
  exp = read_csv( paste("./exp/",lang,".txt",sep=""), 
                 locale = locale(encoding = "UTF-8"))
  exp$exp = 1;
  
  
  
  tot = sum(df$unigram_freq)
  df$wpm = df$unigram_freq / tot * 1000000
  df$p = df$unigram_freq / tot
  df$nlet = nchar(df$unigram)
  df$cum_p = as.numeric(NA)
  df$idx = 1:nrow(df)
  df$cum_p = cumsum(df$p)
  
  df7 = subset(df,nlet ==7)
  tot7 = sum(df7$unigram_freq)
  df7$p7 = df7$unigram_freq / tot7
  df7$cum_p7 = as.numeric(NA)
  df7$cum_p7 = cumsum(df7$p7)
  
  df = subset(df, wpm >= 0.01)
  df7 = subset(df7, wpm >= 0.01)
  
  #merge looks like it works here at first glance, but it 
  #does not because it does not preserve row order
  df7 = merge(df7,exp,by="unigram",all.x=TRUE,all.y=FALSE)
  df7 = df7[order(df7$idx),]
  
  cnt= sum(df7$exp,na.rm=TRUE)
  
  dir.create(file.path("./logs/", "corpus_stats"), showWarnings = FALSE)
  sink(paste("./logs/corpus_stats/",fn,".txt",sep=""))
  
  print("Percent of corpus removed due to language-inappropriate letters in unigrams")
  print(per_invalid)
  
  print("Number of experimental items in corpus")
  print(cnt)
  print("Percent coverage of experimental items")
  print(cnt/400*100)
  
  print("Number of NAs in the full corpus")
  print(sum(is.na(df$unigram)))
  
  #Find all words that did not occur in the corpus but that were in exp.
   if (cnt != 400) {
    t = df;
    t = subset(t,wpm >= 0.01 & nlet == 7);
    t=merge(t, exp, by = "unigram", all.x = FALSE, all.y=TRUE)
    
    #find the rows with missing values.  
    missing = subset(t,is.na(nlet))
    
    write.csv(wr,outloc,quote=FALSE,row.names=FALSE, fileEncoding = "UTF-8")
    write.csv(missing,
              paste("./logs/MISSING_EXP.",fn,".txt",sep=""), 
              quote=FALSE,row.names=FALSE, fileEncoding = "")
    
   }
  
  print("Summary of stats for all words, wpm >= 0.01")
  print(summary(df))
  
  print("Summary of stats for 7-letter words, wpm >= 0.01")
  print(summary(df7))
  
  sink();
  
  print("")
  
  #display results for top k words in each set
  for (k in list(10000,20000,50000)) {  #,25000,50000
    print(paste("Top ", k, " k Word stats",sep=""))
    
    d = df;
    d = d[1:k,]
    
    d7 = df7;
    d7 = subset(d7, cum_p <= max(d$cum_p))
    dir.create(file.path("./logs/", "topk"), showWarnings = FALSE)
    descs(paste("./logs/topk/",fn,".top",k,"k.txt",sep=""),d, d7)
  }  
  
  
  print("")
  print("")
  print("")
  
  #similarly, what if we want the top p percent of frequency-weighted coverage?
  for (prob in list (0.95, 0.97, 0.99)) {
    print(paste("Top ", prob, " percentile Word stats",sep=""))
    
    d = df;
    d = subset(d, cum_p <= prob)
    
    d7 = df7;
    d7 = subset(d7, cum_p <= prob)
    
    dir.create(file.path("./logs/", "percentile"), showWarnings = FALSE)
    descs(paste("./logs/percentile/",fn,".percentile",prob,".txt",sep=""),d, d7)
    
  }
  
  print("")
  print("")
  print("")
 
  
  #what if we force the inclusion of different numbers of 7-letter words?
  for (n in list (3000, 3250, 3500, 4000, 6000, 6500, 7000)) {
    print(paste("Exactly ", n, " 7-letter Word stats",sep=""))
    
    d7 = df7;  
    d7 = d7[1:n,]
    max_cum_p = max(d7$cum_p)
    
    d = df;
    d = subset(d, cum_p <= max_cum_p)
    
    d7 = df7;
    d7 = subset(d7, cum_p <= max_cum_p)
    
    dir.create(file.path("./logs/", "exactly"), showWarnings = FALSE)
    descs(paste("./logs/exactly/",fn,".exactly",n,"words.txt",sep=""),d, d7)
    
    #here is where we can write out corpora of varying sizes.  
    
    wr = d7[,c("unigram","wpm","nlet")]
    #for consistency with other files already created
    colnames(wr) = c("word","wpm","nlet")
    
    logwr= wr;
    logwr$wpm = log10(logwr$wpm+1) *7;  #the *7 is a rough approximation for the
    #log10 transform so that the summed error for the log10 and raw freqs are the
    #same.  It was determined by looking at the smallest sumrawfreq/sumlog10freq in
    #the different corpora, which in this case was for hebrew.  
    
    prefix = f[[3]]
    outloc = paste("./training_corpora/",lang,"_",prefix,"_raw_",n,".txt",sep="")
    logoutloc = paste("./training_corpora/",lang,"_",prefix,"_log10_",n,".txt",sep="") 
    
    if (lang == "hb") {
      write.csv(wr,outloc,quote=FALSE,row.names=FALSE, fileEncoding = "UTF-8")
      write.csv(logwr,logoutloc,quote=FALSE,row.names=FALSE, fileEncoding = "UTF-8")
    } 
    else {
      write.csv(wr,outloc,quote=FALSE,row.names=FALSE)
      write.csv(logwr,logoutloc,quote=FALSE,row.names=FALSE)
    }
    #make histo and density plots of entropy differences for token and type  for outloc and logoutloc
    set.seed(3204)
    french_words <- as.data.frame(read.csv(file =outloc))
    french_word_column <- french_words$word
    french_singles <- vector()
    for (y in french_word_column){if (!(y %in% french_singles)){french_singles<-append(french_singles, y)}} 
    
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
    
    
    #input a  list of pws, compute entropy
    get_single_ent <- function(x){
      ent_so_far <- 0
      for (m in x) {
        ent_so_far <- ent_so_far + (-1 * m * log(m,base = 2))
      }
      
      
      return(ent_so_far)}
    
    #input word, filter it at fixation 6, and get list of pw
    start_to_pw_6 <- function(w){
      filtered <- filter_word_6(w)
      matches <- find_matching_words(filtered)
      pw_matches <- get_pw(matches)
      pw_list <- pw_matches$pw
      return(pw_list)
    }
    
    #input word, filter it at fixation 2, and get list of pw
    start_to_pw_2 <- function(w){
      filtered <- filter_word_2(w)
      matches <- find_matching_words(filtered)
      pw_matches <- get_pw(matches)
      pw_list <- pw_matches$pw
      return(pw_list)
    }
    
    #input a word, get the entropy for that word at position 6
    word_to_ent_6 <- function(w){
      pws <- start_to_pw_6(w)
      return(get_single_ent(pws))
    }
    
    #input a word, get the entropy for that word at position 2
    word_to_ent_2 <- function(w){
      pws <- start_to_pw_2(w)
      return(get_single_ent(pws))
    }
    # input a word, return average entropy for that word after running through the filter some number of times
    get_average_ent_6 <- function(w){
      list_word <- rep(c(w),3)
      list_of_ents <- lapply(list_word,word_to_ent_6)
      vec_of_ents <- unlist(list_of_ents)
      ent_sum <- sum(vec_of_ents)
      return(ent_sum/3)}
    
    
    get_average_ent_2 <- function(w){
      list_word <- rep(c(w),3)
      list_of_ents <- lapply(list_word,word_to_ent_2)
      vec_of_ents <- unlist(list_of_ents)
      ent_sum <- sum(vec_of_ents)
      return(ent_sum/3)
    }
    
    get_average_ents <- function(w){
      return(c(get_average_ent_6(w),get_average_ent_2(w)))
    }
    
    
    
    
    
    
    
    # makes lists of entropy values for each type at fixation 6 and 2
    list_of_entropies<- lapply(french_singles, get_average_ents)
    
    
    
    #make vector of entropy differences for type
    ent_dif <- vector()
    for(i in list_of_entropies){
      ent_dif <- append(ent_dif, i[1]-i[2])
    }
    
    # make vector of entropy differences for token
    ent_dif_token <- vector()
    q <- 1
    for (h in ent_dif){
      ent_dif_token <- append(ent_dif_token,rep(c(h),french_words$wpm[q]))
      q <- q + 1
    }
    
    
    # make density plots and histograms for token and type 
    ent_dif_df <- data.frame(word = french_singles, entropy_dif = ent_dif)
    
    token_dens <- paste(lang, prefix, n,  "density_token.pdf")
    type_dens <- paste(lang, prefix, n, "density_type.pdf")
    token_hist <- paste(lang, prefix, n, "hist_token.pdf")
    type_hist <- paste(lang, prefix, n, "hist_type.pdf")
    
    ggplot(ent_dif_df, aes(x=entropy_dif)) + geom_density()
    ggsave(type_dens)
    
    ggplot(ent_dif_df, aes(x=entropy_dif)) + geom_histogram()
    ggsave(type_hist)
    
    french_token <- vector()
    for(o in french_words$word){
      french_token <- append(french_token,rep(c(o),french_words$wpm[which(french_singles == o)]))
    }
    
    ent_dif_token_df <- data.frame(word = french_token, entropy_dif = ent_dif_token)
    
    ggplot(ent_dif_token_df, aes(x=entropy_dif)) + geom_density()
    ggsave(token_dens)
    
    ggplot(ent_dif_token_df, aes(x=entropy_dif)) + geom_histogram()
    ggsave(token_hist)
    
    
    
    
    
    
    
    
    
  }  
  
  #what if we force all datasets to include all words in our experimental set?
  
  print("FREQUENCY CUTOFF TO INCLUDE ALL EXPERIMENTAL WORDS")
  minfreq = min(subset(df7,exp==1)$wpm)
  
  d = df;
  d = subset(d,wpm >= minfreq)
  
  d7 = df7;
  d7 = subset(d7, wpm >= minfreq) 
  
  dir.create(file.path("./logs/", "allexp_cutoff"), showWarnings = FALSE)
  descs(paste("./logs/allexp_cutoff/",fn,".wfcutoffforAllEXPWords.txt",sep=""),d, d7)
  
  
  
  
}



### COMPUTE CORRELATIONS BETWEEN THE DIFFERENT CORPORA
### This effectively runs independently and could be its own script.


Sys.setlocale(locale = "English")
en_os = read_tsv(paste("./freq_tables/","dedup.enopensub.words.unigrams.tsv.zip",sep=""))
en_wiki = read_tsv(paste("./freq_tables/","dedup.enwiki-meta.words.unigrams.tsv.zip",sep=""))

en_os$lettersonly = en_letters_only(en_os$unigram)
en_os = subset(en_os,lettersonly == TRUE)
en_os$lettersonly = NULL;

en_wiki$lettersonly = en_letters_only(en_wiki$unigram)
en_wiki = subset(en_wiki,lettersonly == TRUE)
en_wiki$lettersonly = NULL;

Sys.setlocale(locale = "Hebrew")
hb_os = read_tsv(paste("./freq_tables/","dedup.heopensub.words.unigrams.tsv.zip",sep=""))
hb_wiki = read_tsv(paste("./freq_tables/","dedup.hewiki-meta.words.unigrams.tsv.zip",sep=""))

hb_os$lettersonly = hb_letters_only(hb_os$unigram)
hb_os = subset(hb_os,lettersonly == TRUE)
hb_os$lettersonly = NULL;

hb_wiki$lettersonly = hb_letters_only(hb_wiki$unigram)
hb_wiki = subset(hb_wiki,lettersonly == TRUE)
hb_wiki$lettersonly = NULL;

Sys.setlocale(locale = "English")
fr_os = read_tsv("dedup.fr.words.unigrams.tsv.zip")
fr_wiki = read_tsv("dedup.frwiki-meta.words.unigrams.tsv.zip")

fr_os$lettersonly = fr_letters_only(fr_os$unigram)
fr_os = subset(fr_os,lettersonly == TRUE)
fr_os$lettersonly = NULL;

fr_wiki$lettersonly = fr_letters_only(fr_wiki$unigram)
fr_wiki = subset(fr_wiki,lettersonly == TRUE)
fr_wiki$lettersonly = NULL;


dfs = list(en_os,en_wiki,hb_os,hb_wiki,fr_os,fr_wiki)

for (i in 1:6) {
  dat = dfs[[i]]
  tot = sum(dat$unigram_freq)
  dat$wpm = dat$unigram_freq / tot * 1000000
  #dat$p = dat$unigram_freq / tot
  dat$nlet = nchar(dat$unigram)
  #dat$cum_p = as.numeric(NA)
  dat$idx = 1:nrow(dat)
  #dat$cum_p = cumsum(dat$p)  
  dat$unigram_freq = NULL;
  
  dat = subset(dat,wpm >= 0.01)
  dfs[[i]] = dat;
  
}

en_os = dfs[[1]]
en_wiki = dfs[[2]]
hb_os = dfs[[3]]
hb_wiki = dfs[[4]]
fr_os = dfs[[5]]
fr_wiki=dfs[[6]]

en = merge(en_os,en_wiki, by = c("unigram","nlet"),all.x=TRUE,all.y=TRUE)
hb = merge(hb_os,hb_wiki, by = c("unigram","nlet"),all.x=TRUE,all.y=TRUE)
fr = merge(fr_os,fr_wiki, by = c("unigram","nlet"),all.x=TRUE, all.y=TRUE)

#drop all rows for which at least one of the corpora does not have a freq > 1 wpm

en = subset(en, (wpm.x >=1 | wpm.y >=1) & (is.na(wpm.x) == FALSE & is.na(wpm.y)==FALSE))
hb = subset(hb, (wpm.x >=1 | wpm.y >=1) & (is.na(wpm.x) == FALSE & is.na(wpm.y)==FALSE))
fr = subset(fr, (wpm.x >=1 | wpm.y >=1) & (is.na(wpm.x) == FALSE & is.na(wpm.y)==FALSE))

sink("./logs/corpus_correlations.txt")

print("Correlation between EN OS (x) and EN Wiki (y)")
print(cor.test(en$wpm.x, en$wpm.y))

print("Correlation between Log10(wf+1) transformed EN OS (x) and EN Wiki (y)")
print(cor.test(log10(en$wpm.x+1), log10(en$wpm.y+1)))


print("Correlation between HB OS (x) and HB Wiki (y)")
print(cor.test(hb$wpm.x, hb$wpm.y))

print("Correlation between Log10(wf+1) transformed HB OS (x) and HB Wiki (y)")
print(cor.test(log10(hb$wpm.x+1), log10(hb$wpm.y+1)))

print("Correlation between FR OS (x) and FR Wiki (y)")
print(cor.test(fr$wpm.x, fr$wpm.y))

print("Correlation between Log10(wf+1) transformed FR OS (x) and FR Wiki (y)")
print(cor.test(log10(fr$wpm.x+1), log10(fr$wpm.y+1)))

sink()

#### PLOTTING DENSITY FUNCTIONS

en_long = melt(en,id.vars = c("unigram","nlet"))
hb_long = melt(hb,id.vars = c("unigram","nlet"))
fr_long = melt(fr,id.vars = c("unigram","nlet"))

en_long$lang = "en"
hb_long$lang = "hb"
fr_long$lang = "fr"

long = rbind(en_long,hb_long,fr_long)
long = subset(long,variable == "wpm.x" | variable == "wpm.y")
long$lang = as.factor(long$lang)
long = long[order(-long$value),]

levels(long$variable)[1] = "subtitles"
levels(long$variable)[3] = "wiki"
long$cond = paste(long$variable,long$lang,sep=" ")
long = droplevels(long)

#remove the NA's artificially introduced when merging the file. (this is not an error)
long = subset(long, !is.na(value))

##note, there are 8 NAs in the dataframe, but these appear to be the 
#literal "NA" word having been incorrectly converted.  


#max value was obtained as max in summary(long$value)
ggplot(long, aes(x=log10(value), color = cond )) + 
  geom_density() + coord_cartesian(xlim = c(0,4.85),ylim = c(0, 0.15))
ggsave("./plots/density_log10wf_density_minwf_1.pdf")

#same as above but higher wf cutoff
ggplot(long, aes(x=log10(value), color = cond )) + 
  geom_density() + coord_cartesian(xlim = c(1,4.85),ylim = c(0, 0.1))


ggplot(long, aes(x=log10(value), color = cond )) + 
  geom_density() + coord_cartesian(xlim = c(-1.5,4.85),ylim = c(0.00001, 0.5)) +scale_y_log10()
ggsave("./plots/density_log10wf_log10_density_minwf_0.03.pdf")


ggplot(subset(long,nlet==7), aes(x=log10(value), color = cond )) + 
  geom_density() + coord_cartesian(xlim = c(-1,3.5),ylim = c(0, 0.3))
ggsave("./plots/density7LETTERS_log10wf_density_minwf_0.1.pdf")

ggplot(subset(long,nlet==7), aes(x=log10(value), color = cond )) + 
  geom_density() + coord_cartesian(xlim = c(-1,3.5),ylim = c(0.00001, 0.3)) +scale_y_log10()
ggsave("./plots/density7LETTERS_log10wf_log10_density_minwf_0.1.pdf")



###################### OLD ECDF PLOTTING CODE / ZIPF CODE ###################
#Related reference:
#Brysbaert et al.
#//journals.sagepub.com/doi/full/10.1177/0963721417727521

#ggplot(df2, aes(x = idx, y = cum_p)) + geom_point()


#based on the curves of probability of words being known, Brysbaert et al. p. 48, Word Freq Zipf figure,
#even for individuals with a high vocabulary, we should expect individuals to not know words with a frequency less than about 2 on a Zipf scale.  

#Zipf scale = log10(freq per billion words)

#what is a zipf = 2?
#reversing the calculations from the zipf
#x = log10(wfpb)
#wfpb = 10^x
#with x = 2, that is wfpb = 100.  
#in wfpm, we divide by 1000.  
#or 100 / 1000 = 0.1 wpm. 

#what about a zipf of 2.5?  This is closer to the 75% cutoff in that paper...
# x = 2.5
# wfpb = 316
# or 0.3 wpm.


#what about a zipf of 1, just to constrain the initial dataset?  
#what is a zipf = 1?
#reversing the calculations from the zipf
#x = log10(wfpb)
#wfpb = 10^x
#with x = 1, that is wfpb = 10.  
#in wfpm, we divide by 1000.  
#or 10 / 1000 = 0.01 wpm. 





#df.ecdf = ecdf(df2$wpm)
#my.ecdf = df.ecdf(sort(df2$wpm))  #this gets you the actual probabilites in the ecdf, so you could reverse
#the plot if needed, or creat your own custom threshold.  
#plot(df.ecdf)

#can also see what proportion of the language is explained by particular thresholds:
#df.ecdf(0.1)





