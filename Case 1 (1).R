#Uploading Libraries
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)
library(dplyr)
library(wordcloud)

#importing the datasets in R

A_Oct2019 <- read.csv("Case/Case I/Data/A_Oct2019.csv", header = TRUE)

B_Nov2019 <- read.csv("Case/Case I/Data/B_Nov2019.csv", header = TRUE)

C_Dec2019 <- read.csv("Case/Case I/Data/C_Dec2019.csv", header = TRUE)

#combine together the datasets
nba <- rbind(A_Oct2019, B_Nov2019, C_Dec2019)

#options and functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#cleanCorpus function
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#sampling data 20%
nba_sample <- sample_frac(nba,0.20)

#doc_id
names(nba_sample)[1] <- 'doc_id'

#finding stop words which are irrelevant for our analysis
stops <- c(stopwords('SMART'),'nba','game', 'tonight', 'trail', 'city', 'basketball', 
                     'season', 'team', 'free', 'fan', 'pts', 'full', 'back', 'ers',
                     'day', 'stars', 'los','the','and','for','new', 'miami', 'boston',
                     'chicago', 'houston')

#using term document matrix and text corpus to organise data
txtCorpus <- VCorpus(VectorSource(nba_sample))
txtCorpus <- cleanCorpus(txtCorpus, stops)
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)

#most frequent terms
topTermsB <- rowSums(tweetTDMm)

#add the terms
topTermsB <- data.frame(terms = rownames(tweetTDMm), freq = topTermsB)

# find the most frequent term
idx <- which.max(topTermsB$freq)
topTermsB[idx, ]

#frequency of data frame
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)


#barplot; values greater than 25000
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 25000) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

#factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# word cloud
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

#Make bi-gram TDM and convert it to matrix
tweetsTDM  <- TermDocumentMatrix(txtCorpus, 
                                 control=list(tokenize=bigramTokens))
tweetsTDMm <- as.matrix(tweetsTDM)

#getting row sums
tweetsTDMv <- sort(rowSums(tweetTDMm), decreasing = TRUE)
tweets_DF   <- data.frame(word = names(tweetsTDMv), freq = tweetsTDMv)

#display all palettes to pick a color
display.brewer.all()

#picking scale of "blues"
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

#making a word cloud
set.seed(1234)
wordcloud(tweets_DF$word,
          tweets_DF$freq,
          max.words    = 20,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))





