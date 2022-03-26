# ***********************************
# Web Scraping - Twitter API   ######
# ***********************************

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------Packages---------------------------------------------------------------
install.packages("multcomp")
install.packages("purrr")
install.packages("twitteR", dependencies = TRUE)
install.packages("party", dependencies = TRUE)


## ----------Twitter API retrieval--------------------------------------------------

# Import library
library(multcomp)
library(twitteR)
library(party)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "auOLd5wyQLmZy7SO0QroBFtxS"
consumer_secret <- "XBuyepKZTsHFBGQBhGWxkj8tfSBZJflPOAZeu5FdAVpN0TUP0E"
access_token <- "61509185-4ZvC4ruyjHtcHoksyv5SIpyCekJOXWBNgd5a9R9Rr"
access_secret <- "djq9A56CUuHIy6CmXOFlY7tzW4mArsgwHVrBMRMJ0JP6w"

typeof(access_secret)

# The setup_twitter_oauth Sets up the OAuth credentials for a twitteR session
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# *********************************************
# Function to view/retrieve Twitter timelines
tweets <- userTimeline("RDataMining", n = 3200)

View(tweets)
n.tweet <- length(tweets)
n.tweet

# Convert tweets to DF
tweets.df <- twListToDF(tweets)
View(tweets.df)
str(tweets.df)

# *********************************************
# Look at tweet 190
tweet_190 <- tweets.df[190, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
tweet_190

# print tweet #188 and make text fit for slide width
writeLines(strwrap(tweets.df$text[188], 60))


## ------ Building Corpus-----------------------------------------------------------

library(tm)

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))

## ------ Ranking of the Corpus ------
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
View(myCorpus)


# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# View a line from the corpus
writeLines(strwrap(myCorpus[[183]]$content, 60))

# remove anything other than English language letters and space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# View a line from the corpus
writeLines(strwrap(myCorpus[[11]]$content, 60))

# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# View a line from the corpus
writeLines(strwrap(myCorpus[[183]]$content, 60))

# remove extra whitespaces
statementmyCorpus <- tm_map(myCorpus, stripWhitespace)
# View a line from the corpus
writeLines(strwrap(myCorpus[[25]]$content, 60))

# keep a copy for stem completion later
myCorpusCopy <- myCorpus

# Stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# View a line from the corpus
writeLines(strwrap(myCorpus[[190]]$content, 60))

## r refer card data mine now provid link packag cran packag
## mapreduc hadoop ad
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
writeLines(strwrap(myCorpus[[190]]$content, 60))


# *********************************************
# count word frequence
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}

n.miner <- wordFreq(myCorpusCopy, "miner")
n.mining <- wordFreq(myCorpusCopy, "mining")
cat(n.miner, n.mining)

# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

myCorpus <- replaceWord(myCorpus, "miner", "mining")
myCorpus <- replaceWord(myCorpus, "universidad", "university")
myCorpus <- replaceWord(myCorpus, "scienc", "science")

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm


# *********************************************
library(tidyr)

idx <- which(dimnames(tdm)$Terms %in% c("r", "data", "mining"))
as.matrix(tdm[idx, 21:30])
idx

# Find the terms used most frequently
# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))


term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)

## ----Visuaization----------------------------------------------

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


## ----Reference----------------------------------------------
#1. https://rpubs.com/Manorama/442299


## ----Quick tips on how to build a corpus--------------------
#myCorpus<-Corpus(VectorSource(myFile$myColumn)) #converts the relevant part of your file into a corpus

#myCorpus = tm_map(myCorpus, PlainTextDocument) # an intermediate preprocessing step

#myCorpus = tm_map(myCorpus, tolower) # converts all text to lower case

#myCorpus = tm-map(myCorpus, removePunctuation) #removes punctuation

#myCorpus = tm_map(myCorpus, removeWords, stopwords("english")) #removes common words like "a", "the" etc

#myCorpus = tm_map(myCorpus, stemDocument) # removes the last few letters of similar words such as get, getting, gets

#dtm = DocumentTermMatrix(myCorpus) #turns the corpus into a document term matrix

#notSparse = removeSparseTerms(dtm, 0.99) # extracts frequently occuring words

#finalWords=as.data.frame(as.matrix(notSparse) # most frequent words remain in a dataframe, with one column per word
