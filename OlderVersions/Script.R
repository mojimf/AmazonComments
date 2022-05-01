# Comment analysis

##libraries
library(tm)
library(tokenizers)
library(textstem)
library(SnowballC)
library(jsonlite)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(LDAvis)
library(servr)


#data loading

RawData <- fromJSON("https://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")

##student number 201582023

set.seed(023)
TestSet <- sample_n(RawData,5000)

#data preprocessing

## dividing the positive and negative comments

stars <- substr(TestSet$review_rating,1,1)
TestSetStar <- cbind(TestSet,stars)
Posit <- subset(TestSetStar, stars == "4"| stars == "5")
Negat <- subset(TestSetStar, stars == "1"| stars == "2")


##binding teh title and text and preparing corpus
TestBind <- paste(TestSet$review_title,TestSet$review_text)
TestBind2 <- cbind(TestBind,TestSet)
reviews <- stringr::str_conv(TestBind2$TestBind, "UTF-8")
corp <- Corpus(VectorSource(reviews))
print(corp[[2]]$content)





## document term matrix -TF

dtm <- DocumentTermMatrix(corp, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))

## frequency matrix and length production

RawSum <- apply(dtm,1,FUN=sum)
dtm <- dtm[RawSum!=0,]
dtmNew <- as.matrix(dtm)
frequency <- colSums(dtmNew)
frequency <- sort(frequency, decreasing = T)
DocLength <- rowSums(dtmNew)

frequency[1:10]


##Tokenisation

WordsToken <- tokenize_words(corp$content)
print(WordsToken[12])

##stem and lemma

TextStem <- tm_map(corp,stemDocument)
TextStem[[2]]$content
WordsStem <- tokenize_words(TextStem$content)
print(WordsStem[1])

TextLemma <- tm_map(corp, lemmatize_strings)
WordsLemma <- tokenize_words(TextLemma$content)
print(WordsLemma[1])

## data cleansing



### finding frequent terms

findFreqTerms(dtm,lowfreq = 200)
findAssocs(dtm,gaksj, corlimit = 0.1)
### removing sparse terms

dtms <- removeSparseTerms(dtm, 0.99)
dtms
findFreqTerms(dtm)
output <- matrix(dtms)

## document term matrix -TFIDF

dtmfidf <- DocumentTermMatrix(corp, control = list(removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf),
                                               weighting = function(x) weightTfIdf(x, normalize = F)))
dtmfidf
findFreqTerms(dtm,lowfreq = 1000)
findFreqTerms(dtmfidf,lowfreq = 1000)
### removing sparse terms

dtmfidfs <- removeSparseTerms(dtm, 0.99)
dtmfidfs
findFreqTerms(dtmfidf)
output2 <- matrix(dtmfidf)

#Topic modeling

