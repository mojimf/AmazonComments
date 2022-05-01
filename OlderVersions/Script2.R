# Comment analysis

##libraries----
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


#data loading----

RawData <- fromJSON("https://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")

##student number 201582023-----

set.seed(023)
TestSet <- sample_n(RawData,5000)


#data preprocessing-----

## dividing the positive and negative comments

stars <- substr(TestSet$review_rating,1,1)
TestSetStar <- cbind(TestSet,stars)
Posit <- subset(TestSetStar, stars == "4"| stars == "5")
Negat <- subset(TestSetStar, stars == "1"| stars == "2")


#positive comments----

##binding the title and text and preparing corpus


TestBindpo <- paste(Posit$review_title,Posit$review_text)
Posit2 <- cbind(TestBindpo,Posit)
reviewspo <- stringr::str_conv(Posit2$TestBind, "UTF-8")
corppo <- Corpus(VectorSource(reviewspo))
print(corppo[[150]]$content)

### Lemmatization and Tokenizationpositive

lemmapo <- tm_map(corppo, lemmatize_strings)
WordsTokenpo <- tokenize_words(lemmapo$content)
print(WordsTokenpo[100])

### dtm - TF-IDF

dtmpo <- DocumentTermMatrix(corppo, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))
RawSumpo <- apply(dtmpo,1,FUN=sum)
dtmpo <- dtmpo[RawSumpo!=0,]


###frequency and removing spares

findFreqTerms(dtmpo,lowfreq = 300)
dtmspo <- removeSparseTerms(dtmpo, 0.97)
findFreqTerms(dtmspo,lowfreq = 300)
outputpo <- as.matrix(dtmspo)

#### frequency table positive
dtmsNewpo <- as.matrix(dtmspo)
frequencypo <- colSums(dtmsNewpo)
frequencypo <- sort(frequencypo, decreasing = T)
DocLengthpo <- rowSums(dtmsNewpo)
words <- names(frequencypo)
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, random.order =F, scale = c(5,0.5), random.color = F, colors=brewer.pal(8,"Dark2"))

#negative comments----

##binding the title and text and preparing corpus

TestBindne <- paste(Negat$review_title,Negat$review_text)
Negat2 <- cbind(TestBindne,Negat)
reviewsne <- stringr::str_conv(Negat2$TestBind, "UTF-8")
corpne <- Corpus(VectorSource(reviewsne))
print(corpne[[150]]$content)

### Lemmatization and Tokenizationpositive

lemmane <- tm_map(corpne, lemmatize_strings)
WordsTokenne <- tokenize_words(lemmane$content)
print(WordsTokenne[100])

### dtm - TF-IDF

dtmne <- DocumentTermMatrix(corpne, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))
RawSumne <- apply(dtmne,1,FUN=sum)
dtmne <- dtmne[RawSumne!=0,]


###frequency and removing spares

findFreqTerms(dtmne,lowfreq = 300)
dtmsne <- removeSparseTerms(dtmne, 0.97)
findFreqTerms(dtmsne,lowfreq = 300)
outputne <- matrix(dtmsne)

#### frequency table positive
dtmsNewne <- as.matrix(dtmsne)
frequencyne <- colSums(dtmsNewne)
frequencyne <- sort(frequencyne, decreasing = T)
DocLengthne <- rowSums(dtmsNewne)
wordsne <- names(frequencyne)
par(mfrow = c(1,1))
wordcloud(wordsne[1:100], frequency[1:100], rot.per=0.15, random.order =F, scale = c(5,0.5), random.color = F, colors=brewer.pal(8,"Dark2"))

-------

# Topic modeling
  
  ##
###Negative comments

TestBindne <- paste(Negat$review_title,Negat$review_text)
Nagat2 <- cbind(TestBindne,Negat)
reviewsne <- stringr::str_conv(Nagat2$TestBind, "UTF-8")
corpne <- Corpus(VectorSource(reviewsne))
print(corpne[[100]]$content)

####Tokenisation and removal negative

WordsTokenne <- tokenize_words(corpne$content)
print(WordsTokenne[12])




## document term matrix -TFIDF

###Positive comments

corppo <- 
dtmpo <- DocumentTermMatrix(corppo, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))
RawSumpo <- apply(dtmpo,1,FUN=sum)
dtmpo <- dtmpo[RawSum!=0,]

#### frequency table positive
dtmNewpo <- as.matrix(dtmpo)
frequencypo <- colSums(dtmNewpo)
frequencypo <- sort(frequencypo, decreasing = T)
DocLengthpo <- rowSums(dtmNewpo)

###Negative comments

dtmne <- DocumentTermMatrix(corpne, control = list(lemma = T, removePunctuation =T, removeNumbers=T,stopwords=T,tolower=T,wordLengths=c(1,Inf), weighting = function(x) weightTfIdf(x, normalize = F)))
RawSumne <- apply(dtmne,1,FUN=sum)
dtmne <- dtmne[RawSum!=0,]

#### frequency table positive
dtmNewne <- as.matrix(dtmne)
frequencyne <- colSums(dtmNewne)
frequencyne <- sort(frequencyne, decreasing = T)
DocLengthne <- rowSums(dtmNewne)


frequencyne[1:20]
frequencypo[1:20]




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


#Topic modeling - sparse removed----

## positive
### choosing K

dtmspoW <- tm::weightTf(dtmspo)
iter <- 2000
coherencepo <- c()
for(i in (3:15)){
  ldaOutpo <- LDA(dtmspoW,i, method="Gibbs", control = list(iter=iter,seed=10000))
  phi <- posterior(ldaOutpo)$terms %>% as.matrix
  theta <- posterior(ldaOutpo)$topics %>% as.matrix
  coherence_one <- mean(textmineR::CalcProbCoherence(phi = phi,
                                                     dtm = dtm.new))
  coherencepo <-append(coherencepo,coherence_one)
}

k <- c(3:15)[which.max(coherencepo)]
print(k)