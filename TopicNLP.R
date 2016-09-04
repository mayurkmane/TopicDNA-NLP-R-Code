##Import All Libraries
library(tm)
library(SnowballC)

topic<-read.csv("train_topic.csv",stringsAsFactors=FALSE)

##Select column from where Text needs to be Extracted
pos.topic<-character(0)
pos.tweet<-character(0)
pos.hashtags<-character(0)

pos.topic<-topic$topic
pos.tweet<-topic$tweet
pos.hashtags<-topic$hashtags

##Create Corpus as reqd by TM package
##Corpus requires a vector source
pos.corpus.topic<-Corpus(VectorSource(pos.topic))
pos.corpus.tweet<-Corpus(VectorSource(pos.tweet))
pos.corpus.hashtags<-Corpus(VectorSource(pos.hashtags))

inspect(pos.corpus.topic) ##Similarly other corpuses can be inspected

##Lets Do some pre processing
##Removing words from Comments which have higher correlation to Output Classification
ExtraStopWords<-c(stopwords("english"),"Sports","Art","Entertainment","Tags")

Pos.corpus.Preprocess=function(corpus)
{
  corpus<-tm_map(corpus,stripWhitespace) ##Remove extra white spaces
  corpus<-tm_map(corpus,removePunctuation)  ## Remove Punctuations
  corpus<-tm_map(corpus,removeNumbers)  ## Remove Numbers
  corpus<-tm_map(corpus,removeWords,ExtraStopWords)   ## Remove Stop Words
  corpus<-tm_map(corpus,tolower) ## Converts to Lower case
  corpus<-tm_map(corpus, stemDocument, language = "english")
  corpus<-tm_map(corpus, PlainTextDocument)
  return(corpus)
}

pos.corpus.topic<-Pos.corpus.Preprocess(pos.corpus.topic)
pos.corpus.tweet<-Pos.corpus.Preprocess(pos.corpus.tweet)
pos.corpus.hashtags<-Pos.corpus.Preprocess(pos.corpus.hashtags)

##Generate a Document Term Matrix, Train Dataset and Test Dataset
pos.DTM.topic<-DocumentTermMatrix(pos.corpus.topic)
pos.DTM.tweet<-DocumentTermMatrix(pos.corpus.tweet)
pos.DTM.hashtags<-DocumentTermMatrix(pos.corpus.hashtags)

findFreqTerms(pos.DTM.tweet, 200) # words occurred more that 200 times
findAssocs(pos.DTM.tweet, "amazing", 0.1) # other words have a high association with "amazing"

##Input the sparse for topic, tweet, hashags accordingly such that Accuracy can be enhanced 
##From our first look at the TDM we know that there are many terms which do not occur very often. It might make sense to simply remove these sparse terms from the analysis.

pos.DTM.topic1<-removeSparseTerms(pos.DTM.topic, 0.1)
pos.DTM.tweet1<-removeSparseTerms(pos.DTM.tweet, 0.1)
pos.DTM.hashtags1<-removeSparseTerms(pos.DTM.hashtags, 0.1)

dim(pos.DTM.tweet)
dim(pos.DTM.tweet1)
dim(pos.DTM.topic)
dim(pos.DTM.topic1)
dim(pos.DTM.hashtags)
dim(pos.DTM.hashtags1)

## Generate Document Term Matrix
Final_DTM<-cbind(inspect(pos.DTM.topic1),inspect(pos.DTM.tweet1),inspect(pos.DTM.hashtags1))
Final_topic_DF <- as.data.frame(Final_DTM)
nrow(Final_DTM)
ncol(Final_DTM)

#topic_Rem<-topic[,c(1,3,4,5,10,11,12)]
#topic_matrix<-as.matrix(topic_Rem)

#Final_AVTM<-cbind(Final_DTM,AVTM_matrix)  ## Matrix form
#Final_AVTM_Df<-as.data.frame(Final_AVTM)   ##DataFrame form



