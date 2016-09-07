##Import All Libraries
library(tm)
library(SnowballC)

Data<-read.csv("LargeData.csv",stringsAsFactors=FALSE)

##Selected column from where Text needs to be Extracted

pos.tweet<-character(0)
pos.hashtags<-character(0)

pos.tweet<-Data$text
pos.hashtags<-Data$hashtags

##Created Corpus as reqd by TM package
##Corpus reqd a vector source

pos.corpus.tweet<-Corpus(VectorSource(pos.tweet))
pos.corpus.hashtags<-Corpus(VectorSource(pos.hashtags))

inspect(pos.corpus.tweet) ##Similarly other corpuses can be inspected

##Lets Do some pre processing
##Removed words from Comments which have higher correlation to Output Classification
ExtraStopWords<-c(stopwords("english"),"Sports","Art","Entertainment","Tags", "the", "bbmas")

Pos.corpus.Preprocess=function(corpus)
{
  corpus<-tm_map(corpus,stripWhitespace) ##Remove extra white spaces
  corpus<-tm_map(corpus,removePunctuation)  ## Remove Punctuations
  corpus<-tm_map(corpus,removeNumbers)  ## Remove Numbers
  corpus<-tm_map(corpus,tolower) ## Converts to Lower case
  corpus<-tm_map(corpus,removeWords,ExtraStopWords)   ## Remove Stop Words
  corpus<-tm_map(corpus, stemDocument, language = "english")
  corpus<-tm_map(corpus, PlainTextDocument)
  return(corpus)
}

pos.corpus.tweet<-Pos.corpus.Preprocess(pos.corpus.tweet)
pos.corpus.hashtags<-Pos.corpus.Preprocess(pos.corpus.hashtags)

##Generate a Document Term Matrix, Train Dataset and Test Dataset

pos.DTM.tweet<-DocumentTermMatrix(pos.corpus.tweet)
pos.DTM.hashtags<-DocumentTermMatrix(pos.corpus.hashtags)

findFreqTerms(pos.DTM.tweet, 300) # words occurred more that 50 times
findAssocs(pos.DTM.tweet, "amp", 0.1) # other words have a high association with “amazing”

##Input the sparse for topic, tweet, hashags accordingly such that Accuracy can be enhanced 
##From first look at the TDM I know that there are many terms which do not occur very often. It might make sense to simply remove these sparse terms from the analysis.

pos.DTM.tweet1<-removeSparseTerms(pos.DTM.tweet, 0.999)
pos.DTM.hashtags1<-removeSparseTerms(pos.DTM.hashtags, 0.9999)

dim(pos.DTM.tweet)
dim(pos.DTM.tweet1)
dim(pos.DTM.hashtags)
dim(pos.DTM.hashtags1)


## Generated Document Term Matrix
Final_DTM<-cbind(inspect(pos.DTM.tweet1),inspect(pos.DTM.hashtags1))
nrow(Final_DTM)
ncol(Final_DTM)

Data_Rem<-Data[,c(1,2)] # Adding remaining variables
Data_matrix<-as.matrix(Data_Rem)

Final_Data<-cbind(Final_DTM,Data_matrix)  ## Matrix form
Final_Data_DF <- as.data.frame(Final_Data)   ##DataFrame form

fix(Final_Data_DF) #Look on final data

## Partition the Data into Train and Test Dataset
set.seed(123)
smp_size <- floor(0.7 * nrow(Final_Data_DF))  #70% of the sample size

train_ind <- sample(seq_len(nrow(Final_Data_DF)), size = smp_size)

Train_Data <- Final_Data_DF[train_ind, ]
Test_Data <- Final_Data_DF[-train_ind, ]

Train_Data <- Train_Data[, !duplicated(colnames(Train_Data), fromLast = TRUE)]
Test_Data <- Test_Data[, !duplicated(colnames(Test_Data), fromLast = TRUE)]

dim.data.frame(Train_Data)
dim.data.frame(Test_Data)

# Weighted K Nearest Neighbour

library(kknn)

##Train the Dataset
Data.kknn<-train.kknn(subtopic~.,Train_Data,distance=5,kernel = c("triangular","gaussian","epanechnikov", "optimal"),kmax=9)
summary(Data.kknn)

##Prediction on TEST and Train data set using Trained model
testPred.kknn<-predict(Data.kknn,Test_Data)
trainPred.kknn<-predict(Data.kknn,Train_Data)

testPred.kknn.DF <- as.data.frame(testPred.kknn)

##Misclassification Matrix
MisClassTest<-table("Predict"=testPred.kknn,"Actual"=Test_Data$subtopic)  ## Test Data Prediction
MisClassTrain<-table("Predict"=trainPred.kknn,"Actual"=Train_Data$subtopic)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracykknn<-(100-mean(c((nrow(Test_Data)-sum(diag(MisClassTest)))/nrow(Test_Data)),(nrow(Train_Data)-sum(diag(MisClassTrain)))/nrow(Train_Data)))
accuracykknn

#Latent Dirichlet Allocation

library(RTextTools)
library(topicmodels)

k <- length(unique(Data$subtopic))
lda <- LDA(Data, k)
str(Data)

# Naive Bayes Classification

library(e1071)
#Training Naive Bayes Classfier
NaiveBe <- naiveBayes(subtopic ~ ., data = Train_Data)
summary(NaiveBe)
str(NaiveBe)

#prediction on train data
train_predict<-predict(NaiveBe, Train_Data)
NaiveClassTrainMatrix<-table(pred=train_predict,true=Train_Data$subtopic)

#prediction on test data p
test_predict<-predict(NaiveBe,Test_Data)
NaiveClassTestMatrix<-table(pred=test_predict,true=Test_Data$subtopic)

#Calculating accuracy
accuracykknn<-(100-mean(c((nrow(Test_Data)-sum(diag(NaiveClassTestMatrix)))/nrow(Test_Data)),(nrow(Train_Data)-sum(diag(NaiveClassTrainMatrix)))/nrow(Train_Data)))
accuracykknn

