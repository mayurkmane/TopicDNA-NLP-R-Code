##Import All Libraries
library(tm)
library(SnowballC)

TrainData<-read.csv("ActualTopic.csv",stringsAsFactors=FALSE)
TestData<-read.csv("TestTopic.csv", stringsAsFactors = FALSE)

##Select column from where Text needs to be Extracted

pos.tweet<-character(0)
pos.hashtags<-character(0)
pos.TestTweet<-character(0)

pos.tweet<-TrainData$tweet
pos.hashtags<-TrainData$hashtags
pos.TestTweet<-TestData$tweet

##Create Corpus as reqd by TM package
##Corpus requires a vector source

pos.corpus.tweet<-Corpus(VectorSource(pos.tweet))
pos.corpus.hashtags<-Corpus(VectorSource(pos.hashtags))
pos.corpus.TestTweet<-Corpus(VectorSource(pos.TestTweet))

inspect(pos.corpus.tweet) ##Similarly other corpuses can be inspected

##Lets Do some pre processing
##Removing words from Comments which have higher correlation to Output Classification
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
pos.corpus.TestTweet<-Pos.corpus.Preprocess(pos.corpus.TestTweet)

##Generate a Document Term Matrix, Train Dataset and Test Dataset

pos.DTM.tweet<-DocumentTermMatrix(pos.corpus.tweet)
pos.DTM.hashtags<-DocumentTermMatrix(pos.corpus.hashtags)
pos.DTM.TestTweet<-DocumentTermMatrix(pos.corpus.TestTweet)

findFreqTerms(pos.DTM.tweet, 50) # words occurred more that 200 times
findAssocs(pos.DTM.tweet, "amp", 0.1) # other words have a high association with "amazing"

##Input the sparse for topic, tweet, hashags accordingly such that Accuracy can be enhanced 
##From our first look at the TDM we know that there are many terms which do not occur very often. It might make sense to simply remove these sparse terms from the analysis.

pos.DTM.tweet1<-removeSparseTerms(pos.DTM.tweet, 0.997)
pos.DTM.TestTweet1<-removeSparseTerms(pos.DTM.TestTweet, 0.997)

dim(pos.DTM.tweet)
dim(pos.DTM.tweet1)
dim(pos.DTM.TestTweet)
dim(pos.DTM.TestTweet1)

## Generate Document Term Matrix
Final_DTM<-cbind(inspect(pos.DTM.tweet1),inspect(pos.DTM.hashtags))
Final_Test_DTM<-inspect(pos.DTM.TestTweet1)
nrow(Final_DTM)
ncol(Final_DTM)

Data_Rem<-TrainData[,c(1,2)] # Adding remaining variables
Data_matrix<-as.matrix(Data_Rem)

Final_Data<-cbind(Final_DTM,Data_matrix)  ## Matrix form
Final_Data_DF <- as.data.frame(Final_Data)   ##DataFrame form
Test_Data_DF <- as.data.frame(Final_Test_DTM)

fix(Final_Data_DF) #Look on final data

## Partition the Data into Train and Test Dataset
set.seed(123)
#smp_size <- floor(0.70 * nrow(Final_Data_DF))  #70% of the sample size

#train_ind <- sample(seq_len(nrow(Final_Data_DF)), size = smp_size)

#Train_Data <- Final_Data_DF[train_ind, ]
#Test_Data <- Final_Data_DF[-train_ind, ]

#Remove Duplicate column names
Train_Data_DF <- Final_Data_DF[, !duplicated(colnames(Final_Data_DF), fromLast = TRUE)]
Test_Data_DF <- Test_Data_DF[, !duplicated(colnames(Test_Data_DF), fromLast = TRUE)]

#common columns only used for analysis in Test data
common<-Test_Data_DF[,which(colnames(Test_Data_DF)%in%colnames(Train_Data_DF))]

# additional column get sorted separately
UniqueTrainCol<-Train_Data_DF[,-which(colnames(Train_Data_DF)%in%colnames(common))]

# put rows in test additional columns to do column bind
UniqueTrainCol<-rbind(UniqueTrainCol,-which(row.names(UniqueTrainCol)%in%row.names(Train_Data_DF)))

# all extra vectors are pointed to zero
UniqueTrainCol[]<-0
UniqueTrainCol[,c("topicName","subtopic")]<-"NA"

# Merged unique test data with additional columns data
Test_Data_DF<-cbind(common,UniqueTrainCol)

#prepare test data columns as per train data
default_cols<-colnames(Train_Data_DF)
Test_Data_DF<- Test_Data_DF[,match(default_cols, colnames(Test_Data_DF))]

dim.data.frame(Train_Data_DF)
dim.data.frame(Test_Data_DF)
fix(Test_Data_DF)
fix(Train_Data_DF)

# Conditional Inference Trees

library(party)

##Train the Dataset
Train.ctree<-ctree(subtopic~.,data=Train_Data,controls = ctree_control(maxsurrogate = 2))
plot(Train.ctree)

##Prediction on TEST and Train data set using Trained model
TestPred.ctree<-predict(Train.ctree,Test_Data)
TrainPred.ctree<-predict(Train.ctree,Train_Data)

TrainPred.ctree.DF<-as.data.frame(TrainPred.ctree)
fix(TrainPred.ctree.DF)
TestPred.ctree.DF<-as.data.frame(TestPred.ctree)
fix(TestPred.ctree.DF)

##Misclassification Matrix
MisClassTest<-table("Predict"=TestPred.ctree,"Actual"=Test_Data$subtopic)  ## Test Data Prediction
MisClassTrain<-table("Predict"=TrainPred.ctree,"Actual"=Train_Data$subtopic)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracyCtree<-(100-mean(c((nrow(Test_Data)-sum(diag(MisClassTest)))/nrow(Test_Data)),(nrow(Train_Data)-sum(diag(MisClassTrain)))/nrow(Train_Data)))
accuracyCtree

# Random Forest Algorithm

##Train the Dataset
Train.RandomForestEnsemble<-cforest(subtopic~.,data=Train_Data,control=cforest_unbiased(ntree=4))

##Predict on Test Data and Train Data
TestPred.RandomForestEnsemble<-predict(Train.RandomForestEnsemble,Test_Data,OOB=TRUE)
TrainPred.RandomForestEnsemble<-predict(Train.RandomForestEnsemble,Train_Data,OOB=TRUE)

##Misclassification Matrix
MisClassTest<-table("Predict"=TestPred.RandomForestEnsemble,"Actual"=Test_Data$subtopic)  
MisClassTrain<-table("Predict"=TrainPred.RandomForestEnsemble,"Actual"=Train_Data$subtopic)   

##Accuracy based on Acceptance criteria
accuracyRF<-(100-mean(c((nrow(Test_Data)-sum(diag(MisClassTest)))/nrow(Test_Data)),(nrow(Train_Data)-sum(diag(MisClassTrain)))/nrow(Train_Data)))
accuracyRF

# Weighted K Nearest Neighbour

library(kknn)

##Train the Dataset
Data.kknn<-train.kknn(subtopic~.,Train_Data_DF,distance=5,kernel = c("triangular","gaussian","epanechnikov", "optimal"),kmax=9)
summary(Data.kknn)

##Prediction on TEST and Train data set using Trained model
testPred.kknn<-predict(Data.kknn,Test_Data_DF)
trainPred.kknn<-predict(Data.kknn,Train_Data_DF)

#debugging
l<-sapply(Test_Data_DF,function(x)is.factor(x))
m<-Test_Data_DF[,names(which(l=="TRUE"))]
ifelse(n<-sapply(m,function(x)length(levels(x)))==1,"DROP","NODROP")

testPred.kknn.DF <- as.data.frame(testPred.kknn)

##Misclassification Matrix
MisClassTest<-table("Predict"=testPred.kknn,"Actual"=Test_Data$subtopic)  ## Test Data Prediction
MisClassTrain<-table("Predict"=trainPred.kknn,"Actual"=Train_Data$subtopic)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracykknn<-(100-mean(c((nrow(Test_Data)-sum(diag(MisClassTest)))/nrow(Test_Data)),(nrow(Train_Data)-sum(diag(MisClassTrain)))/nrow(Train_Data)))
accuracykknn
