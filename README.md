# TopicDNA-NLP-R-Code
This NLP Model I have implemented for tweets analysis.

My twitter data has following variables:

1. Topic
2. Subtopic
3. Sub-subtopic
4. Tweets
5. Hashtags
6. Emojis

I have cleaned ActualTopic.csv data and divided it into train and test data.

Below algorithms I have implemented using this data and for this I have used R Studio R version 3.3.1 (2016-06-21).

1. Conditional Inference Trees
2. Random Forest Algorithm
3. Weighted K Nearest Neighbour

Among these algorithms I have chosen Weighted K Nearest Neighbour because it has given me 99% accuracy in prediction of topic based on words used in tweets.

Algorithm based on below steps:
1. Import require libraries such tm and snowballc
2. Import ActualTopic.csv dataset
3. Create corpus
4. Data Preprocessing
5. Generate Document Term Matrix
6. Add remaning variables to document term matrix
7. Create data frame
8. Divide data frame into test and train in 0.3 and 0.7 respectively
9. Apply Conditional Tree Interference Algorithm
10. Apply Random Forest Algorithm
11. Apply Weighted K Nearest Neighbour

Accuracy of all models I have checked after buidling a respective classifiers in TopicNLP.r file.

TopicNLP.r is a ready code which work successfully however I am working on TopicRandTest.r file to extend the work over large data.

Result of TopicNLP.r is quite good but this model is learning over small data set and hence it is facing difficulty over random large data. To overcome this TopicRandTest.r code will be used which can help to build machine learning model with large dataset and then we can do classification of n number of tweets over different topics. Still updating....................

