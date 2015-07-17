rm(list=ls())
setwd("C:\\Users\\cdron\\Desktop\\Cleanup\\Misc\\Courses\\AnalyticEdge_Jun2015\\kaggle")
library(data.table)
set.seed(123)

train <- read.csv('eBayiPadTrain.csv', stringsAsFactors=FALSE)
test <- read.csv('eBayiPadTest.csv', stringsAsFactors=FALSE)
sub <- read.csv('SampleSubmission.csv', header=TRUE)
head(sub)

test$sold <- 999
nrows <- nrow(train)
nrows


data <- rbind(train,test)
head(data)

# convert to factors 
factorize <- function(df){
 col_names <- names(df)
 print(col_names)
 df[,col_names] <- lapply(df[,col_names], factor)
 return(df)
}

data[,c(2,4:9)] <- factorize(data[,c(2,4:9)])
head(data)
str(data)




# Text Analytics Preprocessing
library(tm)
preprocess <- function(df){
corpusDescription = Corpus(VectorSource(df$description))
corpusDescription = tm_map(corpusDescription, tolower)
corpusDescription = tm_map(corpusDescription, PlainTextDocument)
corpusDescription = tm_map(corpusDescription, removePunctuation)
corpusDescription = tm_map(corpusDescription, removeWords, stopwords('english'))
corpusDescription = tm_map(corpusDescription, stemDocument)
dtmCorpDesc = DocumentTermMatrix(corpusDescription)
print(dim(dtmCorpDesc))
dtmCorpDesc = removeSparseTerms(dtmCorpDesc, 0.997)
print(dim(dtmCorpDesc))
dtmCorpDesc = as.data.frame(as.matrix(dtmCorpDesc))
colnames(dtmCorpDesc) = paste0("T", colnames(dtmCorpDesc))
return(dtmCorpDesc)
}

dtm_df <- preprocess(data)

datawithdtm <- cbind(data, dtm_df)

total_rows = nrow(datawithdtm)


drops <- c("description", "UniqueID")
datawithdtm <- datawithdtm[,!(names(datawithdtm) %in% drops)]

learn_df <- datawithdtm[1:nrows,]
finaltest_df <- datawithdtm[1862:2659,]

dim(learn_df)
tail(learn_df)
str(learn_df)


learn_df$sold <- as.character(learn_df$sold)


library(caTools)
spl = sample.split(learn_df$sold,0.7)
dtrain_df = subset(learn_df,spl==TRUE)
dim(dtrain_df)
dvalid_df = subset(learn_df, spl==FALSE)
dim(dvalid_df)
table(dvalid_df$sold)


library(rpart)
library(rpart.plot)
modelCART <- rpart(sold ~., data =dtrain_df, method="class")
prp(modelCART)
validCART <- predict(modelCART, newdata = dvalid_df, type="class")
table(learn_df$sold)

table(dvalid_df$sold, validCART)
(269+170)/(279+170+31+88)
(266+179)/(266+34+79+179)

library(randomForest)
rf <- randomForest(factor(sold)~., data=dtrain_df,ntree = 20, do.trace = 2)
print(rf)
importance(rf)
table(dvalid_df$sold, predict(rf, newdata=dvalid_df, type="response"))
(257+183)/(257+183+75+43)

(257+187)/(257+187+43+71)



#for Whole traindataset
rf <- randomForest(factor(sold)~.,data=learn_df,ntree = 30, do.trace=2)
predictrf <- predict(rf, newdata=finaltest_df, type="prob")
dim(predictrf)
predictrf[,2]

Probability1 <- predictrf[,2]
UniqueID <- sub$UniqueID 
df <- cbind(UniqueID,Probability1)
head(df)

write.csv(df, file="first_submission_rf.csv",row.names=FALSE,col.names=TRUE,sep=",",quote=FALSE)




