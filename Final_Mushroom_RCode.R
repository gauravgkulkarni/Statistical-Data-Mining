install.packages("gmodels")
install.packages("Hmisc")
install.packages("gmodels")
install.packages("caTools")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("tree")
install.packages("randomForest")
install.packages("caret")
install.packages("party")
install.packages("VIM")
install.packages("DMwR")
install.packages("pROC")

library(gmodels)
library(caTools)
library(rpart)
library(rattle)
library(rpart.plot)
library(tree)
library(randomForest)
library(caret)
library(party)
library(VIM)
library(Hmisc)
library(pROC)
library(DMwR)

# DESCRIPTIVE STATISTICS OF DATASET

#Reading the mushroom data from the csv file
mushroom<-read.csv(file.choose())
#Summary of the data
summary(mushroom)
#Structure of dataset
str(mushroom)
#Lists variables in the dataset
names(mushroom)
#First six rows of dataset
head(mushroom)
#Last six rows
tail(mushroom)



#VISUALIZATION USING BAR GRAPH

PE<-factor(mushroom$PE)
capShape<-factor(mushroom$cap.shape)
capSurface<-factor(mushroom$cap.surface)
capColor<-factor(mushroom$cap.color)
bruises<-factor(mushroom$bruises)
odor<-factor(mushroom$odor)
gillAttachment<-factor(mushroom$gill.attachment)
gillSpacing<-factor(mushroom$gill.spacing)
gillSize<-factor(mushroom$gill.size)
gillColor<-factor(mushroom$gill.color)
stalkShape<-factor(mushroom$stalk.shape)
stalkRoot<-factor(mushroom$stalk.root)
stalkSurfaceAboveRing<-factor(mushroom$stalk.surface.above.ring)
stalkSurfaceBelowRing<-factor(mushroom$stalk.surface.below.ring)
stalkColorAboveRing<-factor(mushroom$stalk.color.above.ring)
stalkcolorBelowRing<-factor(mushroom$stalk.color.below.ring)
veilColor<-factor(mushroom$veil.color)
ringNumber<-factor(mushroom$ring.number)
ringType<-factor(mushroom$ring.type)
sporePrintColor<-factor(mushroom$spore.print.color)
population<-factor(mushroom$population)
habitat<-factor(mushroom$habitat)


#BAR GRAPH FOR MUSHROOM'S CAP SHAPE
jointcapShape=CrossTable(capShape,PE,prop.chisq = FALSE)
joint_counts1=jointcapShape$t
barplot(joint_counts,beside=TRUE,col=cm.colors(6),ylab='Frequency',xlab='PE')
legend('topright',c('bell','conical','convex','flat','knobbed','sunken'),pch=15,col=cm.colors(6))

#BAR GRAPH FOR MUSHROOM'S CAP COLOR
jointcapColor=CrossTable(capColor,PE,prop.chisq = FALSE)
joint_counts1=jointcapColor$t
barplot(joint_counts,beside=TRUE,col=cm.colors(6),ylab='Frequency',xlab='PE')
legend('topright',c('brown','buff','cinnamon','gray','green','pink','purple','red','white','yellow'),pch=15,col=cm.colors(10))

#BAR GRAPH FOR MUSHROOM'S BRUISES
jointbruises=CrossTable(bruises,PE,prop.chisq = FALSE)
joint_counts1=jointbruises$t
barplot(joint_counts,beside=TRUE,col=cm.colors(9),ylab='Frequency',xlab='PE')
legend('topright',c('bruises','no'),pch=15,col=cm.colors(2))

#BAR GRAPH FOR MUSHROOM'S ODOR
jointodor=CrossTable(odor,PE,prop.chisq = FALSE)
joint_counts1=jointodor$t
barplot(joint_counts,beside=TRUE,col=cm.colors(9),ylab='Frequency',xlab='PE')
legend('topright',c('almond','anisel','creosote','fishy','foul','musty','none','pungent','spicy'),pch=15,col=cm.colors(9))

#BAR GRAPH FOR MUSHROOM'S GILL ATTACHMENT
jointgillAttachment=CrossTable(gillAttachment,PE,prop.chisq = FALSE)
joint_counts1=jointgillAttachment$t
barplot(joint_counts,beside=TRUE,col=cm.colors(4),ylab='Frequency',xlab='PE')
legend('topright',c('attached','descending','free','notched'),pch=15,col=cm.colors(4))

#BAR GRAPH FOR MUSHROOM'S GILL SPACING
jointgillSpacing=CrossTable(gillSpacing,PE,prop.chisq = FALSE)
joint_counts1=jointgillSpacing$t
barplot(joint_counts,beside=TRUE,col=cm.colors(3),ylab='Frequency',xlab='PE')
legend('topright',c('close','crowded','distant'),pch=15,col=cm.colors(3))

##BAR GRAPH FOR MUSHROOM'S GILL SIZE
jointgillSize=CrossTable(gillSize,PE,prop.chisq = FALSE)
joint_counts1=jointgillSize$t
barplot(joint_counts,beside=TRUE,col=cm.colors(2),ylab='Frequency',xlab='PE')
legend('topright',c('broad','narrow'),pch=15,col=cm.colors(2))

#BAR GRAPH FOR MUSHROOM'S GILL COLOR
jointgillColor=CrossTable(gillColor,PE,prop.chisq = FALSE)
joint_counts1=jointgillColor$t
barplot(joint_counts,beside=TRUE,col=cm.colors(12),ylab='Frequency',xlab='PE')
legend('topright',c('black','brown','buff','chocolate','gray','green','orange','pink','purple','red','white','yellow'),pch=15,col=cm.colors(12))

#BAR GRAPH FOR MUSHROOM'S STALK SHAPE
jointstalkshape=CrossTable(stalkShape,PE,prop.chisq = FALSE)
joint_counts1=jointstalkshape$t
barplot(joint_counts,beside=TRUE,col=cm.colors(2),ylab='Frequency',xlab='PE')
legend('topright',c('enlarging','tapering'),pch=15,col=cm.colors(2))

#BAR GRAPH FOR MUSHROOM'S STALK ROOT
jointstalkRoot=CrossTable(stalkRoot,PE,prop.chisq = FALSE)
joint_counts1=jointstalkRoot$t
barplot(joint_counts,beside=TRUE,col=cm.colors(6),ylab='Frequency',xlab='PE')
legend('topright',c('bulbous','club','cup','equal','rhizomorphs','rooted'),pch=15,col=cm.colors(6))

#BAR GRAPH FOR MUSHROOM'S STALK SURFACE ABOVE RING
jointstalksurfaceAboveRing=CrossTable(stalkSurfaceAboveRing,PE,prop.chisq = FALSE)
joint_counts1=jointstalksurfaceAboveRing$t
barplot(joint_counts,beside=TRUE,col=cm.colors(4),ylab='Frequency',xlab='PE')
legend('topright',c('fibrous','scaly','silky','smooth'),pch=15,col=cm.colors(4))

#BAR GRAPH FOR MUSHROOM'S STALK SURFACE BELOW RING
jointstalksuracebelowRing=CrossTable(stalkSurfaceBelowRing,PE,prop.chisq = FALSE)
joint_counts1=jointstalksuracebelowRing$t
barplot(joint_counts,beside=TRUE,col=cm.colors(4),ylab='Frequency',xlab='PE')
legend('topright',c('fibrous','scaly','silky','smooth'),pch=15,col=cm.colors(4))

#BAR GRAPH FOR MUSHROOM'S STALK COLOR ABOVE RING
jointstalkcolorAboveRing=CrossTable(stalkColorAboveRing,PE,prop.chisq = FALSE)
joint_counts1=jointstalkcolorAboveRing$t
barplot(joint_counts,beside=TRUE,col=cm.colors(6),ylab='Frequency',xlab='PE')
legend('topright',c('brown','buff','cinnamon','gray','orange','pink','red','white','yellow'),pch=15,col=cm.colors(9))

#BAR GRAPH FOR MUSHROOM'S STALK COLOR BELOW RING
jointstalkcolorBelowRing=CrossTable(stalkcolorBelowRing,PE,prop.chisq = FALSE)
joint_counts1=jointstalkcolorBelowRing$t
barplot(joint_counts,beside=TRUE,col=cm.colors(9),ylab='Frequency',xlab='PE')
legend('topright',c('brown','buff','cinnamon','gray','orange','pink','red','white','yellow'),pch=15,col=cm.colors(9))


#BAR GRAPH FOR MUSHROOM'S VEIL COLOR
jointveilcolor=CrossTable(veilColor,PE,prop.chisq = FALSE)
joint_counts1=jointveilcolor$t
barplot(joint_counts,beside=TRUE,col=cm.colors(4),ylab='Frequency',xlab='PE')
legend('topright',c('brown','orange','white','yellow'),pch=15,col=cm.colors(4))

#BAR GRAPH FOR MUSHROOM'S RING NUMBER
jointringnumber=CrossTable(ringNumber,PE,prop.chisq = FALSE)
joint_counts1=jointringnumber$t
barplot(joint_counts,beside=TRUE,col=cm.colors(3),ylab='Frequency',xlab='PE')
legend('topright',c('none','one','two'),pch=15,col=cm.colors(3))

#BAR GRAPH FOR MUSHROOM'S RING TYPE
jointringtype=CrossTable(ringType,PE,prop.chisq = FALSE)
joint_counts1=jointringtyper$t
barplot(joint_counts,beside=TRUE,col=cm.colors(8),ylab='Frequency',xlab='PE')
legend('topright',c('cobwebby','evanescent','flaring','large','none','pendant','sheathing','zone'),pch=15,col=cm.colors(8))

#BAR GRAPH FOR MUSHROOM'S SPORE COLOR
jointsporeprintcolor=CrossTable(sporePrintColor,PE,prop.chisq = FALSE)
joint_counts1=jointsporeprintcolor$t
barplot(joint_counts,beside=TRUE,col=cm.colors(9),ylab='Frequency',xlab='PE')
legend('topright',c('black','brown','buff','chocolate','green','orange','purple','white','yellow'),pch=15,col=cm.colors(9))

#BAR GRAPH FOR MUSHROOM'S POPULATION
jointpopulation=CrossTable(population,PE,prop.chisq = FALSE)
joint_counts1=jointpopulation$t
barplot(joint_counts,beside=TRUE,col=cm.colors(6),ylab='Frequency',xlab='PE')
legend('topright',c('abundant','clustered','numerous','scattered','several','solitary'),pch=15,col=cm.colors(6))

#BAR GRAPH FOR MUSHROOM'S HABITAT
jointhabitat=CrossTable(habitat,PE,prop.chisq = FALSE)
joint_counts1=jointhabitat$t
barplot(joint_counts,beside=TRUE,col=cm.colors(7),ylab='Frequency',xlab='PE')
legend('topright',c('grasses','leaves','meadows','paths','urban','waste','woods'),pch=15,col=cm.colors(7))

FrequencyRoot <- table(StalkRoot)
RelativeFrequncyRoot <- table(StalkRoot)/8124
cbind(freqStalkroot,relfreqStalkroot)


#Naive Bayes Model

library ("klaR")
library ("caret")
library ("e1071")

mushroom = read.csv(file.choose())

mushroom$PE <- as.factor(mushroom$PE) #If not done, get error : Invalid prediction for "rpart" object
head(mushroom)

set.seed(1234)
Shuffledmushroom <-mushroom[sample(nrow(mushroom)),] #shuffle the data randomly so that zeroes are not there as previous
#Sample Indexes
indexes = sample(1:5644, .7*5644)
# Split data
training = Shuffledmushroom[indexes,]
testing = Shuffledmushroom[-indexes,]
head(training)
head(testing)

#train the model
model <- NaiveBayes(PE ~ ., data=training)

#test the model
predictions <- predict(model, testing)
warnings() #ignore the warnings
confusionMatrix(testing$PE, predictions$class)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(Shuffledmushroom)),breaks=10,labels=FALSE)
head(folds)
tail(folds)
BayesoutputData = 0

# Cross validation
#Perform 10 fold cross validation
for(i in 1:10){
  Sampleindexes <- which(folds==i,arr.ind=TRUE)
  train <- Shuffledmushroom[Sampleindexes, ]
  test <- Shuffledmushroom[-Sampleindexes, ]
  
  classifier = NaiveBayes(PE ~ ., data=train)
  pred = predict(classifier, test)
  misClassifyError = mean(pred$class != test$PE)
  misClassifyError
  Accuracy = 1-misClassifyError
  Accuracy
  BayesoutputData[i] = Accuracy
}

head(BayesoutputData,10)
summary(BayesoutputData)
confusionMatrix(test$PE, pred$class)


#DECISION TREE:

cleaningdata<-read.csv (file.choose())  
#Dropping column veil type since it wouldn't be much useful in modelling
cleaningdata$veil.type<-NULL
#Replacing ? with NA
cleaningdata[cleaningdata$stalk.root=='?',"stalk.root"] <- NA
#Omitting the rows with NA
cleaningdata<-na.omit(cleaningdata)
cleaningdata<-sample.split(Y=cleaningdata$PE,SplitRatio = .6)
train<-cleaningdata[cleaningdata1,]
test<-cleaningdata[!cleaningdata1,]
#building tree_model decision tree 
library(rpart)
tree_model<-rpart(PE~.,data=train,method='class')
plot(tree_model)
text(tree_model,pretty=0)
#Summar of decsion tree and how it is being splitted
summary(tree_model)
#Predicition in the testing dataset
predictionwithClass<-predict(tree_model,test,type='class')
t<-table(predictions=predictionwithClass,actual=test$PE)
#Accuracy matrix to see the accuracy of the model
sum(diag(t))/sum(t)
#Calculation of prediction with probability
predictwithprob<-predict(tree_model,test,type='prob')
#Calculating area under curve
auc<-auc(test$PE,predictwithprob[,2])
#Plotting ROC curve
plot(roc(test$PE,predictwithprob[,2]))

#RANDOM FOREST

#Modelling via random forest
library(randomForest)
Forestmodel<-randomForest(PE~.,data=train,mtry=3,ntree=20)
Forestmodel

#Calculating importance of varialbe using gini
importance(Forestmodel)

#Plotting the variables according to importance
varImpPlot(Forestmodel)

##Predicition in the testing dataset
pred <-predict(Forestmodel,test,type='class')
t<-table(predictions=pred,actual=test$PE)
t
#Accuracy matrix to see the accuracy of the model
sum(diag(t)/sum(t))

#Calculation of prediction with probability
library(pROC)
pred1 <-predict(Forestmodel,test,type='prob')
auc<-auc(test$PE,pred1[,2])
plot(roc(test$PE,pred1[,2]))


#Imputing missing value with mode 

#

data<-read.csv(file.choose()) 
colnames(data)
data$veil.type<-NULL
data[data$stalk.root=='?',"stalk.root"] <- NA

#Replacing it with Mode which is b in our case
data[is.na(data)]<-'b'

#Data splitting in the ratio of 60-40 and splitted into traiing and testing set
summary(data)
library(caTools)
data1<-sample.split(Y=data$PE,SplitRatio = .6)
train<-data[data1,]
test<-data[!data1,]

#building tree_model decision tree 
library(rpart)
tree_model<-rpart(PE~.,data=train,method='class')
plot(tree_model)
text(tree_model,pretty=0)
summary(tree_model)

#Predicition in the testing dataset
predictionwithClass<-predict(tree_model,test,type='class')
t<-table(predictions=predictionwithClass,actual=test$PE)

#Accuracy matrix to see the accuracy of the model

sum(diag(t))/sum(t)

#Calculation of prediction with probability
predictwithprob<-predict(tree_model,test,type='prob')
#Calculating area under curve
auc<-auc(test$PE,predictwithprob[,2])
#Plotting ROC curve
plot(roc(test$PE,predictwithprob[,2]))

#RANDOM FOREST

#Modelling via random forest
library(randomForest)
modelRandom<-randomForest(PE~.,data=train,mtry=3,ntree=20)
modelRandom

#Calculating importance of varialbe using gini
importance(modelRandom)

#Plotting the variables according to importance
varImpPlot(modelRandom)

##Predicition in the testing dataset
predictionwithClass<-predict(modelRandom,test,type='class')
t<-table(predictions=predictionwithClass,actual=test$PE)
t
#Accuracy matrix to see the accuracy of the model
sum(diag(t)/sum(t))

#Calculation of prediction with probability
library(pROC)
predictionProb<-predict(modelRandom,test,type='prob')
auc<-auc(test$PE,predictionProb[,2])
auc
plot(roc(test$PE,predictionProb[,2]))