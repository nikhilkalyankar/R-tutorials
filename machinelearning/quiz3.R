library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- segmentationOriginal[which(segmentationOriginal[,'Case'] == 'Train'),]
testing <- segmentationOriginal[which(segmentationOriginal[,'Case'] == 'Test'),]
dim(training)
dim(testing)

set.seed(125)

modFit <- train(Class ~ ., method = "rpart", data = training)

print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

##########################


library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

modFit <- train(Area ~ .,method = "rpart",data = olive)
pred <- predict(modFit,newdata[,-1])
pred


###########################

library(caret)
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
SAheart <- SAheart[,-c(1,4,5)]
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modFit <- train(chd ~ .,method = "glm",family = binomial,data = trainSA)
predTrain <- predict(modFit,trainSA)
predTest <- predict(modFit,testSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA[,7],prediction = predTrain)
missClass(testSA[,7],prediction = predTest)

################################
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train[,1] <- factor(vowel.train[,1])
vowel.test[,1] <- factor(vowel.test[,1])

set.seed(33833)

modFit <- train(y ~ .,
                data = vowel.train,
                method = "rf",
                prox = TRUE)
modFit <- randomForest(x = vowel.train[,-1],y = vowel.train[,1],proximity = TRUE)






