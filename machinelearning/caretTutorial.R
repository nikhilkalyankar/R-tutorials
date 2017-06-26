#Data Splitting

library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)


#Fit a model
set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

#Final model
#modelFit <- train(type ~.,data=training, method="glm")
modelFit$finalModel

#Prediction
predictions <- predict(modelFit,newdata=testing)
predictions

#Confusion Matrix
confusionMatrix(predictions,testing$type)



### Data Slicing
#K-fold - returns TRAIN
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length)

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE)
sapply(folds,length)

#Resampling
set.seed(32323)
folds <- createResample(y=spam$type,times=10,list=TRUE)
sapply(folds,length)

#Test for k-fold and resampling
#folds[[1]][1:10]

#Time slicing
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,horizon=10)
names(folds)

#Test for time slicing
#folds$train[[1]]
#folds$test[[1]]



#Training options
#args(train.default) #not working
args(train) 
?train
?train.default

# function (x,
#           y,
#           method = "rf",
#           preProcess = NULL,
#           ...,
#           weights = NULL,
#           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
#           maximize = ifelse(metric ==
#                               "RMSE", FALSE, TRUE),
#           trControl = trainControl(),
#           tuneGrid = NULL,
#           tuneLength = 3)
# NULL

# Continous outcomes:
#   
# RMSE = Root mean squared error
# RSquared = R2 from regression models

# Categorical outcomes:
#   
# Accuracy = Fraction correct
# Kappa = A measure of concordance

args(trainControl) #also check ?trainControl
# function (method = "boot",
#           number = ifelse(method %in% c("cv",
#                                         "repeatedcv"), 10, 25),
#           repeats = ifelse(method %in% c("cv",
#                                          "repeatedcv"), 1, number),
#           p = 0.75,
#           initialWindow = NULL,
#           horizon = 1,
#           fixedWindow = TRUE,
#           verboseIter = FALSE,
#           returnData = TRUE,
#           returnResamp = "final",
#           savePredictions = FALSE,
#           classProbs = FALSE,
#           summaryFunction = defaultSummary,
#           selectionFunction = "best",
#           custom = NULL,
#           preProcOptions = list(thresh = 0.95,
#                                 ICAcomp = 3,
#                                 k = 5),
#           index = NULL,
#           indexOut = NULL,
#           timingSamps = 0,
#           predictionBounds = rep(FALSE, 2),
#           seeds = NA,
#           allowParallel = TRUE)
# NULL

# method
#
# boot = bootstrapping
# boot632 = bootstrapping with adjustment
# cv = cross validation
# repeatedcv = repeated cross validation
# LOOCV = leave one out cross validation

# number
#
# For boot/cross validation
# Number of subsamples to take

# repeats
#
# Number of times to repeate subsampling
# If big this can slow things down


######Settng the seed #** Important


#Plotting Predictors
library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

##Feature plot
featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")

#Qplot
qplot(age,wage,data=training)

qplot(age,wage,colour=jobclass,data=training)

qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

#hmisc
library(hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1

p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))

library(gridExtra)
grid.arrange(p1,p2,ncol=2)

#Tables
t1 <- table(cutWage,training$jobclass)
t1

prop.table(t1,1) #proportion


#Density plot
qplot(wage,colour=education,data=training,geom="density")



#Preprocess
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)

sd(training$capitalAve)

#Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 

mean(trainCapAveS)
sd(trainCapAveS)

##Standardizing test
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 

mean(testCapAveS)
sd(testCapAveS)


#Preprocess
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve

mean(trainCapAveS)
sd(trainCapAveS)

##Preprocess test
testCapAveS <- predict(preObj,testing[,-58])$capitalAve

mean(testCapAveS)
sd(testCapAveS)


##Preprocess argument
set.seed(32343)
modelFit <- train(type ~.,data=training,preProcess=c("center","scale"),method="glm")
modelFit

##Preprocess BoxCox
preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1, 2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


##Imputing data
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve #is it capAve or capitalAve?
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

#Check
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])


#Covariate Creation
#Level 1 - Raw to covariates
#Level 2 - Covariates to new covariates

library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage,p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

## ******Important*******
##Convert factor variables to indicator variables (or dummy variables)
table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))


##Removing zero covariates
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

##Spline basis
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis

lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

#Splines on test
predict(bsBasis,age=testing$age)


#Preprocess - PCA (Principal Component Variables)
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

M <- abs(cor(training[, -58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

X <- 0.71*training$num415 + 0.71*training$num857 #See below for explanation
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y) #Adding is better from the graph


#PCA
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

prComp$rotation

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")


#PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#Preprocess PCA

##Code not working
# preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
# trainPC <- predict(preProc,log10(training[,-58]+1))
# modelFit <- train(training$type ~ .,method="glm",data=trainPC)

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
training2 <- cbind(trainPC,type = training$type)
modelFit <- train(type ~ .,method="glm",data=training2)

#Increased accuracy with new code
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

#Alternate PCs
#Code not working
#modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)

modelFit <- train(type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))


#Predicting with Regression
library(caret)
data(faithful)
set.seed(333)
inTrain <- createDataPartition(y = faithful$waiting,p = 0.5,list = FALSE)
trainFaith <- faithful[inTrain, ]
testFaith <- faithful[-inTrain, ]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

coef(lm1)[1] + coef(lm1)[2]*80 #Prediction 1

newdata <- data.frame(waiting=80) #Prediction 2
predict(lm1,newdata)

par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

#Prediction Intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

#With caret
modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)

#Predicting with Multiple covariate Regression
library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training)
dim(testing)

featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")

qplot(age,wage,data=training)

qplot(age,wage,colour=jobclass,data=training) #Colouring job class
qplot(age,wage,colour=education,data=training) #Colouring Education

modFit<- train(wage ~ age + jobclass + education,method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

plot(finMod,1,pch=19,cex=0.5,col="#00000010") #Diagnostic plot
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

plot(finMod$residuals,pch=19)

#Prediction
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

#Using all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)
