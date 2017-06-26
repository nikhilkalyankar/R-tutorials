#Predicting with trees

data(iris)
library(caret)
library(ggplot2)
names(iris)

table(iris$Species)

inTrain <-
  createDataPartition(y = iris$Species,
                      p = 0.7,
                      list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

modFit <- train(Species ~ ., method = "rpart", data = training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

# library(rattle)
# fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata = testing)

#################################################################3
#Bagging (Bootstrap Aggregating)

library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA, nrow = 10, ncol = 155)
for (i in 1:10) {
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
  ll[i,] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

plot(ozone$ozone,
     ozone$temperature,
     pch = 19,
     cex = 0.5)
for (i in 1:10) {
  lines(1:155, ll[i,], col = "grey", lwd = 2)
}
lines(1:155, apply(ll, 2, mean), col = "red", lwd = 2)

#Bagging with caret
# Some models perform bagging for you, in train function consider method options
# bagEarth
# treebag
# bagFDA

#Alternative caret
predictors = data.frame(ozone = ozone$ozone)
temperature = ozone$temperature
treebag <- bag(
  predictors,
  temperature,
  B = 10,
  bagControl = bagControl(
    fit = ctreeBag$fit,
    predict = ctreeBag$pred,
    aggregate = ctreeBag$aggregate
  )
)

plot(ozone$ozone, temperature, col = 'lightgrey', pch = 19)
points(
  ozone$ozone,
  predict(treebag$fits[[1]]$fit, predictors),
  pch = 19,
  col = "red"
)
points(ozone$ozone,
       predict(treebag, predictors),
       pch = 19,
       col = "blue")

#Parts of bagging
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate

#################################################33
#Random Forest
data(iris)
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]


modFit <- train(Species ~ .,
                data = training,
                method = "rf",
                prox = TRUE)
modFit

getTree(modFit$finalModel, k = 2)

irisP <-
  classCenter(training[, c(3, 4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <-
  qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(
  aes(x = Petal.Width, y = Petal.Length, col = Species),
  size = 5,
  shape = 4,
  data = irisP
)

pred <-
  predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)

qplot(
  Petal.Width,
  Petal.Length,
  colour = predRight,
  data = testing,
  main = "newdata Predictions"
)

#############################################################
# Boosting
# gbm - boosting with trees.
# mboost - model based boosting
# ada - statistical boosting based on additive logistic regression
# gamBoost for boosting generalized additive models
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~ .,
                method = "gbm",
                data = training,
                verbose = FALSE)
print(modFit)

qplot(predict(modFit,testing),wage,data=testing)

################################################################
# Model based prediction
data(iris)
library(ggplot2)
library(caret)
names(iris)

table(iris$Species)
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training)
dim(testing)

modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)

















