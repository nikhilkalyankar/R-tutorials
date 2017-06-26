library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train[, 1] <- factor(vowel.train[, 1])
vowel.test[, 1] <- factor(vowel.test[, 1])

set.seed(33833)

mod1 <- train(y ~ ., method = "rf", data = vowel.train)
mod2 <- train(y ~ .,
              method = "gbm",
              data = vowel.train,
              verbose = FALSE)

pred1 <- predict(mod1, vowel.test[,-1])
pred2 <- predict(mod2, vowel.test[,-1])

confusionMatrix(pred1, vowel.test[, 1])
confusionMatrix(pred2, vowel.test[, 1])
confusionMatrix(pred1, pred2)

########################################

library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

modRF <- train(diagnosis ~ ., method = "rf", data = training)
modGBM <-
  train(diagnosis ~ .,
        method = "gbm",
        data = training,
        verbose = FALSE)
modLDA <- train(diagnosis ~ ., method = "lda", data = training)

predRF <- predict(modRF, testing[,-1])
predGBM <- predict(modGBM, testing[,-1])
predLDA <- predict(modLDA, testing[,-1])

predDF <-
  data.frame(diagnosis = testing[, 1], predRF, predGBM, predLDA)

modStack <- train(diagnosis ~ ., method = "rf", data = predDF)
predStack <- predict(modStack, predDF[,-1])

confusionMatrix(predRF, testing[, 1])
confusionMatrix(predGBM, testing[, 1])
confusionMatrix(predLDA, testing[, 1])
confusionMatrix(predStack, predDF[, 1])


########################################################
set.seed(3523)
library(AppliedPredictiveModeling)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3 / 4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]


set.seed(233)

fit <-
  train(CompressiveStrength ~ ., method = "lasso", data = training)
pred <- predict(fit, testing[, -9])

plot(fit, xvar = "penalty")

y <- training[, 9]
x <- training[, -9]

library(lars)
lasso.fit <- lars(as.matrix(x), y, type = "lasso", trace = TRUE)

plot(lasso.fit, breaks = FALSE)
covnames <- colnames(testing[, -9])
legend(
  "topleft",
  covnames,
  pch = 8,
  lty = 1:length(covnames),
  col = 1:length(covnames)
)

###################################################################
library(lubridate) # For year() function below
library(forecast)

dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

mean(testing$visitsTumblr < 784)

fit <- bats(tstrain)
fcast <- forecast(fit)
plot(fcast)
summary(fcast)

predict(fcast$model,testing$visitsTumblr)

fit2 <- bats(training$visitsTumblr)
fcast <- forecast(fit2)
plot(fit2)
summary(fit2)
summary(fcast)

###################################################
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)

fit <- svm(CompressiveStrength ~ .,data = training)

pred <- predict(fit,testing[,-9])
RMSE(pred,testing[,9])


######################################################







