library(e1071)
library(ggplot2)
maintain <- read.csv('Maintain_discrete_broken.csv')
maintain$pressureInd <- NULL
maintain$moistureInd <- NULL
maintain$temperatureInd <- NULL
maintain$provider <- NULL
print(str(maintain))

##CONVERT TO CATEGORICAL:
maintain$broken <- factor(maintain$broken)

##EDA:
pl <- ggplot(maintain,aes(lifetime))
pl <- pl + geom_histogram(aes(fill=broken), bins=40, alpha=0.5)
pl <- pl + theme_classic()
pl
pl <- ggplot(maintain, aes(x=factor(team)))
pl <- pl + geom_bar(aes(fill=broken), position = 'dodge')
pl + theme_classic()
pl
pl <- ggplot(maintain, aes(x=factor(provider)))
pl <- pl + geom_bar(aes(fill=broken), position = 'dodge')
pl + theme_classic()
pl
pl <- ggplot(maintain, aes(team, provider)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(broken, provider)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(lifetime, provider)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(lifetime, team)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(lifetime, pressureInd)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(lifetime, moistureInd)) + geom_point() + theme_classic()
pl
pl <- ggplot(maintain, aes(lifetime, temperatureInd)) + geom_point() + theme_classic()
pl

##SVM MODEL:



#TRAIN TEST SPLIT:
library(caTools)
set.seed(102)
sample <- sample.split(maintain$broken, 0.7)
train <- subset(maintain, sample == T)
test <- subset(maintain, sample == F)

## TRAIN SVM:
library(e1071)
model <- svm(broken ~ ., data = train)
print(summary(model))

predicted.values <- predict(model,test[1:7])
table(predicted.values,test$broken)

tuned.results <- tune(svm, train.x=broken ~ .,data= train, kernel='radial',ranges=list(cost=c(100,200),gamma=c(0.1)))
print(summary(tuned.results))

#cost gamma
##100   0.1

tuned.model <- svm(broken ~ ., data=maintain, cost=100, gamma=0.1)
tuned.predictions <- predict(tuned.model, test)
table(tuned.predictions, test$broken)