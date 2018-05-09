#train <- read.csv("~/Desktop/R Projects/Titanic/train.csv")
#View(train)
#test <- read.csv("~/Desktop/R Projects/Titanic/test.csv")
#View(test)

str(train)


# Survival rates in absolute numbers
table(train$Survived)

# Survival rates in proportions
prop.table(table(train$Survived))

test$Survived <- rep(0, 418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv",row.names=FALSE)


prop.table(table(train$Sex,train$Survived),1)

#pred based on sex
test$Survived[test$Sex == 'female'] <- 1


train$Child <- 0
train$Child[train$Age < 18] <- 1

#pred based on age and sex
summary(train$Age)
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#pred based on age, sex, and fare
summary(train$Fare)
train$Fare2 <- '30+'
train$Fare2[train$Fare>=10 & train$Fare<20] <-'10-20'
train$Fare2[train$Fare>=20 & train$Fare<30] <- '20-30'
train$Fare2[train$Fare<10] <- '<10'
show(train)
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

#output 2
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv",row.names=FALSE)

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)


train[,c("Child","Fare2"):=NULL]

#create new column
test$Survived <- NA
combi <- rbind(train, test)
show(combi$Title)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
Bing, Mr. Lee


combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
 combi$FamilyID <- factor(combi$FamilyID)
 train <- combi[1:891,]
 test <- combi[892:1309,]
 
 fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
              data=train, 
              method="class")
#multi variable prediction
Prediction <- predict(fit, test, type = "class")
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
 write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
 
 summary(combi$Age)
 
#fill the missing age values using prediction
  Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                   data=combi[!is.na(combi$Age),], 
                   method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi) 

summary(combi$Embarked)

# find 2 blank spaces in embarked
which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)

#fill missing afre value
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#reduce factor levels of familid below 32
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

summary(combi$FamilyID2)

install.packages('randomForest')
library(randomForest)

set.seed(415)

train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)
#multi variable predict using random forests
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

install.packages('party')
library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FamilySize + FamilyID + Cabin + Ticket,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest2.csv", row.names = FALSE)


summary(train$Cabin)
summary(train$Ticket)

#tickets and cabin
table(train$Cabin,train$Survived)
train2 <- train
combi$Cabi <- as.character(combi$Cabin)
combi$CNFirst <- sapply(combi$Cabin, FUN=function(x) {strsplit(x, "")[[1]][1]})


