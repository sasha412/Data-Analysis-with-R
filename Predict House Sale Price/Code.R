library(dplyr)
library(randomForest)
library(MASS)

#### Load All Three sets ###
### Set train and validation data sets 

train=train_new %>% mutate_if(is.character, as.factor)
validation=validation_new %>% mutate_if(is.character, as.factor)

#adding dummy column in order to merge
test_new$logSalePrice <- rep(1,nrow(test_new))
test=test_new %>% mutate_if(is.character, as.factor)
#factor levels present in Test not present in Train 
totalData<- rbind(train,validation,test)
for (f in 1:length(names(totalData))) 
  {
  levels(train[, f]) <- levels(totalData[, f])
}

id.train.v <- train$Id
id.validation.v<-validation$Id
id.test.v<- test$Id

train_fixed <- totalData[totalData$Id %in% id.train.v, ]
validation_fixed <- totalData[totalData$Id %in% id.validation.v, ]
test_fixed<-totalData[totalData$Id %in% id.test.v, ]

house.rf=randomForest(logSalePrice~.,data=train_fixed,mtry=15,ntree=1000,importance=TRUE)
house.rf

yhat.bag = predict(house.rf,newdata=validation_fixed)

#changing from log to normal

validation=validation_fixed["logSalePrice"]
validation$logSalePrice <- exp(validation_fixed$logSalePrice)
yhat.bag<- exp(yhat.bag)

# Plot fit curve

plot(yhat.bag, validation$logSalePrice)
abline(0,1)
mean((yhat.bag - validation$logSalePrice)^2)

sqrt(mean((yhat.bag - validation$logSalePrice)^2))/mean(validation$logSalePrice)
#896911771 or 29k over mean of 150k
#assess importance 

importance(house.rf)
varImpPlot(house.rf)

#Try number Mty
acc <- rep(1,40)
for (i in 1:40)
{
  set.seed(25)
  house1.rf=randomForest(logSalePrice~.,data=validation_fixed,mtry=i,ntree=1000,importance=TRUE)
  yhat.trans= predict(house1.rf,validation_fixed)
  acc[i] <- mean((yhat.trans - validation_fixed$logSalePrice)^2)
}


print(paste("min MSE is at ",which.min(acc),"with value of ",min(acc)))

#Min MSe at Mtry=24
# running model at mtry 24
# Ntree best at 500 (ut of 100,100,500,750)

set.seed(25)
house2.rf=randomForest(logSalePrice~.,data=train_fixed,mtry=27,ntree=500,importance=TRUE)
yhat.bag = predict(house2.rf,newdata=validation_fixed)

#undoing log
yhat.bag<- exp(yhat.bag)
mean((yhat.bag - validation)^2)
# MSE 865987655
#Tunning for ntree and removing variables 
#best ntree at 500
importance(house2.rf)
varImpPlot(house2.rf)
mean(boston.test)


######### Bagging Final Model ######################################################
set.seed(25)
house3.rf=randomForest(logSalePrice~.,data=train_fixed,mtry=28,ntree=1000,importance=TRUE)

importance(house3.rf)
varImpPlot(house3.rf)

yhat.bag = predict(house3.rf,newdata=validation_fixed)

#undoing log
validation.test=validation_fixed["logSalePrice"]

plot(yhat.bag, validation.test$logSalePrice)
abline(0,1)

validation.test<- exp(validation_fixed$logSalePrice)
yhat.bag<- exp(yhat.bag)
mean((yhat.bag - validation.test)^2)
sqrt(mean((yhat.bag - validation.test)^2))
mean(exp(validation_fixed$logSalePrice))
#.1422 RMSE over Mean on Validation 
sqrt(mean((yhat.bag - validation.test)^2))/mean(exp(validation_fixed$logSalePrice))

#creating set for kaggle 
yhat.bag = predict(house3.rf,newdata=test_fixed)
yhat.bag<-exp(yhat.bag)
df<-data.frame(id.test.v,yhat.bag)

write.csv(df, file = "submission.csv")

############## final BOosting ######################################################################################################################

library(gbm)
set.seed(25)
boost.boston=gbm(logSalePrice~.-PoolArea -pool_good, -paved -pubutil -RoofMatl_high,data=train_fixed,distribution="gaussian",
                 n.trees=3000,interaction.depth=4,shrinkage=0.01,verbose=F)
summary(boost.boston)

yhat.boost1=predict(boost.boston,newdata=validation_fixed,n.trees=3000,na.action =NULL)
yhat.boost1<-exp(yhat.boost1)
Actual.validation<-exp(validation_fixed$logSalePrice)

# plot fit curve 
plot(yhat.boost1, Actual.validation)
abline(0,1)


mean((yhat.boost1-Actual.validation)^2)
sqrt(mean((yhat.boost1-Actual.validation)^2))
sqrt(mean((yhat.boost1-Actual.validation)^2))/mean(Actual.validation)

#creating set for kaggle. Best set got submission got score of .13119 1135 place 
yhat.boost1=predict(boost.boston,newdata=test_fixed,n.trees=3000,na.action =NULL)
yhat.boost1<-exp(yhat.boost1)
df<-data.frame(id.test.v,yhat.boost1)

write.csv(df, file = "submissionboosted.csv")

library(corrplot)
m<-cor(train_fixed)
corrplot(m, type="upper")

################### Penallized Models  ##########################################################

## Ridge ##

x=model.matrix(logSalePrice~.,train_fixed)[,-1]  # note that [,-1] removes the matrix first column which is an intercept term
y=train_fixed$logSalePrice
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=1,lambda=grid)

#cross validation to find opitmal lamda
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = grid)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

# not used for anything right now 
dim(coef(ridge.mod))
ridge.mod$lambda[.001]
coef(ridge.mod)[,001]

#run model and fit on validation 
ridge.mod=glmnet(x,y,alpha=0,lambda=.01, thresh=1e-12)
valx=model.matrix(logSalePrice~.,validation_fixed)[,-1]  # note that [,-1] removes the matrix first column which is an intercept term
valy=validation_fixed$logSalePrice
ridge.pred=predict(ridge.mod,s=.01,newx=valx)

ridge.pred<-exp(ridge.pred)
Validation.actual<-exp(validation_fixed$logSalePrice)
plot(ridge.pred, Validation.actual)
abline(0,1)

mean((ridge.pred-Validation.actual)^2)
sqrt(mean((ridge.pred-Validation.actual)^2))
sqrt(mean((ridge.pred-Validation.actual)^2))/mean(Validation.actual)

############ for submission into kaggle with Test set #####################################################################
ridge.mod=glmnet(x,y,alpha=0,lambda=.01, thresh=1e-12)
testx=model.matrix(logSalePrice~.,test_fixed)[,-1]  # note that [,-1] removes the matrix first column which is an intercept term
testy=test_fixed$logSalePrice
ridge.pred=predict(ridge.mod,s=4,newx=testx)

ridge.pred<-exp(ridge.pred)
df<-data.frame(id.test.v,ridge.pred)
write.csv(df, file = "submissionRidge.csv")


###Lasso########################################################################################################################
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)

plot(lasso.mod)

###tune for Lamda
cv_fit <- cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

#set vaidation sets 
lasso.mod=glmnet(x,y,alpha=1,lambda=.01)
valx=model.matrix(logSalePrice~.,validation_fixed)[,-1]  # note that [,-1] removes the matrix first column which is an intercept term
valy=validation_fixed$logSalePrice
lasso.pred=predict(lasso.mod,s=.01,newx=valx)


lasso.pred<-exp(lasso.pred)
Validation.actual<-exp(validation_fixed$logSalePrice)
summary(lasso.mod)

mean((ridge.pred-Validation.actual)^2)
sqrt(mean((lasso.pred-Validation.actual)^2))
sqrt(mean((lasso.pred-Validation.actual)^2))/mean(Validation.actual)

###### testing lasso #######################
lasso.mod=glmnet(x,y,alpha=1,lambda=.01)
testx=model.matrix(logSalePrice~.,test_fixed)[,-1]  # note that [,-1] removes the matrix first column which is an intercept term
testy=test_fixed$logSalePrice
lasso.pred=predict(lasso.mod,s=.01,newx=testx)

lasso.pred<-exp(lasso.pred)
df<-data.frame(id.test.v,lasso.pred)
write.csv(df, file = "submissionLasso.csv")


