#library
library(kernlab)
library(caret)
library(glmnet)
library(moments)

# Doc du lieu
df <- read.csv('diabetes.tab.tsv',header=TRUE,sep = '\t')

#train-test
  set.seed(123)
  train.index <- createDataPartition(df$Y, p = .8, list = FALSE)
  train <- df[ train.index,]
  test  <- df[-train.index,]
  
  x.train <- train[,1:10]
  x.test <- test[,1:10]
  y.train <- train[,11]
  y.test <- test[,11]

# Effect of Factors no interactions
av <- aov(Y~.,data=train)
summary(av)

# Effect of Factor with interaction
av <- aov(Y~(AGE+BMI+BP+S1+S2+S3+S5+S6)*S4,data=train)
summary(av)

av <- aov(Y~(AGE+BMI+BP+S1+S2+S3+S5+S6)*SEX,data=train)
summary(av)

av <- aov(Y~AGE*BMI*BP*S1*S2*S3*S5*S6+AGE*SEX,data=train)
summary(av)

#Build Models
  #before process
  #SLR
  fit <- lm(formula = Y~BMI,data=train)
  summary(fit)
  y.test.pred <- predict(fit,newdata=x.test)
  predict <- lm(y.test~y.test.pred)
  summary(predict)
  
  plot(y.test,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test),col='red',lwd=2)
  
  #multiple linear regression
  fit <- lm(formula = Y~SEX+BMI+BP+S1+S2+S5+S6,data=train)
  summary(fit)

  y.test.pred <- predict(fit,newdata=x.test)
  predict <- lm(y.test~y.test.pred)
  summary(predict)
  
  plot(y.test,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test),col='red',lwd=2)
  
  #polynomial regression
  fit <- lm(formula = Y~AGE+BP+BMI+S3+SEX +
                        I(AGE^2)+I(S5^2)+I(S3^2)+
                        I(AGE*SEX)+
                        I(BMI*BP) + I(S1*S2*S3*S5)+
                        I(BMI*S1*S5) + I(BMI*S2*S5)+
                        I(AGE*S2*S3*S6)+                       
                        I(BMI*S1*S2*S3*S5) + I(AGE*BP*S3*S5*S6),
            data=train)
  summary(fit)#0.5213
  y.test.pred <- predict(fit,newdata=x.test) 
  predict <- lm(y.test~y.test.pred)
  summary(predict)#0.5499
  
  plot(y.test,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test),col='red',lwd=2)
  
  #ridge (alpha = 0)
  set.seed(123)
  ridge.fit <- cv.glmnet(as.matrix(x.train),y.train,type.measure ='mse',alpha=0,family='gaussian')
  ridge.fit$lambda.1se
  coef(ridge.fit)
  
    #summary train
    ridge.predict.train <- predict(ridge.fit,s=ridge.fit$lambda.1se,newx = as.matrix(x.train))
    predict.train <- lm(y.train~ridge.predict.train)
    summary(predict.train)
    
    #summary test
    ridge.predict.test <- predict(ridge.fit,s=ridge.fit$lambda.1se,newx = as.matrix(x.test))
    predict.test <- lm(y.test~ridge.predict.test)
    summary(predict.test)
    
    #plot
    plot(y.test,ridge.predict.test, pch = 19, cex = 1, col = "black")
    abline(lm(ridge.predict.test~y.test),col='red',lwd=2)
    
  #lasso (alpha = 1)
  set.seed(123)
  lasso.fit <- cv.glmnet(as.matrix(x.train),y.train,type.measure ='mse',alpha=1,family='gaussian')
  lasso.fit$lambda.1se
  coef(lasso.fit)
  
    #summary train
    lasso.predict.train <- predict(lasso.fit,s=lasso.fit$lambda.1se,newx = as.matrix(x.train))
    predict.train <- lm(y.train~lasso.predict.train)
    summary(predict.train)
  
    #summary test
    lasso.predict.test <- predict(lasso.fit,s=lasso.fit$lambda.1se,newx = as.matrix(x.test))
    predict.test <- lm(y.test~lasso.predict.test)
    summary(predict.test)

    #plot
    plot(y.test,lasso.predict.test, pch = 19, cex = 1, col = "black")
    abline(lm(lasso.predict.test~y.test),col='red',lwd=2)
    
  #elastic net
  results.train <-data.frame()
  for (i in 0:20) 
  {
    set.seed(123)
    fit <- cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", alpha=i/20, 
                     family="gaussian")
    y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.train))
    predict <- lm(y.train~y.pred)
    
    temp <- data.frame(alpha=i/20,R2= summary(predict)$r.squared,Adj_R2=summary(predict)$adj.r.squared,lambda=fit$lambda.1se)
    results.train <- rbind(results.train, temp)
  }#best alpha = 0.05
  
  set.seed(123)
  fit <- cv.glmnet(as.matrix(x.train), y.train, type.measure="mse", alpha=0.05, 
                   family="gaussian")
  fit$lambda.1se
  coef(fit)
  y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.train))
  predict <- lm(y.train~y.pred)
  summary(predict)
  
  y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.test))
  predict <- lm(y.test~y.pred)
  summary(predict)
  
  #plot
  plot(y.test, y.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.pred~y.test),col='red',lwd=2)
  