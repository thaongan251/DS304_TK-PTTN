#PREPROCESSED DATA

#library
library(kernlab)
library(caret)
library(glmnet)

# Doc du lieu
df <- read.csv('diabetes.tab.tsv',header=TRUE,sep = '\t')

#preprocess

df_p = df
for( i in names(df_p))
{
  if(i!='SEX' & i!='Y')
  {
    Q3 = quantile(df_p[,i], 0.75)
    Q1 = quantile(df_p[,i], 0.25)
    IQR = Q3 - Q1
    olr_up = Q3+1.5*IQR
    olr_low = Q1-1.5*IQR
    
    df_p<-df_p[!(df_p[,i] > olr_up), ]
    df_p<-df_p[!(df_p[,i] < olr_low), ]
  }
}
  
#chia train-test
  set.seed(123)
  train.index <- createDataPartition(df_p$Y, p = .8, list = FALSE)
  
  train_p <- df_p[ train.index,]
  test_p  <- df_p[-train.index,]
  
  x.train_p <- train_p[,1:10]
  x.test_p <- test_p[,1:10]
  y.train_p <- train_p[,11]
  y.test_p <- test_p[,11]
  
  
  # Effect of Factors
  av <- aov(Y~.,data=train_p)
  summary(av)
  
  av <- aov(Y~(AGE+BMI+BP+S1+S2+S3+S5+S6)*S4,data=train_p)
  summary(av)

  av <- aov(Y~(AGE+BMI+BP+S1+S2+S3+S5+S6)*SEX,data=train_p)
  summary(av)
  
  # Effect of interaction
  av <- aov(Y~AGE*BMI*BP*S1*S2*S3*S5*S6+AGE*SEX,data=train_p)
  summary(av)
  
  #Build Models
  #before process
  #SLR
  fit <- lm(formula = Y~BMI,data=train_p)
  summary(fit)
  y.test.pred <- predict(fit,newdata=x.test_p)
  predict <- lm(y.test_p~y.test.pred)
  summary(predict)
  
  plot(y.test_p,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test_p),col='red',lwd=2)
  
  #multiple linear regression
  fit <- lm(formula = Y~SEX+BMI+BP+S1+S3+S5,data=train_p)
  summary(fit)
  y.test.pred <- predict(fit,newdata=x.test_p)
  predict <- lm(y.test_p~y.test.pred)
  summary(predict)
  
  plot(y.test_p,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test_p),col='red',lwd=2)
  
  #polynomial regression
  fit <- lm(formula = Y~AGE+BMI+S3+SEX+S5+I(BP^2)+I(AGE*SEX)+I(BMI*BP)+I(BP*S1*S3*S6)+I(AGE*BMI*S1*S3)+
            I(BMI*S2*S5*S6)+I(BP*S1*S2*S3*S6),
            data=train_p)
  summary(fit)#0.4964
  
  y.test.pred <- predict(fit,newdata=x.test_p)
  predict <- lm(y.test_p~y.test.pred)
  summary(predict)#0.5669
  
  plot(y.test_p,y.test.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.test.pred~y.test_p),col='red',lwd=2)
  
  #ridge (alpha = 0)
  set.seed(123)
  ridge.fit <- cv.glmnet(as.matrix(x.train_p),y.train_p,type.measure ='mse',alpha=0,family='gaussian')
  ridge.fit$lambda.1se
  coef(ridge.fit)
  
    #summary train
    ridge.predict.train <- predict(ridge.fit,s=ridge.fit$lambda.1se,newx = as.matrix(x.train_p))
    predict.train <- lm(y.train_p~ridge.predict.train)
    summary(predict.train)
    
    #summary test
    ridge.predict.test <- predict(ridge.fit,s=ridge.fit$lambda.1se,newx = as.matrix(x.test_p))
    predict.test <- lm(y.test_p~ridge.predict.test)
    summary(predict.test)
  
    #plot
    plot(y.test_p,ridge.predict.test, pch = 19, cex = 1, col = "black")
    abline(lm(ridge.predict.test~y.test_p),col='red',lwd=2)
  
  #lasso (alpha = 1)
  set.seed(123)
  lasso.fit <- cv.glmnet(as.matrix(x.train_p),y.train_p,type.measure ='mse',alpha=1,family='gaussian')
  lasso.fit$lambda.1se
  coef(lasso.fit)
  
    #summary train
    lasso.predict.train <- predict(lasso.fit,s=lasso.fit$lambda.1se,newx = as.matrix(x.train_p))
    predict.train <- lm(y.train_p~lasso.predict.train)
    summary(predict.train)
    
    #summary test
    lasso.predict.test <- predict(lasso.fit,s=lasso.fit$lambda.1se,newx = as.matrix(x.test_p))
    predict.test <- lm(y.test_p~lasso.predict.test)
    summary(predict.test)
    
    #plot
    plot(y.test_p,lasso.predict.test, pch = 19, cex = 1, col = "black")
    abline(lm(lasso.predict.test~y.test_p),col='red',lwd=2)
  
  #elastic net
  results.train <-data.frame()
  for (i in 0:20) 
  {
    set.seed(123)
    fit <- cv.glmnet(as.matrix(x.train_p), y.train_p, type.measure="mse", alpha=i/20, 
                     family="gaussian")
    y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.train_p))
    predict <- lm(y.train_p~y.pred)
    
    temp <- data.frame(alpha=i/20,R2= summary(predict)$r.squared,Adj_R2=summary(predict)$adj.r.squared,lambda=fit$lambda.1se)
    results.train <- rbind(results.train, temp)
  }#best alpha = 0.05
  
  set.seed(123)
  fit <- cv.glmnet(as.matrix(x.train_p), y.train_p, type.measure="mse", alpha=0.05, 
                   family="gaussian")
  fit$lambda.1se
  coef(fit)
  
  y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.train_p))
  predict <- lm(y.train_p~y.pred)
  summary(predict)
  
  y.pred <- predict(fit, s=fit$lambda.1se, newx=as.matrix(x.test_p))
  predict <- lm(y.test_p~y.pred)
  summary(predict)
  
  #plot
  plot(y.test_p, y.pred, pch = 19, cex = 1, col = "black")
  abline(lm(y.pred~y.test_p),col='red',lwd=2)
  







