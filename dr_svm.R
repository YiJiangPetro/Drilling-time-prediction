
  library(e1071);
  library(ggplot2);
  rmse <- function(error)
  {
    sqrt(mean(error^2))
  }
  
  df<-read.csv("final_data.csv",header=T);
  df1<-data.frame(df$Horizontal_depth,df$Vertical_depth,df$normalized_E,df$duration,df$conductor,df$surface,df$intermediate,df$liner_length,df$year);
  plot(df$Horizontal_depth,df$duration)
  #plot(df$Horizontal_depth,df$duration)
  plot(df$Vertical_depth,df$duration)
  plot(df$normalized_E,df$duration)
  dmodel<-svm(df.duration ~ ., data=df1,type="nu-regression", cross=10,scale=TRUE);
  predictday<-predict(dmodel,df1);
  #MSE<- rmse(error1);
 # print(MSE);
  tuneResult<- tune(svm, df.duration ~ ., data=df1, ranges=list(epsilon=seq(0.1,1,0.05),cost=2^(0.5:2)));
  print(tuneResult)
  #print(cost)
  summary(tuneResult)
  plot(tuneResult)
  
  tuneModel<- tuneResult$best.model;
  #plot(tuneModel)
  tuneday<-predict(tuneModel,data=df1);
  #tuneerror<-df1$df.duration-tuneday;
  #tunemse<-rmse(tuneerror)
  #print(tunemse);
  #plot(tuneModel);
  x=1:length(tuneday);
  
  plot(x,df1$df.duration);
  points(x,tuneday,col="blue",pch=4);
  error<-sqrt(mean((df$duration-tuneday)^2))
  relerror<-mean(abs(df$duration-tuneday)/df$duration)
  fit1<-lm(df$duration ~ tuneday)
  summary(fit1)
  