
library(nnet);  #  
library(mlbench); 
test.cl=function(true,pred)
  {
  true<-max.col(true);
  cres=max.col(pred);
  table(true,cres)
  };  
 # load the data 
  df<-read.csv("final_data.csv",header=T);
  df1<-data.frame(df$Horizontal_depth,df$Vertical_depth,df$normalized_E,df$duration,df$conductor,df$surface,df$intermediate,df$liner_length,df$year);
  dftrain<-data.frame(df$Horizontal_depth,df$Vertical_depth,df$normalized_E,df$conductor,df$surface,df$intermediate,df$liner_length,df$year);
  dfpredict<-data.frame(df$duration);
  n=length(df1[,1]);
  x=1:n;
  set.seed(1);
  normdftrain<-scale(dftrain);
  normdfpredict<-scale(dfpredict)
  trainindex<-1:128;
  netmodel<-nnet(normdftrain[trainindex,],dfpredict[trainindex,],size=10,decay=0.01,maxit=1000,trace=F,linout=T);
  pretrain<-predict(netmodel,normdftrain[trainindex,]);
  pretest<-predict(netmodel,normdftrain[-trainindex,]);
  plot(trainindex,dfpredict[trainindex,],ylab="duration");
  points(trainindex,pretrain,col="blue",pch=4);
  plot(x[-trainindex],pretest,col="blue",pch=4);
  points(x[-trainindex],dfpredict[-trainindex,]);
  #fit1<-lm(dfpredict[])
  error<-sqrt(mean((dfpredict[-trainindex,]-pretest)^2))
  relerror1<-mean(abs(dfpredict[-trainindex,]-pretest)/dfpredict[-trainindex,])
  #fit2<-lm(dfpredict[-trainindex,] ~ pretest) 
  #plot(fit2)
  #summary(fit2) 
  
  #test.cl(normdfpredict[trainindex,],pretrain);
  #test.cl(normdfpredict[-trainindex,],pretest);
  #plot(df$Horizontal_depth,df$duration)
  #plot(df$Horizontal_depth,df$duration)
 # plot(df$Vertical_depth,df$duration)
  #plot(df$normalized_E,df$duration)
  