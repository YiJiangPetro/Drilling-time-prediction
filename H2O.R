library(h2o)
library(Metrics)
library(jsonlite)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE) 

df<-read.csv("final_data.csv",header=T);
df1<-data.frame(df$Horizontal_depth,df$Vertical_depth,df$normalized_E,df$duration,df$conductor,df$surface,df$intermediate,df$liner_length,df$latitude,df$longitude,df$year);
dff<-data.matrix(df1)
dftrain<-df[,c(2,3,4,6,7,8,9,10,11,12)];
dfpredict<-df[,5];
#class(dfpredict)
#summary(dfpredict);
plot(df$Horizontal_depth,df$duration)
#plot(df$Horizontal_depth,df$duration)
plot(df$Vertical_depth,df$duration)
plot(df$normalized_E,df$duration)

## Convert data into H2O 
train_h2o <- as.h2o(data.frame(df1[1:128,]))


#h2o deep net 
dlmodel <-h2o.deeplearning (x=c(1,2,3,5,6,7,8,11), # x is 2dn snd 3rd columns 
                                y=4,      # y is 4th column, un-scaled 
                                train_h2o, 
                               # classification_stop = -1,  
                                #regression_stop=-1,
                                hidden=c(10,10,10,10),
                                epochs=1000,
                                rate=0.01,
                               # validation_frame = test_h2o,
                                activation=("Tanh"), 
                                adaptive_rate= T,
                                #l1=c(0,1e-5),
                                variable_importances = T,
                                reproducible=T,
                            seed = 0,
                                input_dropout_ratio = c(0.01), # % of inputs dropout 
                                hidden_dropout_ratios = c(0.01,0.01,0.01,0.01), # % for nodes dropout 
                                train_samples_per_iteration = -2, # use all data, not sampled 
) 

# grid search
#grid_search <- h2o.deeplearning(x=c(1,2,3,5,6,7), # x is 2dn snd 3rd columns 
#                                 y=4,      # y is 4th column, un-scaled 
#                                 train_h2o,
#                                 #classification=FALSE,
#                                 #validation_frame = test_h2o,
#                                 hidden=c(10,10), 
#                                 epochs=500,
#                                 #activation=c("Tanh", "Rectifier"), 
#                                 #l1=c(0.0001,0.001)
#                                )
#best_model <- grid_search@model[[1]]
#best_model
#best_params <- best_model@model$params
#best_params$activation
#best_params$hidden
#best_params$l1
#grid <- h2o.grid("deeplearning", x = c(1,2,3,5,6,7), y = 4, training_frame = train_h2o,
#                 hyper_params =list(c(10,10,10),c(20,20,20)))

## outsample 
## Converting H2O format into data frame 
df_yhat_train <- as.data.frame(h2o.predict(dlmodel,train_h2o)) 
predH2oIn<- df_yhat_train$predict 
x=1:length(predH2oIn)
plot(x,predH2oIn,col="red",pch=4,ylim=c(0, 30));
points(x,dfpredict[1:128],col="blue",pch=4);
#plot(predH2O,train_h2o$duration) 
#fit2<-lm(dfpredict ~ predH2OIn) 
#plot(fit2)
#summary(fit2)

#error<-rmse(df$duration,predH2O)
error<-sqrt(mean((df$duration[1:128]-predH2oIn)^2))
relerror<-mean(abs(df$duration[1:128]-predH2oIn)/df$duration[1:128])

x=129:148;
test_data<-sample(x,20);
test_h2o <- as.h2o(df1[test_data,]) 
df_yhat_test <- as.data.frame(h2o.predict(dlmodel, test_h2o)) 
predH2o <-df_yhat_test$predict
plot(test_data,df_yhat_test$predict,col="red",pch=4,ylim=c(0, 30));
points(test_data,dfpredict[test_data],col="blue",pch=4);
#error1<-sqrt(mean((-predH2o)^2))
error1<-sqrt(mean((dfpredict[test_data]-predH2o)^2))
relerror1<-mean(abs(dfpredict[test_data]-predH2o)/dfpredict[test_data])
error
error1
h2o.varimp(dlmodel)
## in sample 
#df_yhat_train <- as.data.frame(h2o.predict(model, train_h2o)) 
#predH2OIn<- df_yhat_train$predict 
#plot(predH2OIn,train_h2o$duration) 
##fit3<-lm(dfpredict ~ predH2OIn) 
#summary(fit3) 
