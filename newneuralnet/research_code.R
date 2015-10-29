
library(neuralnet)
#source("neuralnet/R/neuralnetwork.R")
# Assign the data set
train <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv")
# Have a look at dataset
sample_entries <- sample( 1 : nrow(train), size = nrow(train), replace = FALSE )
# creating the sample
train <- train[sample_entries,]
str(train)

train$Sex = as.numeric(train$Sex)
train$Embarked = as.numeric(train$Embarked)

makeTitanicReport = function(hidden, seed, threshold){
  
  t.accuracy.out = rep(0, hidden)
  s.accuracy.out = rep(0, hidden)
  r.accuracy.out = rep(0, hidden)
  n.accuracy.out = rep(0, hidden)
  
  t.steps = rep(0, hidden)
  s.steps = rep(0, hidden)
  r.steps = rep(0, hidden)
  n.steps = rep(0, hidden)
  
  t.error.in = rep(0, hidden)
  s.error.in = rep(0, hidden)
  r.error.in = rep(0, hidden)
  n.error.in = rep(0, hidden)
  
  for(i in 1 : hidden){

  set.seed(seed)
  #=================================================================================================
  tan.net <- neuralnet(Survived ~ Pclass + Sex + SibSp + 
                           Parch + Fare + Embarked, hidden = rep(3, i),
                         act.fct = "tanh", linear.output=FALSE, 
                       threshold = threshold,
                       lifesign = "full", stepmax = 10 ^ 7,
                           data = train[1:500,]) 
  
  train[1,-2]
  t.prediction <- compute(tan.net, train[501:891,-c(1,2,4,6,9,11)])$net.result
  t.result = rep(0,391)
  t.result[t.prediction >= 0.5] = 1
  #tresult
  t.truth = table(t.result, train[501:891, 2])
  t.accuracy.out[i] = (t.truth[1] + t.truth[4]) / sum(t.truth)
  #sum(tresult == train[501:891, 2]) / sum(ttruth)
  t.steps[i] = tan.net$result.matrix[3]
  t.error.in[i] = tan.net$result.matrix[1]
  #=================================================================================================





  set.seed(seed)
  #=================================================================================================
  sig.net <- neuralnet(Survived ~ Pclass + Sex + SibSp + 
                                  Parch + Fare + Embarked, hidden = rep(3, i),
                                act.fct = "logistic", linear.output=FALSE,
                                lifesign = "full", stepmax = 10 ^ 8,
                                threshold = threshold,
                                data = train[1:500,]) 
  
  train[1,-2]
  s.prediction <- compute(sig.net, train[501:891,-c(1,2,4,6,9,11)])$net.result
  s.result = rep(0,391)
  s.result[s.prediction >= 0.5] = 1
  #sresult
  s.truth = table(s.result, train[501:891, 2])
  s.accuracy.out[i] = (s.truth[1] + s.truth[4]) / sum(s.truth)
  #sum(sresult == train[501:891, 2]) / sum(struth)
  s.steps[i] = sig.net$result.matrix[3]
  s.error.in[i] = sig.net$result.matrix[1]
  #================================================================================================


  
  
  set.seed(seed)
  #================================================================================================
  source("neuralnet/R/neuralnetwork1.R")
  rec.net <- neuralnetwork1(Survived ~ Pclass + Sex + SibSp +                              
                              Parch + Fare + Embarked, hidden = rep(3, i),
                            act.fct = "logistic", linear.output=FALSE, 
                            threshold = threshold, stepmax = 10 ^ 8, lifesign = "full",
                            lifesign.step = 1000,
                            data = train[1:500,]) 
  
  r.prediction <- compute(rec.net, train[501:891,-c(1,2,4,6,9,11)])$net.result
  r.result = rep(0,391)
  r.result[r.prediction >= 0.5] = 1
  r.truth = table(r.result, train[501:891, 2])
  r.accuracy.out[i] = (r.truth[1] + r.truth[4]) / sum(r.truth)
  #sum(result == train[501:891, 2]) / sum(truth)
  r.steps[i] = rec.net$result.matrix[3]
  r.error.in[i] = rec.net$result.matrix[1]
  


  set.seed(seed)
  #================================================================================================
  source("neuralnet/R/neuralnetwork1.R")
  new.net <- neuralnetwork1(Survived ~ Pclass + Sex + SibSp +                              
                             Parch + Fare + Embarked, hidden = rep(3, i),
                           act.fct = "tanh", linear.output=FALSE, 
                           threshold = threshold, stepmax = 10 ^ 7, lifesign = "full",
                           lifesign.step = 1000,
                           data = train[1:500,]) 

  train[1,-2]
  n.prediction <- compute(new.net, train[501:891,-c(1,2,4,6,9,11)])$net.result
  n.result = rep(0,391)
  n.result[n.prediction >= 0.5] = 1
  n.truth = table(n.result, train[501:891, 2])
  n.accuracy.out[i] = (n.truth[1] + n.truth[4]) / sum(n.truth)
  #sum(result == train[501:891, 2]) / sum(truth)
  n.steps[i] = new.net$result.matrix[3]
  n.error.in[i] = new.net$result.matrix[1]
  #=================================================================================================
  
  }
  
  tan.results = list(c("Accuracy" = 100 * round(t.accuracy.out, 4), 
                       "Steps" = t.steps, 
                       "ErrorIn" = round(t.error.in, 4)))
  sig.results = list(c("Accuracy" = 100 * round(s.accuracy.out, 4), 
                       "Steps" = s.steps, 
                       "ErrorIn" = round(s.error.in, 4)))
  rec.results = list(c("Accuracy" = 100 * round(r.accuracy.out, 4), 
                       "Steps" = r.steps, 
                       "ErrorIn" = round(r.error.in, 4)))
  new.results = list(c("Accuracy" = 100 * round(n.accuracy.out, 4), 
                       "Steps" = round(n.steps), 
                       "ErrorIn" = round(n.error.in, 4)))
  
  return(list(c("tan" = tan.results,
                "sig" = sig.results,
                "rec" = rec.results,
                "new" = new.results
                )
              )
         )
}


seed = 5190
titanic <- makeTitanicReport(hidden = 4, seed = seed, threshold = 0.5)
titanic <- as.data.frame(titanic[[1]])

#sink("titanic.txt")
#titanic
#sink()
