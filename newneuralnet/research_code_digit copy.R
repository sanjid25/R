library(neuralnet)
library(compiler)
#newneuralnet <- cmpfun(neuralnetwork2)

######################
# Data preprocessing #
######################

# Assign the data set
digit <- read.csv("Digit/train.csv", header = TRUE)
# Have a look at dataset
str(digit)

# MAKE A SAMPLE
# create sample observation indices
m = 1000
sample_entries <- sample( 1 : nrow(digit), size = m, replace = FALSE )
# create the sample
digit.sample <- digit[sample_entries,] # m number of observations

# DIVIDE THE SAMPLE
# create train and test set indices
split.at <- round(0.7 * m)
m.train = 1 : split.at
m.test = (split.at + 1) : m

# create the training set
digit.train <- digit.sample[m.train,] # 70% of observations
# create the test set
digit.test <- digit.sample[m.test,] # 30% of observations

######################
# Formula for models #
######################

# creating formula for the model
covariates <- colnames(digit)[-1]
formula <- as.formula(paste("label ~ ", paste(covariates, collapse = "+")))

################
# New function #
################

makeNewReport <- function (m, hidden, threshold, fct = "logistic"){

#source("neuralnet/R/neuralnetwork.R")

new.duration = rep(0, hidden)
new.in.accuracy = rep(0, hidden)
new.out.accuracy = rep(0, hidden)
new.in.error = rep(0, hidden)
new.steps = rep(0, hidden)

  for(i in 1:hidden){
    new.duration[i] <- proc.time()[3]
    new.net <- neuralnetwork1(formula, hidden = rep(100, i), lifesign = "full", lifesign.step = 1000,
                     act.fct = fct, threshold = threshold, stepmax = 2000000000, 
                     # algorithm = "backprop", learningrate = 0.1,
                     linear.output = TRUE,
                     data = digit.train)
    new.duration[i] <- proc.time()[3] - new.duration[i]

    # in-sample accuracy
    new.train.prediction <- compute(new.net, digit.train[, -1])$net.result
    new.train.prediction = round(new.train.prediction)
    new.in.accuracy[i] = sum(new.train.prediction == digit.train[, 1]) / nrow(digit.train)

    # out-of-sample accuracy
    new.test.prediction <- compute(new.net, digit.test[, -1])$net.result
    new.test.prediction = round(new.test.prediction)
    new.out.accuracy[i] = sum(new.test.prediction == digit.test[, 1]) / nrow(digit.test)
    new.in.error[i] = new.net$result.matrix[1]
    new.steps[i] = new.net$result.matrix[3]
  }
  
  return(list("steps" = new.steps,
              "time" = new.duration, 
              "in.accuracy" = 100 * round(new.in.accuracy, 4),
              # "in.error" = new.in.error,
              "out.accuracy" = 100 * round(new.out.accuracy, 4))
         )
}

################################
# Hyperbolic tangent & Sigmoid #
################################

makeReport <- function (m, hidden, fct = "logistic"){
  
  old.duration = rep(0, hidden)
  old.in.accuracy = rep(0, hidden)
  old.out.accuracy = rep(0, hidden)
  old.in.error = rep(0, hidden)
  old.steps = rep(0, hidden)
  
  for(i in 1:hidden){
    old.duration[i] <- proc.time()[3]
    old.net <- neuralnet(formula, hidden = rep(2, i), lifesign = "full", lifesign.step = 1000,
                            act.fct = fct, linear.output = FALSE, threshold = 0.00001,
                         stepmax = 2000000000, data = digit.train)
    old.duration[i] <- proc.time()[3] - old.duration[i]
    
    # in-sample accuracy
    old.train.prediction <- compute(old.net, digit.train[, -1])$net.result
    old.train.prediction = round(old.train.prediction)
    old.in.accuracy[i] = sum(old.train.prediction == digit.train[, 1]) / nrow(digit.train)
    
    # out-of-sample accuracy
    otest.prediction <- compute(old.net, digit.test[, -1])$net.result
    otest.prediction = round(otest.prediction)
    old.out.accuracy[i] = sum(otest.prediction == digit.test[, 1]) / nrow(digit.test)
    old.in.error[i] = old.net$result.matrix[1]
    old.steps[i] = old.net$result.matrix[3]
    
    
  }
  
  return(list("steps" = old.steps,
            "time" = old.duration, 
            "in.accuracy" = 100 * round(old.in.accuracy, 4),
            #"in.error" = old.in.error,
            "out.accuracy" = 100 * round(old.out.accuracy, 4)
            ))
}

hidden = 5
#steps = 20000
 
library(parallel)

source("neuralnet/R/neuralnetwork1.R")

i = 24
set.seed(i)
lin <- makeNewReport(m = m, hidden = hidden, threshold = 0.001, "logistic")

set.seed(i)
lec <- makeNewReport(m = m, hidden = hidden, threshold = 0.001, "tanh")

set.seed(i)
sig <- makeOldReport(m = m, hidden = hidden, "logistic")

set.seed(i)
tan <- makeOldReport(m = m, hidden = hidden, "tanh")

t.table = rbind("sig" = t(as.data.frame(sig)), 
                    "tan" = t(as.data.frame(tan)), 
                    "lec" = t(as.data.frame(lin)), 
                    "lin" = t(as.data.frame(lec)))

#sink("digit_final.txt")
#final.table
#sink()
