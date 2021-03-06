## Support Vector Machines {-}


```r
#
# ## required packages
# library(e1071)
#
# ## Training and Testing Data
# hof.train = read.csv("data/HOF_tr.csv");
# hof.test = read.csv("data/HOF_te.csv")
#
# hof = rbind(hof.train, hof.test)
#
# ## create a training and testing set by randomly sampling from all of the data
# ## using the same set as in the random forest example
# set.seed(1002)
# x = sample(nrow(hof), replace = FALSE)
#
# ## remove unwanted columns
# hof = hof[, -c(2:4)]
#
# ## lets train the model on about 90% of the data
# train = hof[x[1:900], ]
# test = hof[-x[1:900], ]
#
# head(train)
# summary(train)
#
# ## train the model
# (tuneModel = tune(svm, HOF ~ ., data = train,
#                   ranges = list(
#                     cost = seq(.1, 5, .25),
#                     gamma = seq(0, .5, .01))))
#
# ## darker colors are better models
# plot(tuneModel, main = "Model Performance")
#
# ## store best parameters
# cst = as.numeric(tuneModel$best.parameters[1])
# gma = as.numeric(tuneModel$best.parameters[2])
#
# ## do an indepth search of the darker grid
# (tuneModel = tune(svm, HOF ~ ., data = train,
#                   ranges = list(
#                     cost = seq(cst-1, cst+1, .01),
#                     gamma = seq(0, gma+.1, .001))))
#
# ## darker colors are better models
# plot(tuneModel, main = "Model Performance")
#
# ## store best parameters
# cst = as.numeric(tuneModel$best.parameters[1])
# gma = as.numeric(tuneModel$best.parameters[2])
#
# ## build model based on the tuned parameters
# mdl = svm(HOF ~ ., data = train, probability = TRUE, cost = cst, gamma = gma)
# x = predict(mdl, test, probability = TRUE)
#
# ## compile results
# results = data.frame(
#   Prediction = x,
#   Actual = test$HOF,
#   Prob.N = attr(x, "probabilities")[, 1],
#   Prob.Y = attr(x, "probabilities")[, 2]
# )
#
# ## accuracy calculation from the random forest example
# metric = function(confusion) {
#   sensitivity = confusion[4] / (confusion[2] + confusion[4])
#   specificity = confusion[1] / (confusion[1] + confusion[3])
#   score = (sensitivity + (3 * specificity)) / 4
#   return(score)
# }
#
# ## confusion matrix and accuracy score
# (confusion = table(Prediction = results$Prediction, Actual = results$Actual))
#
# ## accuracy score for training set
# metric(confusion)
#
# ## look at the probability prediction for all of CV results to see if we should lower the
# ## probability threshold for predicing Y to HOF
# subset(results, Prediction != Actual)
# min = min(subset(results, Prediction != Actual, "Prob.Y"))
#
# ## lower the threshold
# results$Prediction.new = "N"
# results$Prediction.new[results$Prob.Y >= min] = "Y"
#
# ## confusion matrix and accuracy score
# (confusion = table(Prediction = results$Prediction.new, Actual = results$Actual))
#
# ## accuracy score for training set
# metric(confusion)
```
