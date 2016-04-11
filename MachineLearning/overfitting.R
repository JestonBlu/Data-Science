dta = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 
        3, 2, 7, 9, 5, 0, 2, 8, 8, 4, 1, 9, 7, 1, 6, 9, 3, 9, 9, 3, 7, 5, 1, 0, 5, 8, 2, 
        0, 9, 7, 4, 9, 4, 4, 5, 9, 2, 3, 0, 7, 8, 1, 6, 4, 0, 6, 2, 8, 6, 2, 0, 8, 9, 9, 
        8, 6, 2, 8, 0, 3, 4, 8, 2, 5, 3, 4, 2, 1, 1, 7, 0, 6, 7, 9, 8, 2, 1, 4, 8, 0, 8, 
        6, 5, 1, 3, 2, 8, 2, 3, 0, 6, 6, 4, 7, 0, 9, 3, 8, 4, 4, 6, 0, 9, 5, 5, 0, 5, 8, 
        2, 2, 3, 1, 7, 2, 5, 3, 5, 9, 4, 0, 8, 1, 2, 8, 4, 8, 1, 1, 1, 7, 4, 5, 0, 2, 8, 
        4, 1, 0, 2, 7, 0, 1, 9, 3, 8, 5, 2, 1, 1, 0, 5, 5, 5, 9, 6, 4, 4, 6, 2, 2, 9, 4, 
        8, 9, 5, 4, 9, 3, 0, 3, 8, 1, 9, 6, 4, 4, 2, 8, 8, 1, 0, 9, 7, 5, 6, 6, 5, 9, 3, 
        3, 4, 4, 6, 1, 2, 8, 4, 7, 5, 6, 4, 8, 2, 3, 3, 7, 8, 6, 7, 8, 3, 1, 6, 5, 2, 7, 
        1, 2, 0, 1, 9, 0, 9, 1, 4, 5, 6, 4, 8, 5, 6, 6, 9, 2, 3, 4, 6, 0, 3, 4, 8, 6, 1, 
        0, 4, 5, 4, 3, 2, 6, 6, 4, 8, 2, 1, 3, 3, 9, 3, 6, 0, 7, 2, 6, 0, 2, 4, 9, 1, 4, 
        1, 2, 7, 3, 7, 2, 4, 5, 8, 7, 0, 0, 6, 6, 0, 6, 3, 1, 5, 5, 8, 8, 1, 7, 4, 8, 8, 
        1, 5, 2, 0, 9, 2, 0, 9, 6, 2, 8, 2, 9, 2, 5, 4, 0, 9, 1, 7, 1, 5, 3, 6, 4, 3, 6, 
        7, 8, 9, 2, 5, 9, 0, 3, 6, 0, 0, 1, 1, 3, 3, 0, 5, 3, 0, 5, 4, 8, 8, 2, 0, 4, 6, 
        6, 5, 2, 1, 3, 8, 4, 1, 4, 6, 9, 5, 1, 9, 4, 1, 5, 1, 1, 6, 0, 9, 4, 3, 3, 0, 5, 
        7, 2, 7, 0, 3, 6, 5, 7, 5, 9, 5, 9, 1, 9, 5, 3, 0, 9, 2, 1, 8, 6, 1, 1, 7, 3, 8, 
        1, 9, 3, 2, 6, 1, 1, 7, 9, 3, 1, 0, 5, 1, 1, 8, 5, 4, 8, 0, 7, 4, 4, 6, 2, 3, 7, 
        9, 9, 6, 2, 7, 4, 9, 5, 6, 7, 3, 5, 1, 8, 8, 5, 7, 5, 2, 7, 2, 4, 8, 9, 1, 2, 2, 
        7, 9, 3, 8, 1, 8, 3, 0, 1, 1, 9, 4, 9, 1, 2)

dta = data.frame(y = dta)

dta$x1 = 0; dta$x2 = 0; dta$x3 = 0; dta$x4 = 0; dta$x5 = 0

for (i in 2:500) {
  dta$x1[i] = dta$y[i-1]
}

for (i in 3:500) {
  dta$x2[i] = dta$y[i-2]
}

for (i in 4:500) {
  dta$x3[i] = dta$y[i-3]
}

for (i in 5:500) {
  dta$x4[i] = dta$y[i-4]
}

for (i in 6:500) {
  dta$x5[i] = dta$y[i-5]
}

dta[] = lapply(dta, factor)


train = dta[1:400, ]
test = dta[401:500, ]



library(e1071)
mdl.svm = tune(svm, y ~ ., data = train, 
               ranges = list(
                 cost = seq(.1, 1, .1), 
                 gamma = seq(0, 1, .1))
)

mdl.svm = svm(y ~ ., data = train, cost = 1, gamma = .2, probability = TRUE)

tmp = predict(mdl.svm, test, probability = TRUE)

results.svm = data.frame(actual = test$y, predicted = tmp)
results.svm$Result = FALSE
results.svm$Result[results.svm$actual == results.svm$predicted] = TRUE



library(nnet)
mdl.nn = nnet(y ~ ., data = train, size = 5)

tmp = predict(mdl.nn, test, type = "class")
results.nn = data.frame(actual = as.numeric(test$y), predicted = as.numeric(tmp))
results.nn$Result = FALSE
results.nn$Result[results.nn$actual == results.nn$predicted] = TRUE



library(randomForest)
mdl.rf = randomForest(y ~ ., ntree = 250, data = train)

tmp = predict(mdl.rf,)
results.rf = data.frame(actual = as.numeric(test$y), predicted = as.numeric(tmp))
results.rf$Result = FALSE
results.rf$Result[results.rf$actual == results.rf$predicted] = TRUE

## create results for training set
train.results.svm = data.frame(actual = train$y, pred = predict(mdl.svm, train))
train.results.svm$Result = FALSE
train.results.svm$Result[train.results.svm$actual == train.results.svm$pred] = TRUE

train.results.nn = data.frame(actual = train$y, pred = predict(mdl.nn, train, type = "class"))
train.results.nn$Result = FALSE
train.results.nn$Result[train.results.nn$actual == train.results.nn$pred] = TRUE

train.results.rf = data.frame(actual = train$y, pred = mdl.rf$predicted)
train.results.rf$Result = FALSE
train.results.rf$Result[train.results.rf$actual == train.results.rf$pred] = TRUE





results = list(
  SVM.training = table(train.results.svm$Result)/400,
  SVM.testing = table(results.svm$Result)/100,
  NN.training = table(train.results.nn$Result)/400,
  NN.testing = table(results.nn$Result)/100,
  RF.training = table(train.results.rf$Result)/400,
  RF.testing = table(results.rf$Result)/100
)


