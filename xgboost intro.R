######### XGBoost Tutorial ##########

#https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html



# set up ------------------------------------------------------------------

require(xgboost)
data("agaricus.train", package = 'xgboost')
data("agaricus.test", package = 'xgboost')

train <- agaricus.train
test <- agaricus.test


# eda ---------------------------------------------------------------------

str(train)
      #List of 2
      #$ data :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
      #.. ..@ i       : int [1:143286] 2 6 8 11 18 20 21 24 28 32 ...
      #.. ..@ p       : int [1:127] 0 369 372 3306 5845 6489 6513 8380 8384 10991 ...
      #.. ..@ Dim     : int [1:2] 6513 126
      #.. ..@ Dimnames:List of 2
      #.. .. ..$ : NULL
      #.. .. ..$ : chr [1:126] "cap-shape=bell" "cap-shape=conical" "cap-shape=convex" "cap-shape=flat" ...
      #.. ..@ x       : num [1:143286] 1 1 1 1 1 1 1 1 1 1 ...
      #.. ..@ factors : list()
      #$ label: num [1:6513] 1 0 0 1 0 0 0 1 0 0 ...

dim(train$data)
  #[1] 6513  126
dim(test$data)
  #[1] 1611  126


# training ----------------------------------------------------------------

#train decision tree w/ sparse matrix
bstSparse <- xgboost(train$data, label = train$label, nrounds = 2, max_depth = 2,  eta = 1,  nthreads = 2, objective = "binary:logistic")

#train decision tree w/ dense matrix
bstDense <- xgboost(as.matrix(train$data), label = train$label, nrounds = 2, max_depth = 2,  eta = 1,  nthreads = 2, objective = "binary:logistic" )

#another method
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(dtrain, nrounds = 2, max_depth = 2,  eta = 1,  nthreads = 2, objective = "binary:logistic" )


#verbose option

bst <- xgboost(data = dtrain, max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)

# testing -----------------------------------------------------------------

pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))

# limit display of predictions to the first 10
print(head(pred))
# [1] 0.28583017 0.92392391 0.28583017 0.28583017 0.05169873 0.92392391

#transform predictions to classification
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

# measure model performance -----------------------------------------------

#compute average error
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))
#[1] "test-error= 0.0217256362507759"

# !!! The most important thing to remember is that to do a classification, you just do a regression to the label and then apply a threshold.

#view feature importance/influence from the learnt model
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

#plot tree
xgb.plot.tree(model = bst)
