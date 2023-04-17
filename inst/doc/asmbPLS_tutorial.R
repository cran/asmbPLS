## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(asmbPLS) ## load the R package
set.seed(123) ## set seed to generate the same results

## ---- cache=TRUE--------------------------------------------------------------
data(asmbPLS.example) ## load the example data set for asmbPLS

## ---- cache=TRUE--------------------------------------------------------------
## show the first 5 microbial taxa and the first 5 metabolites for the first 5 samples. 
asmbPLS.example$X.matrix[1:5, c(1:5, 201:205)] 
## show the outcome for the first 5 samples.
asmbPLS.example$Y.matrix[1:5,] 

## ---- cache=TRUE--------------------------------------------------------------
X.matrix = asmbPLS.example$X.matrix
X.matrix.new = asmbPLS.example$X.matrix.new
Y.matrix = asmbPLS.example$Y.matrix
PLS.comp = asmbPLS.example$PLS.comp
X.dim = asmbPLS.example$X.dim
quantile.comb.table.cv = asmbPLS.example$quantile.comb.table.cv
Y.indicator = asmbPLS.example$Y.indicator

## cv to find the best quantile combinations for model fitting
cv.results <- asmbPLS.cv(X.matrix = X.matrix, 
                         Y.matrix = Y.matrix, 
                         PLS.comp = PLS.comp, 
                         X.dim = X.dim, 
                         quantile.comb.table = quantile.comb.table.cv, 
                         Y.indicator = Y.indicator,
                         k = 5,
                         ncv = 5)
## obtain the best quantile combination for each PLS component
quantile.comb <- cv.results$quantile_table_CV[,1:length(X.dim)] 
## obtain the optimal number of PLS components
n.PLS <- cv.results$optimal_nPLS 

## ---- cache=TRUE--------------------------------------------------------------
asmbPLS.results <- asmbPLS.fit(X.matrix = X.matrix, 
                               Y.matrix = Y.matrix, 
                               PLS.comp = n.PLS, 
                               X.dim = X.dim, 
                               quantile.comb = quantile.comb)

## ---- cache=TRUE--------------------------------------------------------------
Y.pred <- asmbPLS.predict(asmbPLS.results, X.matrix.new, n.PLS)
head(Y.pred$Y_pred)

## ---- cache=TRUE--------------------------------------------------------------
## prediction for original data to check the data fit
Y.fit <- asmbPLS.predict(asmbPLS.results, X.matrix, n.PLS)
check.fit <- cbind(Y.matrix, Y.fit$Y_pred) 
head(check.fit)

## ---- cache=TRUE--------------------------------------------------------------
data(asmbPLSDA.example) ## load the example data set for asmbPLS-DA

## ---- cache=TRUE--------------------------------------------------------------
## show the first 5 features from block 1 and the first 5 features from block 2 for the first 5 samples.
asmbPLSDA.example$X.matrix[1:5, c(1:5, 201:205)]  
## show the binary outcome for the first 5 samples.
asmbPLSDA.example$Y.matrix.binary[1:5,] 
## show the multiclass outcome for the first 5 samples.
asmbPLSDA.example$Y.matrix.morethan2levels[1:5,] 

## ---- cache=TRUE--------------------------------------------------------------
X.matrix = asmbPLSDA.example$X.matrix
X.matrix.new = asmbPLSDA.example$X.matrix.new
Y.matrix.binary = asmbPLSDA.example$Y.matrix.binary
Y.matrix.multiclass = asmbPLSDA.example$Y.matrix.morethan2levels
X.dim = asmbPLSDA.example$X.dim
PLS.comp = asmbPLSDA.example$PLS.comp
quantile.comb.table.cv = asmbPLSDA.example$quantile.comb.table.cv

## ---- cache=TRUE--------------------------------------------------------------
## cv to find the best quantile combinations for model fitting (binary outcome)
cv.results.binary <- asmbPLSDA.cv(X.matrix = X.matrix, 
                                  Y.matrix = Y.matrix.binary, 
                                  PLS.comp = PLS.comp, 
                                  X.dim = X.dim, 
                                  quantile.comb.table = quantile.comb.table.cv, 
                                  outcome.type = "binary",
                                  k = 5,
                                  ncv = 5)
quantile.comb.binary <- cv.results.binary$quantile_table_CV[,1:length(X.dim)]
n.PLS.binary <- cv.results.binary$optimal_nPLS

## ---- cache=TRUE--------------------------------------------------------------
## cv to find the best quantile combinations for model fitting 
## (categorical outcome with more than 2 levels)
cv.results.multiclass <- asmbPLSDA.cv(X.matrix = X.matrix, 
                                      Y.matrix = Y.matrix.multiclass, 
                                      PLS.comp = PLS.comp, 
                                      X.dim = X.dim, 
                                      quantile.comb.table = quantile.comb.table.cv, 
                                      outcome.type = "multiclass",
                                      k = 5,
                                      ncv = 5)
quantile.comb.multiclass <- cv.results.multiclass$quantile_table_CV[,1:length(X.dim)]
n.PLS.multiclass <- cv.results.multiclass$optimal_nPLS

## ---- cache=TRUE--------------------------------------------------------------
## asmbPLSDA fit using the selected quantile combination (binary outcome)
asmbPLSDA.fit.binary <- asmbPLSDA.fit(X.matrix = X.matrix, 
                                      Y.matrix = Y.matrix.binary, 
                                      PLS.comp = n.PLS.binary, 
                                      X.dim = X.dim, 
                                      quantile.comb = quantile.comb.binary,
                                      outcome.type = "binary")

## ---- cache=TRUE--------------------------------------------------------------
## asmbPLSDA fit (categorical outcome with more than 2 levels)
asmbPLSDA.fit.multiclass <- asmbPLSDA.fit(X.matrix = X.matrix, 
                                          Y.matrix = Y.matrix.multiclass, 
                                          PLS.comp = n.PLS.multiclass, 
                                          X.dim = X.dim, 
                                          quantile.comb = quantile.comb.multiclass,
                                          outcome.type = "multiclass")

## ---- cache=TRUE--------------------------------------------------------------
## classification for the new data based on the asmbPLS-DA model with the binary outcome.
Y.pred.binary <- asmbPLSDA.predict(asmbPLSDA.fit.binary, 
                                   X.matrix.new, 
                                   PLS.comp = n.PLS.binary)
## classification for the new data based on the asmbPLS-DA model with the multiclass outcome.
Y.pred.multiclass <- asmbPLSDA.predict(asmbPLSDA.fit.multiclass,
                                       X.matrix.new, 
                                       PLS.comp = n.PLS.multiclass)

## ---- cache=TRUE--------------------------------------------------------------
cv.results.cutoff <- cv.results.binary
quantile.comb.cutoff <- cv.results.cutoff$quantile_table_CV
## Cross validation using Euclidean distance of X super score
cv.results.EDX <- asmbPLSDA.cv(X.matrix = X.matrix, 
                               Y.matrix = Y.matrix.binary,
                               PLS.comp = PLS.comp, 
                               X.dim = X.dim, 
                               quantile.comb.table = quantile.comb.table.cv, 
                               outcome.type = "binary", 
                               method = "Euclidean_distance_X",
                               k = 5,
                               ncv = 5)
quantile.comb.EDX <- cv.results.EDX$quantile_table_CV

## Cross validation using Mahalanobis distance of X super score
cv.results.MDX <- asmbPLSDA.cv(X.matrix = X.matrix, 
                                  Y.matrix = Y.matrix.binary,
                                  PLS.comp = PLS.comp, 
                                  X.dim = X.dim, 
                                  quantile.comb.table = quantile.comb.table.cv, 
                                  outcome.type = "binary", 
                                  method = "Mahalanobis_distance_X",
                                  k = 5,
                                  ncv = 5)
quantile.comb.MDX <- cv.results.MDX$quantile_table_CV

## ---- cache=TRUE--------------------------------------------------------------
#### vote list ####
cv.results.list = list(fixed_cutoff = quantile.comb.cutoff,
                       Euclidean_distance_X = quantile.comb.EDX,
                       Mahalanobis_distance_X = quantile.comb.MDX)

## ---- cache=TRUE--------------------------------------------------------------
vote.fit <- asmbPLSDA.vote.fit(X.matrix = X.matrix, 
                               Y.matrix = Y.matrix.binary, 
                               X.dim = X.dim, 
                               nPLS = c(cv.results.cutoff$optimal_nPLS, 
                               cv.results.EDX$optimal_nPLS, 
                               cv.results.MDX$optimal_nPLS),
                               cv.results.list = cv.results.list, 
                               outcome.type = "binary",
                               method = "weighted")

## ---- cache=TRUE--------------------------------------------------------------
## classification
vote.predict <- asmbPLSDA.vote.predict(vote.fit, X.matrix.new)
head(vote.predict)

## ---- cache=TRUE--------------------------------------------------------------
## custom block.name and group.name
plotCor(asmbPLSDA.fit.binary, 
        ncomp = 1, 
        block.name = c("mRNA", "protein"), 
        group.name = c("control", "case"))

## ---- cache=TRUE--------------------------------------------------------------
## custom block.name and group.name
plotPLS(asmbPLSDA.fit.binary, 
        comp.X = 1, 
        comp.Y = 2, 
        group.name = c("control", "case"))

## -----------------------------------------------------------------------------
plotRelevance(asmbPLSDA.fit.binary, 
              n.top = 5,
              block.name = c("mRNA", "protein"))

