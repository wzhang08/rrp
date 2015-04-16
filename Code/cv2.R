source('~/Desktop/Kaggle/RestaurantRevenuePrediction/Code/PrepareData.R')
library(gbm)
set.seed(0)
# LOOCV
n_pca <- seq(from = 3, to = 10, by = 1)
#n_shrink <- c(0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05)
# machine 1
n_shrink <- c(0.00001, 0.00005, 0.0001)
# machine 2
#n_shrink <- c(0.0005, 0.001, 0.005)
# machine 3
#n_shrink <- c(0.01, 0.05)

n_depth <- seq(from = 1, to = 5, by = 1)
n_modify <- c('a', 'b') # how to modify prediction
n_dist <- c('gaussian', 'laplace', 'tdist')

par_mat <- expand.grid(n_pca, n_shrink, n_depth, n_modify, n_dist)
names(par_mat) <- c('pca', 'shrink', 'depth', 'modify', 'dist')

err_vec <- rep(NA, nrow(par_mat))

for (i in 1:length(err_vec)) {
  print(i)
  data_use <- train[, c(1, 3:(4 + par_mat$pca[i]), ncol(train))]
  err_loocv <- rep(NA, nrow(data_use))
  for (j in 1:length(err_loocv)) {
    cv_test <- data_use[j, -ncol(data_use)]
    cv_train <- data_use[-j, ]
    cv_fit <- gbm(formula = log(revenue) ~., data = cv_train, distribution = as.character(par_mat$dist[i]), n.trees = 10000
                  , interaction.depth = par_mat$depth[i], shrinkage = par_mat$shrink[i], cv.folds = 10, n.cores = 8) 
    cv_pred <- predict(object = cv_fit, newdata = cv_test)
    cv_resi <- log(cv_train$revenue) - cv_fit$fit
    if (par_mat$modify[i] == 'a') {
      # Modified using log normal of error term
      cv_pred <- exp(cv_pred + var(cv_resi) / 2)
    }
    else if (par_mat$modify[i] == 'b') {
      # Modified using sample mean of error term
      cv_pred <- exp(cv_pred) * mean(exp(cv_resi))
    }
    err_loocv[j] <- abs(cv_pred - data_use$revenue[j])
  }
  err_vec[i] <- mean(err_loocv)
}
