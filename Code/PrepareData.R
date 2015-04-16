train <- read.csv("~/Desktop/Kaggle/RestaurantRevenuePrediction/Data/train.csv", stringsAsFactors=TRUE)
test <- read.csv("~/Desktop/Kaggle/RestaurantRevenuePrediction/Data/test.csv", stringsAsFactors=TRUE)

# Shuffle training data
train <- train[sample(x = 1:137, size = 137, replace = F), ]

train$Id <- NULL
test$Id <- NULL
train$Open.Date <- as.numeric(as.Date(train$Open.Date, format = '%m/%d/%Y'))
train$Type[train$Type == 'DT'] <- NA
test$Open.Date <- as.numeric(as.Date(test$Open.Date, format = '%m/%d/%Y'))


# PCA on P1 ~ P37
pca_x <- prcomp(rbind(train[, 5:41], test[, 5:41]), scale = T)$x
plot(1 - apply(X = pca_x, MARGIN = 2, FUN = var) / ncol(pca_x), type = 'l')

# Replace P1 ~ P37 with PCA projections
train[, c(5:41)] <- pca_x[1:137, ]
test[, c(5:41)] <- pca_x[-(1:137), ]
all_x <- rbind(train[, 1:41], test)

