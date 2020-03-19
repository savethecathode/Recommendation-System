# Purpose: The validation data should NOT be used for training your algorithm and should ONLY be used for evaluating the RMSE of your final algorithm. You should split the edx data into separate training and test sets to design and test your algorithm.

# Parent sets
# training: edx
# test set: validation

# Kid sets
# training: train
# test set: test

# Partition edx into test and training sets based on rating (outcomes).
test_indices <- createDataPartition(y=edx$rating, times=1, p=0.1, list=FALSE)
train <- edx[-test_indices,]
temp  <- edx[test_indices,]
# donnot include movies and users in the test set that do not appear in the training set
test  <- temp %>% 
  semi_join(train, by="movieId") %>%
  semi_join(train, by="userId")

# Checks:
# Inspect data sets
length(setdiff(edx$movieId, validation$movieId))
length(setdiff(edx$userId, validation$userId))
# OBS: X-many movies, and X-many users in edx set not present in validation set.

# CHECK: no movies or users in validatin set that are not present in the edx set.
length(setdiff(validation$movieId, edx$movieId))
length(setdiff(validation$userId, edx$userId))
# OBS: no movies or users in validatin set that are not present in the edx set.

length(setdiff(train$movieId, test$movieId))
length(setdiff(train$userId, test$userId))
# OBS: X-many movies, and X-many users in training set not present in test set.

# CHECK: no movies or users in the test set that are not in the training set.
length(setdiff(test$movieId, train$movieId))
length(setdiff(test$userId, train$userId))
# OBS: no movies or users in the test set that are not in the training set.
