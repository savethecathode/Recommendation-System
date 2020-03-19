# Analysis: Matrix Factorization
# things to consider: discrete outcomes, "regularization", SVD
# When evaluating models must take into consideration that outcomes are discrete (no. stars)


###***NEED TO EXPERIMENT WITH: library(plyr), round_any(data, 0.5) # to round to nearest 0.5***************************


# Define loss function: residual mean squared error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# BULK ANALYSIS
# Simplest model
mu <- mean(train_set$rating)
prediction <- rep(mu, nrow(test_set))
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- tibble(method="Simplest Model", RMSE=model_results)

# Estimate bias using least squares: The movie effect
# WARNING: SLOW! estimate bias for each movie
###fit <- lm(rating ~ as.factor(userId), data=movielens)
# THINK: bias is effectively: < Y_ui > - total_ave
movie_eff <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
prediction <- mu + test_set %>%
  left_join(movie_eff, by='movieId') %>%
  .$b_i
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                          tibble(method="Movie Effect",
                                 RMSE=model_results))

# Regularization of Movie Effect and the User Effect
lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(lambda){
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

prediction <- test_set %>%
  left_join(b_i, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Regularization of Movie Effects",
                                  RMSE=model_results))

# The user-effect
user_eff <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))
prediction <- mu + test_set %>%
  left_join(user_eff, by='userId') %>%
  .$b_u
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="User Effect",
                                  RMSE=model_results))

# Combining movie-effect and user effect
# WARNING: CRASHES!
###fit <- lm(rating ~ as.factor(movieId) + as.factor(userId))
prediction <- test_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff,  by='userId')  %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Movie+User Effects",
                                  RMSE=model_results))

# User effect: only include users w/ > 100 ratings
# Obs: no impart on RMSE
user_eff2 <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu)) %>%
  filter(n()>=100)
prediction <- mu + test_set %>%
  left_join(user_eff2, by='userId') %>%
  .$b_u
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="User Effect 2",
                                  RMSE=model_results))

# Computing User Effect using Mean and Movie Effect as the baseline
# Obs: improves accuracy compared to user effect computed in absernce of the movie effect included in the baseline prediction
# Note: establish case for using an updated baseline prediction.
user_eff3 <- train_set %>%
  left_join(movie_eff, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
prediction <- mu + test_set %>%
  left_join(user_eff3, by='userId') %>%
  .$b_u
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="User Effect 3",
                                  RMSE=model_results))

# Combine Move Effect and User Effect 3
# Obs: improved accuracy using modified User Effect
prediction <- test_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff3,  by='userId')  %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Movie+User Effects 2",
                                  RMSE=model_results))

# Regularization of Movie Effect and the User Effect
lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(lambda){
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

prediction <- test_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Regularization of Movie+User Effects",
                                  RMSE=model_results))

# Number of Genres Effect
nog_effect <- train_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff3, by='userId') %>%
  group_by(no_genres) %>%
  summarize(b_nog = mean(rating - mu - b_i - b_u))
prediction <- test_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff3, by='userId') %>%
  left_join(nog_effect, by='no_genres') %>%
  mutate(pred = mu + b_i + b_u + b_nog) %>%
  .$pred
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="NoG Effect",
                                  RMSE=model_results))

# Genres (set)
genres_effect <- train_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff3, by='userId') %>%
  group_by(genres) %>%
  summarize(b_gs = mean(rating - mu - b_i - b_u))
prediction <- test_set %>%
  left_join(movie_eff, by='movieId') %>%
  left_join(user_eff3, by='userId') %>%
  left_join(genres_effect, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_gs) %>%
  .$pred
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Genres Effect",
                                  RMSE=model_results))

# Genre (single)
#genre_effect <- train_set %>%
#  left_join(movie_eff, by='movieId') %>%
#  left_join(user_eff3, by='userId') %>%
#  group_by(genre) %>%
#  summarize(b_g = mean(rating - mu - b_i - b_u))
#prediction <- test_set %>%
#  left_join(movie_eff, by='movieId') %>%
#  left_join(user_eff3, by='userId') %>%
#  left_join(genre_effect, by='genre') %>%
#  mutate(pred = mu + b_i + b_u + b_g) %>%
#  .$pred
#model_results <- RMSE(prediction, test_set$rating)
#rmse_results  <- bind_rows(rmse_results,
#                           tibble(method="Genre Effect",
#                                  RMSE=model_results))



# To know the movie and user effects wrt year, month, and day, get the averages for each 
# Ave. Rating by Year
# "Effects" are deviations from the avg rating.
# NOW use avg rating for a year, month, day AS THE model, instead of the bulk avg, and instead of calculating effects.
year_avgs <- train_set %>%
  group_by(year) %>%
  summarize(mu_year = mean(rating))
prediction <- test_set %>%
  left_join(year_avgs, by='year') %>%
  .$mu_year
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Ave. by Year",
                                  RMSE=model_results))

# Movie Effect by Year (if you must know)
round2 = function(x, n){
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
temp_set <- train_set %>%
  left_join(year_avgs, by='year')
movie_year_avgs <- train_set %>%
  group_by(movieId, year) %>%
  summarize(movie_year_avgs = mean(rating))
temp_set <- temp_set %>%
  left_join(movie_year_avgs, by=c('movieId', 'year')) %>% # join according to movieId and year!!!
  mutate(movie_year_eff = mu_year - movie_year_avgs)
prediction <- temp_set %>%
  mutate(pred = mu_year - movie_year_eff) %>%
  pull(pred)
#model_results <- RMSE(prediction, test_set$rating)
#rmse_results  <- bind_rows(rmse_results,
#                           tibble(method="Movie-Year Effect",
#                                  RMSE=model_results))
# Note: estimates take on multiple decimals but only multiples of 0.5 are possible.





# predict()

# NEXT: regularization and overfitting

# NEXT: Single Value Decomposition (SVD)



# NEXT: principal component analysis (PCA)

# 10 points: 0.86550 <= RMSE <= 0.89999
# 15 points: 0.86500 <= RMSE <= 0.86549
# 20 points: 0.86490 <= RMSE <= 0.86499
# 25 points: RMSE < 0.86490
options(digits=10)
rmse_results

rmse_results %>% knitr::kable()
# SIDE: independent project, predict helix v sheet from aa sequence using AMPdatabase

# Append results to .RData file for access in Rmarkdown
library(cgwtools)
resave(rmse_results, file="movielensdata.RData")
