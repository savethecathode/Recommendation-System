# Analysis: Matrix Factorization
# things to consider: discrete outcomes, "regularization", SVD
# When evaluating models must take into consideration that outcomes are discrete (no. stars)

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

# The year effect
# WARNING!!!
# lm(rating ~ as.factor(timestamp), data=training_set)
year_eff <- train_set %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu))
prediction <- mu + test_set %>%
  left_join(year_eff, by='year') %>%
  .$b_y
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Year Effect",
                                  RMSE=model_results))

# The month effect
month_eff <- train_set %>%
  group_by(month) %>%
  summarize(b_m = mean(rating - mu))
prediction <- mu + test_set %>%
  left_join(month_eff, by='month') %>%
  .$b_m
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Month Effect",
                                  RMSE=model_results))

# The day-of-the-week effect
day_eff <- train_set %>%
  group_by(day) %>%
  summarize(b_d = mean(rating - mu))
prediction <- mu + test_set %>%
  left_join(day_eff, by='day') %>%
  .$b_d
model_results <- RMSE(prediction, test_set$rating)
rmse_results  <- bind_rows(rmse_results,
                           tibble(method="Day Effect",
                                  RMSE=model_results))

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

# Number to beat: 0.86490
rmse_results

#rmse_results %>% knitr::kable()
# SIDE: independent project, predict helix v sheet from aa sequence using AMPdatabase
