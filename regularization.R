# regularization of movie rating by movieId, userId, date (rounded by ?), genres, no. genres, debut (year)

mu <- mean(train_set$rating)
lambdas <- seq(5, 6, 0.1)
start_time <- Sys.time()
rmses <- sapply(lambdas, function(lambda){
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
  
  b_date <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(date_week) %>%
    summarize(b_date = sum(rating - mu - b_i - b_u)/(n()+lambda))
  
  b_gen <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_date, by="date_week") %>%
    group_by(genres) %>%
    summarize(b_gen = sum(rating - mu - b_i - b_u - b_date)/(n()+lambda))
  
  b_deb <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_date, by="date_week") %>%
    left_join(b_gen, by="genres") %>%
    group_by(debut) %>%
    summarize(b_deb = sum(rating - mu - b_i - b_u - b_date - b_gen)/(n()+lambda))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_date, by="date_week") %>%
    left_join(b_gen, by="genres") %>%
    left_join(b_deb, by="debut") %>%
    mutate(pred = mu + b_i + b_u + b_date + b_gen + b_deb) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})
end_time <- Sys.time()
end_time - start_time   # = 

min(rmses)  # = 0.864125177
lambda <- lambdas[which.min(rmses)]  # = (edx: 5.5)

plot(lambdas, rmses)
points(lambda, min(rmses), col="red")