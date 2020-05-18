# Move to working directory
#setwd("~/TemplRprojects/MovieLens")


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Use the lubridate package to extract date-time info from timestamp data.
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")


# MovieLens 10M dataset: (in tidy format)
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

#set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx  <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
# semi_join() - keeps elements of the 1st table for which there is data in the 2nd table, and does not add columns of the 2nd.
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
# anti_join() - keeps elements in 1st table for which there is no information in the 2nd. 
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Save edx and validation sets as RData file for access by Rmarkdown 
save(edx, validation, file='movielensdata.RData')


# Use lubridate package to compute date from timestamp rounded by week
edx  <- edx %>% mutate(date_week = round_date(as_datetime(timestamp), unit="week"))
validation  <- validation %>% mutate(date_week = round_date(as_datetime(timestamp), unit="week"))


# Define a loss function: residual mean squared error
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}


# Estimate movie ratings using regularization applied to movieId, userId, date rounded by week, and genres.

# Compute average movie rating for edx set
mu <- mean(edx$rating)


# Determine lambda value by cross-validation
lambdas <- seq(4, 6, 0.1)

rmses <- sapply(lambdas, function(lambda){
  
  b_movie <- edx %>%
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - mu)/(n()+lambda))
  
  b_user <- edx %>%
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - mu - b_movie)/(n()+lambda))
  
  b_date <- edx %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user,  by="userId") %>%
    group_by(date_week) %>%
    summarize(b_date = sum(rating - mu - b_movie - b_user)/(n()+lambda))
  
  b_genres <- edx %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user,  by="userId") %>%
    left_join(b_date,  by="date_week") %>%
    group_by(genres) %>%
    summarize(b_genres = sum(rating - mu - b_movie - b_user - b_date)/(n()+lambda))
  
  predicted_ratings <- validation %>%
    left_join(b_movie, by="movieId") %>%
    left_join(b_user,  by="userId") %>%
    left_join(b_date,  by="date_week") %>%
    left_join(b_genres, by="genres") %>%
    mutate(prediction = mu + b_movie + b_user + b_date + b_genres) %>%
    mutate(pred = ifelse(prediction < 0.5, 0.5,
                         ifelse(prediction > 5, 5, prediction))) %>% .$prediction
  
  return(RMSE(predicted_ratings, validation$rating))
})

# Regularization lambda
lambda <- lambdas[which.min(rmses)]

# Minimum RMSE
rmse <- min(rmses)

# Plot results of cross-validation
plot(lambdas, rmses, xlab="lambda", ylab="RMSE")
points(lambda, min(rmses), col="red")


  
