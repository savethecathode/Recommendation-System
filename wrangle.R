# Wrangle Script
# predictors: timestamp, title, genres.
# THINK: bias is based on user difference from average


# Pick a data set to work on: train_set, test_set, edx, validation
#train_set <- train
#test_set  <- test
#edx_mod   <- edx
#val_mod   <- validation
train_set <- edx
test_set <- validation

# (predictor) timestamp: seconds since midnight Coordinated Universal Time (UTC "epoch") of January 1, 1970. 
# http://files.grouplens.org/datasets/movielens/ml-10m-README.html
# THINK:
# year,
# hour("dayparting"): earlymorn, morning, day, night, latenight
# day: monday-sunday
# diff in year bn date of rating and date of movie (in 'title')
#library(lubridate)
# SLOW!!!...cant do all at once?
#set <- set %>% mutate(year=year(as_datetime(timestamp)),
#                      month=month(as_datetime(timestamp)),
#                      hour=hour(as_datetime(timestamp)),
#                      day=weekdays(as_date(timestamp)))
train_set <- train_set %>% mutate(year = year(as_datetime(timestamp)))
train_set <- train_set %>% mutate(month = month(as_datetime(timestamp)), label=TRUE) # label=TRUE allows tic marks as names, but breaks
train_set <- train_set %>% mutate(day = weekdays(as_datetime(timestamp)))
train_set <- train_set %>% mutate(date_day = round_date(as_datetime(timestamp), unit="day"))
train_set <- train_set %>% mutate(date_week = round_date(as_datetime(timestamp), unit="week"))
train_set <- train_set %>% mutate(date_month = round_date(as_datetime(timestamp), unit="month"))

test_set  <- test_set %>% mutate(year = year(as_datetime(timestamp)))
test_set  <- test_set %>% mutate(month = month(as_datetime(timestamp)), label=TRUE)
test_set  <- test_set %>% mutate(day = weekdays(as_datetime(timestamp)))
test_set  <- test_set %>% mutate(date_day = round_date(as_datetime(timestamp), unit="day"))
test_set  <- test_set %>% mutate(date_week = round_date(as_datetime(timestamp), unit="week"))
test_set  <- test_set %>% mutate(date_month = round_date(as_datetime(timestamp), unit="month"))


#edx_mod <- edx_mod %>% mutate(year=year(as_datetime(timestamp)))
#edx_mod <- edx_mod %>% mutate(month=month(as_datetime(timestamp)), label=TRUE)
#edx_mod <- edx_mod %>% mutate(day=weekdays(as_datetime(timestamp)))

#val_mod <- val_mod %>% mutate(year=year(as_datetime(timestamp)))
#val_mod <- val_mod %>% mutate(month=month(as_datetime(timestamp)), label=TRUE)
#val_mod <- val_mod %>% mutate(day=weekdays(as_datetime(timestamp)))

# Get year of movie debut from the title entry using regular expressions
train_set <- train_set %>% select(title) %>% extract(col=title, into="debut", regex="(\\d{4})", convert=TRUE) %>% cbind(train_set, .)
train_set <- train_set %>% mutate(title=str_remove(title, " \\(\\d{4}\\)")) # remove redundant year info from title

test_set <- test_set %>% select(title) %>% extract(col=title, into="debut", regex="(\\d{4})", convert=TRUE) %>% cbind(test_set, .)
test_set <- test_set %>% mutate(title=str_remove(title, " \\(\\d{4}\\)")) # remove redundant year info from title


# ??? Not every movie has a debut date in the title ??? what is going on here???
#edx_mod <- edx_mod %>% select(title) %>% extract(col=title, into="debut", regex="(\\d{4})", convert=TRUE) %>% cbind(test_set, .)
#edx_mod <- edx_mod %>% mutate(title=str_remove(title, " \\(\\d{4}\\)")) # remove redundant year info from title

#val_mod <- val_mod %>% select(title) %>% extract(col=title, into="debut", regex="(\\d{4})", convert=TRUE) %>% cbind(test_set, .)
#val_mod <- val_mod %>% mutate(title=str_remove(title, " \\(\\d{4}\\)")) # remove redundant year info from title



# (predictor) genres: multiple genres listed in string separated by pipe.
# How to implement conditional logic, ie: if you like comedy what are the chances you also like horror?
# basics:
###movie_genres <- edx %>% separate_rows(genres, sep="\\|") %>% select(genres) %>% factor()  # get list of genres (NOTE: factor() is slow!)
###levels(movie_genres)  # show list of genres
# Separate genres into separate rows by pipe (creates duplicate-entries)
# OBS: duplicate entries result in artificial/spurious/false increase in model accuracy (in this case)!
#train_set <- train_set %>% mutate(genre = genres)
#train_set <- train_set %>% separate_rows(genre, sep="\\|")
#test_set  <- test_set  %>% mutate(genre = genres)
#test_set  <- test_set  %>% separate_rows(genre, sep="\\|")

# predictor: number of genres
train_set <- train_set %>% mutate(no_genres = str_count(genres, fixed("|"))+1)
test_set  <- test_set  %>% mutate(no_genres = str_count(genres, fixed("|"))+1)


# (predictors) titles: Sentiment Analysis


# filter() (dplyr) for Bayesian logic...

# Save data sets to access them in Rmarkdown
save(train_set, test_set, file='movielensdata.RData')

