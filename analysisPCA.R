

train_small <- train_set %>%
  group_by(movieId) %>%
  filter(n() >= 50) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# add names and rows to columns to facilitate exploration
rownames(y) <- y[,1]
y <- y[,-1]
movie_titles <- train_set %>%
  select(movieId, title) %>%
  distinct()
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# Convert to residuals (subtract col and row avgs)
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))