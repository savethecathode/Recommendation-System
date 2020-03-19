

# Use CAret package to perform ML algo
#...use the trainControl() function, ie: use 10-fold cross-validation (10 validation samples each with 10\% of the observations)  
control <- trainControl(method="cv", number=10, p=.9)

train_knn <- train(rating ~ ???,
                   method="knn",
                   data=train_set,
                   tuneGrid=data.frame(k=seq(9,71,2)),
                   trControl=control)


confusionMatrix(data=predict(), reference=)$overall["Accuracy"]