

setwd("~/TemplRprojects/MovieLens") # move to working dir


library(tidyverse)  # wrangling, ggplot2
library(caret)      # ML
library(lubridate)  # date-time
library(matrixStats)
# install.packages(SentimentAnalysis)
#library(SentimentAnalysis)


if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
library(gam)

if(!require(cgwtools)) install.packages("cgwtools", repos = "http://cran.us.r-project.org")# To append to .RData file use resave()
library(cgwtools)

#update.packages(ask=FALSE, checkBuilt=TRUE) # update R packages, or use RStudio Packages tab
