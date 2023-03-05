library(tidyverse)
source("plotting_functions.R")

details <- read.csv("data/details.csv")
categories <- read.csv("data/categories.csv")
details_and_categories <- merge(details, categories)
mechanics <- read.csv("data/mechanics.csv")
details_and_mechanics <- merge(details, mechanics)
designers <- read.csv("data/designers.csv")
details_and_designers <- merge(details, designers)

dfs <- list(details, details_and_categories, details_and_mechanics, details_and_designers)
features <- c("average", "averageweight", "owned", "playingtime", "minage", "yearpublished")

prop_remaining <- data.frame(df = c(), feature = c(), prop_remaining = c())

for (i in 1:length(dfs)) {
  df <- data.frame(dfs[i])
  for (feature in features) {
    prop <- nrow(remove_outliers(df, feature)) / nrow(df)
    prop_remaining <- rbind(prop_remaining, data.frame(df = c(i),
                                                       feature = c(feature),
                                                       prop_remaining = c(prop)))
  }
}

prop_remaining
