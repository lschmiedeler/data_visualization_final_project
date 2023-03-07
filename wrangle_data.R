library(tidyverse)
library(stringr)

create_expanded_df <- function(df, column_name, expanded_df_names) {
  do.call("rbind", lapply(1:nrow(df), function(i) {
    column_info <- str_replace_all(df[[column_name]][i], "([A-z]),\\s", "\\1 ")
    column_info <- str_replace_all(column_info, "(\"|')", "")
    column_info <- str_extract_all(column_info, "([^,]+)(,|])")[[1]]
    data.frame(id = rep(df$id[i], length(column_info)), info = str_sub(column_info, start = 2, end = nchar(column_info) - 1))})) %>% 
    mutate(info = as.factor(info)) %>%
    setNames(expanded_df_names)
}

# read in the data (source: Kaggle)
# https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews?resource=download&select=games_detailed_info.csv
df <- read.csv("data/games_detailed_info.csv", row.names = 1)

# create a details dataframe that contains a subset of the columns in df
details <- df %>% select(id, primary, image, description, yearpublished, 
                         minplayers, maxplayers, minplaytime, maxplaytime, 
                         minage, average, owned, averageweight) %>%
  mutate(primary = as.factor(primary)) %>%
  rename(name = primary) %>%
  mutate(description = str_trim(str_replace_all(description, "&#10;", "<br>"))) %>%
  mutate(description = str_replace_all(description, "(<br>)(<br>)+", "<br><br>")) %>%
  mutate(description = str_replace_all(description, "(<br>)+$", ""))

# create three expanded dataframes: categories, mechanics, and designers
categories <- create_expanded_df(df, "boardgamecategory", c("id", "category"))
mechanics <- create_expanded_df(df, "boardgamemechanic", c("id", "mechanic"))
designers <- create_expanded_df(df, "boardgamedesigner", c("id", "designer"))

# write the details, categories, mechanics, and designers dataframes to csv files
write.csv(details, "data/details.csv", row.names = FALSE)
write.csv(categories, "data/categories.csv", row.names = FALSE)
write.csv(mechanics, "data/mechanics.csv", row.names = FALSE)
write.csv(designers, "data/designers.csv", row.names = FALSE)
