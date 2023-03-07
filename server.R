library(shiny)
library(tidyverse)
source("plotting_functions.R")

details <- read.csv("data/details.csv")
categories <- read.csv("data/categories.csv")
details_and_categories <- merge(details, categories)
mechanics <- read.csv("data/mechanics.csv")
details_and_mechanics <- merge(details, mechanics)
designers <- read.csv("data/designers.csv")
details_and_designers <- merge(details, designers)

extra <- filter(details, str_starts(name, "\\["))
details <- filter(details, str_starts(name, "\\[", negate = TRUE))
details <- rbind(filter(details, str_starts(name, "[A-z]")) %>% arrange(name), 
                 filter(details, str_starts(name, "[A-z]", negate = TRUE)) %>% arrange(name), extra)
games_list <- as.list(details$id)
names(games_list) <- details$name

labels <- c("Average Rating", "Average Complexity", "Number Owned",
            "Year Published", "Minimum Number of Players", "Maximum Number of Players",
            "Minimum Playing Time", "Maximum Playing Time", "Minimum Age")

find_feature <- function(label) {
  case_when(label == "Average Rating" ~ "average", label == "Average Complexity" ~ "averageweight", 
            label == "Number Owned" ~ "owned", label == "Year Published" ~ "yearpublished", 
            label == "Minimum Number of Players" ~ "minplayers", label == "Maximum Number of Players" ~ "maxplayers", 
            label == "Minimum Playing Time" ~ "minplaytime", label == "Maximum Playing Time" ~ "maxplaytime",
            label == "Minimum Age" ~ "minage", TRUE ~ label)
}

pick_expanded_details <- function(group) {
  if (group == "category") { return(details_and_categories) }
  if (group == "mechanic") { return(details_and_mechanics) }
  if (group == "designer") { return(details_and_designers) }
}

pick_expanded_column <- function(group) {
  if (group == "category") { return(categories) }
  if (group == "mechanic") { return(mechanics) }
  if (group == "designer") { return(designers) }
}

filter_feature <- function(df, feature, range) {
  if (length(range) == 0) { df }
  else if (is.na(range[[1]])) { filter(df, get(feature) <= range[2]) }
  else if (is.na(range[[2]])) { filter(df, get(feature) >= range[1]) }
  else {filter(df, get(feature) >= range[1], get(feature) <= range[2]) }
}

function(input, output, session) {
  updateSelectizeInput(session, "game_id", choices = games_list, server = TRUE)
  game_information <- reactive({ details %>% filter(id == input$game_id) })
  
  popular_games <- reactive({
    if (is.na(input$n_popular_all)) { data.frame() }
    else {
      details %>% select(name, average, owned) %>% top_n(input$n_popular_all, owned) %>%
        arrange(-owned) %>% rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
    }
  })
  output$popular_games <- renderTable(popular_games())
  highest_rated_popular_games <- reactive({
    if (is.na(input$n_highest_rated_popular_all)) { data.frame() }
    else {
      
      details %>% filter(owned >= quantile(details$owned, 1 - min(1, 10 * input$n_highest_rated_popular_all / nrow(details)), na.rm = TRUE)) %>% 
        select(name, average, owned) %>% top_n(input$n_highest_rated_popular_all, average) %>% arrange(-average) %>% 
        rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
    }
  })
  output$highest_rated_popular_games <- renderTable(highest_rated_popular_games())
  
  selected_game_link <- reactive({ renderUI(HTML(paste0('<a href="https://boardgamegeek.com/boardgame/', input$game_id, '">', game_information()$name , '</a>'))) })
  output$selected_game_1 <- renderUI(selected_game_link())
  output$selected_game_2 <- renderUI(selected_game_link())
  output$selected_game_description <- renderUI(HTML(game_information()$description))
  
  selected_game_text <- reactive({ HTML(paste("<b>Selected Game:</b>", game_information()$name)) })
  output$selected_game_3 <- renderUI(selected_game_text())
  output$selected_game_4 <- renderUI(selected_game_text())
  
  image_src <- reactive({ game_information()$image })
  output$game_image <- renderText({paste0("<img src=\"", image_src(), "\" style=\"width:80%\">")})
  
  game_details_float <- reactive({
    if (nchar(input$game_id) > 0) {
      df <- data.frame(t(game_information() %>%  select(average, averageweight))) %>% mutate(title = c("Average Rating", "Average Complexity"))
      names(df) <- c("value", "title")
      df <- select(df, title, value) 
      names(df) <- c(" ", " ")
      df
    }
    else { data.frame() }
  })
  game_details_int <- reactive({
    if (nchar(input$game_id) > 0) {
      df <- data.frame(t(game_information() %>% select(owned, yearpublished, minplayers, maxplayers, minplaytime, maxplaytime, minage))) %>% 
        mutate(title = c("Number Owned", "Year Published", "Minimum Number of Players", "Maximum Number of Players", "Minimum Playing Time", "Maximum Playing Time", "Minimum Age"))
      names(df) <- c("value", "title")
      df <- select(df, title, value) 
      names(df) <- c(" ", " ")
      df
    }
    else { data.frame() }
  })
  output$game_details_float <- renderTable(game_details_float())
  output$game_details_int <- renderTable(game_details_int())
  
  output$game_categories <- renderTable(categories %>% filter(id == input$game_id) %>%  select(category) %>% rename("Categories" = category))
  output$game_mechanics <- renderTable(mechanics %>% filter(id == input$game_id) %>% select(mechanic) %>% rename("Mechanics" = mechanic))
  output$game_designers <- renderTable(designers %>% filter(id == input$game_id) %>% select(designer) %>% rename("Designers" = designer))
  
  updateSelectizeInput(session, "feature_1", choices = labels, selected = c("Average Rating", "Average Complexity"), server = TRUE)
  all_game_comparison <- reactive({
    if (nchar(input$game_id) == 0 | length(input$feature_1) == 0) { ggplot() }
    else {
      plot_single_comparison(details, input$game_id, as.character(lapply(input$feature_1, find_feature)), as.logical(input$remove_extreme_values_1), input$plot_type_1, input$feature_1)
    }
  })
  output$all_game_comparison <- renderPlot(all_game_comparison())
  
  updateSelectizeInput(session, "feature_2", choices = labels, selected = c("Average Rating", "Average Complexity"), server = TRUE)
  similar_game_comparison <- reactive({
    if (nchar(input$game_id) == 0 | length(input$feature_2) == 0) { ggplot() }
    else {
      plot_group_comparison(
        pick_expanded_details(input$group_1), pick_expanded_column(input$group_1), input$game_id, input$group_1, NA, NA, NA, 
        as.character(lapply(input$feature_2, find_feature)), as.logical(input$remove_extreme_values_2), input$plot_type_2, TRUE, input$feature_2
      )
    }
  })
  output$similar_game_comparison <- renderPlot(similar_game_comparison())
  
  updateSelectizeInput(session, "group_2", choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer"), server = TRUE)
  group <- reactive({ input$group_2 })
  levels <- reactive({
    if (group() == "category") { return(sort(unique(categories$category))) }
    if (group() == "mechanic") { return(sort(unique(mechanics$mechanic))) }
    if (group() == "designer") { return(sort(unique(designers$designer))) }
  })
  observe({ updateSelectizeInput(session, "level_1", choices = levels(), server = TRUE) })
  level <- reactive({ input$level_1 })
  level_data <- reactive({
    if (group() == "category") { details_and_categories %>% filter(category == input$level_1) }
    else if (group() == "mechanic") { details_and_mechanics %>% filter(mechanic == input$level_1) }
    else if (group() == "designer") { details_and_designers %>% filter(designer == input$level_1) }
  })
  
  output$number_of_games <- renderText(
    paste("<b>Number of Different Games:</b>", nrow(level_data()))
  )
  updateSelectizeInput(session, "feature_3", choices = labels, selected = c("Average Rating", "Average Complexity"), server = TRUE)
  level_comparison <- reactive({
    if (nchar(input$group_2) == 0 | nchar(input$level_1) == 0 | length(input$feature_3) == 0) { ggplot() }
    else {
      plot_group_comparison(
        pick_expanded_details(input$group_2), pick_expanded_column(input$group_2), NA, input$group_2, input$level_1, NA, NA,
        as.character(lapply(input$feature_3, find_feature)), as.logical(input$remove_extreme_values_3), input$plot_type_3, FALSE, input$feature_3
      )
    }
  })
  output$level_comparison <- renderPlot(level_comparison())
  popular_games_in_level <- reactive({
    if (nchar(input$group_2) == 0 | nchar(input$level_1) == 0 | is.na(input$n_popular)) { data.frame() }
    else {
      level_data() %>% top_n(input$n_popular, owned) %>% arrange(-owned) %>% select(name, average, owned) %>%
        rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
    }
  })
  output$popular_games_in_level <- renderTable(popular_games_in_level())
  highest_rated_games_in_level <- reactive({
    if (nchar(input$group_2) == 0 | nchar(input$level_1) == 0 | is.na(input$n_highest_rated)) { data.frame() }
    else {
      level_data() %>% top_n(input$n_highest_rated, average) %>% arrange(-average) %>% select(name, average, owned) %>%
        rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
    }
  })
  output$highest_rated_games_in_level <- renderTable(highest_rated_games_in_level())
  highest_rated_popular_games_in_level <- reactive({
    if (nchar(input$group_2) == 0 | nchar(input$level_1) == 0 | is.na(input$n_highest_rated_popular)) { data.frame() }
    else {
      level_data() %>% filter(owned >= quantile(level_data()$owned, 1 - min(1, 2 * input$n_highest_rated_popular / nrow(level_data())), na.rm = TRUE)) %>%
        top_n(input$n_highest_rated_popular, average) %>% arrange(-average) %>% select(name, average, owned) %>%
        rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
      
    }
  })
  output$highest_rated_popular_games_in_level <- renderTable(highest_rated_popular_games_in_level())

  output$top_bar_chart <- renderPlot(
    plot_top_bar_chart(pick_expanded_details(input$group_3), input$group_3, input$metric_1, input$n_1)
  )

  output$top_levels_comparison <- renderPlot(
    plot_group_comparison(
      pick_expanded_details(input$group_4), pick_expanded_column(input$group_4), NA, input$group_4, NA, input$metric_2, input$n_2, 
      find_feature(input$feature_4), as.logical(input$remove_extreme_values_4), input$plot_type_4, as.logical(input$sort_1), input$feature_4
    )
  )
  
  output$groups_over_time <- renderPlot(
    plot_groups_over_time(
      pick_expanded_details(input$group_5), input$group_5, input$metric_3, input$n_3, find_feature(input$feature_5), as.logical(input$remove_extreme_values_5), 
      input$years_1, input$plot_type_5, as.logical(input$add_line_1), as.logical(input$add_curve_1), input$agg_metric, input$sort_2, input$feature_5
    )
  )

  filtered_games <- reactive({
    df <- details
    features <- as.character(lapply(features, find_feature))
    ranges <- list(input$rating_range, input$complexity_range, input$owned_range, input$year_range, input$minplayers_range, 
                   input$maxplayers_range, input$minplay_range, input$maxplay_range, input$minage_range)
    dfs <- lapply(1:length(features), function(x) { filter_feature(df, features[x], ranges[[x]]) }) %>% reduce(inner_join)
  })
  top_filtered_games <- reactive({
    if (is.na(input$n_games)) { return(data.frame()) }
    df <- filtered_games()
    if (input$reverse_sort) { 
      df <- df %>% top_n(input$n_games, -get(find_feature(input$sort_feature))) %>%
        arrange(get(find_feature(input$sort_feature)))
    } 
    else {
      df <- df %>% top_n(input$n_games, get(find_feature(input$sort_feature))) %>%
        arrange(desc(get(find_feature(input$sort_feature))))
    }
    df <- df %>% select(name, average, averageweight, owned, yearpublished, minplayers, maxplayers, minplaytime, maxplaytime, minage)
    names(df) <- c("Name", labels)
    df
  })
  output$total_number_of_games <- renderUI(HTML(paste("<b>Total Number of Games:</b>", nrow(filtered_games()))))
  output$top_filtered_games <- renderTable(top_filtered_games())
  
  output$games_over_time <- renderPlot(
    plot_games_over_time(
      details, find_feature(input$feature_6), as.logical(input$remove_extreme_values_6), input$years_2, input$plot_type_6,
      as.logical(input$add_line_2), as.logical(input$add_curve_2), input$year_bin_size_1, input$feature_6
    )
  )
}
