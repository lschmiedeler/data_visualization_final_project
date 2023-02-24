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

find_label <- function(feature) {
  case_when(feature == "yearpublished" ~ "Year Published", feature == "playingtime" ~ "Playing Time",
            feature == "minage" ~ "Minimum Age", feature == "average" ~ "Average Rating",
            feature == "averageweight" ~ "Average Complexity", feature == "owned" ~ "Number Owned")
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

function(input, output, session) {
  updateSelectizeInput(session, "game_id", choices = games_list, server = TRUE)
  
  output$popular_games <- renderTable(
    details %>% select(name, owned) %>% top_n(20, owned) %>% arrange(-owned) %>% rename("Name" = name, "Number Owned" = owned)
  )
  output$highest_rated_popular_games <- renderTable(
    details %>% filter(owned >= 10000) %>% select(name, average, owned) %>% top_n(20, average) %>% arrange(-average) %>% 
      rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
  )
  
  selected_game <- reactive({ paste("<b> Selected Game: </b>", (details %>% filter(id == input$game_id))$name) })
  output$selected_game_1 <- renderUI(HTML(selected_game()))
  output$selected_game_2 <- renderUI(HTML(selected_game()))
  output$selected_game_3 <- renderUI(HTML(selected_game()))
  
  output$selected_feature_1 <- renderUI(HTML(paste("<b> Feature Value for Selected Game: </b>", filter(details, id == input$game_id)[[input$feature_1]])))
  output$selected_feature_2 <- renderText(HTML(paste("<b> Feature Value for Selected Game: </b>", filter(details, id == input$game_id)[[input$feature_2]])))
  
  output$game_details <- renderTable(
    details %>% filter(id == input$game_id) %>%
      select(name, yearpublished, playingtime, minage, average, averageweight, owned) %>%
      rename("Name" = name, "Year Published" = yearpublished, "Playing Time" = playingtime, "Minimum Age" = minage,
             "Average Rating" = average, "Average Complexity" = averageweight, "Number Owned" = owned)
  )
  
  output$game_categories <- renderTable(categories %>% filter(id == input$game_id) %>%  select(category) %>% rename("Categories" = category))
  output$game_mechanics <- renderTable(mechanics %>% filter(id == input$game_id) %>% select(mechanic) %>% rename("Mechanics" = mechanic))
  output$game_designers <- renderTable(designers %>% filter(id == input$game_id) %>% select(designer) %>% rename("Designers" = designer))
  
  output$all_game_comparison <- renderPlot(
    plot_game_comparison(details, input$game_id, input$plot_type_1, input$feature_1, as.logical(input$remove_outliers_1), find_label(input$feature_1))
  )
  
  output$similar_game_comparison <- renderPlot(
    plot_group_comparison(
      pick_expanded_details(input$group_1), pick_expanded_column(input$group_1),  input$game_id, NA, NA, input$plot_type_2, input$feature_2, 
      input$group_1, as.logical(input$remove_outliers_2), as.logical(input$sort_1), find_label(input$feature_2)
    )
  )
  
  output$top_bar_chart <- renderPlot(
    plot_top_bar_chart(pick_expanded_details(input$group_2), input$group_2, input$metric_1, input$n_1)
  )

  output$top_levels_comparison <- renderPlot(
    plot_group_comparison(
      pick_expanded_details(input$group_3), pick_expanded_column(input$group_3), NA, input$n_2, input$metric_2, input$plot_type_3, input$feature_3, 
      input$group_3, as.logical(input$remove_outliers_3), as.logical(input$sort_2), find_label(input$feature_3)
    )
  )
  
  output$games_over_time <- renderPlot(
    plot_games_over_time(
      details, input$plot_type_4, input$feature_4, input$years, input$year_bin_size, as.logical(input$remove_outliers_4), 
      as.logical(input$add_line), find_label(input$feature_4)
    )
  )
}
