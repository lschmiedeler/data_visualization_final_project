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

find_feature <- function(label) {
  case_when(label == "Year Published" ~ "yearpublished", label == "Playing Time" ~ "playingtime",
            label == "Minimum Age" ~ "minage", label == "Average Rating" ~ "average",
            label == "Average Complexity" ~ "averageweight", label == "Number Owned" ~ "owned",
            TRUE ~ label)
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
  
  selected_game_1 <- reactive({ (details %>% filter(id == input$game_id))$name })
  output$selected_game_1 <- renderUI(HTML(selected_game_1()))
  selected_game_2 <- reactive({ paste("<b>Selected Game:</b>", (details %>% filter(id == input$game_id))$name) })
  output$selected_game_2 <- renderUI(HTML(selected_game_2()))
  output$selected_game_3 <- renderUI(HTML(selected_game_2()))
  
  image_src <- reactive({ (details %>% filter(id == input$game_id))$image })
  output$game_image <- renderText({paste0("<img src=\"", image_src(), "\" style=\"width:75%\">")})
  
  output$game_details <- renderTable(
    details %>% filter(id == input$game_id) %>%
      select(name, averageweight, average, minage, owned, playingtime, yearpublished) %>%
      rename("Name" = name, "Average Complexity" = averageweight, "Average Rating" = average, "Minimum Age" = minage,
             "Number Owned" = owned, "Playing Time" = playingtime, "Year Published" = yearpublished)
  )
  
  output$game_categories <- renderTable(categories %>% filter(id == input$game_id) %>%  select(category) %>% rename("Categories" = category))
  output$game_mechanics <- renderTable(mechanics %>% filter(id == input$game_id) %>% select(mechanic) %>% rename("Mechanics" = mechanic))
  output$game_designers <- renderTable(designers %>% filter(id == input$game_id) %>% select(designer) %>% rename("Designers" = designer))
  
  output$all_game_comparison <- renderPlot(
    plot_single_comparison(details, input$game_id, NA, NA, find_feature(input$feature_1), as.logical(input$remove_outliers_1), input$plot_type_1, input$feature_1)
  )
  
  output$similar_game_comparison <- renderPlot(
    plot_group_comparison(
      pick_expanded_details(input$group_1), pick_expanded_column(input$group_1), input$game_id, input$group_1, NA, NA, 
      find_feature(input$feature_2), as.logical(input$remove_outliers_2), input$plot_type_2, TRUE, input$feature_2
    )
  )
  
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
  output$feature_summary <- renderTable(
    data.frame(do.call(cbind, lapply(level_data() %>% select(averageweight, average, minage, owned, playingtime, yearpublished), summary))) %>%
      rename("Average Complexity" = averageweight, "Average Rating" = average, "Minimum Age" = minage,
             "Number Owned" = owned, "Playing Time" = playingtime, "Year Published" = yearpublished) %>%
      mutate(metric = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum")) %>%
      relocate(metric) %>%
      rename(" " = metric)
  )
  output$level_data <- renderPlot(
    plot_single_comparison(pick_expanded_details(input$group_2), NA, input$group_2, input$level_1, "All", input$remove_outliers_3, input$plot_type_3, "")
  )
  output$popular_games_in_level <- renderTable(
    level_data() %>% top_n(20, owned) %>% arrange(-owned) %>% select(name, average, owned) %>%
      rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
  )
  output$highest_rated_games_in_level <- renderTable(
    level_data() %>% top_n(20, average) %>% arrange(-average) %>% select(name, average, owned) %>%
      rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
  )
  output$highest_rated_popular_games_in_level <- renderTable(
    level_data() %>% filter(owned >= quantile(level_data()$owned, 0.95)) %>%
      top_n(20, average) %>% arrange(-average) %>% select(name, average, owned) %>%
      rename("Name" = name, "Average Rating" = average, "Number Owned" = owned)
  )

  output$top_bar_chart <- renderPlot(
    plot_top_bar_chart(pick_expanded_details(input$group_3), input$group_3, input$metric_1, input$n_1)
  )

  output$top_levels_comparison <- renderPlot(
    plot_group_comparison(
      pick_expanded_details(input$group_4), pick_expanded_column(input$group_4), NA, input$group_4, input$metric_2, input$n_2, 
      find_feature(input$feature_3), as.logical(input$remove_outliers_4), input$plot_type_4, as.logical(input$sort_1), input$feature_3
    )
  )
  
  output$groups_over_time <- renderPlot(
    plot_groups_over_time(
      pick_expanded_details(input$group_5), input$group_5, input$metric_3, input$n_3, find_feature(input$feature_4),
      as.logical(input$remove_outliers_5), input$years_1, input$plot_type_5, as.logical(input$add_line_1), input$feature_4
    )
  )

  output$games_over_time <- renderPlot(
    plot_games_over_time(
      details, find_feature(input$feature_5), as.logical(input$remove_outliers_6), input$years_2, 
      input$plot_type_6, as.logical(input$add_line_2), input$year_bin_size_1, input$feature_5
    )
  )
}
