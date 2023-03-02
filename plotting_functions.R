library(tidyverse)
library(ggridges)

remove_outliers <- function(df, feature) {
  # remove the data points that are more than 1.5 * IQR away from the 1st and 3rd quartiles for the specified feature
  filter(df, get(feature) >= quantile(df[[feature]], 0.25) - 1.5 * IQR(df[[feature]]),
         get(feature) <= quantile(df[[feature]], 0.75) + 1.5 * IQR(df[[feature]]))
}

sort_data <- function(df, group, feature) {
  # convert the specified group into a factor where the order of the levels is determined by the median feature values for each level
  correct_order <- df %>% group_by(get(group)) %>%
    summarize(median = median(get(feature))) %>% arrange(-median)
  names(correct_order) <- c(group, "median")
  correct_order <- correct_order[[group]]
  df[[group]] <- factor(df[[group]], levels = rev(correct_order))
  df
}

find_top_n_levels <- function(df, group, n, metric) {
  # find the top n levels of the specified group according to the specified metric
  df <- group_by(df, get(group))
  if (metric == "Number of Different Games") { df <- summarize(df, value = n()) }
  if (metric == "Number Owned") { df <- summarize(df, value = sum(owned)) }
  if (metric == "Average Rating") { df <- summarize(df, value = mean(average)) }
  df <- top_n(df, n, value) %>% arrange(value)
  names(df) <- c(group, "value")
  df[[group]] <- factor(df[[group]], levels = df[[group]])
  df
}

plot_game_comparison <- function(details, game_id, plot_type, feature, remove_outliers, x_label) {
  # create plotting data
  if (feature == "All") {
    features <- c("average", "averageweight", "owned", "playingtime", "minage", "yearpublished")
    titles <- c("Average Rating", "Average Complexity", "Number Owned", "Playing Time", "Minimum Age", "Year Published")
    plotting_data <- do.call("rbind", lapply(1:length(features), function(x) {
      if (remove_outliers) { details <- remove_outliers(details, features[x]) }
      df <- details %>% select(name, id, as.character(features[x]))
      names(df) <- c("name", "id", "value")
      df$feature <- rep(titles[x], nrow(df))
      df
    }))
  }
  else {
    if (remove_outliers) { details <- remove_outliers(details, feature) }
    plotting_data <- details %>% select(name, id, feature)
    names(plotting_data) <- c("name", "id", "value")
  }
  game_values <- plotting_data %>% filter(id == game_id)
  
  # create one of the two possible plots: boxplot or density plot
  plot <- ggplot(plotting_data, aes(x = value)) + theme_bw() +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  # create a boxplot
  if (plot_type == "Boxplot") {
    plot <- plot + geom_boxplot(fill = "black", alpha = 0.25, linewidth = 1.2, fatten = 1, outlier.size = 2.5) +
      scale_y_continuous(limits = c(-2, 2))
  }
  # create a density plot
  if (plot_type == "Density Plot") { 
    plot <- plot + geom_density(fill = "black", alpha = 0.25, linewidth = 1.2)
  }
  if (feature == "All") {
    plot <- plot + facet_wrap(~feature, scales = "free") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12), strip.text = element_text(size = 14))
  }
  if (feature != "All") {
    plot <- plot + labs(x = x_label)
  }
  # add the dashed line that represents the feature value associated with the selected game
  plot + geom_vline(data = game_values, aes(xintercept = value), color = "red", linewidth = 1.2, linetype = "longdash") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
}

plot_group_comparison <- function(expanded_details, expanded_column, game_id, n, metric, plot_type, feature, group, remove_outliers, sort, x_label) {
  # filter the data so that the only levels of the group remaining are those associated with the specified game
  if (is.na(n)) { 
    expanded_details <- filter(expanded_details, get(group) %in% (expanded_column %>% filter(id == game_id))[[group]])
    if (feature == "All") {
      features <- c("average", "averageweight", "owned", "playingtime", "minage", "yearpublished")
      titles <- c("Average Rating", "Average Complexity", "Number Owned", "Playing Time", "Minimum Age", "Year Published")
      plotting_data <- do.call("rbind", lapply(1:length(features), function(x) {
        if (remove_outliers) { expanded_details <- remove_outliers(expanded_details, features[x]) }
        df <- expanded_details %>% select(name, id, as.character(group), as.character(features[x]))
        names(df) <- c("name", "id", group, "value")
        df$feature <- rep(titles[x], nrow(df))
        df
      }))
    }
    else {
      if (remove_outliers) { expanded_details <- remove_outliers(expanded_details, feature) }
      plotting_data <- select(expanded_details, id, name, group, feature)
      names(plotting_data) <- c("id", "name", group, "value")
      # sort the data based on the median feature values for each level of the group
      if (sort) { plotting_data <- sort_data(plotting_data, group, "value") }
    }
    game_values <- plotting_data %>% filter(id == game_id)
  }
  # filter the data so that the only levels of the group remaining are the top n levels
  if (is.na(game_id)) {
    if (remove_outliers) { expanded_details <- remove_outliers(expanded_details, feature) }
    # remove the observations that have no designer
    if (group == "designer") { expanded_details <- filter(expanded_details, designer != "(Uncredited)") }
    top_levels <- find_top_n_levels(expanded_details, group, n, metric)[[group]]
    plotting_data <- filter(expanded_details, get(group) %in% as.character(top_levels))
    plotting_data[[group]] <- factor(plotting_data[[group]], levels = top_levels)
    plotting_data <- select(plotting_data, id, name, group, feature)
    names(plotting_data) <- c("id", "name", group, "value")
    # sort the data based on the median feature values for each level of the group
    if (sort) { plotting_data <- sort_data(plotting_data, group, "value") }
  }
  
  # create one of the four possible plots: boxplot, violin plot, density plot, or ridgeline plot
  plot <- ggplot(plotting_data, aes(x = value, color = get(group), fill = get(group))) + theme_bw()
  # create a boxplot
  if (plot_type == "Boxplot") {
    plot <- plot + geom_boxplot(aes(y = get(group)), alpha = 0.25, linewidth = 1.2, fatten = 1, outlier.size = 2.5) +
      theme(legend.position = "none", axis.title.y = element_blank())
  }
  # create a violin plot
  if (plot_type == "Violin Plot") {
    plot <- plot + geom_violin(aes(y = get(group)), alpha = 0.25, linewidth = 1.2) +
      theme(legend.position = "none", axis.title.y = element_blank())
  }
  # create a density plot
  if (plot_type == "Density Plot") {
    plot <- plot + geom_density(alpha = 0.1, linewidth = 1.2) + 
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  # create a ridgeline plot
  if (plot_type == "Ridgeline Plot") {
    plot <- plot + geom_density_ridges(aes(y = get(group)), alpha = 0.25, size = 1.2) +
      theme(legend.position = "none", axis.title.y = element_blank())
  }
  if (feature == "All") {
    plot <- plot + facet_wrap(~feature, scales = "free_x") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), strip.text = element_text(size = 14))
  }
  # add the dashed line that represents the feature value associated with the selected game
  if (is.na(n)) {
    plot <- plot + geom_vline(data = game_values, aes(xintercept = value), color = "black", linewidth = 1.2, linetype = "longdash")
  }
  plot + labs(x = x_label, color = str_to_title(group), fill = str_to_title(group)) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
}

plot_games_over_time <- function(details, plot_type, feature, years, year_bin_size, remove_outliers, add_line, label) {
  # remove outliers
  if (remove_outliers) { details <- remove_outliers(details, feature) }
  # filter the years based on the specified year range
  plotting_data <- filter(details, yearpublished >= years[1], yearpublished <= years[2])
  
  # create one of the two possible plots: scatterplot or ridgeline plot
  # create a scatterplot
  if (plot_type == "Scatterplot") {
    plot <- ggplot(plotting_data, aes(x = yearpublished, y = get(feature))) +
      geom_jitter(alpha = 0.25, height = 0.5, width = 0.5) +
      labs(x = "Year Published", y = label)
    # add a linear regression line
    if (add_line) {plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.5)}
  }
  # create a ridgeline plot
  if (plot_type == "Ridgeline Plot") {
    plotting_data$yearpublished <- plotting_data$yearpublished - plotting_data$yearpublished %% year_bin_size
    plot <- ggplot(plotting_data, aes(y = as.factor(yearpublished), x = get(feature), fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 4) +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = label, y = "Year Published")
  }
  plot <- plot + theme_bw() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none")
  if (plot_type == "Ridgeline Plot" & (years[2] - years[1]) / year_bin_size >= 40) {
    plot <- plot + theme(axis.text.y = element_text(size = 12))
    if ((years[2] - years[1]) / year_bin_size >= 50) { plot <- plot + theme(axis.text.y = element_text(size = 10)) }
    if ((years[2] - years[1]) / year_bin_size >= 60) { plot <- plot + theme(axis.text.y = element_text(size = 8)) }
  }
  plot
}

plot_top_bar_chart <- function(expanded_details, group, metric, n) {
  # remove the observations that have no designer
  if (group == "designer") { expanded_details <- filter(expanded_details, designer != "(Uncredited)") }
  # find the top levels in the specified group according to the specified metric
  plotting_data <- find_top_n_levels(expanded_details, group, n, metric)
  
  # create a bar plot
  plot <- ggplot(plotting_data, aes(x = get(group), y = value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(y = metric) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.text = element_text(size = 14), axis.title = element_text(size = 16))
  if (metric == "Average Rating" & group %in% c("category", "designer")) { plot <- plot + scale_y_continuous(limits = c(0, 10.25)) }
  if (metric == "Number Owned") { plot <- plot + scale_y_continuous(labels = scales::comma) }
  if (metric == "Number Owned" & group == "mechanic") { plot <- plot + scale_y_continuous(limits = c(0, 12500000), labels = scales::comma) }
  if (n >= 40) {
    plot <- plot + theme(axis.text.y = element_text(size = 12))
    if (n >= 45) { plot <- plot + theme(axis.text.y = element_text(size = 10)) }
  }
  plot
}

plot_heat_map <- function() {
  
}