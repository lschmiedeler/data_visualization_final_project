library(tidyverse)
library(ggridges)

remove_outliers <- function(df, feature) {
  # remove the data points that are more than 1.5 * IQR away from the 1st and 3rd quartiles for the specified feature
  filter(df, get(feature) >= quantile(df[[feature]], 0.25) - 1.5 * IQR(df[[feature]]),
         get(feature) <= quantile(df[[feature]], 0.75) + 1.5 * IQR(df[[feature]]))
}

sort_data_by_median <- function(df, group, feature) {
  # convert the specified group into a factor where the order of the levels is determined by the median feature values for each level
  correct_order <- df %>% group_by(get(group)) %>% summarize(median = median(get(feature))) %>% arrange(-median)
  names(correct_order) <- c(group, "median")
  df[[group]] <- factor(df[[group]], levels = correct_order[[group]])
  df
}

create_plotting_data <- function(df, group, remove_outliers) {
  # reformat the data so that it can be easily faceted on the feature variable
  features <- c("average", "averageweight", "owned", "playingtime", "minage", "yearpublished")
  titles <- c("Average Rating", "Average Complexity", "Number Owned", "Playing Time", "Minimum Age", "Year Published")
  do.call("rbind", lapply(1:length(features), function(x) {
    if (remove_outliers) { df <- remove_outliers(df, features[x]) }
    if (is.na(group)) {
      temp_df <- select(df, id, name, as.character(features[x]))
      names(temp_df) <- c("id", "name", "value")
    }
    else {
      temp_df <- select(df, id, name, as.character(group), as.character(features[x]))
      names(temp_df) <- c("id", "name", group, "value")
    }
    temp_df$feature <- rep(titles[x], nrow(temp_df))
    temp_df
  }))
}

find_top_n_levels <- function(df, group, metric, n) {
  # find the top n levels of the specified group according to the specified metric
  df <- group_by(df, get(group))
  if (metric == "Number of Different Games") { df <- summarize(df, value = n()) }
  if (metric == "Number Owned") { df <- summarize(df, value = sum(owned)) }
  if (metric == "Average Rating") { df <- summarize(df, value = mean(average)) }
  df <- top_n(df, n, value) %>% arrange(-value)
  names(df) <- c(group, "value")
  df[[group]] <- factor(df[[group]], levels = df[[group]])
  df
}

filter_top_n_levels <- function(df, group, metric, n, feature, remove_outliers, year, sort) {
  # filter the data so that the only levels of the group remaining are the top n levels
  if (remove_outliers) { df <- remove_outliers(df, feature) }
  if (group == "designer") { df <- filter(df, designer != "(Uncredited)") }
  top_levels <- find_top_n_levels(df, group, metric, n)[[group]]
  plotting_data <- filter(df, get(group) %in% as.character(top_levels))
  plotting_data[[group]] <- factor(plotting_data[[group]], levels = top_levels)
  if (year) { 
    plotting_data <- select(plotting_data, id, name, yearpublished, as.character(group), as.character(feature))
    names(plotting_data) <- c("id", "name", "yearpublished", group, "value")
  }
  else {
    plotting_data <- select(plotting_data, id, name, as.character(group), as.character(feature))
    names(plotting_data) <- c("id", "name", group, "value")
  }
  if (sort) { plotting_data <- sort_data_by_median(plotting_data, group, "value") }
  plotting_data
}

plot_single_comparison <- function(details, game_id, group, level, feature, remove_outliers, plot_type, x_label) {
  # create plotting data
  if (!is.na(level)) { details <- filter(details, get(group) == level) }
  if (feature == "All") { plotting_data <- create_plotting_data(details, NA, remove_outliers) }
  else {
    if (remove_outliers) { details <- remove_outliers(details, feature) }
    plotting_data <- select(details, name, id, as.character(feature))
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
      theme(axis.title.x = element_blank(), strip.text = element_text(size = 16))
  }
  if (feature != "All") { plot <- plot + labs(x = x_label) }
  # add the dashed line that represents the feature value associated with the selected game
  plot + geom_vline(data = game_values, aes(xintercept = value), color = "red", linewidth = 1.2, linetype = "longdash") + 
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))
}

plot_group_comparison <- function(expanded_details, expanded_column, game_id, group, metric, n,
                                  feature, remove_outliers, plot_type, sort, x_label) {
  # create plotting data
  # filter the data so that the only levels of the group remaining are those associated with the specified game
  if (is.na(n)) { 
    expanded_details <- filter(expanded_details, get(group) %in% (expanded_column %>% filter(id == game_id))[[group]])
    if (feature == "All") { plotting_data <- create_plotting_data(expanded_details, group, remove_outliers) }
    else {
      if (remove_outliers) { expanded_details <- remove_outliers(expanded_details, feature) }
      plotting_data <- select(expanded_details, id, name, as.character(group), as.character(feature))
      names(plotting_data) <- c("id", "name", group, "value")
      if (sort) { plotting_data <- sort_data_by_median(plotting_data, group, "value") }
    }
    game_values <- plotting_data %>% filter(id == game_id)
  }
  # filter the data so that the only levels of the group remaining are the top n levels
  else { plotting_data <- filter_top_n_levels(expanded_details, group, metric, n, feature, remove_outliers, FALSE, sort) }
  
  # create one of the four possible plots: boxplot, violin plot, density plot, or ridgeline plot
  plot <- ggplot(plotting_data, aes(x = value, color = get(group), fill = get(group))) + theme_bw()
  # create a boxplot
  if (plot_type == "Boxplot") { plot <- plot + geom_boxplot(aes(y = get(group)), alpha = 0.25, linewidth = 1.2, fatten = 1, outlier.size = 2.5) }
  # create a violin plot
  if (plot_type == "Violin Plot") { plot <- plot + geom_violin(aes(y = get(group)), alpha = 0.25, linewidth = 1.2) }
  # create a density plot
  if (plot_type == "Density Plot") {
    plot <- plot + geom_density(alpha = 0.1, linewidth = 1.2) + 
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = 14))
  }
  # create a ridgeline plot
  if (plot_type == "Ridgeline Plot") { plot <- plot + geom_density_ridges(aes(y = get(group)), alpha = 0.25, size = 1.2) }
  if (plot_type %in% c("Boxplot", "Violin Plot", "Ridgeline Plot")) { plot <- plot + theme(legend.position = "none", axis.title.y = element_blank()) }
  if (feature == "All") {
    if (plot_type == "Density Plot") { 
      plot <- plot + facet_wrap(~feature, scales = "free") + 
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
    else { plot <- plot + facet_wrap(~feature, scales = "free_x") } 
    plot <- plot + theme(axis.title.x = element_blank(), strip.text = element_text(size = 16))
  }
  if (plot_type != "Density Plot") { plot <- plot + scale_y_discrete(limits = rev) }
  # add the dashed line that represents the feature value associated with the selected game
  if (is.na(n)) { plot <- plot + geom_vline(data = game_values, aes(xintercept = value), color = "black", linewidth = 1.2, linetype = "longdash") }
  if (!(is.na(n)) & n >= 40) {
    plot <- plot + theme(axis.text.y = element_text(size = 12))
    if (n >= 45) { plot <- plot + theme(axis.text.y = element_text(size = 10)) }
  }
  plot + labs(x = x_label, color = str_to_title(group), fill = str_to_title(group)) +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))
}

plot_top_bar_chart <- function(expanded_details, group, metric, n) {
  # create plotting data
  # remove the observations that have no designer
  if (group == "designer") { expanded_details <- filter(expanded_details, designer != "(Uncredited)") }
  # find the top levels in the specified group according to the specified metric
  plotting_data <- find_top_n_levels(expanded_details, group, metric, n)
  
  # create a bar plot
  plot <- ggplot(plotting_data, aes(x = get(group), y = value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_discrete(limits = rev) + 
    labs(y = metric) +
    theme_bw() +
    theme(axis.title.y = element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 16))
  if (metric == "Average Rating" & group %in% c("category", "designer")) { plot <- plot + scale_y_continuous(limits = c(0, 10.25)) }
  if (metric == "Number Owned") { plot <- plot + scale_y_continuous(labels = scales::comma) }
  if (metric == "Number Owned" & group == "mechanic") { plot <- plot + scale_y_continuous(limits = c(0, 12500000), labels = scales::comma) }
  if (n >= 40) {
    plot <- plot + theme(axis.text.y = element_text(size = 12))
    if (n >= 45) { plot <- plot + theme(axis.text.y = element_text(size = 10)) }
  }
  plot
}

plot_groups_over_time <- function(expanded_details, group, metric, n, feature, remove_outliers, years, plot_type, add_line, add_curve, agg_metric, sort, label) {
  # create plotting data
  # filter the data based on the specified year range
  expanded_details <- filter(expanded_details, yearpublished >= years[1], yearpublished <= years[2])
  plotting_data <- filter_top_n_levels(expanded_details, group, metric, n, feature, remove_outliers, TRUE, sort)
  
  # create one of the two possible plots: scatterplot or heat map
  plot <- ggplot(plotting_data, aes(x = yearpublished)) + theme_bw()
  # create a scatterplot
  if (plot_type == "Scatterplot") {
    plot <- plot + geom_jitter(aes(y = value, color = get(group)), alpha = 0.25, height = 0.5, width = 0.5) +
      facet_wrap(~get(group)) +
      theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), 
            strip.text = element_text(size = 14), legend.position = "none") +
      labs(x = "Year Published", y = label)
    # add a linear regression line
    if (add_line) { plot <- plot + geom_smooth(aes(x = yearpublished, y = value), method = "lm", se = FALSE, color = "blue") }
    # add a smooth curve
    if (add_curve) { plot <- plot + geom_smooth(aes(x = yearpublished, y = value), method = "gam", se = FALSE, color = "red")}
    if (n >= 15) {
      plot <- plot + theme(axis.text = element_text(size = 12))
      if (n >= 20) { plot <- plot + theme(axis.text = element_text(size = 10)) }
    }
  }
  # create a heat map
  if (plot_type == "Heat Map") {
    if (agg_metric == "Mean") { plotting_data <- plotting_data %>% group_by(yearpublished, get(group)) %>% summarize(value = mean(value)) }
    if (agg_metric == "Median") { plotting_data <- plotting_data %>% group_by(yearpublished, get(group)) %>% summarize(value = median(value)) }
    names(plotting_data) <- c("yearpublished", group, "value")
    plot <- plot + geom_tile(data = plotting_data, aes(y = get(group), fill = value)) +
      coord_fixed() +
      scale_y_discrete(limits = rev) + 
      scale_fill_viridis_c(option = "inferno") +
      labs(x = "Year Published", fill = label) +
      theme(axis.title.x = element_text(size = 16), axis.text = element_text(size = 14), axis.title.y = element_blank(),
            legend.position = "top", legend.title = element_text(size = 16), legend.text = element_text(size = 14), legend.key.width = unit(2, "cm"))
    if (n >= 40) {
      plot <- plot + theme(axis.text.y = element_text(size = 12))
      if (n >= 45) { plot <- plot + theme(axis.text.y = element_text(size = 10)) }
    }
  }
  plot
}

plot_games_over_time <- function(details, feature, remove_outliers, years, plot_type, add_line, add_curve, year_bin_size, label) {
  # create plotting data
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
    if (add_line) { plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1.5) }
    # add a smooth curve
    if (add_curve) { plot <- plot + geom_smooth(method = "gam", se = FALSE, color = "red", linewidth = 1.5) }
  }
  # create a ridgeline plot
  if (plot_type == "Ridgeline Plot") {
    plotting_data$yearpublished <- plotting_data$yearpublished - plotting_data$yearpublished %% year_bin_size
    plot <- ggplot(plotting_data, aes(y = as.factor(yearpublished), x = get(feature), fill = after_stat(x))) +
      geom_density_ridges_gradient(scale = 3, lwd = 1.25) +
      scale_fill_viridis_c(option = "inferno") +
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