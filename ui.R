library(shiny)
library(shinyWidgets)

features <- c("Average Rating", "Average Complexity", "Number Owned",
              "Year Published", "Minimum Number of Players", "Maximum Number of Players",
              "Minimum Playing Time", "Maximum Playing Time", "Minimum Age")

fluidPage(
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "zephyr"),
    "Board Game Exploration",
    tabPanel(
      "Home",
      h4("Board Game Exploration"),
      HTML("<em>All of the data used to create this application comes from the <a href = 'https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews'><b>BoardGameGeek Reviews</b></a> dataset on Kaggle.</em>"),
      br(), br(),
      HTML("The <a href = 'https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews?select=games_detailed_info.csv'><b>games_detailed_info.csv</b></a> file in this dataset contains information about 21,631 different board games.
           The following 15 features are a subset of the features in this file and are used in this application:
           <ul>
           <li> <b>Name:</b> the name of the game </li>
           <li> <b>Image:</b> an image of the game </li>
           <li> <b>Description:</b> a description of the game</li>
           <li> <b>Average Rating:</b> the average user rating of the game (from 0 to 10) </li>
           <li> <b>Average Complexity:</b> the average user complexity rating of the game (measures how difficult the game is to understand on a scale from 0 to 5) </li> 
           <li> <b>Number Owned:</b> the number of BoardGameGeek users who own the game </li>
           <li> <b>Year Published:</b> the year of the game's publication </li>
           <li> <b>Minimum Number of Players:</b> the minimum number of players that can play the game</li>
           <li> <b>Maximum Number of Players:</b> the maximum number of players that can play the game</li>
           <li> <b>Minimum Playing Time:</b> the minimum playing time of the game (in minutes) </li>
           <li> <b>Maximum Playing Time:</b> the maximum playing time of the game (in minutes) </li>
           <li> <b>Minimum Age:</b> the recommended minimum age for playing the game </li>
           <li> <b>Categories:</b> the categories to which the game belongs </li>
           <li> <b>Mechanics:</b> the mechanics employed by the game </li>
           <li> <b>Designers:</b> the designers of the game </li>
           </ul>"),
    ),
    navbarMenu(
      "Analyze One Game",
      tabPanel("Step 1: Select a Game",
        fluidRow(
          column(
            4, selectizeInput("game_id", HTML("<h4>Select a Game</h4> To search, remove the selected game by hitting backspace and then type the name of the game you want to analyze."), choices = NULL),
            htmlOutput("selected_game_1"), htmlOutput("game_image_large")
          ),
          column(
            4, h4("Popular Games"), numericInput("n_popular_all", strong("Select an Output Limit [1, 500]"), min = 1, max = 500, value = 25),
            "", tableOutput("popular_games")
          ),
          column(
            4, h4("Highest Rated Popular Games"), numericInput("n_highest_rated_popular_all", strong("Select an Output Limit [1, 500]"), min = 1, max = 500, value = 25),
            "", tableOutput("highest_rated_popular_games")
          )
        )
      ),
      tabPanel("Step 2: View Game Information", h4("View Game Information"),
        hr(), h5("Selected Game"), htmlOutput("selected_game_2"), htmlOutput("game_image_small"),
        hr(), h5("Description"), htmlOutput("selected_game_description"),
        hr(), h5("Link to BoardGameGeeks Page"), htmlOutput("selected_game_link"),
        hr(), fluidRow(
          column(6, h5("Feature Values"), tableOutput("game_details_float"), tableOutput("game_details_int")),
          column(
            6, h5("Categories, Mechanics, and Designers"), tableOutput("game_categories"),
            tableOutput("game_mechanics"), tableOutput("game_designers")
          )
        )
      ),
      tabPanel("Step 3: Compare Game to All Games", h4("Compare Game to All Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_3"), 
            em("The feature value associated with the selected game is represented by a dashed line."),
            hr(), selectizeInput("feature_1", HTML("<b>Select Features</b><br>To add a feature, select it from the list.  To remove a feature, select it and hit backspace."), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_1", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_1", strong("Select a Plot Type"), choices = list("Boxplot", "Histogram", "Density Plot")),
          ),
          mainPanel(plotOutput("all_game_comparison", height = 700))
        )
      ),
      tabPanel("Step 4: Compare Game to Similar Games", h4("Compare Game to Similar Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_4"), 
            em("The feature value associated with the selected game is represented by a dashed line."),
            hr(),
            radioButtons("group_1", strong("Filter by"), choices = list("Category" = "category", "Mechanic" = "mechanic")),
            selectizeInput("feature_2", HTML("<b>Select Features</b><br>To add a feature, select it from the list.  To remove a feature, select it and hit backspace."), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_2", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_2", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
          ),
          mainPanel(plotOutput("similar_game_comparison", height = 800))
        )
      )
    ),
    navbarMenu(
      "Analyze Groups of Games",
      tabPanel(
        "View Top Categories, Mechanics, and Designers",
        h4("View Top Categories, Mechanics, and Designers"),
        sidebarLayout(
          sidebarPanel(
            radioButtons("group_3", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            radioButtons("metric_1", htmlOutput("group_label_2"), choices = list("Number of Different Games", "Total Number Owned")),
            sliderInput("n_1", htmlOutput("group_label_3"), min = 1, max = 50, value = 10)
          ),
          mainPanel(plotOutput("top_bar_chart", height = 700))
        )
      ),
      tabPanel(
        "Compare Top Categories, Mechanics, and Designers",
        h4("Compare Top Categories, Mechanics, and Designers"),
        hr(), fluidRow(
          column(
            6, radioButtons("group_4", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            radioButtons("metric_2", htmlOutput("group_label_4"), choices = list("Number of Different Games", "Total Number Owned"), width = "100%"),
            sliderInput("n_2", htmlOutput("group_label_5"), min = 1, max = 50, value = 10, width = "75%")
          ),
          column(
            3, radioButtons("feature_4", strong("Select a Feature"), choices = features),
            radioButtons("remove_extreme_values_4", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          column(
            3, radioButtons("plot_type_4", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
            radioButtons("sort_1", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("top_levels_comparison", height = 800)
      ),
      tabPanel(
        "View Category, Mechanic, and Designer Information",
        h4("View Category, Mechanic, and Designer Information"),
        hr(), selectizeInput("group_2", strong("Select a Group"), choices = 3),
        selectizeInput("level_1", htmlOutput("group_label_1"), choices = 100),
        htmlOutput("number_of_games"),
        br(), hr(), h5("Plot Feature Values"),
        sidebarLayout(
          sidebarPanel(
            selectizeInput("feature_3", HTML("<b>Select Features</b><br>To add a feature, select it from the list.  To remove a feature, select it and hit backspace."), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_3", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_3", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot"))
          ),
          mainPanel(
            plotOutput("level_comparison", height = 700)
          )
        ),
        br(), br(), hr(), fluidRow(
          column(4, h5("Popular Games"), numericInput("n_popular", strong("Select an Output Limit [1, 500]"), min = 1, max = 500, value = 25),
                 "", tableOutput("popular_games_in_level")),
          column(4, h5("Highest Rated Games"), numericInput("n_highest_rated", strong("Select an Output Limit [1, 500]"), min = 1, max = 500, value = 25),
                 "", tableOutput("highest_rated_games_in_level")),
          column(4, h5("Highest Rated Popular Games"), numericInput("n_highest_rated_popular", strong("Select an Output Limit [1, 500]"), min = 1, max = 500, value = 25),
                 "", tableOutput("highest_rated_popular_games_in_level")),
        )
      ),
      tabPanel(
        "Filter Games by Categories and Mechanics",
        h4("Filter Games by Categories and Mechanics"),
        sidebarLayout(
          sidebarPanel(
            selectizeInput("categories", HTML("<b>Select Categories</b><br>To add a category, select it from the list.  To remove a category, select it and hit backspace."), choices = NULL, multiple = TRUE),
            selectizeInput("mechanics", HTML("<b>Select Mechanics</b><br>To add a mechanic, select it from the list.  To remove a mechanic, select it and hit backspace."), choices = NULL, multiple = TRUE),
            numericInput("n_games_1", strong("Select an Output Limit [1, 1000]"), min = 1, max = 1000, value = 100),
            radioButtons("sort_feature_1", strong("Sort by"), choices = c("Average Rating", "Average Complexity", "Number Owned", "Year Published")),
            htmlOutput("total_number_of_games_in_groups")
          ),
          mainPanel(tableOutput("top_games_in_groups"))
        )
      ),
      tabPanel(
        "Plot Top Categories and Mechanics Over Time",
        h4("Plot Top Categories and Mechanics Over Time"),
        hr(), fluidRow(
          column(
            5, radioButtons("group_5", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic")),
            radioButtons("metric_3", htmlOutput("group_label_6"), choices = list("Number of Different Games", "Total Number Owned"), width = "100%"),
            sliderInput("n_3", htmlOutput("group_label_7"), min = 1, max = 50, value = 10, width = "75%"),
            sliderInput("years_1", strong("Select a Year Range"), min = 1950, max = 2023, value = c(2000, 2020), width = "75%")
          ),
          column(
            3, radioButtons("feature_5", strong("Select a Feature"), choices = features[features != "Year Published"]),
            radioButtons("remove_extreme_values_5", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_5", strong("Select a Plot Type"), choices = list("Scatterplot", "Heat Map")),
          ),
          column(
            4, radioButtons("add_line_1", strong("Add (Blue) Lines to Scatterplots?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("add_curve_1", strong("Add (Purple) Curves to Scatterplots?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("agg_metric", strong("Select an Aggregation Metric for the Heatmap"), choices = c("Mean", "Median")),
            radioButtons("sort_2", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("groups_over_time", height = 900)
      )
    ),
    navbarMenu(
      "Analyze All Games",
      tabPanel(
        "Filter Games by Feature Values",
        h4("Filter Games by Feature Values"),
        hr(), fluidRow(
          column(
            4, numericRangeInput("rating_range", strong("Average Rating Range [1, 10]"), min = 1, max = 10, value = c(1, 10), width = "90%"),
            numericRangeInput("complexity_range", strong("Average Complexity Range [1, 5]"), min = 1, max = 5, value = c(1, 5), width = "90%"),
            numericRangeInput("owned_range", strong("Number Owned Range [1, 175,000]"), min = 1, max = 175000, value = c(1, 175000), width = "90%"),
            numericRangeInput("year_range", strong("Year Published Range [-3500, 2023]"), min = -3500, max = 2023, value = c(-3500, 2023), width = "90%")
          ),
          column(
            4, numericRangeInput("minplayers_range", strong("Minimum Number of Players Range [1, 10]"), min = 1, max = 10, value = c(1, 10), width = "90%"),
            numericRangeInput("maxplayers_range", strong("Maximum Number of Players Range [1, 100]"), min = 1, max = 100, value = c(1, 100), width = "90%"),
            numericRangeInput("minplay_range", strong("Minimum Playing Time Range [1, 60000]"), min = 1, max = 60000, value = c(1, 60000), width = "90%"),
            numericRangeInput("maxplay_range", strong("Maximum Playing Time Range [1, 60000]"), min = 1, max = 60000, value = c(1, 60000), width = "90%")
          ),
          column(
            4, numericRangeInput("minage_range", strong("Minimum Age Range [1, 25]"), min = 1, max = 25, value = c(1, 25), width = "90%"),
            numericInput("n_games_2", strong("Select an Output Limit [1, 1000]"), min = 1, max = 1000, value = 100),
            selectInput("sort_feature_2", strong("Sort by"), choices = features),
            radioButtons("reverse_sort", strong("Reverse Sort?"), choices = list("No" = FALSE, "Yes" = TRUE)),
          )
        ),
        hr(), htmlOutput("total_number_of_games"), tableOutput("top_filtered_games")
      ),
      tabPanel(
        "Plot Games Over Time",
        h4("Plot Games Over Time"),
        hr(), fluidRow(
          column(
            3, radioButtons("feature_6", strong("Select a Feature"), choices = features[features != "Year Published"]),
            radioButtons("remove_extreme_values_6", strong("Remove Feature Extreme Values?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          column(
            5, sliderInput("years_2", strong("Select a Year Range"), min = 1950, max = 2023, value = c(2000, 2020), width = "75%"),
            sliderInput("year_bin_size_1", strong("Select a Year Bin Size for the Ridgeline Plot"), min = 1, max = 10, value = 2, width = "75%")
          ),
          column(
            4, radioButtons("plot_type_6", strong("Select a Plot Type"), choices = list("Scatterplot", "Ridgeline Plot")),
            radioButtons("add_line_2", strong("Add (Blue) Line to Scatterplot?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("add_curve_2", strong("Add (Purple) Curve to Scatterplot?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("games_over_time", height = 800)
      )
    )
  )
)
