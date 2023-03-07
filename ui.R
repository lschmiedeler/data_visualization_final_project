library(shiny)
library(bslib)

features <- c("Average Rating", "Average Complexity", "Number Owned",
              "Year Published", "Minimum Number of Players", "Maximum Number of Players",
              "Minimum Playing Time", "Maximum Playing Time", "Minimum Age")

fluidPage(
  navbarPage(
    theme = bs_theme(bootswatch = "zephyr"),
    "Board Game Exploration",
    tabPanel(
      "Home",
      h4("Board Game Exploration"),
      HTML("<em>All of the data used to create this application comes from the <b>BoardGameGeek Reviews</b> dataset on Kaggle.</em>"),
      br(), br(),
      HTML("The <b>games_detailed_info.csv</b> file in this dataset contains information about 21,631 board games.
           The following 15 features are a subset of the features in this file and are used in this dashboard:
           <ul>
           <li> <b>Name:</b> the name of the board game </li>
           <li> <b>Image:</b> an image of the board game </li>
           <li> <b>Description:</b> a description of the board game</li>
           <li> <b>Average Rating:</b> the average user rating (from 0 to 10) </li>
           <li> <b>Average Complexity:</b> the average user complexity rating (measures how difficult the game is to understand on a scale from 0 to 5) </li> 
           <li> <b>Number Owned:</b> the number of BoardGameGeek users who own the game </li>
           <li> <b>Year Published:</b> the year of the board game's publication </li>
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
      tabPanel(
        "Select a Game",
        fluidRow(
          column(
            4, selectizeInput("game_id", h4("Select a Game"), choices = NULL),
            h5("Selected Game"), htmlOutput("selected_game_1"), htmlOutput("game_image")
          ),
          column(3, h4("Popular Games"), tableOutput("popular_games")),
          column(5, h4("Highest Rated Popular Games"), tableOutput("highest_rated_popular_games"))
        )
      ),
      tabPanel(
        "View Game Information",
        h4("View Game Information"),
        hr(), h5("Selected Game"), htmlOutput("selected_game_2"),
        hr(), h5("Description"), htmlOutput("selected_game_description"),
        hr(), fluidRow(
          column(
            6, h5("Feature Values"), tableOutput("game_details_float"), tableOutput("game_details_int")
          ),
          column(
            6, h5("Categories, Mechanics, and Designers"), tableOutput("game_categories"),
            tableOutput("game_mechanics"), tableOutput("game_designers")
          )
        )
      ),
      tabPanel(
        "Compare Game to All Games",
        h4("Compare Game to All Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_3"), 
            em("The feature value associated with the selected game is represented by a black dashed line."),
            hr(), selectizeInput("feature_1", strong("Select Features"), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_1", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_1", strong("Select a Plot Type"), choices = list("Boxplot", "Histogram", "Density Plot")),
          ),
          mainPanel(plotOutput("all_game_comparison", height = 700))
        )
      ),
      tabPanel(
        "Compare Game to Similar Games",
        h4("Compare Game to Similar Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_4"), 
            em("The feature value associated with the selected game is represented by a black dashed line."),
            hr(),
            radioButtons("group_1", strong("Filter by"), choices = list("Category" = "category", "Mechanic" = "mechanic")),
            selectizeInput("feature_2", strong("Select Features"), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_2", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_2", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
          ),
          mainPanel(plotOutput("similar_game_comparison", height = 850))
        )
      )
    ),
    navbarMenu(
      "Anaylze Groups of Games",
      tabPanel(
        "View and Plot Category, Mechanic, and Designer Information",
        h4("View and Plot Category, Mechanic, and Designer Information"),
        hr(), selectizeInput("group_2", strong("Select a Group"), choices = 3),
        selectizeInput("level_1", strong("Select a Level"), choices = 100),
        htmlOutput("number_of_games"),
        br(), hr(), h5("Plot Feature Values"),
        sidebarLayout(
          sidebarPanel(
            selectizeInput("feature_3", strong("Select Features"), choices = NULL, multiple = TRUE),
            radioButtons("remove_extreme_values_3", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_3", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot"))
          ),
          mainPanel(
            plotOutput("level_data", height = 700)
          )
        ),
        br(), br(), hr(), fluidRow(
          column(4, h5("Popular Games"), tableOutput("popular_games_in_level")),
          column(4, h5("Highest Rated Games"), tableOutput("highest_rated_games_in_level")),
          column(4, h5("Highest Rated Popular Games"), tableOutput("highest_rated_popular_games_in_level"))
        )
      ),
      tabPanel(
        "View Top Categories, Mechanics, and Designers",
        h4("View Top Categories, Mechanics, and Designers"),
        sidebarLayout(
          sidebarPanel(
            radioButtons("group_3", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            radioButtons(
              "metric_1", strong("Select a Metric to Determine the Top Levels of the Group"),
              choices = list("Number of Different Games", "Number Owned", "Average Rating")
            ),
            sliderInput("n_1", strong("Select a Limit for the Number of Top Levels"), min = 1, max = 50, value = 10)
          ),
          mainPanel(plotOutput("top_bar_chart", height = 600))
        )
      ),
      tabPanel(
        "Compare Top Categories, Mechanics, and Designers",
        h4("Compare Top Categories, Mechanics, and Designers"),
        hr(), fluidRow(
          column(
            6,
            radioButtons("group_4", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            radioButtons(
              "metric_2", strong("Select a Metric to Determine the Top Levels of the Group"), 
              choices = list("Number of Different Games", "Number Owned", "Average Rating"),
              width = "100%",
            ),
            sliderInput("n_2", strong("Select a Limit for the Number of Top Levels"), min = 1, max = 50, value = 10, width = "75%")
          ),
          column(
            3,
            radioButtons("feature_4", strong("Select a Feature"), choices = features),
            radioButtons("remove_extreme_values_4", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          column(
            3,
            radioButtons("plot_type_4", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
            radioButtons("sort_1", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("top_levels_comparison", height = 750)
      ),
      tabPanel(
        "Plot Top Categories and Mechanics Over Time",
        h4("Plot Top Categories and Mechanics Over Time"),
        hr(), fluidRow(
          column(
            5,
            radioButtons("group_5", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic")),
            radioButtons(
              "metric_3", strong("Select a Metric to Determine the Top Levels of the Group"), 
              choices = list("Number of Different Games", "Number Owned", "Average Rating"), width = "100%"
            ),
            sliderInput("n_3", strong("Select a Limit for the Number of Top Levels"), min = 1, max = 50, value = 10, width = "75%"),
            sliderInput("years_1", strong("Select a Year Range"), min = 1950, max = 2023, value = c(2000, 2020), width = "75%")
          ),
          column(
            3,
            radioButtons("feature_5", strong("Select a Feature"), choices = features[features != "Year Published"]),
            radioButtons("remove_extreme_values_5", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("plot_type_5", strong("Select a Plot Type"), choices = list("Scatterplot", "Heat Map")),
          ),
          column(
            4,
            radioButtons("add_line_1", strong("Add (Black) Lines to Scatterplots?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("add_curve_1", strong("Add (Blue) Curves to Scatterplots?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("agg_metric", strong("Select an Aggregation Metric for the Heatmap"), choices = c("Mean", "Median")),
            radioButtons("sort_2", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("groups_over_time", height = 750)
      )
    ),
    navbarMenu(
      "Analyze All Games",
      tabPanel(
        "Filter and View Games",
        h4("Filter and View Games"),
        sidebarLayout(
          sidebarPanel(
            "sidebar panel"
          ),
          mainPanel(
            "main panel"
          )
        )
        # overall summary of data
        # include table and plots (boxplot and density plot)
        
        # data table
        # filter on feature values and display result
        # ability to then sort on a feature and reverse sort
        # select how many rows are shown
      ),
      tabPanel(
        "Plot Games Over Time",
        h4("Plot Games Over Time"),
        hr(), fluidRow(
          column(
            3,
            radioButtons("feature_6", strong("Select a Feature"), choices = features),
            radioButtons("remove_extreme_values_6", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          column(
            5,
            sliderInput("years_2", strong("Select a Year Range"), min = 1950, max = 2023, value = c(2000, 2020), width = "75%"),
            sliderInput("year_bin_size_1", strong("Select a Year Bin Size for the Ridgeline Plot"), min = 1, max = 10, value = 2, width = "75%")
          ),
          column(
            4,
            radioButtons("plot_type_6", strong("Select a Plot Type"), choices = list("Scatterplot", "Ridgeline Plot")),
            radioButtons("add_line_2", strong("Add (Black) Line to Scatterplot?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            radioButtons("add_curve_2", strong("Add (Blue) Curve to Scatterplot?"), choices = list("No" = FALSE, "Yes" = TRUE))
          )
        ),
        hr(), plotOutput("games_over_time", height = 750)
      )
    )
  )
)
