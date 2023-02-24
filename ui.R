library(shiny)
library(shinythemes)

fluidPage(
  navbarPage(
    theme = shinytheme("yeti"),
    "Board Game Data Dashboard",
    tabPanel(
      "Home",
      HTML("All of the data used to create this dashboard comes from the <b>BoardGameGeek Reviews</b> dataset on Kaggle."),
      br(), br(),
      HTML("The <b>games_detailed_info.csv</b> file in this dataset contains information about 21,631 board games.
            The file contains 56 features, but only the following 10 features are used in this dashboard:
           <ul>
           <li> <b>Name:</b> name of the board game </li>
           <li> <b>Year Published:</b> year of the board game's publication </li>
           <li> <b>Playing Time:</b> average playing time in minutes</li>
           <li> <b>Minimum Age:</b> recommended minimum age for playing the game </li>
           <li> <b>Average Rating:</b> average user rating (from 0 to 10) </li>
           <li> <b>Average Complexity:</b> average user complexity rating (measures how difficult the game is to understand on a scale from 0 to 5) </li> 
           <li> <b>Number Owned:</b> the number of BoardGameGeek users who own the game </li>
           <li> <b>Categories:</b> the categories to which the game belongs </li>
           <li> <b>Mechanics:</b> the mechanics employed by the game </li>
           <li> <b>Designers:</b> the designers of the game </li>
           </ul>"),
    ),
    navbarMenu(
      "Analyze One Game",
      tabPanel(
        "Select a Game",
        column(
          3, selectizeInput("game_id", h4("Select a Game"), choices = 100),
          em("If you do not see the game you want to analyze, search for it."),
          br(), br(), htmlOutput("selected_game_1")
        ),
        column(3, h4("Popular Games"), tableOutput("popular_games")),
        column(6, h4("Highest Rated Popular Games"), tableOutput("highest_rated_popular_games"))
      ),
      tabPanel(
        "View Game Information",
        h4("View Game Information"),
        tableOutput("game_details"),
        tableOutput("game_categories"),
        tableOutput("game_mechanics"),
        tableOutput("game_designers")
      ),
      tabPanel(
        "Compare Game to All Games",
        h4("Compare Game to All Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_2"), 
            htmlOutput("selected_feature_1"),
            em("The feature value associated with the selected game is represented by a black dashed line."),
            br(), br(),
            selectInput(
              "feature_1", strong("Select a Feature"),
              choices = list("Average Rating" = "average", "Average Complexity" = "averageweight", "Number Owned" = "owned",
                             "Playing Time" = "playingtime", "Minimum Age" = "minage", "Year Published" = "yearpublished")
            ),
            radioButtons("remove_outliers_1", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            selectInput("plot_type_1", strong("Select a Plot Type"), choices = list("Boxplot", "Density Plot")),
          ),
          mainPanel(plotOutput("all_game_comparison", height = 600))
        )
      ),
      tabPanel(
        "Compare Game to Similar Games",
        h4("Compare Game to Similar Games"),
        sidebarLayout(
          sidebarPanel(
            htmlOutput("selected_game_3"), 
            htmlOutput("selected_feature_2"),
            em("The feature value associated with the selected game is represented by a black dashed line."),
            br(), br(),
            selectInput("group_1", strong("Filter by"), choices = list("Category" = "category", "Mechanic" = "mechanic")),
            selectInput(
              "feature_2", strong("Select a Feature"),
              choices = list("Average Rating" = "average", "Average Complexity" = "averageweight", "Number Owned" = "owned",
                             "Playing Time" = "playingtime", "Minimum Age" = "minage", "Year Published" = "yearpublished")
            ),
            radioButtons("remove_outliers_2", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            selectInput("plot_type_2", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
            radioButtons("sort_1", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          mainPanel(plotOutput("similar_game_comparison", height = 600))
        )
      )
    ),
    navbarMenu(
      "Anaylze Groups of Games",
      tabPanel(
        "View Category, Mechanic, and Designer Information",
        h4("View Category, Mechanic, and Designer Information")
        # data table
        # select a group: categories, mechanics, or designers
        # select a level within the group
        # view information about the level (total number of games owned, top games, and average feature values)
      ),
      tabPanel(
        "View Top Categories, Mechanics, and Designers",
        h4("View Top Categories, Mechanics, and Designers"),
        sidebarLayout(
          sidebarPanel(
            selectInput("group_2", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            selectInput(
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
        sidebarLayout(
          sidebarPanel(
            selectInput("group_3", strong("Select a Group"), choices = list("Categories" = "category", "Mechanics" = "mechanic", "Designers" = "designer")),
            selectInput(
              "metric_2", strong("Select a Metric to Determine the Top Levels of the Group"), 
              choices = list("Number of Different Games", "Number Owned", "Average Rating")
            ),
            sliderInput("n_2", strong("Select a Limit for the Number of Top Levels"), min = 1, max = 50, value = 10),
            selectInput(
              "feature_3", strong("Select a Feature"),
              choices = list("Average Rating" = "average", "Average Complexity" = "averageweight", "Number Owned" = "owned",
                             "Playing Time" = "playingtime", "Minimum Age" = "minage", "Year Published" = "yearpublished")
            ),
            radioButtons("remove_outliers_3", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            selectInput("plot_type_3", strong("Select a Plot Type"), choices = list("Boxplot", "Violin Plot", "Density Plot", "Ridgeline Plot")),
            radioButtons("sort_2", strong("Sort by Median Feature Value?"), choices = list("No" = FALSE, "Yes" = TRUE))
          ),
          mainPanel(plotOutput("top_levels_comparison", height = 600))
        )
      ),
      tabPanel(
        "Plot Top Categories and Mechanics Over Time",
        h4("Plot Top Categories and Mechanics Over Time"),
        sidebarLayout(
          sidebarPanel(
            "sidebar panel"
          ),
          mainPanel(
            "main panel"
          )
        )
        # heat map
        # select a group: categories or mechanics
        # select a metric: average rating, average complexity, others?
        # select a limit: from 1-50
        # select a feature: number owned, average rating, average weight, others?
        # remove feature outliers: yes or no
        # select a year range: from 1950-2023
        # select a year bin size: 1-10
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
        # data table
        # filter on feature values
      ),
      tabPanel(
        "Plot Games Over Time",
        h4("Plot Games Over Time"),
        sidebarLayout(
          sidebarPanel(
            em("Note that these plots often take few seconds to load."),
            br(), br(),
            selectInput(
              "feature_4", strong("Select a Feature"),
              choices = list("Average Rating" = "average", "Average Complexity" = "averageweight", "Number Owned" = "owned",
                             "Playing Time" = "playingtime", "Minimum Age" = "minage")
            ),
            radioButtons("remove_outliers_4", strong("Remove Feature Outliers?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            sliderInput("years", strong("Select a Year Range"), min = 1950, max = 2023, value = c(2000, 2023)),
            selectInput("plot_type_4", strong("Select a Plot Type"), choices = list("Scatterplot", "Ridgeline Plot")),
            radioButtons("add_line", strong("Add Line to Scatterplot?"), choices = list("No" = FALSE, "Yes" = TRUE)),
            sliderInput("year_bin_size", strong("Select a Year Bin Size for the Ridgeline Plot"), min = 1, max = 10, value = 1)
          ),
          mainPanel(plotOutput("games_over_time", height = 600))
        )
      )
    )
  )
)
