library(shiny)
library(shinydashboard)
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(tidytext)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(corrplot)
library(scales)
library(tidyr)


# Load the data
tuesdata <- tidytuesdayR::tt_load('2024-10-29')

# Assign data to variables
monster_movie_genres <- tuesdata$monster_movie_genres
monster_movies <- tuesdata$monster_movies

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(    title = HTML('<span style="font-size: 18px; white-space:nowrap; margin-right:20px; color: white; font-family: Arial, sans-serif; font-weight: bold;">Monster Movie Analysis</span>')
),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Genre Distribution", tabName = "genre", icon = icon("bar-chart")),
      menuItem("Rating Distribution", tabName = "rating", icon = icon("bar-chart")),
      menuItem("Top Monster Movies", tabName = "top_movies", icon = icon("film")),
      menuItem("Yearly Trends", tabName = "yearly", icon = icon("line-chart")),
      menuItem("Rating by Genre", tabName = "rating_genre", icon = icon("star")),
      menuItem("Vote vs Rating", tabName = "vote_rating", icon = icon("thumbs-up")),
      menuItem("Rating Distribution by Genre", tabName = "rating_boxplot", icon = icon("box")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Correlation Matrix", tabName = "correlation", icon = icon("cogs")),
      menuItem("Model Predictions", tabName = "prediction", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "genre",
              fluidRow(
                box(title = "Most Common Genres for Monster Movies", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("genre_plot"))
              )
      ),
      tabItem(tabName = "rating",
              fluidRow(
                box(title = "Average Rating for Monster Movies", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("rating_plot"))
              )
      ),
      tabItem(tabName = "top_movies",
              fluidRow(
                box(title = "Top Monster Movies", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("top_movies_table"))
              )
      ),
      tabItem(tabName = "yearly",
              fluidRow(
                box(title = "Count of Monster Movies Over Time", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("yearly_plot"))
              )
      ),
      tabItem(tabName = "rating_genre",
              fluidRow(
                box(title = "Average Rating by Genre", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("rating_genre_plot"))
              )
      ),
      tabItem(tabName = "vote_rating",
              fluidRow(
                box(title = "Votes vs Rating for Monster Movies", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("vote_rating_plot"))
              )
      ),
      tabItem(tabName = "rating_boxplot",
              fluidRow(
                box(title = "Rating Distribution by Genre", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("rating_boxplot_plot"))
              )
      ),
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Common Words used in Monster Movies Titles", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("wordcloud_plot"))
              )
      ),
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Correlation Matrix", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("correlation_plot")),
                box(title = "Explanation", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("correlation_explanation"))
              )
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Predict Movie Rating", status = "primary", solidHeader = TRUE, width = 12,
                    numericInput("runtime", "Enter Runtime (Minutes):", value = 100, min = 1),
                    numericInput("votes", "Enter Number of Votes:", value = 5000, min = 1),
                    actionButton("predict_button", "Predict Rating"),
                    verbatimTextOutput("prediction_result"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #genre distribution
  output$genre_plot <- renderPlot({
    sorted_genre <- monster_movie_genres %>%
      count(genres, sort = TRUE) %>%
      filter(!is.na(genres))
    
    ggplot(sorted_genre, aes(x = reorder(genres, n), y = n)) +
      geom_bar(stat = "identity", fill = "#191970") +
      coord_flip() + 
      labs(title = "",
           x = "Genre", y = "Count of Title")
  })
  
  #avgg rating distribution
  output$rating_plot <- renderPlot({
    ggplot(monster_movies, aes(x = average_rating)) +
      geom_histogram(binwidth = 0.5, fill = "#191970") +
      labs(title = "",
           x = "Average Rating", y = "Count of Movies")
  })
  
  #top 5 movies
  output$top_movies_table <- renderDT({
    top_monster_movies <- monster_movies %>%
      arrange(desc(num_votes)) %>%
      select(primary_title, runtime_minutes, genres, year, num_votes, average_rating) %>%
      head(5) 
    
    # Ensure proper column names for clarity
    colnames(top_monster_movies) <- c("Title", "Runtime (Minutes)", "Genres", "Year", "Votes", "Rating")
    
    datatable(
      top_monster_movies,
      options = list(
        pageLength = 5,  # Ensure only 5 rows are displayed
        autoWidth = TRUE, 
        searching = FALSE, 
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
  
  #yearly trend movies
  output$yearly_plot <- renderPlot({
    yearly_monster_movies <- monster_movies %>%
      count(year) %>%
      filter(!is.na(year))
    
    ggplot(yearly_monster_movies, aes(x = year, y = n)) +
      geom_line(color = "black") +
      geom_point(color = "#191970") +
      labs(title = "",
           x = "Year", y = "Count of Movies")
  })
  
  #avg rating by genre
  output$rating_genre_plot <- renderPlot({
    monster_movies_split_genre <- monster_movies %>%
      separate_rows(genres, sep = ",")
    
    genres_rating <- monster_movie_genres %>%
      inner_join(monster_movies_split_genre, by = "tconst", relationship = "many-to-many") %>%
      group_by(genres.x) %>%
      summarise(avg_rating = mean(average_rating, na.rm = TRUE), count = n()) %>%
      arrange(desc(avg_rating)) %>%
      filter(!is.na(genres.x))
    
    ggplot(genres_rating, aes(x = reorder(genres.x, avg_rating), y = avg_rating)) +
      geom_bar(stat = "identity", fill = "#191970") +
      coord_flip() +
      labs(title = "",
           x = "Genre", y = "Average Rating")
  })
  
  #vote vs rating
  output$vote_rating_plot <- renderPlot({
    ggplot(monster_movies, aes(x = num_votes, y = average_rating)) +
      geom_point(color = "#191970") +
      labs(title = "",
           x = "Number of Votes", y = "Average Rating")+
      scale_x_log10(labels = label_number())
  })
  
  #boxplot distribution of rating
  output$rating_boxplot_plot <- renderPlot({
    monster_movies_split_genre <- monster_movies %>%
      separate_rows(genres, sep = ",")
    
    ggplot(monster_movies_split_genre, aes(x = genres, y = average_rating)) +
      geom_boxplot(fill = "lightblue", color = "#191970") +
      theme_minimal() +
      coord_flip() +
      labs(title = "",
           x = "Genre", y = "Average Rating") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })
  
  #wordcloud
  output$wordcloud_plot <- renderPlot({
    stop_words <- stopwords("en")
    common_words_title <- monster_movies %>%
      select(primary_title) %>%
      unnest_tokens(word, primary_title)
    
    common_exclude_monster <- common_words_title %>%
      filter(!(word %in% c("monster", "monsters", stop_words))) %>%
      count(word, sort = TRUE)
    
    
    wordcloud::wordcloud(words = common_exclude_monster$word,
                         freq = common_exclude_monster$n,
                         min.freq = 1,
                         max.words = 100,
                         random.order = FALSE,
                         colors = brewer.pal(8, "Dark2"))
  })
  
  #correlation matrix
  output$correlation_plot <- renderPlot({
    movie_data <- monster_movies %>%
      select(runtime_minutes, num_votes, average_rating)
    
    corr_matrix <- cor(movie_data, use = "complete.obs")
    corrplot(corr_matrix, method = "circle", type = "lower", tl.col = "black", tl.cex = 1.2)
  })
  output$correlation_explanation <- renderPrint({
    movie_data <- monster_movies %>%
      select(runtime_minutes, num_votes, average_rating)
    
    corr_matrix <- cor(movie_data, use = "complete.obs")
    
    explanation <- c(
      
      paste("1. Runtime vs Number of Votes: Correlation =", round(corr_matrix[1, 2], 2)),
      paste("2. Runtime vs Average Rating: Correlation =", round(corr_matrix[1, 3], 2)),
      paste("3. Number of Votes vs Average Rating: Correlation =", round(corr_matrix[2, 3], 2)),
      "",
      "Interpretation:",
      "- Strong correlations between Runtime and Number of Votes.",
      "- Moderata -ve correlations between Runtime and Average Rating.",
      "- Weak +ve correlations between Number of Votes and Average Rating."
    )
    
    cat(paste(explanation, collapse = "\n"))
  })
  
  #model prediction
  observeEvent(input$predict_button, {
    new_data <- data.frame(runtime_minutes = input$runtime, num_votes = input$votes)
    
    lm_model_multiple <- lm(average_rating ~ runtime_minutes + num_votes, data = monster_movies)
    predicted_rating <- predict(lm_model_multiple, newdata = new_data)
    
    output$prediction_result <- renderPrint({
      cat("Predicted Rating:", predicted_rating, "\n")
    })
  })
}

#run
shinyApp(ui = ui, server = server)
