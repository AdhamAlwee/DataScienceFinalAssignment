library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# Load datasets
ratings <- read.csv('/mnt/data/ratings.csv')
movies <- read.csv('/mnt/data/movies.csv')
links <- read.csv('/mnt/data/links.csv')
tags <- read.csv('/mnt/data/tags.csv')

check_missing_data <- function(data) {
  missing_counts <- colSums(is.na(data))
  missing_summary <- data.frame(Column = names(data), Missing_Values = missing_counts)
  missing_summary <- missing_summary[missing_summary$Missing_Values > 0, ]
  if (nrow(missing_summary) == 0) print("No missing values found!") else print(missing_summary)
  return(missing_summary)
}

check_missing_data(ratings)
check_missing_data(movies)
check_missing_data(links)
check_missing_data(tags)

# Merge data
ratings <- ratings %>% left_join(movies, by = c('movieId' = 'movieId'))

# Create a user-item rating matrix
rating_matrix <- ratings %>%
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = movieId, values_from = rating) %>%
  column_to_rownames('userId')
rating_matrix[is.na(rating_matrix)] <- 0

# Split data into training and testing (80-20 split)
set.seed(123)
test_indices <- sample(1:nrow(ratings), size = 0.2 * nrow(ratings))
test_set <- ratings[test_indices, ]
train_set <- ratings[-test_indices, ]

# User-based Collaborative Filtering Function
get_recommendations <- function(movie_name, user_rating, num_recommendations) {
  movie_id <- movies %>% filter(str_detect(title, regex(movie_name, ignore_case = TRUE))) %>% pull(movieId)
  if (length(movie_id) == 0) return('Movie not found.')
  similar_users <- ratings %>%
    filter(movieId %in% movie_id & rating >= user_rating - 0.5 & rating <= user_rating + 0.5) %>%
    pull(userId) %>%
    unique()
  if (length(similar_users) == 0) return('No similar users found.')
  recommendations <- ratings %>%
    filter(userId %in% similar_users) %>%
    group_by(movieId) %>%
    summarize(avg_rating = mean(rating), .groups = 'drop') %>%
    arrange(desc(avg_rating)) %>%
    head(num_recommendations) %>%
    left_join(movies, by = 'movieId') %>%
    select(title, avg_rating)
  return(recommendations)
}

# Shiny App for Visualization
ui <- fluidPage(
  titlePanel('Movie Recommendation System'),
  sidebarLayout(
    sidebarPanel(
      textInput('movie_name', 'Enter Movie Name:', value = 'Toy Story'),
      numericInput('user_rating', 'Enter Your Rating (1-5):', value = 4.5, min = 0.5, max = 5, step = 0.5),
      actionButton('recommend_btn', 'Get Recommendations')
    ),
    mainPanel(
      div(style = 'margin-bottom: 20px; border: 2px solid #4CAF50; padding: 10px; background-color: #f0f8ff; font-weight: bold; text-align: center;', textOutput('rmse_value')),
      plotOutput('recommendation_plot'),
      tableOutput('recommendation_table')
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues(rmse_value = NULL)
  
  observeEvent(input$recommend_btn, {
    predicted_ratings <- test_set %>%
      group_by(userId, movieId) %>%
      summarize(pred_rating = mean(input$user_rating), .groups = 'drop')
    rv$rmse_value <- sqrt(mean((test_set$rating - as.numeric(predicted_ratings$pred_rating)) ^ 2, na.rm = TRUE))
  })
  
  output$rmse_value <- renderText({
    if (!is.null(rv$rmse_value)) {
      paste('RMSE:', round(rv$rmse_value, 4))
    } else {
      'Click the button to calculate RMSE'
    }
  })
  
  recommendations <- eventReactive(input$recommend_btn, {
    get_recommendations(input$movie_name, input$user_rating, 10)
  })
  
  output$recommendation_table <- renderTable({
    recommendations()
  })
  
  output$recommendation_plot <- renderPlot({
    recs <- recommendations()
    if (!is.null(recs) && nrow(recs) > 0) {
      ggplot(recs, aes(x = reorder(title, avg_rating), y = avg_rating)) +
        geom_bar(stat = 'identity', fill = 'steelblue') +
        coord_flip() +
        labs(title = 'Top 10 Recommended Movies',
             x = 'Movies', y = 'Average Rating') +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)