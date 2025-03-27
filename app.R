library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(proxy)
library(bslib)
library(rsconnect)

setwd("C:/Users/user/Desktop/DataScienceFinalAssignment")
getwd()
movies_data <- read.csv("datasets/movies.csv")
ratings_data <- read.csv("datasets/ratings.csv")

# Function to check missing data
check_missing_data <- function(data) {
  missing_counts <- colSums(is.na(data))
  missing_summary <- data.frame(Column = names(data), Missing_Values = missing_counts)
  missing_summary <- missing_summary[missing_summary$Missing_Values > 0, ]
  if (nrow(missing_summary) == 0) print("No missing values found!") else print(missing_summary)
  return(missing_summary)
}

# Function to check duplicate rows in the dataset
check_duplicates <- function(data, key_columns) {
  duplicate_rows <- data %>% 
    group_by(across(all_of(key_columns))) %>% 
    filter(n() > 1)
  
  if (nrow(duplicate_rows) == 0) {
    print("No duplicate rows found!")
  } else {
    print(paste("Number of duplicate rows:", nrow(duplicate_rows)))
    print(head(duplicate_rows))
  }
  return(duplicate_rows)
}

# Check missing values and duplicates in datasets
check_missing_data(movies_data)
check_missing_data(ratings_data)
check_duplicates(movies_data, c("movieId", "title"))
check_duplicates(ratings_data, c("userId", "movieId", "rating"))


#most common genre
genre_analysis <- movies_data %>%
  separate_rows(genres, sep = "\\|") %>%
  count(genres, sort = TRUE)
ggplot(genre_analysis, aes(x = reorder(genres, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Most Common Movie Genres", x = "Genre", y = "Count")

# Function to analyze distribution of movies by release year
movies_data$year <- as.numeric(str_extract(movies_data$title, "\\d{4}(?=\\))"))
year_distribution <- movies_data %>% count(year)
ggplot(year_distribution, aes(x = year, y = n)) +
  geom_line(color = "blue") +
  labs(title = "Number of Movies Released Per Year", x = "Year", y = "Count")

# Function to analyze distribution of ratings
rating_distribution <- ratings_data %>% count(rating)
ggplot(rating_distribution, aes(x = rating, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count")

# Function to find most-rated movies
most_rated_movies <- ratings_data %>%
  count(movieId, sort = TRUE) %>%
  top_n(10, n) %>%
  left_join(movies_data, by = "movieId")
ggplot(most_rated_movies, aes(x = reorder(title, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Most-Rated Movies", x = "Movie", y = "Number of Ratings")

# Function to recommend movies using TF-IDF
recommend_movies_tfidf <- function(data, movie_title, num_recommendations = 10) {
  if (!"title" %in% colnames(data) || !"genres" %in% colnames(data)) {
    stop("The dataset must contain 'title' and 'genres' columns")
  }
  
  # Create a corpus from genres
  corpus <- VCorpus(VectorSource(data$genres))
  
  # Apply TF-IDF transformation
  dtm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf_matrix <- t(as.matrix(dtm))  # Transpose so movies are rows
  
  # Ensure movie titles match the TF-IDF matrix order
  if (nrow(tfidf_matrix) != nrow(data)) {
    stop("Mismatch: TF-IDF matrix and dataset row counts differ.")
  }
  rownames(tfidf_matrix) <- data$title  # Assign movie titles as row names
  
  # Find the index of the input movie
  movie_index <- which(rownames(tfidf_matrix) == movie_title)
  if (length(movie_index) == 0) {
    stop("Movie not found in dataset")
  }
  
  # Compute cosine similarity
  similarity_scores <- proxy::dist(tfidf_matrix, method = "cosine")
  similarity_matrix <- as.matrix(similarity_scores)  # Convert to matrix
  
  # Extract similarity scores for the input movie
  movie_similarities <- similarity_matrix[movie_index, , drop = FALSE]
  
  # Get top recommendations (excluding the input movie itself)
  recommended_indices <- order(movie_similarities)[2:(num_recommendations + 1)]
  recommendations <- rownames(tfidf_matrix)[recommended_indices]
  
  print("Recommended Movies:")
  
  return(recommendations)
}

calculate_precision_recall <- function(recommendations, ratings_data, movies_data, threshold = 4.0) {
  # Merge ratings_data with movies_data to get movie titles
  ratings_with_titles <- merge(ratings_data, movies_data, by = "movieId")
  
  # Display recommended movies before evaluation
  print("Recommended Movies Before Evaluation:")
  print(recommendations)
  
  # Extract movies with ratings >= threshold
  relevant_movies <- ratings_with_titles[ratings_with_titles$rating >= threshold, "title"]
  
  # Handle case where no relevant movies exist
  if (length(relevant_movies) == 0) {
    warning("No movies meet the rating threshold. Recall is set to NA.")
    recall <- NA
  } else {
    retrieved_relevant <- sum(recommendations %in% relevant_movies)
    recall <- retrieved_relevant / length(relevant_movies)
  }
  
  # Handle case where no recommendations are made
  if (length(recommendations) == 0) {
    warning("No recommendations were made. Precision is set to NA.")
    precision <- NA
  } else {
    retrieved_relevant <- sum(recommendations %in% relevant_movies)
    precision <- retrieved_relevant / length(recommendations)
  }
  
  return(list(precision = precision, recall = recall))
}

# Example usage
recommendations <- recommend_movies_tfidf(movies_data, "Toy Story (1995)", 5)
print(recommendations)

precision_recall <- calculate_precision_recall(recommendations, ratings_data, movies_data)
print(precision_recall)

ui <- fluidPage(
  titlePanel("Movie Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_movie", "Choose a Movie:", choices = unique(movies_data$title)),
      numericInput("num_recommendations", "Number of Recommendations:", value = 5, min = 1, max = 20),
      actionButton("recommend", "Get Recommendations"),
      textOutput("precision_recall")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Most Common Genres", plotOutput("genrePlot")),
        tabPanel("Movies Per Year", plotOutput("yearPlot")),
        tabPanel("Ratings Distribution", plotOutput("ratingPlot")),
        tabPanel("Most Rated Movies", plotOutput("mostRatedPlot")),
        tabPanel("Recommendations", verbatimTextOutput("recommendationOutput"))
      )
    )
  )
)

server <- function(input, output) {
  output$genrePlot <- renderPlot({
    ggplot(genre_analysis, aes(x = reorder(genres, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Most Common Movie Genres", x = "Genre", y = "Count")
  })
  
  output$yearPlot <- renderPlot({
    ggplot(year_distribution, aes(x = year, y = n)) +
      geom_line(color = "blue") +
      labs(title = "Number of Movies Released Per Year", x = "Year", y = "Count")
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(rating_distribution, aes(x = rating, y = n)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(title = "Distribution of Ratings", x = "Rating", y = "Count")
  })
  
  output$mostRatedPlot <- renderPlot({
    ggplot(most_rated_movies, aes(x = reorder(title, n), y = n)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(title = "Top 10 Most-Rated Movies", x = "Movie", y = "Number of Ratings")
  })
  
  observeEvent(input$recommend, {
    recommendations <- recommend_movies_tfidf(movies_data, input$selected_movie, input$num_recommendations)
    output$recommendationOutput <- renderText({ paste("Recommended Movies:", paste(recommendations, collapse = ", ")) })
    
    precision_recall <- calculate_precision_recall(recommendations, ratings_data, movies_data)
    output$precision_recall <- renderText({ paste("Precision:", precision_recall$precision, "Recall:", precision_recall$recall) })
  })
}

shinyApp(ui, server)