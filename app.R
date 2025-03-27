options(shinyapps.http.response.timeout = 300)

library(shiny)
library(data.table)
library(dplyr)
library(tm)
library(proxy)
library(Matrix)

# Load datasets safely
movies_data <- fread("reduced_movies.csv", encoding = "UTF-8")
ratings_data <- fread("reduced_ratings.csv", encoding = "UTF-8")

# Function to compute cosine similarity
dist_cosine <- function(matrix) {
  norm_matrix <- sqrt(rowSums(matrix^2))
  norm_matrix[norm_matrix == 0] <- 1  # Avoid division by zero
  sim_matrix <- tcrossprod(matrix) / (norm_matrix %o% norm_matrix)
  return(as.matrix(sim_matrix))
}

# Function to recommend movies using TF-IDF
recommend_movies_tfidf <- function(data, movie_title, num_recommendations = 10) {
  corpus <- Corpus(VectorSource(data$genres))
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  tfidf_matrix <- as.matrix(dtm)
  
  rownames(tfidf_matrix) <- data$title
  
  if (!(movie_title %in% rownames(tfidf_matrix))) {
    return(character(0))  # Return empty vector if movie not found
  }
  
  similarity_matrix <- dist_cosine(tfidf_matrix)
  movie_index <- which(rownames(tfidf_matrix) == movie_title)
  similarities <- similarity_matrix[movie_index, ]
  recommendations <- order(-similarities)[2:(num_recommendations + 1)]
  
  return(rownames(tfidf_matrix)[recommendations])
}

# Function to calculate precision and recall
calculate_precision_recall <- function(recommendations, ratings_data, movies_data, threshold = 4.0) {
  if (length(recommendations) == 0) return(list(precision = NA, recall = NA))
  
  relevant_movies <- ratings_data[rating >= threshold, unique(movieId)]
  relevant_titles <- movies_data[movieId %in% relevant_movies, title]
  
  retrieved_relevant <- sum(recommendations %in% relevant_titles)
  precision <- ifelse(length(recommendations) > 0, retrieved_relevant / length(recommendations), 0)
  recall <- ifelse(length(relevant_titles) > 0, retrieved_relevant / length(relevant_titles), 0)
  
  return(list(precision = precision, recall = recall))
}

# Define UI
ui <- fluidPage(
  titlePanel("Movie Recommendation System"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("movie", "Choose a Movie:", choices = movies_data$title, multiple = FALSE),
      numericInput("num_recommendations", "Number of Recommendations:", 5, min = 1, max = 20),
      actionButton("recommend_btn", "Get Recommendations")
    ),
    mainPanel(
      h3("Recommended Movies"),
      verbatimTextOutput("recommendations"),
      h3("Precision & Recall"),
      verbatimTextOutput("precision_recall")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  recommendations <- eventReactive(input$recommend_btn, {
    validate(need(input$movie, "Please select a movie."))
    recommend_movies_tfidf(movies_data, input$movie, input$num_recommendations)
  })
  
  output$recommendations <- renderPrint({
    recs <- recommendations()
    if (length(recs) == 0) return("No recommendations found.")
    recs
  })
  
  output$precision_recall <- renderPrint({
    recs <- recommendations()
    if (length(recs) == 0) return("No recommendations found.")
    calculate_precision_recall(recs, ratings_data, movies_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
