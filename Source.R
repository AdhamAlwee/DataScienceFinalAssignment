library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(proxy)

setwd("C:/Users/user/OneDrive - Universiti Teknologi PETRONAS/Jan 25/Data Science/Project Assignment/DataScienceFinalAssignment")
getwd()
movies_data <- read.csv("datasets/movies.csv")

check_missing_data <- function(data) {
  missing_counts <- colSums(is.na(data))
  missing_summary <- data.frame(Column = names(data), Missing_Values = missing_counts)
  missing_summary <- missing_summary[missing_summary$Missing_Values > 0, ]
  if (nrow(missing_summary) == 0) print("No missing values found!") else print(missing_summary)
  return(missing_summary)
}

check_missing_data(movies_data)

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

# Example usage
recommend_movies_tfidf(movies_data, "Toy Story (1995)", 2)