library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(proxy)

library(tidyverse)

# Load datasets
ratings <- read.csv('ratings/movies.csv')
movies <- read.csv('datasets/movies.csv')
links <- read.csv('links/movies.csv')
tags <- read.csv('tags/movies.csv')

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

# User-based Collaborative Filtering Function
get_recommendations <- function(movie_name, user_rating) {
  # Get movieId for the input movie
  movie_id <- movies %>% filter(str_detect(title, regex(movie_name, ignore_case = TRUE))) %>% pull(movieId)
  if (length(movie_id) == 0) {
    return('Movie not found.')
  }
  
  # Find users who rated the movie similarly
  similar_users <- ratings %>%
    filter(movieId %in% movie_id & rating >= user_rating - 0.5 & rating <= user_rating + 0.5) %>%
    pull(userId) %>%
    unique()
  
  if (length(similar_users) == 0) {
    return('No similar users found.')
  }
  
  # Get highly rated movies from similar users
  recommendations <- ratings %>%
    filter(userId %in% similar_users) %>%
    group_by(movieId) %>%
    summarize(avg_rating = mean(rating), .groups = 'drop') %>%
    arrange(desc(avg_rating)) %>%
    head(10) %>%
    left_join(movies, by = c('movieId' = 'movieId')) %>%
    pull(title)
  
  return(recommendations)
}

# Item-based Collaborative Filtering Function
get_item_based_recommendations <- function(movie_name) {
  # Get movieId for the input movie
  movie_id <- movies %>% filter(str_detect(title, regex(movie_name, ignore_case = TRUE))) %>% pull(movieId)
  if (length(movie_id) == 0) {
    return('Movie not found.')
  }
  
  # Find similar movies based on user ratings
  movie_ratings <- rating_matrix[, as.character(movie_id), drop = FALSE]
  similarity_scores <- cor(rating_matrix, movie_ratings, use = 'pairwise.complete.obs')
  similarity_scores <- as.data.frame(similarity_scores)
  similarity_scores$movieId <- as.integer(rownames(similarity_scores))
  similarity_scores <- similarity_scores %>% arrange(desc(similarity_scores[, 1]))
  
  # Get top 10 similar movies excluding the input movie
  top_movies <- similarity_scores %>% filter(movieId != movie_id) %>% head(10) %>% pull(movieId)
  recommendations <- movies %>% filter(movieId %in% top_movies) %>% pull(title)
  
  return(recommendations)
}

#Example user input
movie_input <- 'Toy Story'
user_rating <- 4.5

# Example usage for User-based
user_recommendations <- get_recommendations(movie_input, user_rating)
print(paste('Recommendations based on', movie_input, 'with rating', user_rating, ':'))
print(user_recommendations)

# Example usage for Item-based
item_recommendations <- get_item_based_recommendations(movie_input)
cat('Item-based Recommendations for movies similar to', movie_input, ':\n')
print(item_recommendations)