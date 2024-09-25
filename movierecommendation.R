# Install necessary packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")  # For string manipulation
install.packages("tm")        # For text mining

# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(tm)

# Load the Netflix dataset
netflix_data <- read.csv("/Users/yallareddysadumchinnapareddigari/Desktop/netflix_titles.csv")

# Select relevant columns for the recommendation system
netflix_data <- netflix_data %>%
  select(title, description, rating, duration, listed_in)

# Handle missing values by removing rows with NA descriptions
netflix_data <- netflix_data %>%
  filter(!is.na(description))

# Preprocess the description: Convert to lowercase and remove punctuation
netflix_data$clean_description <- tolower(netflix_data$description)
netflix_data$clean_description <- str_replace_all(netflix_data$clean_description, "[[:punct:]]", "")

# Function to calculate cosine similarity
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Function to recommend movies based on description similarity
recommend_based_on_description <- function(movie_title, data, num_recommendations = 5) {
  # Find the index of the movie
  movie_index <- which(data$title == movie_title)
  
  if (length(movie_index) == 0) {
    stop("Movie not found!")
  }
  
  # Create a term-document matrix
  corpus <- Corpus(VectorSource(data$clean_description))
  dtm <- TermDocumentMatrix(corpus)
  
  # Convert to matrix
  mat <- as.matrix(dtm)
  
  # Calculate cosine similarity for the selected movie with all others
  similarities <- sapply(1:ncol(mat), function(i) {
    if (i != movie_index) {
      cosine_similarity(mat[, movie_index], mat[, i])
    } else {
      NA
    }
  })
  
  # Sort the similarity scores
  similar_movies <- sort(similarities, decreasing = TRUE, na.last = TRUE)[1:num_recommendations]
  
  # Get the titles of the recommended movies
  recommended_titles <- data$title[order(similarities, decreasing = TRUE)][1:num_recommendations]
  
  return(recommended_titles)
}

# Example: Get recommendations for a specific movie
movie_title <- "The Irishman"
description_recommendations <- recommend_based_on_description(movie_title, netflix_data, 5)

# Print recommendations
cat("Movies similar to", movie_title, ":\n")
print(description_recommendations)

# Function to visualize recommendations
visualize_recommendations <- function(recommendations, data) {
  rec_data <- data %>% filter(title %in% recommendations)
  
  ggplot(rec_data, aes(x = reorder(title, rating), y = rating)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(title = "Recommended Movies",
         x = "Movies",
         y = "Rating") +
    coord_flip() +
    theme_minimal()
}

# Visualize recommendations based on description
visualize_recommendations(description_recommendations, netflix_data)
