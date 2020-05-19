################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Check if edx and validation objects are already saved in working directory and load them if they are
ifelse(file.exists("edx.rds") && file.exists("validation.rds"),
       {edx <- readRDS("edx.rds")
       validation <- readRDS("validation.rds")},
       {

         # Downloads data if edx and validation objects not found
         dl <- tempfile()
         download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

         ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                          col.names = c("userId", "movieId", "rating", "timestamp"))

         movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)


         colnames(movies) <- c("movieId", "title", "genres")
         movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                                    title = as.character(title),
                                                    genres = as.character(genres))

         movielens <- left_join(ratings, movies, by = "movieId")

         # Validation set will be 10% of MovieLens data
         set.seed(1, sample.kind="Rounding")

         test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
         edx <- movielens[-test_index,]
         temp <- movielens[test_index,]

         # Make sure userId and movieId in validation set are also in edx set
         validation <- temp %>%
           semi_join(edx, by = "movieId") %>%
           semi_join(edx, by = "userId")

         # Add rows removed from validation set back into edx set
         removed <- anti_join(temp, validation)
         edx <- rbind(edx, removed)

         rm(dl, ratings, movies, test_index, temp, movielens, removed)
       
         })


#####################################################################
# Calculate years, review dates, and ratings per year on training set
#####################################################################

# Extract movie release years from title names
years <- edx %>% group_by(title) %>%
  summarise(year = as.numeric(substr(str_extract(title[1], "\\d{4}\\)$"), 0, 4)))

# Join years data onto training set
edx <- edx %>% left_join(years, by = "title")

# Calculate review date month from timestamp data
edx <- edx %>% mutate(date_mth = round_date(as_datetime(timestamp), unit = "month"))

# Maximum year in our review dates
max_yr <- year(max(edx$date_mth))

# Calculate ratings per year since each movie's release date
rtgs_year <- edx %>% group_by(title) %>%
  summarize(rtgs_year = round(n()/(max_yr - year[1])))

# Join ratings per year data onto training set
edx <- edx %>% left_join(rtgs_year, by = "title")


####################################################
# Run algorithm on validation set and calculate RMSE
####################################################

#Calculate mean rating in training set
mu <- mean(edx$rating)

# Calculate movie title bias terms
b_i <- edx %>%
  group_by(title) %>%
  summarize(b_i = sum(rating - mu)/(n()+4.7))

# Calculate user bias terms
b_u <- edx %>%
  left_join(b_i, by="title") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+4.7))

# Calculate release year bias terms
b_y <- edx %>%
  left_join(b_i, by="title") %>%
  left_join(b_u, by="userId") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+4.7))

# Calculate genres bias terms
b_g <- edx %>%
  left_join(b_i, by="title") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+4.7))

# Calculate review dates bias terms
b_d <- edx %>%
  left_join(b_i, by="title") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year") %>%
  left_join(b_g, by="genres") %>%
  group_by(date_mth) %>%
  summarize(b_d = sum(rating - b_g - b_y - b_u - b_i - mu)/(n()+4.7))

# Calculate movie popularity (ratings per year) bias terms
b_p <- edx %>%
  left_join(b_i, by="title") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_d, by="date_mth") %>%
  group_by(rtgs_year) %>%
  summarize(b_p = sum(rating - b_d - b_g - b_y - b_u - b_i - mu)/(n()+4.7))

# Calculate predicted ratings for the validation set
predicted_ratings <- validation %>%
  left_join(years, by = "title") %>%
  left_join(rtgs_year, by = "title") %>%
  mutate(date_mth = round_date(as_datetime(timestamp), unit = "month")) %>%
  left_join(b_i, by = "title") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_d, by = "date_mth") %>%
  left_join(b_p, by = "rtgs_year") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g + b_d + b_p) %>%
  .$pred

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

# Calculate the final RMSE
rmse <- RMSE(validation$rating, predicted_ratings)
rmse




