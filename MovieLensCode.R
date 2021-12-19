# MovieLens project
# Penny Cookson
# 10/12/2021
# HarvardX Data Science Capstone
#########################################################
#Note: Code created and compiled in R version 3.6.3
#Running this code will recreate the .RData files used in the report
#It is not necessary to run the code since the .RData files are already provided

# SECTION 0 - START: Section directly from course materials for set up
###################################################################################
#########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#remove these comments if you want to download the data again instead of using the saved version
# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
# 
# ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                  col.names = c("userId", "movieId", "rating", "timestamp"))
# 
# movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
# colnames(movies) <- c("movieId", "title", "genres")
# 
# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))
#
# 
# movielens <- left_join(ratings, movies, by = "movieId")
# 
# Validation set will be 10% of MovieLens data
# set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
# test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
# edx <- movielens[-test_index,]
# temp <- movielens[test_index,]
# 
# Make sure userId and movieId in validation set are also in edx set
# validation <- temp %>% 
#   semi_join(edx, by = "movieId") %>%
#   semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
# removed <- anti_join(temp, validation)
# edx <- rbind(edx, removed)
# 
# rm(dl, ratings, movies, test_index, temp, movielens, removed)
# 
# Save the files for future use
# save(validation,file="validation.RData")
# save(edx,file="edx.RData")

# load the provided files instead of downloading
load("validation.RData")
load("edx.RData")

# SECTION 0 - END: Section directly from course materials for set up
###################################################################################


# SECTION 1 - START: Load additional libraries
###################################################################################

#Load all additional libraries needed in the script
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
library(stringr)  
library(purrr)
library(ggthemes)
library(ggrepel)

# SECTION 1 - END: Load additional libraries
###################################################################################



# SECTION 2 - START: Inspect data and check data quality
###################################################################################
#inspect the structure of edx
str(edx)

#find the number of rows
nrow(edx)

#find the number of movies
edx %>% group_by (movieId) %>% summarise(number = n()) %>% nrow()

#find the number of users
edx %>% group_by (userId) %>% summarise(number = n()) %>% nrow()

#find the number of ratings per user
perUser <- edx %>% group_by (userId) %>% summarise(number = n()) 
min(perUser$number)
max(perUser$number)
mean(perUser$number)
rm(perUser)

#find the number of ratings per movie
perMovie <- edx %>% group_by (movieId) %>% summarise(number = n()) 
min(perMovie$number)
max(perMovie$number)
mean(perMovie$number)
rm(perMovie)

#check for null and is.na values, these should all return 0
nrow(edx %>% filter(is.na(userId)))
nrow(edx %>% filter(is.null(userId)))
nrow(edx %>% filter(!is.integer(userId)))
nrow(edx %>% filter(is.na(movieId)))
nrow(edx %>% filter(is.null(movieId)))
nrow(edx %>% filter(!is.numeric(movieId)))
nrow(edx %>% filter(is.na(title)))
nrow(edx %>% filter(is.null(title)))
nrow(edx %>% filter(is.na(genres)))
nrow(edx %>% filter(is.null(genres)))
nrow(edx %>% filter(is.na(timestamp)))
nrow(edx %>% filter(is.null(timestamp)))
nrow(edx %>% filter(!is.integer(timestamp)))

#check that all rating values are in the correct value set, this should return 0
nrow(edx %>% filter(!rating %in% c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)))

#check for inconsistencies in derived movie data, these chould return the same number
edx %>% group_by (movieId) %>% summarise(number = n()) %>% nrow()
edx %>% group_by (movieId,title,genres) %>% summarise(number = n()) %>% nrow()

# SECTION 2 - END: Inspect data and check data quality
###################################################################################


# SECTION 3 - START: Create train and test sets
###################################################################################

set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- edx[-test_index,]
temp_set <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from validation set back into train set
removed <- anti_join(temp_set, test_set)
train_set <- rbind(train_set, removed)

#clean up
rm(test_index, temp_set,removed)

#save the data
save(train_set,file="train_set.RData")
save(test_set,file="test_set.RData")

#check the data
# should be 7200089
nrow(train_set)
# should be 1799966
nrow(test_set)

# SECTION 3 - END: Create train and test sets
###################################################################################


# SECTION 4 - START: Split the genres 
###################################################################################
# This section splits the genres in order to investigate whether there is a genre effect overall, and a genre effect for each user

# split the genres character string at each | character to form a vector with each genre as an entry, and see the maximum we have
edx1 <- edx %>% mutate(num_genres = str_count(genres,fixed("|"))+1, genres_split = strsplit(genres,"|",fixed=TRUE))
max(edx1$num_genres)
#8

# Convert the entries to columns
# note this code takes a few minutes to run
genres <- tibble(gen1 = map_chr(edx1$genres_split, 1, .default = "NA"),  
                 gen2 = map_chr(edx1$genres_split, 2, .default = "NA"),
                 gen3 = map_chr(edx1$genres_split, 3, .default = "NA"),
                 gen4 = map_chr(edx1$genres_split, 4, .default = "NA"),
                 gen5 = map_chr(edx1$genres_split, 5, .default = "NA"),
                 gen6 = map_chr(edx1$genres_split, 6, .default = "NA"),
                 gen7 = map_chr(edx1$genres_split, 7, .default = "NA"),
                 gen8 = map_chr(edx1$genres_split, 8, .default = "NA")
)


# use this to create a table of unique genres
unique_genres <- unique(dplyr::union(unique(tibble(genre = genres$gen1))
                                     , unique(tibble(genre = genres$gen2))
                                     ,unique(tibble(genre = genres$gen3))
                                     ,unique(tibble(genre = genres$gen4))
                                     ,unique(tibble(genre = genres$gen5))
                                     ,unique(tibble(genre = genres$gen6))
                                     ,unique(tibble(genre = genres$gen7))
                                     ,unique(tibble(genre = genres$gen8))))

save(unique_genres,file="unique_genres.RData")

nrow(unique_genres)
# 21

# create functions that for each distinct genre will get the count of ratings and average rating for movies that have that genre.
getCount <- function(n) {
  train_set %>% mutate(genre_included = str_detect(genres,n) ) %>% filter(genre_included) %>% nrow()
}

getMean <- function(n) {
  train_set %>% mutate(genre_included = str_detect(genres,n) ) %>% filter(genre_included) %>% 
    summarise(a=mean(rating)) %>% pull(a)
}

# get the avg and  count for each genre
# note this code takes about a minute to run
genres <- as.vector(unique_genres$genre)
count_vector <- sapply(genres, getCount)
avg_vector <- sapply(genres, getMean)

#create a table of unique genres, the number of and avg of their ratings from the 3 vectors
genre_data <- data.frame(genre = genres, countRatings = count_vector, avgRatings = avg_vector)

#save this for use in the report graphs
save(genre_data, file="genre_data.RData")

# SECTION 4 - END: Split the genres 
###################################################################################


# SECTION 5 - START: Investigate data graphically part 1
###################################################################################
#plot movie effect for top 50 rated movies
train_set  %>%
  group_by(movieId) %>% 
  summarize(Number = n(), avg = mean(rating))  %>%
  top_n(50,Number)  %>% 
  ggplot(aes(reorder(as.character(movieId),avg), avg,fill=Number)) + 
  geom_col() +
  coord_flip() +
  ylab("Average Rating") + 
  xlab("Movie Id") +
  ggtitle("Average Rating by Movie (Most Frequently Rated Movies)") +
  theme_clean() 


#plot movies by the number of times they are rated
train_set  %>%
  group_by(movieId) %>% 
  summarize(Number = n(),avg = mean(rating)) %>%
  mutate(numBand=ntile(Number,40)) %>% 
  group_by(numBand) %>% 
  summarize(bandavg = mean(avg))  %>%  
  ggplot(aes(reorder(as.character(numBand),numBand), bandavg,fill=numBand)) + 
  geom_col() +
  coord_flip() +
  ylab("Average Rating") + 
  xlab("Number Ratings (Band)") +
  ggtitle("Average Movie Rating by Number of Ratings") +
  theme_clean() 

#Look for outliers in movie ratings 
train_set  %>%
  group_by(movieId) %>% 
  summarize(Number = n(),avg = mean(rating)) %>%
  mutate(numBand=ntile(Number,40)) %>% 
  ggplot(aes(reorder(as.character(numBand),numBand), avg))+ geom_boxplot() + coord_flip()+
  ylab("Avg Rating Diff") + 
  xlab("Band") +
  ggtitle("Average Rating by Number of Movie Ratings (band)") +
  theme_clean() 


#Plot user effect for top 50 raters
train_set  %>%
  group_by(userId) %>% 
  summarize(Number = n(), avg = mean(rating))  %>%
  top_n(50,Number) %>%
  ggplot(aes(reorder(as.character(userId),avg), avg,fill=Number)) + 
  geom_col() +
  coord_flip() +
  ylab("Average Rating") + 
  xlab("User Id") +
  ggtitle("Average Rating by User (Top 50 Raters)") +
  theme_clean() 


#plot Users by the number of times they rate
train_set  %>%
  group_by(userId) %>% 
  summarize(Number = n(),avg = mean(rating)) %>%
  mutate(numBand=ntile(Number,40)) %>% 
  group_by(numBand) %>% 
  summarize(bandavg = mean(avg))  %>%  
  ggplot(aes(reorder(as.character(numBand),numBand), bandavg,fill=numBand)) + 
  geom_col() +
  coord_flip() +
  ylab("Average Rating") + 
  xlab("Number Ratings (Band)") +
  ggtitle("Average User Rating by Number of Ratings") +
  theme_clean() 


#Look for outliers in user ratings 
train_set  %>%
  group_by(userId) %>% 
  summarize(Number = n(),avg = mean(rating)) %>%
  mutate(numBand=ntile(Number,40)) %>% 
  ggplot(aes(reorder(as.character(numBand),numBand), avg))+ geom_boxplot() + coord_flip()+
  ylab("Avg Rating Diff") + 
  xlab("Band") +
  ggtitle("Average Rating by Number of User Ratings (band)") +
  theme_clean() 


#plot time effect on the average rating
train_set %>% mutate(tm = trunc(timestamp/10000000)) %>% 
  group_by(tm) %>%
  summarize(avg = mean(rating) ,num = n()) %>% 
  ggplot(aes(tm, avg)) + 
  geom_line() +
  xlab("Time") + 
  ylab("Average Rating") +
  ggtitle("Average Rating over Time") +
  theme_clean() 


#plot time effect on the number of ratings
train_set %>% mutate(tm = trunc(timestamp/10000000)) %>% 
  group_by(tm) %>%
  summarize(avg = mean(rating) ,num = n()) %>% 
  ggplot(aes(tm, num)) + 
  geom_line() +
  xlab("Time") + 
  ylab("Number of Ratings") +
  ggtitle("Number of Ratings over Time") +
  theme_clean() 

# plot the genre effect overall (not considering user preferences)
mu <- mean(train_set$rating)

genre_data %>% ggplot(aes(x=reorder(genre,avgRatings-mu), y=avgRatings-mu))+   
  geom_col(aes(fill=genre))+theme_clean() +
  coord_flip() +
  ylab("Avg Rating Diff") + 
  xlab("Genre") +
  ggtitle("Average Rating Diff from Mean") +
  theme_clean() 

# SECTION 5 - END: Investigate data graphically part 1
###################################################################################


# SECTION 6 - START: Investigate data graphically part 2
###################################################################################
#this section structures thed ata to look for genre effects for individual users
# new data frame with all genre columns separately
# the genre column has the rating if the genre is relevant to the movie rating otherwise NA
train_set_wide_rates <- train_set %>% mutate(Comedy = ifelse(str_detect(genres,"Comedy"),rating,NA)
                                             ,Action = ifelse(str_detect(genres,"Action"),rating,NA)
                                             ,Children = ifelse(str_detect(genres,"Children"),rating,NA)                          
                                             ,Adventure = ifelse(str_detect(genres,"Adventure"),rating,NA)     
                                             ,Animation = ifelse(str_detect(genres,"Animation"),rating,NA)     
                                             ,Drama = ifelse(str_detect(genres,"Drama"),rating,NA)     
                                             ,Crime = ifelse(str_detect(genres,"Crime"),rating,NA)   
                                             ,SciFi = ifelse(str_detect(genres,"Sci-Fi"),rating,NA)                              
                                             ,Horror =ifelse(str_detect(genres,"Horror"),rating,NA)                              
                                             ,Thriller = ifelse(str_detect(genres,"Thriller"),rating,NA)                              
                                             ,FilmNoir = ifelse(str_detect(genres,"Action"),rating,NA)                              
                                             ,Mystery = ifelse(str_detect(genres,"Mystery"),rating,NA)                             
                                             ,Documentary = ifelse(str_detect(genres,"Documentary"),rating,NA)                             
                                             ,Romance = ifelse(str_detect(genres,"Romance"),rating,NA)                              
                                             ,Fantasy = ifelse(str_detect(genres,"Fantasy"),rating,NA)                              
                                             ,Musical = ifelse(str_detect(genres,"Musical"),rating,NA)                              
                                             ,War = ifelse(str_detect(genres,"War"),rating,NA)                             
                                             ,IMAX = ifelse(str_detect(genres,"IMAX"),rating,NA))                         

save(train_set_wide_rates,file="train_set_wide_rates.RData") 


# use this to create a table which assigns an average difference in rating from the mean to each genre for each user
user_genre_prefs <- train_set_wide_rates %>% group_by(userId) %>% summarise(ComedyFactor=mean(rating) - mean(Comedy, na.rm=TRUE)
                                                                            ,ActionFactor=mean(rating) - mean(Action, na.rm=TRUE)                                    
                                                                            ,ChildrenFactor=mean(rating) - mean(Children, na.rm=TRUE)      
                                                                            ,AdventureFactor=mean(rating) - mean(Adventure, na.rm=TRUE)      
                                                                            ,AnimationFactor=mean(rating) - mean(Animation, na.rm=TRUE)      
                                                                            ,DramaFactor=mean(rating) - mean(Drama, na.rm=TRUE)    
                                                                            ,CrimeFactor=mean(rating) - mean(Crime, na.rm=TRUE)   
                                                                            ,SciFiFactor=mean(rating) - mean(SciFi, na.rm=TRUE)                                    
                                                                            ,HorrorFactor=mean(rating) - mean(Comedy, na.rm=TRUE)      
                                                                            ,ThrillerFactor=mean(rating) - mean(Thriller, na.rm=TRUE)      
                                                                            ,FilmNoirFactor=mean(rating) - mean(FilmNoir, na.rm=TRUE)      
                                                                            ,MysteryFactor=mean(rating) - mean(Mystery, na.rm=TRUE)    
                                                                            ,DocumentaryFactor=mean(rating) - mean(Documentary, na.rm=TRUE)   
                                                                            ,RomanceFactor=mean(rating) - mean(Romance, na.rm=TRUE)                                    
                                                                            ,FantasyFactor=mean(rating) - mean(Fantasy, na.rm=TRUE)      
                                                                            ,MusicalFactor=mean(rating) - mean(Musical, na.rm=TRUE)      
                                                                            ,WarFactor=mean(rating) - mean(War, na.rm=TRUE)      
                                                                            ,IMAXFactor=mean(rating) - mean(IMAX, na.rm=TRUE))

#set this to zero if its NA
user_genre_prefs [is.na(user_genre_prefs )] <- 0

save(user_genre_prefs,file="user_genre_prefs.RData")  



#plots to examine the genre effect for user preferences
# use the top 20 raters
top_20 <-  train_set  %>%
  group_by(userId) %>% 
  summarize(num = n())  %>%
  top_n(20,num) 

#join the top 20 raters to their genre preferences
top_20_train <- top_20 %>% left_join (user_genre_prefs,  by = "userId") %>% select(-num)

#pivot the genre values for plotting
top_20_train_pivot <- top_20_train %>% 
  pivot_longer(!userId, names_to = "genre", values_to = "diffmean")

top_20_train_pivot %>% 
  ggplot(aes(genre, diffmean))+ geom_boxplot() + coord_flip()+
  ylab("Avg Rating Diff") + 
  xlab("Genre") +
  ggtitle("Average Rating Diff from Mean by Genre") +
  theme_clean() 


top_20_train_pivot %>% 
  ggplot(aes(as.character(userId), diffmean,color=genre))+ geom_point() + coord_flip() +
  ylab("UserId") + 
  xlab("Diff From Mean") +
  ggtitle("Rating Diff from Mean by User and Genre")

# save for use in graphs for report
save(top_20_train_pivot, file="top_20_train_pivot.RData")


# SECTION 6 - END: Investigate data graphically part 2
###################################################################################



# SECTION 7 - START: Investigate models for prediction
###################################################################################
#create the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#first model
# first just use the model given in the notes, but evaluate the tuning parameters from the train set 
# course notes used the test set, this replicates subsets from train 
# and evaluates against a separate validation set
# this takes several minutes to run
set.seed(1, sample.kind="Rounding") 

lambdas <- seq(0, 10, 0.25)  
numtimes <- 7
rep <- replicate(numtimes, {
  val_index <- createDataPartition(y = train_set$rating, times = 1, p = 0.2, 
                                   list = FALSE)
  val_set <- train_set[val_index,]  
  tune_set <- train_set[-val_index,]  
  
  val_set <- val_set %>% 
    semi_join(tune_set, by = "movieId")  %>% 
    semi_join(tune_set, by = "userId") 
  
  rmses <- sapply(lambdas, function(l){
    mu <- mean(tune_set$rating)    
    b_i <- tune_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- tune_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predicted_ratings <- 
      val_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%      
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, val_set$rating))
  })
})

rmses <- rowMeans(rep)

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda
#4.75

#use the optimised parameter
l <- 4.75
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

#test the model against the test set

predicted_ratings <- 
  test_set %>% mutate(tm = trunc(timestamp/10000000)) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u ) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating) 
#RMSE is 0.8652421


# second model
# this adds time as a separate component, re-evalautes the tuning parameter for that 
# and then tests against the test set
# this takes several minutes to run
set.seed(1, sample.kind="Rounding") 

lambdas <- seq(0, 10, 0.25)  
numtimes <- 7
train_set_tm <- train_set %>% mutate(tm = trunc(timestamp/10000000))  
rep <- replicate(numtimes, {
  val_index <- createDataPartition(y = train_set_tm$rating, times = 1, p = 0.2, 
                                   list = FALSE)
  val_set <- train_set_tm[val_index,]  
  tune_set <- train_set_tm[-val_index,]  
  
  val_set <- val_set %>% 
    semi_join(tune_set, by = "movieId")  %>% 
    semi_join(tune_set, by = "userId") %>%
    semi_join(tune_set, by = "tm") 
  
  rmses <- sapply(lambdas, function(l){
    mu <- mean(tune_set$rating)    
    b_i <- tune_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- tune_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    b_t <- tune_set %>% 
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%  
      group_by(tm) %>%
      summarize(b_t = sum(rating - b_i -b_u - mu)/(n()+l))    
    
    predicted_ratings <- 
      val_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%   
      left_join(b_t, by = "tm") %>%         
      mutate(pred = mu + b_i + b_u + b_t) %>%
      
      pull(pred)
    
    return(RMSE(predicted_ratings, val_set$rating))
  })
})

rmses <- rowMeans(rep)

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda
#5


#use the optimised parameter
l <- 5
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_t <- train_set_tm %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%  
  group_by(tm) %>%
  summarize(b_t = sum(rating - b_i -b_u - mu)/(n()+l))

#test the model against the test set
predicted_ratings <- 
  test_set %>% mutate(tm = trunc(timestamp/10000000)) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "tm") %>%    
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating) 
#RMSE is .8651951


#Not significantly better so try a new approach using genres
# third model
# add a factor that uses the users genre preferences

#join the test set to the movies_wide data set to get the genres that are relevant
#then join it to the user_genre_factors data set so we can apply a genre preference factor
# for the user and movie combination
# use a proportion of the preference factor depening on how many genres apply
# for example if there are 4 genres we apply 1/4 of each genre factor

# for each movie note which genres are relevant
movies <- train_set %>% select(movieId,genres) %>% distinct()
movies_wide  <- movies %>% mutate(Comedy = as.numeric(str_detect(genres,"Comedy"))
                                  ,Action = as.numeric(str_detect(genres,"Action"))
                                  ,Children = as.numeric(str_detect(genres,"Children"))                          
                                  ,Adventure = as.numeric(str_detect(genres,"Adventure"))     
                                  ,Animation = as.numeric(str_detect(genres,"Animation"))     
                                  ,Drama = as.numeric(str_detect(genres,"Drama"))     
                                  ,Crime = as.numeric(str_detect(genres,"Crime"))   
                                  ,SciFi = as.numeric(str_detect(genres,"Sci-Fi"))                              
                                  ,Horror = as.numeric(str_detect(genres,"Horror"))                              
                                  ,Thriller = as.numeric(str_detect(genres,"Thriller"))                              
                                  ,FilmNoir = as.numeric(str_detect(genres,"FilmNoir"))                              
                                  ,Mystery = as.numeric(str_detect(genres,"Mystery"))                             
                                  ,Documentary = as.numeric(str_detect(genres,"Documentary"))                             
                                  ,Romance = as.numeric(str_detect(genres,"Romance"))                              
                                  ,Fantasy = as.numeric(str_detect(genres,"Fantasy"))                              
                                  ,Musical = as.numeric(str_detect(genres,"Musical"))                              
                                  ,War = as.numeric(str_detect(genres,"War"))                             
                                  ,IMAX = as.numeric(str_detect(genres,"IMAX")))              

save(movies_wide, file="movies_wide.RData")

# optimise the tuning parameter

set.seed(1, sample.kind="Rounding") 

lambdas <- seq(0, 10, 0.25)  
numtimes <- 7
train_set_tm <- train_set %>% mutate(tm = trunc(timestamp/10000000))  
rep <- replicate(numtimes, {
  val_index <- createDataPartition(y = train_set_tm$rating, times = 1, p = 0.2, 
                                   list = FALSE)
  val_set <- train_set_tm[val_index,]  
  tune_set <- train_set_tm[-val_index,]  
  
  val_set <- val_set %>% 
    semi_join(tune_set, by = "movieId")  %>% 
    semi_join(tune_set, by = "userId") %>%
    semi_join(tune_set, by = "tm") 
  
  rmses <- sapply(lambdas, function(l){
    mu <- mean(tune_set$rating)    
    b_i <- tune_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- tune_set %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    b_t <- tune_set %>% 
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%  
      group_by(tm) %>%
      summarize(b_t = sum(rating - b_i -b_u - mu)/(n()+l))    
    
    predicted_ratings <- 
      val_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%   
      left_join(b_t, by = "tm") %>%         
      left_join (movies_wide, by="movieId") %>% 
      left_join (user_genre_prefs, by="userId") %>%  
      mutate (numgenres = rowSums(.[12:29]))   %>%  
      mutate(user_genre_factor = 
               ifelse
             (numgenres==0,0,           
               (Comedy * ComedyFactor +
                  Action * ActionFactor +
                  Children * ChildrenFactor +
                  Adventure  * AdventureFactor +
                  Animation * AnimationFactor +
                  Drama * DramaFactor +
                  Crime * CrimeFactor +
                  SciFi * SciFiFactor +
                  Horror * HorrorFactor +
                  Thriller * ThrillerFactor +
                  FilmNoir * FilmNoirFactor +
                  Mystery * MysteryFactor +
                  Documentary * DocumentaryFactor +
                  Romance * RomanceFactor +
                  Fantasy * FantasyFactor +
                  Musical * MusicalFactor +
                  War * WarFactor +
                  IMAX * IMAXFactor )
               /(numgenres )))  %>%
      mutate(pred = mu + b_i + b_u + b_t - user_genre_factor) %>%
      
      pull(pred)
    
    return(RMSE(predicted_ratings, val_set$rating))
  })
})

rmses <- rowMeans(rep)

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda
#5

#use the optimised tuning parameter 
mu <- mean(train_set$rating)
l <- 5

b_i <- train_set_tm  %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- train_set_tm  %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_t <- train_set_tm  %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%  
  group_by(tm) %>%
  summarize(b_t = sum(rating - b_i -b_u - mu)/(n()+l))   


#test the model against the test set
test_set <- test_set %>% mutate(tm = trunc(timestamp/10000000))  
predicted_ratings <- 
  test_set  %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "tm") %>%      
  left_join (movies_wide, by="movieId") %>% 
  left_join (user_genre_prefs, by="userId") %>%  
  mutate (numgenres = rowSums(.[12:29]))   %>%  
  mutate(user_genre_factor = 
           ifelse
         (numgenres==0,0,           
           (Comedy * ComedyFactor +
              Action * ActionFactor +
              Children * ChildrenFactor +
              Adventure  * AdventureFactor +
              Animation * AnimationFactor +
              Drama * DramaFactor +
              Crime * CrimeFactor +
              SciFi * SciFiFactor +
              Horror * HorrorFactor +
              Thriller * ThrillerFactor +
              FilmNoir * FilmNoirFactor +
              Mystery * MysteryFactor +
              Documentary * DocumentaryFactor +
              Romance * RomanceFactor +
              Fantasy * FantasyFactor +
              Musical * MusicalFactor +
              War * WarFactor +
              IMAX * IMAXFactor )
           /(numgenres )))  %>%
  mutate(pred = mu + b_i + b_u - user_genre_factor) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)
#0.8595488


#Model 3 is selected for use

# SECTION 7 - END: Investigate models for prediction
##################################################################################


# SECTION 8 - START: Apply the final model to the validate set
###################################################################################
validation <- validation %>% mutate(tm = trunc(timestamp/10000000))  

predicted_ratings_validation <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "tm") %>%      
  left_join (movies_wide, by="movieId") %>% 
  left_join (user_genre_prefs, by="userId") %>%  
  mutate (numgenres = rowSums(.[12:29]))   %>%  
  mutate(user_genre_factor = 
           ifelse
         (numgenres==0,0,           
           (Comedy * ComedyFactor +
              Action * ActionFactor +
              Children * ChildrenFactor +
              Adventure  * AdventureFactor +
              Animation * AnimationFactor +
              Drama * DramaFactor +
              Crime * CrimeFactor +
              SciFi * SciFiFactor +
              Horror * HorrorFactor +
              Thriller * ThrillerFactor +
              FilmNoir * FilmNoirFactor +
              Mystery * MysteryFactor +
              Documentary * DocumentaryFactor +
              Romance * RomanceFactor +
              Fantasy * FantasyFactor +
              Musical * MusicalFactor +
              War * WarFactor +
              IMAX * IMAXFactor )
           /(numgenres )))  %>%
  mutate(pred = mu + b_i + b_u - user_genre_factor) %>%
  pull(pred)

RMSE(predicted_ratings_validation, validation$rating)
#859562
# this is the final RMSE

save(predicted_ratings,file="predicted_ratings.RData")

# SECTION 8 - END: Apply the model to the validate set
##################################################################################

