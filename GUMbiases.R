# MovieLens G.U.M. Biases

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggrepel)

# RMSE LOSS FUNCTION ----
# loss function that computes the RMSE for ratings and their corresponding predictors:
RMSE <- function(True_Ratings, Predicted_Ratings){
  sqrt(mean((True_Ratings - Predicted_Ratings)^2))
}

# NAIVE MEAN MODEL ----
# histogram showing the distribution of ratings in the edx training data:
hist(edx$rating)

# naive mean model assumes estimation by the mean rating of all movies with differences explained by random variation:
mu_hat <- mean(edx$rating)
mu_hat
[1] 3.512465

# RMSE of the naive mean model
naive_mean_rmse <- RMSE(edx$rating, mu_hat)
naive_mean_rmse
[1] 1.060331

# start a results table to compare different approaches:
rmse_results <- tibble(method = "naive mean model", RMSE = naive_mean_rmse)
rmse_results
# A tibble: 1 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model  1.06

# MOVIE BIAS MODEL ----
# movie bias for each movie is estimated by the average of the differences between the movie's ratings and mu_hat:
movie_bias <- edx %>% group_by(movieId) %>% summarize(b_i_hat = mean(rating - mu_hat))
movie_bias
# A tibble: 10,677 x 2
   movieId b_i_hat
     <dbl>   <dbl>
 1       1  0.415 
 2       2 -0.307 
 3       3 -0.365 
 4       4 -0.648 
 5       5 -0.444 
 6       6  0.303 
 7       7 -0.154 
 8       8 -0.378 
 9       9 -0.515 
10      10 -0.0866
# ... with 10,667 more rows

# histogram showing the distribution of the estimated movie bias for all the movies in the edx training data:
qplot(b_i_hat, data = movie_bias, bins = 30, color = I("black"), main = "Distribution of the Estimated Movie Bias")

# the movie bias model adds the estimated bias of each movie to the mean rating of all movies from the naive mean model:
movie_bias_model <- edx %>% left_join(movie_bias, by = 'movieId') %>% mutate(y_hat = mu_hat + b_i_hat) %>% pull(y_hat)

# RMSE of the movie bias model:
movie_bias_rmse <- RMSE(edx$rating, movie_bias_model)
movie_bias_rmse
[1] 0.9423475

# add movie bias model results to the RMSE table to compare different approaches:
rmse_results <- rmse_results %>% add_row(method = "movie bias model", RMSE = movie_bias_rmse)
rmse_results
# A tibble: 2 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942

# USER BIAS ----
# count the number of movies rated by each user in the edx training data set and summarize the distribution:
edx %>% group_by(userId) %>% summarize(count = n()) %>% summary()
# Summary of Counts (Number of Movies Rated by a User):
    userId          count       
Min.   :    1   Min.   :  10.0  
1st Qu.:17943   1st Qu.:  32.0  
Median :35799   Median :  62.0  
Mean   :35782   Mean   : 128.8  
3rd Qu.:53620   3rd Qu.: 141.0  
Max.   :71567   Max.   :6616.0

# count the number of movies rated by each user in the edx training data set and plot the lower end of the distribution:
edx %>% group_by(userId) %>%  
  summarize(count = n()) %>%
  as.data.frame() %>%
  filter(count <= 30) %>% 
  ggplot(aes(count)) + 
  geom_histogram(bins = 60, color = "black") +
  labs(title = "Distribution of Number of Movies Rated around Peak of Users in the edx Training Data Set", 
       x = "Number of Movies Rated", y = "Number of Users", 
       subtitle = "Plot shows that, of the peak number of users (1500) in the distribution, each user rated only 20 movies.")
# The plot shows that of the peak number of users (1500) in the distribution, each user rated only 20 movies. 

# A bias, by definition, is always present in a measurement.  Therefore, user bias should be present for users that have 
# rated as few as 10 movies, which is the minimum found above in the Summary of Counts (Number of Movies Rated by a User).
# Compute the average rating for users that have rated 10 or more movies and plot the distribution of the ratings:
edx %>% group_by(userId) %>% 
  filter(n() >= 10) %>%
  summarize(user_mean = mean(rating)) %>% 
  ggplot(aes(user_mean)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Average Rating for Users that have Rated 10 or More Movies in the edx Training Data Set", 
       x = "Average of Movie Ratings", y = "Number of Users",
       subtitle = "Plot shows a symmetrical normal distribution in the average ratings with minor tailing below 2.5 stars.")
# The plot shows a symmetrical normal distribution in the average ratings with minor tailing below 2.5 stars.

# user bias, b_u_hat, is estimated by the average a user's movie ratings after subtracting out mu_hat and b_i_hat for each 
# movie:
user_bias <- edx %>% left_join(movie_bias, by='movieId') %>% group_by(userId) %>% 
  summarize(b_u_hat = mean(rating - mu_hat - b_i_hat))
user_bias
# A tibble: 69,878 x 2
   userId b_u_hat
    <int>   <dbl>
 1      1  1.68  
 2      2 -0.236 
 3      3  0.264 
 4      4  0.652 
 5      5  0.0853
 6      6  0.346 
 7      7  0.0238
 8      8  0.203 
 9      9  0.232 
10     10  0.0833
# ... with 69,868 more rows

# histogram showing the distribution of the estimated user bias for all users in the edx training data:
qplot(b_u_hat, data = user_bias, bins = 30, color = I("black"), main = "Distribution of the Estimated User Bias")
# Histogram shows a symmetrical normal distribution of the Estimated User Bias centered around 0 and between -2 and +2 stars
# for over 99.9% of the users.  Less than 0.1% of Estimated User Bias is between -2 and -3.4 stars.

# calculation of number of users with bias less than -2 stars:
user_bias %>% filter(b_u_hat < -2) %>% tally()
# A tibble: 1 x 1
      n
  <int>
1    41

# calculation of number of users with bias greater than or equal to -2 stars:
user_bias %>% filter(b_u_hat >= -2) %>% tally()
# A tibble: 1 x 1
       n
   <int>
1  69837

# calculation of Percentage of all users with Estimated User Bias less than -2 stars:
user_bias %>% filter(b_u_hat < -2) %>% tally() / user_bias %>% tally() * 100
            
1  0.05867369%

# dataframe that includes all 41 users with bias less than -2 stars and shows userId, user_mean, and number of movies rated:
edx %>% left_join(user_bias, by = 'userId') %>% group_by(userId) %>% 
  summarise(user_mean = mean(rating), n = n(), user_bias = first(b_u_hat)) %>% 
  arrange(user_bias, .by_group = TRUE) %>% as.data.frame() %>% top_n(-45)

   userId user_mean   n user_bias
1   13496 0.5000000  17 -3.390564
2   48146 0.5000000  25 -3.254557
3   49862 0.5000000  17 -3.141272
4   63381 0.5000000  18 -2.962958
5   62815 0.5000000  20 -2.943231
6    6322 0.7058824  17 -2.869108
7   42019 1.1481481  27 -2.669645
8    6907 1.1190476  21 -2.603818
9   15515 1.0833333  30 -2.577369
10   3724 1.2105263  19 -2.541786
11  43628 1.0526316  19 -2.540670
12   8920 1.1000000  15 -2.431369
13  31710 1.1315789  19 -2.391057
14  30253 1.2368421  19 -2.389619
15  19059 1.1250000  20 -2.367403
16  59342 1.0639944 711 -2.355186
17  25969 1.4577465  71 -2.344140
18  28416 1.0384615  26 -2.292809
19  25674 1.2164179  67 -2.290715
20  44684 1.4473684  38 -2.283182
21  26150 1.3370474 359 -2.254580
22  52934 1.3750000  20 -2.224198
23  20313 1.3250000  20 -2.195336
24  24176 1.0000000 131 -2.187262
25  26955 1.5000000  18 -2.147655
26  45064 1.3333333  21 -2.131813
27  67667 1.3333333  21 -2.130104
28  46059 1.6451104 317 -2.118345
29  35297 1.5362319  69 -2.107609
30  24490 1.0000000  17 -2.098076
31  61607 1.3611111  18 -2.096669
32  30585 1.0869565  23 -2.083776
33  59661 1.3636364  22 -2.080620
34  59908 1.3500000  20 -2.068424
35  62257 1.4210526  19 -2.064021
36  63806 1.5500000  20 -2.038859
37  20021 1.6022727  88 -2.029449
38  13438 1.2000000  20 -2.022150
39  50560 1.4944238 269 -2.018233
40  69898 1.3146067  89 -2.002869
41  49572 1.6770833  48 -2.000464
42  52056 1.3611111  18 -1.994111
43  20240 1.3689320 103 -1.991730
44  10912 1.5000000  19 -1.991188
45  13063 1.6315789  19 -1.986473

# USER BIAS + MOVIE BIAS MODEL ----
# um_bias_model adds the estimated bias of each user to the estimated bias of each movie and the mean rating of all movies:
um_bias_model <- edx %>% left_join(movie_bias, by='movieId') %>% left_join(user_bias, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_hat + b_u_hat) %>% pull(y_hat)

# RMSE of the user bias + movie bias model:
um_bias_rmse <- RMSE(edx$rating, um_bias_model)
um_bias_rmse
[1] 0.8567039

# add RMSE result of user bias + movie bias model to the RMSE table to compare different models:
rmse_results <- rmse_results %>% add_row(method = "U + M bias model", RMSE = um_bias_rmse)
rmse_results
# A tibble: 3 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857

# TEST OF THE USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
um_bias_model_test <- validation %>% left_join(movie_bias, by='movieId') %>% left_join(user_bias, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_hat + b_u_hat) %>% pull(y_hat)

um_bias_test_rmse <- RMSE(validation$rating, um_bias_model_test)
um_bias_test_rmse
[1] 0.8653488

# start a results table to compare different approaches:
rmse_testresults <- tibble(method = "U + M bias model", RMSE = um_bias_test_rmse)
rmse_testresults
# A tibble: 1 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865

# Remove large data files no longer needed (can always recalculate if needed again):
rm(movie_bias_model, um_bias_model, um_bias_model_test)


# GENRE BIAS ----

# Compute the average rating of the movie genres and plot the distribution of the ratings:
edx %>% group_by(genres) %>% 
  summarize(genre_mean = mean(rating)) %>% 
  ggplot(aes(genre_mean)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Average Rating of Movie Genres in the edx Training Data Set", 
       x = "Average of Movie Ratings", y = "Number of Genres",
       subtitle = "Plot shows a symmetrical normal distribution in the average ratings with tailing below 2.5 stars.")
# The plot shows a symmetrical normal distribution in the average movie ratings of the genres, with tailing below 2.5 stars.

# genre bias, b_g_hat, is estimated by the average of a genre's movie ratings after subtracting out mu_hat, b_i_hat, 
# and b_u_hat for each movie and user combination:
genre_bias <- edx %>% left_join(movie_bias, by='movieId') %>% left_join(user_bias, by='userId') %>% group_by(genres) %>% 
  summarize(b_g_hat = mean(rating - mu_hat - b_i_hat - b_u_hat))

genre_bias
# A tibble: 797 x 2
   genres                                             b_g_hat
   <chr>                                                <dbl>
 1 (no genres listed)                                  0.232 
 2 Action                                             -0.0345
 3 Action|Adventure                                   -0.0131
 4 Action|Adventure|Animation|Children|Comedy          0.0109
 5 Action|Adventure|Animation|Children|Comedy|Fantasy -0.0199
 6 Action|Adventure|Animation|Children|Comedy|IMAX     0.0139
 7 Action|Adventure|Animation|Children|Comedy|Sci-Fi   0.0126
 8 Action|Adventure|Animation|Children|Fantasy        -0.0805
 9 Action|Adventure|Animation|Children|Sci-Fi         -0.0309
10 Action|Adventure|Animation|Comedy|Drama             0.0584
# ... with 787 more rows


# histogram showing the distribution of the estimated genre bias for all genres in the edx training data:
qplot(b_g_hat, data = genre_bias, bins = 30, color = I("black"), main = "Distribution of the Estimated Genre Bias")
# Histogram shows a fairly normal distribution of the Estimated Genre Bias centered around 0 that is moderately skewed 
# toward positive bias.


# GENRE BIAS + USER BIAS + MOVIE BIAS MODEL ----
# gum_bias_model adds the estimated biases of the corresponding genres, users, and movies to the mean rating of all movies:
gum_bias_model <- edx %>% left_join(genre_bias, by='genres') %>% 
                            left_join(user_bias, by='userId') %>% 
                            left_join(movie_bias, by='movieId') %>% 
                            mutate(y_hat = mu_hat + b_g_hat + b_u_hat + b_i_hat) %>% pull(y_hat)

# RMSE of the genre bias + user bias + movie bias model:
gum_bias_rmse <- RMSE(edx$rating, gum_bias_model)
gum_bias_rmse
[1] 0.8563595

# add RMSE result of genre bias + user bias + movie bias model to the RMSE table to compare different models:
rmse_results <- rmse_results %>% add_row(method = "G+U+M bias model", RMSE = gum_bias_rmse)
rmse_results
# A tibble: 4 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 G+U+M bias model 0.856


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|G+U+M bias model | 0.8563595|


# TEST OF THE GENRE BIAS + USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
gum_bias_model_test <- validation %>% left_join(genre_bias, by='genres') %>% 
                                        left_join(user_bias, by='userId') %>% 
                                        left_join(movie_bias, by='movieId') %>% 
                                        mutate(y_hat = mu_hat + b_g_hat + b_u_hat + b_i_hat) %>% pull(y_hat)

gum_bias_test_rmse <- RMSE(validation$rating, gum_bias_model_test)
gum_bias_test_rmse
[1] 0.8649469

rmse_testresults <- rmse_testresults %>% add_row(method = "G+U+M bias model", RMSE = gum_bias_test_rmse)
rmse_testresults
# A tibble: 2 × 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 G+U+M bias model 0.865


rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|G+U+M bias model | 0.8649469|
  

rm(gum_bias_model, gum_bias_model_test)


