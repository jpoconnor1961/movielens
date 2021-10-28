# Cross Validation of Lambdas on bootstraps of edx_train with prediction against edx_test were not as 
# accurate as CV of Lambdas on bootstraps of the full edx with prediction against the full edx$rating.
# See RERUN-3a and RERUN-3b at bottom of script for fianal method, code and results.

library(tidyverse)
library(caret)
library(data.table)
library(rsample)
library(purrr)

# CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR MOVIE BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples
set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:25)
MinRMSE_Lambda_MovieBias <- sapply(Bootstraps, function(B) {
  edx_train_bootstrap <- slice_sample(edx_train, prop = 0.25, replace = TRUE)
  mu <- mean(edx_train_bootstrap$rating)
  Lambdas <- seq(0, 10, 0.20)
  rmses <- sapply(Lambdas, function(L){
    b_i <- edx_train_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+L))
    predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(predicted_ratings, edx_test$rating))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

MinRMSE_Lambda_MovieBias
         [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]     
Min_RMSE 0.9462263 0.9464048 0.9461469 0.9461315 0.9461248 0.9461267 0.9461121
Lambda   2.6       3         2.8       3         3         2.6       2.8      
         [,8]      [,9]      [,10]     [,11]    [,12]     [,13]    [,14]    
Min_RMSE 0.9462792 0.9462745 0.9461651 0.946139 0.9462351 0.946305 0.9462117
Lambda   3         2.8       2.8       2.6      2.8       2.8      2.6      
         [,15]     [,16]     [,17]     [,18]    [,19]     [,20]     [,21]    
Min_RMSE 0.9462002 0.9464401 0.9462533 0.946234 0.9460804 0.9464173 0.9460578
Lambda   2.8       2.8       2.6       2.8      2.8       3         3        
         [,22]     [,23]     [,24]     [,25]    
Min_RMSE 0.9461632 0.9460791 0.9462567 0.9460331
Lambda   2.8       3         2.8       3        

t(MinRMSE_Lambda_MovieBias)
      Min_RMSE  Lambda
 [1,] 0.9462263 2.6   
 [2,] 0.9464048 3     
 [3,] 0.9461469 2.8   
 [4,] 0.9461315 3     
 [5,] 0.9461248 3     
 [6,] 0.9461267 2.6   
 [7,] 0.9461121 2.8   
 [8,] 0.9462792 3     
 [9,] 0.9462745 2.8   
[10,] 0.9461651 2.8   
[11,] 0.946139  2.6   
[12,] 0.9462351 2.8   
[13,] 0.946305  2.8   
[14,] 0.9462117 2.6   
[15,] 0.9462002 2.8   
[16,] 0.9464401 2.8   
[17,] 0.9462533 2.6   
[18,] 0.946234  2.8   
[19,] 0.9460804 2.8   
[20,] 0.9464173 3     
[21,] 0.9460578 3     
[22,] 0.9461632 2.8   
[23,] 0.9460791 3     
[24,] 0.9462567 2.8   
[25,] 0.9460331 3


as.data.frame(t(MinRMSE_Lambda_MovieBias))
    Min_RMSE Lambda
1  0.9462263    2.6
2  0.9464048      3
3  0.9461469    2.8
4  0.9461315      3
5  0.9461248      3
6  0.9461267    2.6
7  0.9461121    2.8
8  0.9462792      3
9  0.9462745    2.8
10 0.9461651    2.8
11  0.946139    2.6
12 0.9462351    2.8
13  0.946305    2.8
14 0.9462117    2.6
15 0.9462002    2.8
16 0.9464401    2.8
17 0.9462533    2.6
18  0.946234    2.8
19 0.9460804    2.8
20 0.9464173      3
21 0.9460578      3
22 0.9461632    2.8
23 0.9460791      3
24 0.9462567    2.8
25 0.9460331      3


MinRMSE_Lambda_MovieBias <- as.data.frame(t(MinRMSE_Lambda_MovieBias))

as.numeric(as.vector(MinRMSE_Lambda_MovieBias$Lambda))
[1] 2.6 3.0 2.8 3.0 3.0 2.6 2.8 3.0 2.8 2.8 2.6 2.8 2.8 2.6 2.8 2.8 2.6 2.8 2.8 3.0
[21] 3.0 2.8 3.0 2.8 3.0

Lambda_MovieBias <- mean(as.numeric(as.vector(MinRMSE_Lambda_MovieBias$Lambda)))
Lambda_MovieBias
[1] 2.824

# Apply Lambda_MovieBias to the full edx data set and determine RMSE results.

movie_bias_regularized <- edx %>% group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu_hat)/(n() + Lambda_MovieBias))

movie_bias_model_reg <- mu_hat + edx %>% 
  left_join(movie_bias_regularized, by = 'movieId') %>% pull(b_i_reg)

movie_bias_reg_rmse <- RMSE(edx$rating, movie_bias_model_reg)
movie_bias_reg_rmse
[1] 0.942404

rmse_results <- rmse_results %>% add_row(method = "RegMovBias model", RMSE = movie_bias_reg_rmse)
rmse_results
# A tibble: 3 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 RegMovBias model 0.942

  
# CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR U+M BIAS MODEL----
# rsample bootstraps() function used to generate random bootstrap samples
# because set.seed() can be used to generate a reproduceable set of bootstraps
set.seed(1, sample.kind = "Rounding")
edx_train_bootstraps <- bootstraps(edx_train, times = 25)

edx_train_bootstraps
# 25 Bootstrap Samples (with Replacement) of Random 37% Proportion of edx_train
# A tibble: 25 x 2
   splits                    id         
   <list>                    <chr>      
 1 <split [8100065/2979448]> Bootstrap01
 2 <split [8100065/2980583]> Bootstrap02
 3 <split [8100065/2979554]> Bootstrap03
 4 <split [8100065/2980561]> Bootstrap04
 5 <split [8100065/2979634]> Bootstrap05
 6 <split [8100065/2979872]> Bootstrap06
 7 <split [8100065/2980183]> Bootstrap07
 8 <split [8100065/2980739]> Bootstrap08
 9 <split [8100065/2979558]> Bootstrap09
10 <split [8100065/2979758]> Bootstrap10
# ... with 15 more rows


# Note that this step in the cross validation process takes about 18 hours to 
# complete on a standard laptop computer.
MinRMSE_Lambda_U.M.Bias <- sapply(edx_train_bootstraps$splits, function(s) {
  mu <- mean(as.data.frame(s)$rating)
  Lambdas <- seq(0, 10, 0.20)
  rmses <- sapply(Lambdas, function(L){
    b_i <- as.data.frame(s) %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+L))
    b_u <- as.data.frame(s) %>% 
      left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+L))
    predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(edx_test$rating, predicted_ratings))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})


t(MinRMSE_Lambda_U.M.Bias)
      Min_RMSE  Lambda
 [1,] 0.8679377 9.4   
 [2,] 0.8677803 9.2   
 [3,] 0.867812  9.2   
 [4,] 0.8678556 9.4   
 [5,] 0.8677473 9.4   
 [6,] 0.867708  9.4   
 [7,] 0.8678238 9.4   
 [8,] 0.8679702 9.4   
 [9,] 0.8677923 9.4   
[10,] 0.8679436 9.4   
[11,] 0.8677855 9.4   
[12,] 0.8677497 9.6   
[13,] 0.8677316 9.2   
[14,] 0.8677876 9.2   
[15,] 0.8678387 9.4   
[16,] 0.8679073 9.4   
[17,] 0.867755  9.2   
[18,] 0.8678816 9.4   
[19,] 0.8678266 9.2   
[20,] 0.8678185 9.2   
[21,] 0.8679795 9.4   
[22,] 0.8677838 9.2   
[23,] 0.8677838 9.4   
[24,] 0.8680412 9.4   
[25,] 0.8679146 9.4

MinRMSE_Lambda_U.M.Bias <- as.data.frame(t(MinRMSE_Lambda_U.M.Bias))

Lambda_U.M.Bias <- mean(as.numeric(as.vector(MinRMSE_Lambda_U.M.Bias$Lambda)))
Lambda_U.M.Bias
[1] 9.344

# Apply Lambda_U.M.Bias to the full edx data set and determine RMSE results.

user_bias_regularized <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n() + Lambda_U.M.Bias))

U.M_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_reg = mu_hat + b_i_reg + b_u_reg) %>% pull(y_reg)

U.M_bias_reg_rmse <- RMSE(edx$rating, U.M_bias_model_reg)
U.M_bias_reg_rmse
[1] 0.8576188

rmse_results <- rmse_results %>% add_row(method = "Reg U+M Bias model", RMSE = U.M_bias_reg_rmse)
rmse_results
# A tibble: 5 x 2
  method              RMSE
  <chr>              <dbl>
1 naive mean model   1.06 
2 movie bias model   0.942
3 RegMovBias model   0.942
4 U + M bias model   0.857
5 Reg U+M Bias model 0.858


rmse_results %>% knitr::kable()

|method             |      RMSE|
|:------------------|---------:|
|naive mean model   | 1.0603313|
|movie bias model   | 0.9423475|
|RegMovBias model   | 0.9424040|
|U + M bias model   | 0.8567039|
|Reg U+M Bias model | 0.8576188|


# TEST OF THE REGULARIZED USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
U.M_bias_model_reg_test <- validation %>% 
  left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% pull(y_hat)

U.M_bias_reg_rmse_test <- RMSE(validation$rating, U.M_bias_model_reg_test)
U.M_bias_reg_rmse_test
[1] 0.8649749



# Exploratory Data Analysis of Lambda Parameters ----

lambdas <- seq(0, 10, 0.25)
edx_train_mu <- mean(edx_train$rating)
just_the_sum <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - edx_train_mu), n_i = n())

just_the_sum
# A tibble: 10,677 x 3
   movieId      s   n_i
     <dbl>  <dbl> <int>
 1       1  8879. 21395
 2       2 -2958.  9653
 3       3 -2300.  6365
 4       4  -911.  1430
 5       5 -2562.  5801
 6       6  3356. 11115
 7       7  -994.  6526
 8       8  -295.   742
 9       9 -1046.  2036
10      10 -1171. 13677
# ... with 10,667 more rows

rmses1 <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = edx_train_mu + b_i) %>%
    .$pred
  return(RMSE(edx_test$rating, predicted_ratings))
})

rmses1
 [1] 0.9429615 0.9429516 0.9429456 0.9429417 0.9429391 0.9429376 0.9429370
 [8] 0.9429370 0.9429377 0.9429389 0.9429406 0.9429427 0.9429453 0.9429482
[15] 0.9429515 0.9429551 0.9429590 0.9429632 0.9429676 0.9429723 0.9429772
[22] 0.9429824 0.9429877 0.9429933 0.9429990 0.9430049 0.9430110 0.9430172
[29] 0.9430236 0.9430301 0.9430367 0.9430435 0.9430504 0.9430574 0.9430646
[36] 0.9430718 0.9430792 0.9430866 0.9430941 0.9431018 0.9431095

min(rmses1)
[1] 0.942937

lambdas[which.min(rmses1)]
[1] 1.5

qplot(lambdas, rmses1, 
      main = "RMSE as a function of Lambda for Regularization of Movie Bias")  


lambdas <- seq(0, 10, 0.25)
rmses2 <- sapply(lambdas, function(l){
  edx_train_mu <- mean(edx_train$rating)
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_train_mu)/(n()+l))
  b_u <- edx_train %>% 
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - edx_train_mu)/(n()+l))
  predicted_ratings <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = edx_train_mu + b_i + b_u) %>%
    .$pred
  return(RMSE(edx_test$rating, predicted_ratings))
})

min(rmses2)
[1] 0.8641362

lambdas[which.min(rmses2)]
[1] 5

qplot(lambdas, rmses2, 
      main = "RMSE as a function of Lambda for Regularization of Movie and User Biases")  

dev_Lambda.1 <- 1.5

dev_Lambda.2 <- 5

dev_movie_bias_regularized <- edx %>% group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu_hat)/(n() + dev_Lambda.1))

dev_user_bias_regularized <- edx %>% left_join(dev_movie_bias_regularized, by='movieId') %>% 
  group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n() + dev_Lambda.2))

dev_U.M_bias_model_reg <- edx %>% left_join(dev_movie_bias_regularized, by='movieId') %>% 
  left_join(dev_user_bias_regularized, by='userId') %>%
  mutate(y_reg = mu_hat + b_i_reg + b_u_reg) %>% pull(y_reg)

dev_U.M_bias_reg_rmse <- RMSE(edx$rating, dev_U.M_bias_model_reg)
dev_U.M_bias_reg_rmse

[1] 0.8577397  #  L1=9.344  L2=9.344
[1] 0.8576188  #  L1=2.824  L2=9.344
[1] 0.8570452  #  L1=5      L2=5
[1] 0.8570188  #  L1=2.824  L2=5
[1] 0.8570155  #  L1=1.5    L2=5


dev_U.M_bias_model_reg_test <- validation %>% 
  left_join(dev_movie_bias_regularized, by='movieId') %>% 
  left_join(dev_user_bias_regularized, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% pull(y_hat)

dev_U.M_bias_reg_rmse_test <- RMSE(validation$rating, dev_U.M_bias_model_reg_test)
dev_U.M_bias_reg_rmse_test

[1] 0.8649749  #  L1=2.824  L2=9.344
[1] 0.8648721  #  L1=1.5    L2=5



# RERUN-1a CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR MOVIE BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples at 100% sample size

set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:10)
MinRMSE_Lambda_MovieBias <- sapply(Bootstraps, function(B) {
  edx_train_bootstrap <- slice_sample(edx_train, prop = 1.00, replace = TRUE)
  mu <- mean(edx_train_bootstrap$rating)
  Lambdas <- seq(0, 10, 0.20)
  rmses <- sapply(Lambdas, function(L){
    b_i <- edx_train_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+L))
    predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(predicted_ratings, edx_test$rating))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

t(MinRMSE_Lambda_MovieBias)
      Min_RMSE  Lambda
 [1,] 0.9436018 3.8   
 [2,] 0.9435273 3.8   
 [3,] 0.9436005 4     
 [4,] 0.9437004 4     
 [5,] 0.9436164 4     
 [6,] 0.9435717 4     
 [7,] 0.9436165 4.4   
 [8,] 0.9436542 4     
 [9,] 0.943537  3.8   
[10,] 0.9436674 4.2

MinRMSE_Lambda_MovieBias <- as.data.frame(t(MinRMSE_Lambda_MovieBias))

Lambda_MovieBias <- mean(as.numeric(as.vector(MinRMSE_Lambda_MovieBias$Lambda)))
Lambda_MovieBias
[1] 4

# Apply Lambda_MovieBias to the full edx data set and determine RMSE results.

movie_bias_regularized <- edx %>% group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu_hat)/(n() + Lambda_MovieBias))

movie_bias_model_reg <- mu_hat + edx %>% 
  left_join(movie_bias_regularized, by = 'movieId') %>% pull(b_i_reg)

movie_bias_reg_rmse <- RMSE(edx$rating, movie_bias_model_reg)
movie_bias_reg_rmse
[1] 0.9424392

rmse_results <- rmse_results %>% add_row(method = "RegMovBias model", RMSE = movie_bias_reg_rmse)
rmse_results
# A tibble: 4 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942


# RERUN-1b CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR U+M BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples at 100% sample size
# Lambda_MovieBias used for regularization of b_i
set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:10)
MinRMSE_Lambda_U.M.Bias <- sapply(Bootstraps, function(B) {
  edx_train_bootstrap <- slice_sample(edx_train, prop = 1.00, replace = TRUE)
  mu <- mean(edx_train_bootstrap$rating)
  Lambdas <- seq(5, 15, 0.20)
  rmses <- sapply(Lambdas, function(L){
    b_i <- edx_train_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + Lambda_MovieBias))
    b_u <- edx_train_bootstrap %>% 
      left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n() + L))
    predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(edx_test$rating, predicted_ratings))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

t(MinRMSE_Lambda_U.M.Bias)
      Min_RMSE  Lambda
 [1,] 0.8679516 9.8   
 [2,] 0.8677977 9.8   
 [3,] 0.867831  9.6   
 [4,] 0.8678857 9.8   
 [5,] 0.8677692 9.8   
 [6,] 0.8677292 9.8   
 [7,] 0.8678575 9.6   
 [8,] 0.8679989 9.8   
 [9,] 0.8678087 9.8   
[10,] 0.8679703 9.8

MinRMSE_Lambda_U.M.Bias <- as.data.frame(t(MinRMSE_Lambda_U.M.Bias))

Lambda_U.M.Bias <- mean(as.numeric(as.vector(MinRMSE_Lambda_U.M.Bias$Lambda)))
Lambda_U.M.Bias
[1] 9.76

# Apply Lambda_U.M.Bias to the full edx data set and determine RMSE results.

user_bias_regularized <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n() + Lambda_U.M.Bias))

U.M_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_reg = mu_hat + b_i_reg + b_u_reg) %>% pull(y_reg)

U.M_bias_reg_rmse <- RMSE(edx$rating, U.M_bias_model_reg)
U.M_bias_reg_rmse
[1] 0.8576959

rmse_results <- rmse_results %>% add_row(method = "RegU+Mbias model", RMSE = U.M_bias_reg_rmse)
rmse_results
# A tibble: 5 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942
5 RegU+Mbias model 0.858


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|RegMovBias model | 0.9424392|
|RegU+Mbias model | 0.8576959|


# TEST OF THE REGULARIZED USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
U.M_bias_model_reg_test <- validation %>% 
  left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% pull(y_hat)

U.M_bias_reg_rmse_test <- RMSE(validation$rating, U.M_bias_model_reg_test)
U.M_bias_reg_rmse_test
[1] 0.8649865


# RERUN-2 CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR U+M BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples at 100% sample size
# Same Lambdas used for regularization of b_i and b_u, and for rmse calculations
set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:10)
MinRMSE_Lambda_U.M.Bias <- sapply(Bootstraps, function(B) {
  edx_train_bootstrap <- slice_sample(edx_train, prop = 1.00, replace = TRUE)
  mu <- mean(edx_train_bootstrap$rating)
  Lambdas <- seq(0, 15, 0.20)
  rmses <- sapply(Lambdas, function(L){
    b_i <- edx_train_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + L))
    b_u <- edx_train_bootstrap %>% 
      left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n() + L))
    predicted_ratings <- edx_test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(edx_test$rating, predicted_ratings))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

t(MinRMSE_Lambda_U.M.Bias)
      Min_RMSE  Lambda
 [1,] 0.8679377 9.4   
 [2,] 0.8677803 9.2   
 [3,] 0.867812  9.2   
 [4,] 0.8678556 9.4   
 [5,] 0.8677473 9.4   
 [6,] 0.867708  9.4   
 [7,] 0.8678238 9.4   
 [8,] 0.8679702 9.4   
 [9,] 0.8677923 9.4   
[10,] 0.8679436 9.4

MinRMSE_Lambda_U.M.Bias <- as.data.frame(t(MinRMSE_Lambda_U.M.Bias))

Lambda_U.M.Bias <- mean(as.numeric(as.vector(MinRMSE_Lambda_U.M.Bias$Lambda)))
Lambda_U.M.Bias
[1] 9.36

# Apply Lambda_U.M.Bias to the full edx data set and determine RMSE results.
movie_bias_regularized <- edx %>% group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu_hat)/(n() + Lambda_U.M.Bias))

movie_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by = 'movieId') %>%
  mutate(y_reg1 = mu_hat + b_i_reg) %>% pull(y_reg1)

movie_bias_reg_rmse <- RMSE(edx$rating, movie_bias_model_reg)
movie_bias_reg_rmse
[1] 0.94263

rmse_results <- rmse_results %>% add_row(method = "RegMovBias model", RMSE = movie_bias_reg_rmse)
rmse_results
# A tibble: 6 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942
5 RegU+Mbias model 0.858
6 RegMovBias model 0.943

user_bias_regularized <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n() + Lambda_U.M.Bias))

U.M_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_reg2 = mu_hat + b_i_reg + b_u_reg) %>% pull(y_reg2)

U.M_bias_reg_rmse <- RMSE(edx$rating, U.M_bias_model_reg)
U.M_bias_reg_rmse
[1] 0.8577426

rmse_results <- rmse_results %>% add_row(method = "RegU+Mbias model", RMSE = U.M_bias_reg_rmse)
rmse_results
# A tibble: 7 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942
5 RegU+Mbias model 0.858
6 RegMovBias model 0.943
7 RegU+Mbias model 0.858


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|RegMovBias model | 0.9424392|
|RegU+Mbias model | 0.8576959|
|RegMovBias model | 0.9426300|
|RegU+Mbias model | 0.8577426|
  
  
# TEST OF THE REGULARIZED USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
U.M_bias_model_reg_test <- validation %>% 
  left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% pull(y_hat)

U.M_bias_reg_rmse_test <- RMSE(validation$rating, U.M_bias_model_reg_test)
U.M_bias_reg_rmse_test
[1] 0.8649889



# RERUN-3a CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR MOVIE BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples at 100% sample size
# of full edx data, and full edx$rating used to test predicted ratings
set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:10)
MinRMSE_Lambda_MovieBias <- sapply(Bootstraps, function(B) {
  edx_bootstrap <- slice_sample(edx, prop = 1.00, replace = TRUE)
  mu <- mean(edx_bootstrap$rating)
  Lambdas <- seq(0, 10, 0.20)
  rmses <- sapply(Lambdas, function(L){
    MovieBiasReg <- edx_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + L))
    predicted_ratings <- edx %>% 
      left_join(MovieBiasReg, by = "movieId") %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(edx$rating, predicted_ratings))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

t(MinRMSE_Lambda_MovieBias)
      Min_RMSE  Lambda
 [1,] 0.9429886 1.4   
 [2,] 0.9429533 1.4   
 [3,] 0.9429821 1.4   
 [4,] 0.9429712 1.4   
 [5,] 0.9429777 1.6   
 [6,] 0.9429814 1.4   
 [7,] 0.9429946 1.4   
 [8,] 0.9429939 1.4   
 [9,] 0.9429719 1.4   
[10,] 0.9429876 1.4

MinRMSE_Lambda_MovieBias <- as.data.frame(t(MinRMSE_Lambda_MovieBias))

Lambda_MovieBias <- mean(as.numeric(as.vector(MinRMSE_Lambda_MovieBias$Lambda)))
Lambda_MovieBias
[1] 1.42

# Apply Lambda_MovieBias to the full edx data set and determine RMSE results of the 
# Regularized Movie Bias Model:

movie_bias_regularized <- edx %>% group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu_hat)/(n() + Lambda_MovieBias))

movie_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by = 'movieId') %>%
  mutate(y_reg1 = mu_hat + b_i_reg) %>% pull(y_reg1)

movie_bias_reg_rmse <- RMSE(edx$rating, movie_bias_model_reg)
movie_bias_reg_rmse
[1] 0.9423683

rmse_results <- rmse_results %>% add_row(method = "RegMovBias model", RMSE = movie_bias_reg_rmse)
rmse_results
# A tibble: 8 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942
5 RegU+Mbias model 0.858
6 RegMovBias model 0.943
7 RegU+Mbias model 0.858
8 RegMovBias model 0.942


# RERUN-3b CROSS VALIDATION OF REGULARIZATION PARAMETER LAMBDA FOR U+M BIAS MODEL----
# dplyr slice_sample() function used to generate random bootstrap samples at 100% sample size
# of full edx data, and full edx$rating used to test predicted ratings.
# Lambda_MovieBias used for regularization of b_i in calculation of Lambda_U.M.Bias for 
# regularization of b_u.  Respective Lambdas also used in rmse calculations.
set.seed(1, sample.kind = "Rounding")
Bootstraps <- seq(1:10)
MinRMSE_Lambda_U.M.Bias <- sapply(Bootstraps, function(B) {
  edx_bootstrap <- slice_sample(edx, prop = 1.00, replace = TRUE)
  mu <- mean(edx_bootstrap$rating)
  Lambdas <- seq(0, 10, 0.20)
  rmses <- sapply(Lambdas, function(L){
    MovieBiasReg <- edx_bootstrap %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + Lambda_MovieBias))
    UserBiasReg <- edx_bootstrap %>% 
      left_join(MovieBiasReg, by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n() + L))
    predicted_ratings <- edx %>% 
      left_join(MovieBiasReg, by = "movieId") %>%
      left_join(UserBiasReg, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    predicted_ratings[is.na(predicted_ratings)] <- 0
    return(RMSE(edx$rating, predicted_ratings))
  })
  tibble(Min_RMSE = min(rmses), Lambda = Lambdas[which.min(rmses)])
})

t(MinRMSE_Lambda_U.M.Bias)
      Min_RMSE  Lambda
 [1,] 0.8606696 4.2   
 [2,] 0.8606383 4.2   
 [3,] 0.8606926 4.2   
 [4,] 0.8606714 4.2   
 [5,] 0.86065   4.2   
 [6,] 0.8606621 4     
 [7,] 0.8607112 4.2   
 [8,] 0.860611  4     
 [9,] 0.8606338 4.2   
[10,] 0.8606236 4  

MinRMSE_Lambda_U.M.Bias <- as.data.frame(t(MinRMSE_Lambda_U.M.Bias))

Lambda_U.M.Bias <- mean(as.numeric(as.vector(MinRMSE_Lambda_U.M.Bias$Lambda)))
Lambda_U.M.Bias
[1] 4.14

# Apply Lambda_U.M.Bias to the full edx data set and determine Regularized User Bias, as well as 
# RMSE results of the Regularized User+Movie Bias Model based on the respective Lambdas:

user_bias_regularized <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n() + Lambda_U.M.Bias))

U.M_bias_model_reg <- edx %>% left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_reg2 = mu_hat + b_i_reg + b_u_reg) %>% pull(y_reg2)

U.M_bias_reg_rmse <- RMSE(edx$rating, U.M_bias_model_reg)
U.M_bias_reg_rmse
[1] 0.8569222

rmse_results <- rmse_results %>% add_row(method = "RegU+Mbias model", RMSE = U.M_bias_reg_rmse)
rmse_results
# A tibble: 9 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 RegMovBias model 0.942
5 RegU+Mbias model 0.858
6 RegMovBias model 0.943
7 RegU+Mbias model 0.858
8 RegMovBias model 0.942
9 RegU+Mbias model 0.857


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|RegMovBias model | 0.9424392|
|RegU+Mbias model | 0.8576959|
|RegMovBias model | 0.9426300|
|RegU+Mbias model | 0.8577426|
|RegMovBias model | 0.9423683|
|RegU+Mbias model | 0.8569222|


# Summary of rmse_results from all approaches to determination of the Regularized U+M Bias Model: ----
[1] 0.8577426  #  L1=9.36   L2=9.36    CV with 10 x slice_sample with replacement at 100% edx_train (RERUN-2)
[2] 0.8577397  #  L1=9.344  L2=9.344   Recalculation of RMSE using L1=L2=9.344 with edx_train
[3] 0.8576959  #  L1=4      L2=9.76    CV with 10 x slice_sample with replacement at 100% edx_train (RERUN-1)
[4] 0.8576188  #  L1=2.824  L2=9.344   CV with 25 x slice_sample 25% with replacement and 25 x rsample bootstraps 37% of edx_train
[5] 0.8570452  #  L1=5      L2=5       Recalculation of RMSE using L1=L2=5 with 100% edx, NO Bootstraps, NO CV
[6] 0.8570188  #  L1=2.824  L2=5       Recalculation of RMSE using L1=2.824 with L2=5
[7] 0.8570155  #  L1=1.5    L2=5       Initial Optimization of Lambda wtih 100% edx, NO Bootstraps, NO CV
[8] 0.8569222  #  L1=1.42   L2=4.14    CV with 10 x slice_sample with replacement at 100% of full edx vs full edx$rating (RERUN-3)
[9] 0.8567039  #  U + M bias model     Basic bias model on full edx and full edx$rating with NO Regularization

# CONCLUSION:  
# Go with RERUN-3 Cross Validation of Lambdas to avoid model that is Overtrained to the Noise
# of very low number observations in the Non-Regularized Basic Bias Model, U + M bias model.

# VERIFICATION OF CONCLUSION:
# TEST OF THE REGULARIZED USER BIAS + MOVIE BIAS MODEL ON THE VALIDATION DATA SET: ----
U.M_bias_model_reg_test <- validation %>% 
  left_join(movie_bias_regularized, by='movieId') %>% 
  left_join(user_bias_regularized, by='userId') %>%
  mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% pull(y_hat)

U.M_bias_reg_test_rmse <- RMSE(validation$rating, U.M_bias_model_reg_test)
U.M_bias_reg_test_rmse
[1] 0.8648873 # RegU+Mbias model, L1=1.42, L2=4.14, CV 10 x slice_sample 100% full edx vs full edx$rating (RERUN-3)
# which is 0.0004615 better (less) than:
[1] 0.8653488 # U + M bias model, basic bias model with NO Regularization

0.8653488 - 0.8648873
[1] 0.0004615

# start a results table to compare different approaches:
rmse_testresults <- rmse_testresults %>% add_row(method = "RegU+Mbias model", RMSE = U.M_bias_reg_test_rmse)

rmse_testresults
# A tibble: 2 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865


rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|


