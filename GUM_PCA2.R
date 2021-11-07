library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggrepel)

# Calculate residual matrix mug50r_p2q2 from product of the 2nd principle components of p (pca_mug50r$x) and q (pca_mug50r$rotation):
mug50r_p2q2 <- pca_mug50r$x[, 2] %*% t(pca_mug50r$rotation[, 2])

dim(mug50r_p2q2)
[1] 40595  7081

# Convert matrix mug50r_p2q2 to a data frame:
mug50r_p2q2_df <- as.data.frame(mug50r_p2q2)

rm(mug50r_p2q2)

# Add userId column to mug50r_p2q2_df data frame and save as data frame object x:
x <- mug50r_p2q2_df %>% mutate(userId = rownames(pca_mug50r$x))

# Create data frame object y with a 1st column of userIds from row names of user effects matrix pca_mug50r$x:
y <- data.frame(userId = rownames(pca_mug50r$x))

rm(mug50r_p2q2_df)

# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
mug50r_p2q2_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

dim(mug50r_p2q2_dfw)
[1] 40595  7082

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y)

# Create a long data frame mug50r_p2q2_dfl from the wide data frame to get all rows into the edx data frame order and format:
mug50r_p2q2_dfl <- pivot_longer(data = mug50r_p2q2_dfw, cols = 2:7082, names_to = "movieId", values_to = "p2q2")

# Convert userId and movieId columns from <chr> to <int> values in the long data frame:
mug50r_p2q2_dfl <- mug50r_p2q2_dfl %>% mutate(userId = as.integer(userId), movieId = as.integer(movieId))

mug50r_p2q2_dfl
# A tibble: 287,453,195 × 3
   userId movieId      p2q2
    <int>   <int>     <dbl>
 1      5       1 -0.0630  
 2      5       7  0.00396 
 3      5      25 -0.00178 
 4      5      28  0.00122 
 5      5      30  0.00136 
 6      5      32 -0.00878 
 7      5      47 -0.0191  
 8      5      52  0.00115 
 9      5      57  0.00198 
10      5      58  0.000647
# … with 287,453,185 more rows

sum(is.na(mug50r_p2q2_dfl$p2q2) == TRUE)
[1] 0

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(mug50r_p2q2_dfw)

# mug50r_p2q2_edx organizes the p2q2 elements of the long data frame, mug50r_p2q2_dfl, to the corresponding
# userId and movieId of edx:
mug50r_p2q2_edx <- edx %>% select(userId, movieId) %>% left_join(mug50r_p2q2_dfl, by = c('userId', 'movieId'))

# 958,197 observations with NA due to 29283 fewer users and 3596 fewer movies in the matrix factorization:
sum(is.na(mug50r_p2q2_edx$p2q2) == TRUE)
[1] 958197

sum(is.na(mug50r_p2q2_edx$p2q2) == FALSE)
[1] 8041858

958197 + 8041858 == 9000055
[1] TRUE

# Replace NA with zero (0) in the 958,197 observations of mug50r_p2q2_edx$p2q2, and verify that no NA remains:
mug50r_p2q2_edx$p2q2[is.na(mug50r_p2q2_edx$p2q2)] <- 0

sum(is.na(mug50r_p2q2_edx$p2q2) == TRUE)
[1] 0

sum(is.na(mug50r_p2q2_edx$p2q2) == FALSE)
[1] 9000055

mug50r_p2q2_edx[100:110]
    userId movieId         p2q2
 1:      4     592  0.000000000
 2:      4     595  0.000000000
 3:      4     597  0.000000000
 4:      5       1 -0.063031424
 5:      5       7  0.003959314
 6:      5      25 -0.001778316
 7:      5      28  0.001223767
 8:      5      30  0.001357435
 9:      5      32 -0.008782811
10:      5      47 -0.019133636
11:      5      52  0.001148525


# mug50r_p2q2_val organizes the p2q2 elements of the long data frame, mug50r_p2q2_dfl, to the corresponding
# userId and movieId of validation:
mug50r_p2q2_val <- validation %>% select(userId, movieId) %>% left_join(mug50r_p2q2_dfl, by = c('userId', 'movieId'))

sum(is.na(mug50r_p2q2_val$p2q2) == TRUE)
[1] 109507

sum(is.na(mug50r_p2q2_val$p2q2) == FALSE)
[1] 890492

109507 + 890492 == 999999
[1] TRUE

# Replace NA with zero (0) in the 958,197 observations of mug50r_p2q2_val$p2q2, and verify that no NA remains:
mug50r_p2q2_val$p2q2[is.na(mug50r_p2q2_val$p2q2)] <- 0

sum(is.na(mug50r_p2q2_val$p2q2) == TRUE)
[1] 0

sum(is.na(mug50r_p2q2_val$p2q2) == FALSE)
[1] 999999

mug50r_p2q2_val
        userId movieId        p2q2
     1:      1     231 0.000000000
     2:      1     480 0.000000000
     3:      1     586 0.000000000
     4:      2     151 0.000000000
     5:      2     858 0.000000000
    ---                           
999995:  71566     235 0.000000000
999996:  71566     273 0.000000000
999997:  71566     434 0.000000000
999998:  71567     480 0.139719391
999999:  71567     898 0.001590026


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(mug50r_p2q2_dfl)

# TEST OF THE mug50r_p2q2_model ON THE EDX DATA SET: ----
# mug50r_p2q2_model_edx adds the corresponding movie bias, user bias, genre bias, and p1q1 to p2q2 mug50r residual factors 
# to the mean rating of all edx movies, in the context of the edx data frame:
mug50r_p2q2_model_edx <- edx %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias, by = 'userId') %>%
  left_join(movie_bias, by = 'movieId') %>%
  left_join(genre_bias, by = 'genreId') %>%
  mutate(p1q1 = mug50r_p1q1_edx$p1q1) %>%
  mutate(p2q2 = mug50r_p2q2_edx$p2q2) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1 + p2q2) %>% 
  pull(y_hat)

length(mug50r_p2q2_model_edx)
[1] 9000055

# RMSE of the mug50r_p2q2_model_edx where y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1 + p2q2:
mug50r_p2q2_rmse_edx <- RMSE(edx$rating, mug50r_p2q2_model_edx)

mug50r_p2q2_rmse_edx
[1] 0.8433755

# add RMSE result of mug50r_p2q2_model_edx to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "GUM + P2Q2 model", RMSE = mug50r_p2q2_rmse_edx)

rmse_results
# A tibble: 6 × 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 G+U+M bias model 0.856
5 GUM + P1Q1 model 0.848
6 GUM + P2Q2 model 0.843


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|G+U+M bias model | 0.8563595|
|GUM + P1Q1 model | 0.8482445|
|GUM + P2Q2 model | 0.8433755|


rm(mug50r_p2q2_model_edx)


# TEST OF THE mug50r_p2q2_model ON THE VALIDATION DATA SET: ----
# mug50r_p2q2_model_val adds the corresponding movie bias, user bias, genre bias, and p1q1 to p2q2 mug50r residual factors
# to the mean rating of all edx movies, in the context of the validation data frame:
mug50r_p2q2_model_val <- validation %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias, by = 'userId') %>%
  left_join(movie_bias, by = 'movieId') %>%
  left_join(genre_bias, by = 'genreId') %>%
  mutate(p1q1 = mug50r_p1q1_val$p1q1) %>%
  mutate(p2q2 = mug50r_p2q2_val$p2q2) %>% 
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1 + p2q2) %>% 
  pull(y_hat)

length(mug50r_p2q2_model_val)
[1] 999999

mug50r_p2q2_rmse_val <- RMSE(validation$rating, mug50r_p2q2_model_val)
mug50r_p2q2_rmse_val
[1] 0.8539592

rmse_testresults <- rmse_testresults %>% add_row(method = "GUM + P2Q2 model", RMSE = mug50r_p2q2_rmse_val)
rmse_testresults
# A tibble: 4 × 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 G+U+M bias model 0.865
3 GUM + P1Q1 model 0.858
4 GUM + P2Q2 model 0.854


rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|G+U+M bias model | 0.8649469|
|GUM + P1Q1 model | 0.8578414|
|GUM + P2Q2 model | 0.8539592|


rm(mug50r_p2q2_model_val)


