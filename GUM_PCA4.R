library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

mug50r_pq <- pca_mug50r$x[, 4] %*% t(pca_mug50r$rotation[, 4])

mug50r_pq_df <- as.data.frame(mug50r_pq)
rm(mug50r_pq)
x <- mug50r_pq_df %>% mutate(userId = rownames(pca_mug50r$x))
y <- data.frame(userId = rownames(pca_mug50r$x))
rm(mug50r_pq_df)
mug50r_pq_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")
rm(x, y)

mug50r_pq_dfl <- pivot_longer(data = mug50r_pq_dfw, cols = 2:7082, names_to = "movieId", values_to = "p4q4")

mug50r_pq_dfl <- mug50r_pq_dfl %>% mutate(userId = as.integer(userId), movieId = as.integer(movieId))
rm(mug50r_pq_dfw)

mug50r_p4q4_edx <- edx %>% select(userId, movieId) %>% left_join(mug50r_pq_dfl, by = c('userId', 'movieId'))
mug50r_p4q4_edx$p4q4[is.na(mug50r_p4q4_edx$p4q4)] <- 0
mug50r_p4q4_val <- validation %>% select(userId, movieId) %>% left_join(mug50r_pq_dfl, by = c('userId', 'movieId'))
mug50r_p4q4_val$p4q4[is.na(mug50r_p4q4_val$p4q4)] <- 0

rm(mug50r_pq_dfl)

mug50r_pq_model_edx <- edx %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias, by = 'userId') %>%
  left_join(movie_bias, by = 'movieId') %>%
  left_join(genre_bias, by = 'genreId') %>%
  mutate(p1q1 = mug50r_p1q1_edx$p1q1) %>%
  mutate(p2q2 = mug50r_p2q2_edx$p2q2) %>%
  mutate(p3q3 = mug50r_p3q3_edx$p3q3) %>%
  mutate(p4q4 = mug50r_p4q4_edx$p4q4) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1 + p2q2 + p3q3 + p4q4) %>% 
  pull(y_hat)
mug50r_p4q4_rmse_edx <- RMSE(edx$rating, mug50r_pq_model_edx)
mug50r_p4q4_rmse_edx
[1] 0.8358572

rmse_results <- rmse_results %>% add_row(method = "GUM + P4Q4 model", RMSE = mug50r_p4q4_rmse_edx)
rmse_results
# A tibble: 8 × 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 G+U+M bias model 0.856
5 GUM + P1Q1 model 0.848
6 GUM + P2Q2 model 0.843
7 GUM + P3Q3 model 0.839
8 GUM + P4Q4 model 0.836


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|G+U+M bias model | 0.8563595|
|GUM + P1Q1 model | 0.8482445|
|GUM + P2Q2 model | 0.8433755|
|GUM + P3Q3 model | 0.8390438|
|GUM + P4Q4 model | 0.8358572|


rm(mug50r_pq_model_edx)

mug50r_pq_model_val <- validation %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias, by = 'userId') %>%
  left_join(movie_bias, by = 'movieId') %>%
  left_join(genre_bias, by = 'genreId') %>%
  mutate(p1q1 = mug50r_p1q1_val$p1q1) %>%
  mutate(p2q2 = mug50r_p2q2_val$p2q2) %>%
  mutate(p3q3 = mug50r_p3q3_val$p3q3) %>%
  mutate(p4q4 = mug50r_p4q4_val$p4q4) %>%
  mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1 + p2q2 + p3q3 + p4q4) %>% 
  pull(y_hat)
mug50r_p4q4_rmse_val <- RMSE(validation$rating, mug50r_pq_model_val)
mug50r_p4q4_rmse_val
[1] 0.8484766

rmse_testresults <- rmse_testresults %>% add_row(method = "GUM + P4Q4 model", RMSE = mug50r_p4q4_rmse_val)
rmse_testresults
# A tibble: 6 × 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 G+U+M bias model 0.865
3 GUM + P1Q1 model 0.858
4 GUM + P2Q2 model 0.854
5 GUM + P3Q3 model 0.851
6 GUM + P4Q4 model 0.848


rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|G+U+M bias model | 0.8649469|
|GUM + P1Q1 model | 0.8578414|
|GUM + P2Q2 model | 0.8539592|
|GUM + P3Q3 model | 0.8505624|
|GUM + P4Q4 model | 0.8484766|


rm(mug50r_pq_model_val)


