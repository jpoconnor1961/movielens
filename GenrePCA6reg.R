library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

GENr10reg_pq <- pcaGENr10reg$x[,6] %*% t(pcaGENr10reg$rotation[,6])

GENr10reg_pq_df <- as.data.frame(GENr10reg_pq)

rm(GENr10reg_pq)

x <- GENr10reg_pq_df %>% mutate(userId = rownames(pcaGENr10reg$x))

rm(GENr10reg_pq_df)

y <- data.frame(userId = rownames(pcaGENr10reg$x))

GENr10reg_pq_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

rm(x, y)

GENr10reg_pq_dfl <- pivot_longer(data = GENr10reg_pq_dfw, cols = 2:763, names_to = "genreId", values_to = "p6q6")

rm(GENr10reg_pq_dfw)

GENr10reg_pq_dfl <- GENr10reg_pq_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

pq_U.M_reg_partA <- edx %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias_regularized, by = 'userId') %>% 
  left_join(movie_bias_regularized, by = 'movieId') %>% 
  plyr::join(P1Q1_to_P5Q5_reg, by = c('userId', 'genreId'), type = "left", match = "first")

rm(P1Q1_to_P5Q5_reg)

pq_U.M_reg <- plyr::join(pq_U.M_reg_partA, GENr10reg_pq_dfl, by = c('userId', 'genreId'), type = "left", match = "first")

rm(GENr10reg_pq_dfl, pq_U.M_reg_partA)

pq_U.M_reg$p6q6[is.na(pq_U.M_reg$p6q6)] <- 0

P1Q1_to_P6Q6_reg <- pq_U.M_reg %>% select(userId, genreId, p1q1, p2q2, p3q3, p4q4, p5q5, p6q6)

P1Q1_to_P6Q6_reg
         userId genreId          p1q1          p2q2          p3q3          p4q4          p5q5          p6q6
      1:      1       1  0.0065397603 -0.0169612290  0.1020318379 -6.236647e-04  0.0271653397  0.0351313298
      2:      1       2  0.0204719222  0.0153556703  0.0311073523 -3.179366e-02  0.0182728238  0.0054432299
      3:      1       3  0.0249042703  0.0003516963  0.0204765201 -1.518001e-02 -0.0041548163  0.0014702530
      4:      1       4  0.0726147432  0.0139178490 -0.1707460857  5.408927e-02 -0.0690068238  0.0749328177
      5:      1       5  0.0259740732  0.0014596536 -0.0083621212 -3.129926e-03 -0.0090234936  0.0109907007
---                                                                                                   
9000051:  32620      61 -0.1426287970  0.0838360622  0.0486993511 -8.407503e-02  0.1806868925  0.0406347563
9000052:  40976     128  0.0700127182  0.0275372294  0.0056762579 -1.021133e-02 -0.0464927756 -0.0024672561
9000053:  59269     597  0.0003446536  0.0015239991 -0.0002825926  5.336206e-05 -0.0001199366  0.0001192565
9000054:  60713     128  0.0531311241  0.0071940651  0.0151419916  1.294939e-02  0.0570405795 -0.0015110649
9000055:  64621      49  0.0002787500  0.0042794231 -0.1537777001 -5.824105e-02 -0.0382165690  0.0238541214

PQ_UM_reg_model <- pq_U.M_reg %>% 
  mutate(pq_um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3 + p4q4 + p5q5 + p6q6) %>% 
  pull(pq_um_reg)

rm(pq_U.M_reg)

P6Q6UM_reg_rmse <- RMSE(edx$rating, PQ_UM_reg_model)

P6Q6UM_reg_rmse
[1] 0.8320133

rmse_results <- rmse_results %>% add_row(method = "P6Q6U+Mreg model", RMSE = P6Q6UM_reg_rmse)

rmse_results
# A tibble: 12 x 2
   method            RMSE
   <chr>            <dbl>
 1 naive mean model 1.06 
 2 movie bias model 0.942
 3 RegMovBias model 0.942
 4 U + M bias model 0.857
 5 RegU+Mbias model 0.857
 6 P1Q1 + U+M model 0.849
 7 P1Q1U+Mreg model 0.849
 8 P2Q2U+Mreg model 0.845
 9 P3Q3U+Mreg model 0.842
10 P4Q4U+Mreg model 0.838
11 P5Q5U+Mreg model 0.835
12 P6Q6U+Mreg model 0.832


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|RegMovBias model | 0.9423683|
|U + M bias model | 0.8567039|
|RegU+Mbias model | 0.8569222|
|P1Q1 + U+M model | 0.8493028|
|P1Q1U+Mreg model | 0.8493764|
|P2Q2U+Mreg model | 0.8446755|
|P3Q3U+Mreg model | 0.8417031|
|P4Q4U+Mreg model | 0.8375441|
|P5Q5U+Mreg model | 0.8347762|
|P6Q6U+Mreg model | 0.8320133|


rm(PQ_UM_reg_model)


PQ_UM_reg_test <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId') %>% 
  plyr::join(P1Q1_to_P6Q6_reg, by = c('userId', 'genreId'), type = "left", match = "first")

PQ_UM_reg_test$p1q1[is.na(PQ_UM_reg_test$p1q1)] <- 0
PQ_UM_reg_test$p2q2[is.na(PQ_UM_reg_test$p2q2)] <- 0
PQ_UM_reg_test$p3q3[is.na(PQ_UM_reg_test$p3q3)] <- 0
PQ_UM_reg_test$p4q4[is.na(PQ_UM_reg_test$p4q4)] <- 0
PQ_UM_reg_test$p5q5[is.na(PQ_UM_reg_test$p5q5)] <- 0
PQ_UM_reg_test$p6q6[is.na(PQ_UM_reg_test$p6q6)] <- 0

PQ_UM_reg_test
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2         p3q3        p4q4        p5q5          p6q6
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342  0.158458182  0.08367722  0.04815221 -0.0638857903
     2:      1      31     480  1.3787473  0.15104929  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000
     3:      1      61     586  1.3787473 -0.45676603  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000
     4:      2      33     151 -0.1901245  0.01758977  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000
     5:      2      66     858 -0.1901245  0.90282854  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000
---                                                                                                                          
999995:  71566      43     235  0.2366598  0.15098277  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717 -0.002702233 -0.01983065 -0.03183783 -0.0064632143
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792 -0.010911548 -0.07822204  0.01653825 -0.0007738561
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364  0.267021160  0.01009616 -0.02068114  0.0003278727
999999:  71567       1     898 -0.0694971  0.70352257  0.000000000  0.00000000  0.000000000  0.00000000  0.00000000  0.0000000000

PQ_UM_reg_model_test <- PQ_UM_reg_test %>% 
  mutate(pq_um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3 + p4q4 + p5q5 + p6q6) %>% 
  pull(pq_um_reg)

P6Q6UM_reg_test_rmse <- RMSE(validation$rating, PQ_UM_reg_model_test)
P6Q6UM_reg_test_rmse
[1] 0.8640007

rm(PQ_UM_reg_test, PQ_UM_reg_model_test)

rmse_testresults <- rmse_testresults %>% add_row(method = "P6Q6U+Mreg model", RMSE = P6Q6UM_reg_test_rmse)

rmse_testresults
# A tibble: 8 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865
3 P1Q1U+Mreg model 0.862
4 P2Q2U+Mreg model 0.862
5 P3Q3U+Mreg model 0.863
6 P4Q4U+Mreg model 0.863
7 P5Q5U+Mreg model 0.863
8 P6Q6U+Mreg model 0.864

rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|
|P1Q1U+Mreg model | 0.8624939|
|P2Q2U+Mreg model | 0.8620102|
|P3Q3U+Mreg model | 0.8628955|
|P4Q4U+Mreg model | 0.8625435|
|P5Q5U+Mreg model | 0.8628890|
|P6Q6U+Mreg model | 0.8640007|


