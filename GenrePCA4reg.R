library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

GENr10reg_p4q4 <- pcaGENr10reg$x[,4] %*% t(pcaGENr10reg$rotation[,4])

dim(GENr10reg_p4q4)
[1] 69878   762

GENr10reg_p4q4_df <- as.data.frame(GENr10reg_p4q4)

rm(GENr10reg_p4q4)

x <- GENr10reg_p4q4_df %>% mutate(userId = rownames(pcaGENr10reg$x))

rm(GENr10reg_p4q4_df)

y <- data.frame(userId = rownames(pcaGENr10reg$x))

GENr10reg_p4q4_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

dim(GENr10reg_p4q4_dfw)
[1] 69878   763

rm(x, y)

GENr10reg_p4q4_dfl <- pivot_longer(data = GENr10reg_p4q4_dfw, cols = 2:763, names_to = "genreId", values_to = "p4q4")

rm(GENr10reg_p4q4_dfw)

GENr10reg_p4q4_dfl <- GENr10reg_p4q4_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

GENr10reg_p4q4_dfl
# A tibble: 53,247,036 x 3
   userId genreId      p4q4
    <int>   <int>     <dbl>
 1      1       1 -0.000624
 2      1       2 -0.0318  
 3      1       3 -0.0152  
 4      1       4  0.0541  
 5      1       5 -0.00313 
 6      1       6  0.0151  
 7      1       7 -0.124   
 8      1       8  0.00507 
 9      1       9  0.0196  
10      1      10  0.0214  
# ... with 53,247,026 more rows

p4q4_U.M_reg_partA <- edx %>% select(userId, movieId, genreId) %>% 
  left_join(user_bias_regularized, by = 'userId') %>% 
  left_join(movie_bias_regularized, by = 'movieId') 

dim(p4q4_U.M_reg_partA)
[1] 9000055       5

p4q4_U.M_reg_partA <- plyr::join(p4q4_U.M_reg_partA, P1Q1_P2Q2_P3Q3_regularized, by = c('userId', 'genreId'), 
                                 type = "left", match = "first")

sum(is.na(p4q4_U.M_reg_partA$p1q1) == TRUE)
[1] 0

sum(is.na(p4q4_U.M_reg_partA$p2q2) == TRUE)
[1] 0

sum(is.na(p4q4_U.M_reg_partA$p3q3) == TRUE)
[1] 0

dim(p4q4_U.M_reg_partA)
[1] 9000055       8

rm(P1Q1_P2Q2_P3Q3_regularized)

p4q4_U.M_reg_partB <- p4q4_U.M_reg_partA %>% left_join(GENr10reg_p4q4_dfl, by = c('userId', 'genreId'))

p4q4_U.M_reg_partB
         userId genreId movieId     b_u_reg     b_i_reg          p1q1          p2q2          p3q3          p4q4
      1:      1       1     122  1.37874734 -0.65345331  0.0065397603 -0.0169612290  0.1020318379 -6.236647e-04
      2:      1       2     185  1.37874734 -0.38309079  0.0204719222  0.0153556703  0.0311073523 -3.179366e-02
      3:      1       3     292  1.37874734 -0.09444526  0.0249042703  0.0003516963  0.0204765201 -1.518001e-02
      4:      1       4     316  1.37874734 -0.16277459  0.0726147432  0.0139178490 -0.1707460857  5.408927e-02
      5:      1       5     329  1.37874734 -0.17499108  0.0259740732  0.0014596536 -0.0083621212 -3.129926e-03
---                                                                                                       
9000051:  32620      61   33140  0.41365199 -0.00515091 -0.1426287970  0.0838360622  0.0486993511 -8.407503e-02
9000052:  40976     128   61913 -0.18616444 -0.21176248  0.0700127182  0.0275372294  0.0056762579 -1.021133e-02
9000053:  59269     597   63141 -0.01568136 -0.62498562  0.0003446536  0.0015239991 -0.0002825926  5.336206e-05
9000054:  60713     128    4820 -0.47007568 -0.62498562  0.0531311241  0.0071940651  0.0151419916  1.294939e-02
9000055:  64621      49   39429  0.02768848 -0.41837405  0.0002787500  0.0042794231 -0.1537777001 -5.824105e-02

rm(GENr10reg_p4q4_dfl, p4q4_U.M_reg_partA)

sum(is.na(p4q4_U.M_reg_partB$p4q4) == TRUE)
[1] 172

sum(is.na(p4q4_U.M_reg_partB$p4q4) == FALSE)
[1] 8999883

172 + 8999883 == 9000055
[1] TRUE

p4q4_U.M_reg_partB$p4q4[is.na(p4q4_U.M_reg_partB$p4q4)] <- 0

sum(is.na(p4q4_U.M_reg_partB$p4q4) == TRUE)
[1] 0

sum(is.na(p4q4_U.M_reg_partB$p4q4) == FALSE)
[1] 9000055

P1Q1_P2Q2_P3Q3_P4Q4_reg <- p4q4_U.M_reg_partB %>% select(userId, genreId, p1q1, p2q2, p3q3, p4q4)

P1Q1_P2Q2_P3Q3_P4Q4_reg
         userId genreId          p1q1          p2q2          p3q3          p4q4
      1:      1       1  0.0065397603 -0.0169612290  0.1020318379 -6.236647e-04
      2:      1       2  0.0204719222  0.0153556703  0.0311073523 -3.179366e-02
      3:      1       3  0.0249042703  0.0003516963  0.0204765201 -1.518001e-02
      4:      1       4  0.0726147432  0.0139178490 -0.1707460857  5.408927e-02
      5:      1       5  0.0259740732  0.0014596536 -0.0083621212 -3.129926e-03
---                                                                       
9000051:  32620      61 -0.1426287970  0.0838360622  0.0486993511 -8.407503e-02
9000052:  40976     128  0.0700127182  0.0275372294  0.0056762579 -1.021133e-02
9000053:  59269     597  0.0003446536  0.0015239991 -0.0002825926  5.336206e-05
9000054:  60713     128  0.0531311241  0.0071940651  0.0151419916  1.294939e-02
9000055:  64621      49  0.0002787500  0.0042794231 -0.1537777001 -5.824105e-02

P4Q4UM_reg_model <- p4q4_U.M_reg_partB %>% mutate(p4q4um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3 + p4q4) %>% 
                    pull(p4q4um_reg)

length(P4Q4UM_reg_model)
[1] 9000055

rm(p4q4_U.M_reg_partB)

P4Q4UM_reg_rmse <- RMSE(edx$rating, P4Q4UM_reg_model)

P4Q4UM_reg_rmse
[1] 0.8375441

rmse_results <- rmse_results %>% add_row(method = "P4Q4U+Mreg model", RMSE = P4Q4UM_reg_rmse)

rmse_results
# A tibble: 10 x 2
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


rm(P4Q4UM_reg_model)

# TEST OF THE REGULARIZED USER + MOVIE BIAS + P1Q1 + P2Q2 + P3Q3 + P4Q4 MODEL ON THE VALIDATION DATA SET: ----
P4Q4UM_reg_model_test_partA <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId')

P4Q4UM_reg_model_test_partB <- plyr::join(P4Q4UM_reg_model_test_partA, P1Q1_P2Q2_P3Q3_P4Q4_reg, 
                                          by = c('userId', 'genreId'), type = "left", match = "first")

sum(is.na(P4Q4UM_reg_model_test_partB$p1q1) == TRUE)
[1] 336086

sum(is.na(P4Q4UM_reg_model_test_partB$p2q2) == TRUE)
[1] 336086

sum(is.na(P4Q4UM_reg_model_test_partB$p3q3) == TRUE)
[1] 336086

sum(is.na(P4Q4UM_reg_model_test_partB$p4q4) == TRUE)
[1] 336086

P4Q4UM_reg_model_test_partB$p1q1[is.na(P4Q4UM_reg_model_test_partB$p1q1)] <- 0
P4Q4UM_reg_model_test_partB$p2q2[is.na(P4Q4UM_reg_model_test_partB$p2q2)] <- 0
P4Q4UM_reg_model_test_partB$p3q3[is.na(P4Q4UM_reg_model_test_partB$p3q3)] <- 0
P4Q4UM_reg_model_test_partB$p4q4[is.na(P4Q4UM_reg_model_test_partB$p4q4)] <- 0

P4Q4UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2         p3q3        p4q4
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342  0.158458182  0.08367722
     2:      1      31     480  1.3787473  0.15104929  0.000000000  0.00000000  0.000000000  0.00000000
     3:      1      61     586  1.3787473 -0.45676603  0.000000000  0.00000000  0.000000000  0.00000000
     4:      2      33     151 -0.1901245  0.01758977  0.000000000  0.00000000  0.000000000  0.00000000
     5:      2      66     858 -0.1901245  0.90282854  0.000000000  0.00000000  0.000000000  0.00000000
---                                                                                                
999995:  71566      43     235  0.2366598  0.15098277  0.000000000  0.00000000  0.000000000  0.00000000
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717 -0.002702233 -0.01983065
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792 -0.010911548 -0.07822204
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364  0.267021160  0.01009616
999999:  71567       1     898 -0.0694971  0.70352257  0.000000000  0.00000000  0.000000000  0.00000000

sum(is.na(P4Q4UM_reg_model_test_partB$p1q1) == TRUE)
[1] 0

sum(is.na(P4Q4UM_reg_model_test_partB$p2q2) == TRUE)
[1] 0

sum(is.na(P4Q4UM_reg_model_test_partB$p3q3) == TRUE)
[1] 0

sum(is.na(P4Q4UM_reg_model_test_partB$p4q4) == TRUE)
[1] 0

rm(P4Q4UM_reg_model_test_partA)

P4Q4UM_reg_model_test <- P4Q4UM_reg_model_test_partB %>% 
  mutate(p4q4um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3 + p4q4) %>% 
  pull(p4q4um_reg)

length(P4Q4UM_reg_model_test)
[1] 999999

P4Q4UM_reg_test_rmse <- RMSE(validation$rating, P4Q4UM_reg_model_test)
P4Q4UM_reg_test_rmse
[1] 0.8625435

rm(P4Q4UM_reg_model_test_partB, P4Q4UM_reg_model_test)

rmse_testresults <- rmse_testresults %>% add_row(method = "P4Q4U+Mreg model", RMSE = P4Q4UM_reg_test_rmse)

rmse_testresults
# A tibble: 6 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865
3 P1Q1U+Mreg model 0.862
4 P2Q2U+Mreg model 0.862
5 P3Q3U+Mreg model 0.863
6 P4Q4U+Mreg model 0.863

rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|
|P1Q1U+Mreg model | 0.8624939|
|P2Q2U+Mreg model | 0.8620102|
|P3Q3U+Mreg model | 0.8628955|
|P4Q4U+Mreg model | 0.8625435|


