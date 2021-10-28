library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

# Calculate residual matrix GENr10reg_p3q3 from product of the 3rd principle components of p (pcaGENr10reg$x) 
# and q (pcaGENr10reg$rotation):
GENr10reg_p3q3 <- pcaGENr10reg$x[,3] %*% t(pcaGENr10reg$rotation[,3])

dim(GENr10reg_p3q3)
[1] 69878   762

# Convert matrix GENr10reg_p3q3 to a data frame:
GENr10reg_p3q3_df <- as.data.frame(GENr10reg_p3q3)

rm(GENr10reg_p3q3)

# Add userId column to GENr10reg_p3q3_df data frame and save as data frame object x:
x <- GENr10reg_p3q3_df %>% mutate(userId = rownames(pcaGENr10reg$x))

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(GENr10reg_p3q3_df)

# Create data frame object y with a 1st column of userIds from row names of pcaGENr10reg$x:
y <- data.frame(userId = rownames(pcaGENr10reg$x))

# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
GENr10reg_p3q3_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

dim(GENr10reg_p3q3_dfw)
[1] 69878   763

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y)

# Create long data frame GENr10reg_p3q3_dfl from the wide data frame to arrange p3q3 data into a table joining format:
GENr10reg_p3q3_dfl <- pivot_longer(data = GENr10reg_p3q3_dfw, cols = 2:763, names_to = "genreId", values_to = "p3q3")

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(GENr10reg_p3q3_dfw)

# Convert userId and genreId columns from <chr> to <int> values in the long data frame:
GENr10reg_p3q3_dfl <- GENr10reg_p3q3_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

GENr10reg_p3q3_dfl
# A tibble: 53,247,036 x 3
   userId genreId     p3q3
    <int>   <int>    <dbl>
 1      1       1  0.102  
 2      1       2  0.0311 
 3      1       3  0.0205 
 4      1       4 -0.171  
 5      1       5 -0.00836
 6      1       6  0.0108 
 7      1       7 -0.125  
 8      1       8 -0.00601
 9      1       9 -0.0744 
10      1      10  0.0573 
# ... with 53,247,026 more rows


# p3q3_U.M_reg_model adds the corresponding p3q3 genre factor to the p1q1 & p2q2 genre factors, 
# the regularized user bias, the regularized movie bias, and the mean rating of all edx movies. 
# Model built in stages to avoid crashing R/RStudio:

p3q3_U.M_reg_partA <- edx %>% select(-rating, -timestamp, -title) %>% 
                      left_join(user_bias_regularized, by = 'userId') %>% 
                      left_join(movie_bias_regularized, by = 'movieId') 

dim(p3q3_U.M_reg_partA)
[1] 9000055       6

p3q3_U.M_reg_partA <- plyr::join(p3q3_U.M_reg_partA, P1Q1_P2Q2_regularized, by = c('userId', 'genreId'), 
                                 type = "left", match = "first")

sum(is.na(p3q3_U.M_reg_partA$p1q1) == TRUE)
[1] 0

sum(is.na(p3q3_U.M_reg_partA$p2q2) == TRUE)
[1] 0

dim(p3q3_U.M_reg_partA)
[1] 9000055       8

p3q3_U.M_reg_partA
         userId genreId movieId                        genres     b_u_reg     b_i_reg          p1q1          p2q2
      1:      1       1     122                Comedy|Romance  1.37874734 -0.65345331  0.0065397603 -0.0169612290
      2:      1       2     185         Action|Crime|Thriller  1.37874734 -0.38309079  0.0204719222  0.0153556703
      3:      1       3     292  Action|Drama|Sci-Fi|Thriller  1.37874734 -0.09444526  0.0249042703  0.0003516963
      4:      1       4     316       Action|Adventure|Sci-Fi  1.37874734 -0.16277459  0.0726147432  0.0139178490
      5:      1       5     329 Action|Adventure|Drama|Sci-Fi  1.37874734 -0.17499108  0.0259740732  0.0014596536
---                                                                                                         
9000051:  32620      61   33140               Children|Comedy  0.41365199 -0.00515091 -0.1426287970  0.0838360622
9000052:  40976     128   61913                   Documentary -0.18616444 -0.21176248  0.0700127182  0.0275372294
9000053:  59269     597   63141        Comedy|Musical|Western -0.01568136 -0.62498562  0.0003446536  0.0015239991
9000054:  60713     128    4820                   Documentary -0.47007568 -0.62498562  0.0531311241  0.0071940651
9000055:  64621      49   39429                Drama|Thriller  0.02768848 -0.41837405  0.0002787500  0.0042794231


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P1Q1_P2Q2_regularized)


p3q3_U.M_reg_partB <- p3q3_U.M_reg_partA %>% left_join(GENr10reg_p3q3_dfl, by = c('userId', 'genreId'))

p3q3_U.M_reg_partB
         userId genreId movieId                        genres     b_u_reg     b_i_reg          p1q1          p2q2          p3q3
      1:      1       1     122                Comedy|Romance  1.37874734 -0.65345331  0.0065397603 -0.0169612290  0.1020318379
      2:      1       2     185         Action|Crime|Thriller  1.37874734 -0.38309079  0.0204719222  0.0153556703  0.0311073523
      3:      1       3     292  Action|Drama|Sci-Fi|Thriller  1.37874734 -0.09444526  0.0249042703  0.0003516963  0.0204765201
      4:      1       4     316       Action|Adventure|Sci-Fi  1.37874734 -0.16277459  0.0726147432  0.0139178490 -0.1707460857
      5:      1       5     329 Action|Adventure|Drama|Sci-Fi  1.37874734 -0.17499108  0.0259740732  0.0014596536 -0.0083621212
---                                                                                                                       
9000051:  32620      61   33140               Children|Comedy  0.41365199 -0.00515091 -0.1426287970  0.0838360622  0.0486993511
9000052:  40976     128   61913                   Documentary -0.18616444 -0.21176248  0.0700127182  0.0275372294  0.0056762579
9000053:  59269     597   63141        Comedy|Musical|Western -0.01568136 -0.62498562  0.0003446536  0.0015239991 -0.0002825926
9000054:  60713     128    4820                   Documentary -0.47007568 -0.62498562  0.0531311241  0.0071940651  0.0151419916
9000055:  64621      49   39429                Drama|Thriller  0.02768848 -0.41837405  0.0002787500  0.0042794231 -0.1537777001

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(GENr10reg_p3q3_dfl, p3q3_U.M_reg_partA)

# 172 observations with NA due to 35 genres with 9 or fewer ratings:
sum(is.na(p3q3_U.M_reg_partB$p3q3) == TRUE)
[1] 172

sum(is.na(p3q3_U.M_reg_partB$p3q3) == FALSE)
[1] 8999883

172 + 8999883 == 9000055
[1] TRUE

# REGULARIZATION PART 2 ----
# Replace NA with zero (0) in the 172 observations of p3q3_U.M_reg_partB$p3q3, and verify that no NA remains.
# (See comments in "REGULARIZATION PART 1" at beginning of "MATRIX FACTORIZATION" section in MovieLensGenrePCA1reg tab.)
p3q3_U.M_reg_partB$p3q3[is.na(p3q3_U.M_reg_partB$p3q3)] <- 0

sum(is.na(p3q3_U.M_reg_partB$p3q3) == TRUE)
[1] 0

sum(is.na(p3q3_U.M_reg_partB$p3q3) == FALSE)
[1] 9000055


P1Q1_P2Q2_P3Q3_regularized <- p3q3_U.M_reg_partB %>% select(userId, genreId, p1q1, p2q2, p3q3)

P1Q1_P2Q2_P3Q3_regularized
         userId genreId          p1q1          p2q2          p3q3
      1:      1       1  0.0065397603 -0.0169612290  0.1020318379
      2:      1       2  0.0204719222  0.0153556703  0.0311073523
      3:      1       3  0.0249042703  0.0003516963  0.0204765201
      4:      1       4  0.0726147432  0.0139178490 -0.1707460857
      5:      1       5  0.0259740732  0.0014596536 -0.0083621212
---                                                         
9000051:  32620      61 -0.1426287970  0.0838360622  0.0486993511
9000052:  40976     128  0.0700127182  0.0275372294  0.0056762579
9000053:  59269     597  0.0003446536  0.0015239991 -0.0002825926
9000054:  60713     128  0.0531311241  0.0071940651  0.0151419916
9000055:  64621      49  0.0002787500  0.0042794231 -0.1537777001


P3Q3UM_reg_model <- p3q3_U.M_reg_partB %>% mutate(p3q3um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3) %>% 
                    pull(p3q3um_reg)

P3Q3UM_reg_model[1:10]
[1] 4.329370 4.575057 4.842500 4.644224 4.735293 3.886466 5.280129 4.825947 5.057545 4.418714

length(P3Q3UM_reg_model)
[1] 9000055

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(p3q3_U.M_reg_partB)

# RMSE of the P3Q3UM_reg_model where p3q3um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 +p3q3:
P3Q3UM_reg_rmse <- RMSE(edx$rating, P3Q3UM_reg_model)

P3Q3UM_reg_rmse
[1] 0.8417031

# add RMSE result of P3Q3UM_reg_model to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "P3Q3U+Mreg model", RMSE = P3Q3UM_reg_rmse)

rmse_results
# A tibble: 9 x 2
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

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P3Q3UM_reg_model)

# TEST OF THE REGULARIZED USER + MOVIE BIAS + P1Q1 + P2Q2 + P3Q3 MODEL ON THE VALIDATION DATA SET: ----
P3Q3UM_reg_model_test_partA <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId')

P3Q3UM_reg_model_test_partB <- plyr::join(P3Q3UM_reg_model_test_partA, P1Q1_P2Q2_P3Q3_regularized, 
                                          by = c('userId', 'genreId'), type = "left", match = "first")

P3Q3UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2         p3q3
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342  0.158458182
     2:      1      31     480  1.3787473  0.15104929           NA          NA           NA
     3:      1      61     586  1.3787473 -0.45676603           NA          NA           NA
     4:      2      33     151 -0.1901245  0.01758977           NA          NA           NA
     5:      2      66     858 -0.1901245  0.90282854           NA          NA           NA
---                                                                                    
999995:  71566      43     235  0.2366598  0.15098277           NA          NA           NA
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717 -0.002702233
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792 -0.010911548
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364  0.267021160
999999:  71567       1     898 -0.0694971  0.70352257           NA          NA           NA


sum(is.na(P3Q3UM_reg_model_test_partB$p1q1) == TRUE)
[1] 336086

sum(is.na(P3Q3UM_reg_model_test_partB$p2q2) == TRUE)
[1] 336086

sum(is.na(P3Q3UM_reg_model_test_partB$p3q3) == TRUE)
[1] 336086

# Replace NA with zero (0) in P3Q3UM_reg_model_test_partB$p1q1, in P3Q3UM_reg_model_test_partB$p2q2, 
# in P3Q3UM_reg_model_test_partB$p3q3 and verify that no NA remains:
P3Q3UM_reg_model_test_partB$p1q1[is.na(P3Q3UM_reg_model_test_partB$p1q1)] <- 0
P3Q3UM_reg_model_test_partB$p2q2[is.na(P3Q3UM_reg_model_test_partB$p2q2)] <- 0
P3Q3UM_reg_model_test_partB$p3q3[is.na(P3Q3UM_reg_model_test_partB$p3q3)] <- 0

P3Q3UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2         p3q3
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342  0.158458182
     2:      1      31     480  1.3787473  0.15104929  0.000000000  0.00000000  0.000000000
     3:      1      61     586  1.3787473 -0.45676603  0.000000000  0.00000000  0.000000000
     4:      2      33     151 -0.1901245  0.01758977  0.000000000  0.00000000  0.000000000
     5:      2      66     858 -0.1901245  0.90282854  0.000000000  0.00000000  0.000000000
---                                                                                    
999995:  71566      43     235  0.2366598  0.15098277  0.000000000  0.00000000  0.000000000
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717 -0.002702233
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792 -0.010911548
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364  0.267021160
999999:  71567       1     898 -0.0694971  0.70352257  0.000000000  0.00000000  0.000000000

sum(is.na(P3Q3UM_reg_model_test_partB$p1q1) == TRUE)
[1] 0

sum(is.na(P3Q3UM_reg_model_test_partB$p2q2) == TRUE)
[1] 0

sum(is.na(P3Q3UM_reg_model_test_partB$p3q3) == TRUE)
[1] 0

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P3Q3UM_reg_model_test_partA)

P3Q3UM_reg_model_test <- P3Q3UM_reg_model_test_partB %>% 
                         mutate(p3q3um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2 + p3q3) %>% 
                         pull(p3q3um_reg)

P3Q3UM_reg_model_test[1:10]
[1] 4.480525 5.042262 4.434447 3.339930 4.225169 2.755264 3.975956 4.139397 4.288728 3.317691

length(P3Q3UM_reg_model_test)
[1] 999999

P3Q3UM_reg_test_rmse <- RMSE(validation$rating, P3Q3UM_reg_model_test)
P3Q3UM_reg_test_rmse
[1] 0.8628955

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P3Q3UM_reg_model_test_partB, P3Q3UM_reg_model_test)

# add RMSE result of P3Q3UM_reg_model_test to the RMSE table to compare the different models:
rmse_testresults <- rmse_testresults %>% add_row(method = "P3Q3U+Mreg model", RMSE = P3Q3UM_reg_test_rmse)

rmse_testresults
# A tibble: 5 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865
3 P1Q1U+Mreg model 0.862
4 P2Q2U+Mreg model 0.862
5 P3Q3U+Mreg model 0.863

rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|
|P1Q1U+Mreg model | 0.8624939|
|P2Q2U+Mreg model | 0.8620102|
|P3Q3U+Mreg model | 0.8628955|


