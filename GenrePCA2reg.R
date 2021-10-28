library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

# Calculate residual matrix GENr10reg_p2q2 from product of the 2nd principle components of p (pcaGENr10reg$x) 
# and q (pcaGENr10reg$rotation):
GENr10reg_p2q2 <- pcaGENr10reg$x[,2] %*% t(pcaGENr10reg$rotation[,2])

dim(GENr10reg_p2q2)
[1] 69878   762

# Convert matrix GENr10reg_p2q2 to a data frame:
GENr10reg_p2q2_df <- as.data.frame(GENr10reg_p2q2)

rm(GENr10reg_p2q2)

# Add userId column to GENr10reg_p2q2_df data frame and save as data frame object x:
x <- GENr10reg_p2q2_df %>% mutate(userId = rownames(pcaGENr10reg$x))

x[1:10, 755:763]
             771           774           776           779           782           783           784           789 userId
1   2.641542e-05  6.354231e-06 -1.872728e-06  3.249154e-06  7.383916e-07  5.075495e-06  1.107161e-05  3.248411e-07      1
2   7.684614e-05  1.848535e-05 -5.448028e-06  9.452245e-06  2.148084e-06  1.476533e-05  3.220886e-05  9.450082e-07      2
3  -4.612736e-05 -1.109594e-05  3.270212e-06 -5.673767e-06 -1.289401e-06 -8.862977e-06 -1.933357e-05 -5.672469e-07      3
4  -1.857118e-04 -4.467298e-05  1.316609e-05 -2.284296e-05 -5.191211e-06 -3.568292e-05 -7.783819e-05 -2.283773e-06      4
5   2.254981e-04  5.424358e-05 -1.598675e-05  2.773676e-05  6.303359e-06  4.332751e-05  9.451400e-05  2.773041e-06      5
6  -1.282995e-04 -3.086246e-05  9.095828e-06 -1.578112e-05 -3.586363e-06 -2.465165e-05 -5.377474e-05 -1.577751e-06      6
7  -7.607976e-05 -1.830099e-05  5.393695e-06 -9.357979e-06 -2.126662e-06 -1.461807e-05 -3.188765e-05 -9.355837e-07      7
8   7.494712e-04  1.802854e-04 -5.313396e-05  9.218661e-05  2.095001e-05  1.440045e-04  3.141292e-04  9.216552e-06      8
9  -5.597919e-05 -1.346580e-05  3.968660e-06 -6.885564e-06 -1.564789e-06 -1.075592e-05 -2.346280e-05 -6.883988e-07      9
10 -3.382542e-04 -8.136708e-05  2.398063e-05 -4.160602e-05 -9.455239e-06 -6.499264e-05 -1.417740e-04 -4.159650e-06     10

x[69869:69878, 755:763]
                771           774           776           779           782           783           784           789 userId
69869 -4.150631e-04 -9.984347e-05  2.942601e-05 -5.105368e-05 -1.160228e-05 -7.975082e-05 -1.739672e-04 -5.104200e-06  71558
69870 -5.974088e-06 -1.437067e-06  4.235346e-07 -7.348260e-07 -1.669940e-07 -1.147870e-06 -2.503946e-06 -7.346578e-08  71559
69871 -8.279226e-05 -1.991569e-05  5.869580e-06 -1.018363e-05 -2.314297e-06 -1.590782e-05 -3.470109e-05 -1.018130e-06  71560
69872  2.887201e-04  6.945164e-05 -2.046889e-05  3.551321e-05  8.070608e-06  5.547508e-05  1.210125e-04  3.550508e-06  71561
69873 -1.732279e-04 -4.166998e-05  1.228104e-05 -2.130741e-05 -4.842249e-06 -3.328425e-05 -7.260577e-05 -2.130254e-06  71562
69874  9.866966e-06  2.373499e-06 -6.995212e-07  1.213659e-06  2.758118e-07  1.895853e-06  4.135585e-06  1.213381e-07  71563
69875 -6.853856e-06 -1.648696e-06  4.859060e-07 -8.430395e-07 -1.915862e-07 -1.316910e-06 -2.872687e-06 -8.428466e-08  71564
69876 -5.831322e-05 -1.402725e-05  4.134132e-06 -7.172655e-06 -1.630033e-06 -1.120438e-05 -2.444108e-05 -7.171014e-07  71565
69877 -1.002892e-04 -2.412458e-05  7.110031e-06 -1.233580e-05 -2.803390e-06 -1.926971e-05 -4.203466e-05 -1.233297e-06  71566
69878 -1.080150e-04 -2.598301e-05  7.657749e-06 -1.328608e-05 -3.019348e-06 -2.075415e-05 -4.527278e-05 -1.328304e-06  71567

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(GENr10reg_p2q2_df)

# Create data frame object y with a 1st column of userIds from row names of pcaGENr10reg$x:
y <- data.frame(userId = rownames(pcaGENr10reg$x))

head(y)
  userId
1      1
2      2
3      3
4      4
5      5
6      6

tail(y)
      userId
69873  71562
69874  71563
69875  71564
69876  71565
69877  71566
69878  71567

# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
GENr10reg_p2q2_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

dim(GENr10reg_p2q2_dfw)
[1] 69878   763

GENr10reg_p2q2_dfw[1:10, 1:9]
   userId           1           2             3           4            5            6           7            8
1       1 -0.01696123  0.01535567  0.0003516963  0.01391785  0.001459654 -0.003855310 -0.01364869 -0.001650090
2       2 -0.04934259  0.04467179  0.0010231338  0.04048897  0.004246337 -0.011215637 -0.03970594 -0.004800343
3       3  0.02961819 -0.02681452 -0.0006141423 -0.02430375 -0.002548889  0.006732254  0.02383373  0.002881435
4       4  0.11924475 -0.10795698 -0.0024725768 -0.09784848 -0.010261994  0.027104494  0.09595615  0.011600845
5       5 -0.14479136  0.13108534  0.0030022936  0.11881122  0.012460491 -0.032911274 -0.11651348 -0.014086173
6       6  0.08238058 -0.07458239 -0.0017081867 -0.06759890 -0.007089528  0.018725217  0.06629158  0.008014477
7       7  0.04885050 -0.04422629 -0.0010129302 -0.04008518 -0.004203988  0.011103785  0.03930995  0.004752470
8       8 -0.48123234  0.43567863  0.0099785011  0.39488406  0.041414011 -0.109384764 -0.38724723 -0.046817171
9       9  0.03594400 -0.03254153 -0.0007453100 -0.02949452 -0.003093278  0.008170121  0.02892411  0.003496849
10     10  0.21719163 -0.19663216 -0.0045035355 -0.17822060 -0.018691131  0.049367953  0.17477391  0.021129706

GENr10reg_p2q2_dfw[69869:69878, 1:9]
      userId            1            2             3            4             5             6            7             8
69869  71558  0.266510310 -0.241282306 -5.526174e-03 -0.218689946 -0.0229354099  0.0605781556  0.214460610  0.0259277230
69870  71559  0.003835937 -0.003472825 -7.953934e-05 -0.003147649 -0.0003301140  0.0008719137  0.003086775  0.0003731830
69871  71560  0.053160567 -0.048128360 -1.102301e-03 -0.043621883 -0.0045749051  0.0120834690  0.042778261  0.0051717791
69872  71561 -0.185385964  0.167837232  3.844035e-03  0.152121869  0.0159539910 -0.0421384815 -0.149179921 -0.0180354596
69873  71562  0.111228906 -0.100699920 -2.306366e-03 -0.091270929 -0.0095721646  0.0252824816  0.089505802  0.0108210158
69874  71563 -0.006335538  0.005735812  1.313693e-04  0.005198742  0.0005452253 -0.0014400763 -0.005098202 -0.0006163591
69875  71564  0.004400833 -0.003984248 -9.125263e-05 -0.003611184 -0.0003787279  0.0010003153  0.003541346  0.0004281394
69876  71565  0.037442677 -0.033898334 -7.763855e-04 -0.030724278 -0.0032222511  0.0085107713  0.030130089  0.0036426484
69877  71566  0.064395291 -0.058299599 -1.335256e-03 -0.052840742 -0.0055417458  0.0146371371  0.051818833  0.0062647605
69878  71567  0.069355953 -0.062790682 -1.438117e-03 -0.056911305 -0.0059686517  0.0157647023  0.055810674  0.0067473635

GENr10reg_p2q2_dfw[69869:69878, 756:763]
                771           774           776           779           782           783           784           789
69869 -4.150631e-04 -9.984347e-05  2.942601e-05 -5.105368e-05 -1.160228e-05 -7.975082e-05 -1.739672e-04 -5.104200e-06
69870 -5.974088e-06 -1.437067e-06  4.235346e-07 -7.348260e-07 -1.669940e-07 -1.147870e-06 -2.503946e-06 -7.346578e-08
69871 -8.279226e-05 -1.991569e-05  5.869580e-06 -1.018363e-05 -2.314297e-06 -1.590782e-05 -3.470109e-05 -1.018130e-06
69872  2.887201e-04  6.945164e-05 -2.046889e-05  3.551321e-05  8.070608e-06  5.547508e-05  1.210125e-04  3.550508e-06
69873 -1.732279e-04 -4.166998e-05  1.228104e-05 -2.130741e-05 -4.842249e-06 -3.328425e-05 -7.260577e-05 -2.130254e-06
69874  9.866966e-06  2.373499e-06 -6.995212e-07  1.213659e-06  2.758118e-07  1.895853e-06  4.135585e-06  1.213381e-07
69875 -6.853856e-06 -1.648696e-06  4.859060e-07 -8.430395e-07 -1.915862e-07 -1.316910e-06 -2.872687e-06 -8.428466e-08
69876 -5.831322e-05 -1.402725e-05  4.134132e-06 -7.172655e-06 -1.630033e-06 -1.120438e-05 -2.444108e-05 -7.171014e-07
69877 -1.002892e-04 -2.412458e-05  7.110031e-06 -1.233580e-05 -2.803390e-06 -1.926971e-05 -4.203466e-05 -1.233297e-06
69878 -1.080150e-04 -2.598301e-05  7.657749e-06 -1.328608e-05 -3.019348e-06 -2.075415e-05 -4.527278e-05 -1.328304e-06

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y)

# Create long data frame GENr10reg_p2q2_dfl from the wide data frame to arrange p2q2 data into a table joining format:
GENr10reg_p2q2_dfl <- pivot_longer(data = GENr10reg_p2q2_dfw, cols = 2:763, names_to = "genreId", values_to = "p2q2")

GENr10reg_p2q2_dfl
# A tibble: 53,247,036 x 3
   userId genreId      p2q2
   <chr>  <chr>       <dbl>
 1 1      1       -0.0170  
 2 1      2        0.0154  
 3 1      3        0.000352
 4 1      4        0.0139  
 5 1      5        0.00146 
 6 1      6       -0.00386 
 7 1      7       -0.0136  
 8 1      8       -0.00165 
 9 1      9       -0.0136  
10 1      10       0.00214 
# ... with 53,247,026 more rows

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(GENr10reg_p2q2_dfw)

# Convert userId and genreId columns from <chr> to <int> values in the long data frame:
GENr10reg_p2q2_dfl <- GENr10reg_p2q2_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

GENr10reg_p2q2_dfl
# A tibble: 53,247,036 x 3
   userId genreId      p2q2
    <int>   <int>     <dbl>
 1      1       1 -0.0170  
 2      1       2  0.0154  
 3      1       3  0.000352
 4      1       4  0.0139  
 5      1       5  0.00146 
 6      1       6 -0.00386 
 7      1       7 -0.0136  
 8      1       8 -0.00165 
 9      1       9 -0.0136  
10      1      10  0.00214 
# ... with 53,247,026 more rows


# p2q2_U.M_reg_model adds the corresponding p2q2 genre factor to the p1q1 genre factor, the regularized user bias,
# the regularized movie bias, and the mean rating of all edx movies. 
# Model built in stages to avoid crashing R/RStudio:

p2q2_U.M_reg_partA <- edx %>% select(-rating, -timestamp, -title) %>% 
  left_join(user_bias_regularized, by = 'userId') %>% 
  left_join(movie_bias_regularized, by = 'movieId') 

dim(p2q2_U.M_reg_partA)
[1] 9000055       6

p2q2_U.M_reg_partA <- plyr::join(p2q2_U.M_reg_partA, P1Q1_regularized, by = c('userId', 'genreId'), 
                                 type = "left", match = "first")

sum(is.na(p2q2_U.M_reg_partA$p1q1) == TRUE)
[1] 0

dim(p2q2_U.M_reg_partA)
[1] 9000055       7

p2q2_U.M_reg_partA
         userId genreId movieId                        genres     b_u_reg     b_i_reg          p1q1
      1:      1       1     122                Comedy|Romance  1.37874734 -0.65345331  0.0065397603
      2:      1       2     185         Action|Crime|Thriller  1.37874734 -0.38309079  0.0204719222
      3:      1       3     292  Action|Drama|Sci-Fi|Thriller  1.37874734 -0.09444526  0.0249042703
      4:      1       4     316       Action|Adventure|Sci-Fi  1.37874734 -0.16277459  0.0726147432
      5:      1       5     329 Action|Adventure|Drama|Sci-Fi  1.37874734 -0.17499108  0.0259740732
---                                                                                           
9000051:  32620      61   33140               Children|Comedy  0.41365199 -0.00515091 -0.1426287970
9000052:  40976     128   61913                   Documentary -0.18616444 -0.21176248  0.0700127182
9000053:  59269     597   63141        Comedy|Musical|Western -0.01568136 -0.62498562  0.0003446536
9000054:  60713     128    4820                   Documentary -0.47007568 -0.62498562  0.0531311241
9000055:  64621      49   39429                Drama|Thriller  0.02768848 -0.41837405  0.0002787500


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P1Q1_regularized)


p2q2_U.M_reg_partB <- p2q2_U.M_reg_partA %>% left_join(GENr10reg_p2q2_dfl, by = c('userId', 'genreId'))

p2q2_U.M_reg_partB
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
rm(GENr10reg_p2q2_dfl, p2q2_U.M_reg_partA)

# 172 observations with NA due to 35 genres with 9 or fewer ratings:
sum(is.na(p2q2_U.M_reg_partB$p2q2) == TRUE)
[1] 172

sum(is.na(p2q2_U.M_reg_partB$p2q2) == FALSE)
[1] 8999883

172 + 8999883 == 9000055
[1] TRUE

# REGULARIZATION PART 2 ----
# Replace NA with zero (0) in the 172 observations of p2q2_U.M_reg_partB$p2q2, and verify that no NA remains.
# (See comments in "REGULARIZATION PART 1" at beginning of "MATRIX FACTORIZATION" section in MovieLensGenrePCA1reg tab.)
p2q2_U.M_reg_partB$p2q2[is.na(p2q2_U.M_reg_partB$p2q2)] <- 0

sum(is.na(p2q2_U.M_reg_partB$p2q2) == TRUE)
[1] 0

sum(is.na(p2q2_U.M_reg_partB$p2q2) == FALSE)
[1] 9000055


P1Q1_P2Q2_regularized <- p2q2_U.M_reg_partB %>% select(userId, genreId, p1q1, p2q2)

P1Q1_P2Q2_regularized
         userId genreId          p1q1          p2q2
      1:      1       1  0.0065397603 -0.0169612290
      2:      1       2  0.0204719222  0.0153556703
      3:      1       3  0.0249042703  0.0003516963
      4:      1       4  0.0726147432  0.0139178490
      5:      1       5  0.0259740732  0.0014596536
---                                           
9000051:  32620      61 -0.1426287970  0.0838360622
9000052:  40976     128  0.0700127182  0.0275372294
9000053:  59269     597  0.0003446536  0.0015239991
9000054:  60713     128  0.0531311241  0.0071940651
9000055:  64621      49  0.0002787500  0.0042794231


P2Q2UM_reg_model <- p2q2_U.M_reg_partB %>% mutate(p2q2um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2) %>% 
                    pull(p2q2um_reg)

P2Q2UM_reg_model[1:10]
[1] 4.227338 4.543949 4.822023 4.814971 4.743655 3.875661 5.404727 4.831962 5.131978 4.361370

length(P2Q2UM_reg_model)
[1] 9000055

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(p2q2_U.M_reg_partB)

# RMSE of the P2Q2UM_reg_model where p2q2um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2:
P2Q2UM_reg_rmse <- RMSE(edx$rating, P2Q2UM_reg_model)

P2Q2UM_reg_rmse
[1] 0.8446755

# add RMSE result of P2Q2UM_reg_model to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "P2Q2U+Mreg model", RMSE = P2Q2UM_reg_rmse)

rmse_results
# A tibble: 8 x 2
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

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P2Q2UM_reg_model)

# TEST OF THE REGULARIZED USER + MOVIE BIAS + P1Q1 + P2Q2 MODEL ON THE VALIDATION DATA SET: ----
P2Q2UM_reg_model_test_partA <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId')

P2Q2UM_reg_model_test_partA
        userId movieId genreId    b_u_reg     b_i_reg
     1:      1     231      14  1.3787473 -0.57729297
     2:      1     480      31  1.3787473  0.15104929
     3:      1     586      61  1.3787473 -0.45676603
     4:      2     151      33 -0.1901245  0.01758977
     5:      2     858      66 -0.1901245  0.90282854
---                                              
999995:  71566     235      43  0.2366598  0.15098277
999996:  71566     273      55  0.2366598 -0.36744915
999997:  71566     434      24  0.2366598 -0.43687885
999998:  71567     480      31 -0.0694971  0.15104929
999999:  71567     898       1 -0.0694971  0.70352257


P2Q2UM_reg_model_test_partB <- plyr::join(P2Q2UM_reg_model_test_partA, P1Q1_P2Q2_regularized, 
                                          by = c('userId', 'genreId'), type = "left", match = "first")

P2Q2UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342
     2:      1      31     480  1.3787473  0.15104929           NA          NA
     3:      1      61     586  1.3787473 -0.45676603           NA          NA
     4:      2      33     151 -0.1901245  0.01758977           NA          NA
     5:      2      66     858 -0.1901245  0.90282854           NA          NA
---                                                                       
999995:  71566      43     235  0.2366598  0.15098277           NA          NA
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364
999999:  71567       1     898 -0.0694971  0.70352257           NA          NA


sum(is.na(P2Q2UM_reg_model_test_partB$p1q1) == TRUE)
[1] 336086

sum(is.na(P2Q2UM_reg_model_test_partB$p2q2) == TRUE)
[1] 336086

# Replace NA with zero (0) in P2Q2UM_reg_model_test_partB$p1q1, in P2Q2UM_reg_model_test_partB$p2q2 
# and verify that no NA remains:
P2Q2UM_reg_model_test_partB$p1q1[is.na(P2Q2UM_reg_model_test_partB$p1q1)] <- 0
P2Q2UM_reg_model_test_partB$p2q2[is.na(P2Q2UM_reg_model_test_partB$p2q2)] <- 0

P2Q2UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1        p2q2
     1:      1      14     231  1.3787473 -0.57729297  0.002264051  0.00588342
     2:      1      31     480  1.3787473  0.15104929  0.000000000  0.00000000
     3:      1      61     586  1.3787473 -0.45676603  0.000000000  0.00000000
     4:      2      33     151 -0.1901245  0.01758977  0.000000000  0.00000000
     5:      2      66     858 -0.1901245  0.90282854  0.000000000  0.00000000
---                                                                       
999995:  71566      43     235  0.2366598  0.15098277  0.000000000  0.00000000
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821 -0.02301717
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159 -0.01769792
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787 -0.02400364
999999:  71567       1     898 -0.0694971  0.70352257  0.000000000  0.00000000

sum(is.na(P2Q2UM_reg_model_test_partB$p1q1) == TRUE)
[1] 0

sum(is.na(P2Q2UM_reg_model_test_partB$p2q2) == TRUE)
[1] 0

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P2Q2UM_reg_model_test_partA)

P2Q2UM_reg_model_test <- P2Q2UM_reg_model_test_partB %>% 
  mutate(p2q2um_reg = mu_hat + b_u_reg + b_i_reg + p1q1 + p2q2) %>% 
  pull(p2q2um_reg)

P2Q2UM_reg_model_test[1:10]
[1] 4.322067 5.042262 4.434447 3.339930 4.225169 2.755264 3.975956 4.139397 4.288728 3.317691

length(P2Q2UM_reg_model_test)
[1] 999999

P2Q2UM_reg_test_rmse <- RMSE(validation$rating, P2Q2UM_reg_model_test)
P2Q2UM_reg_test_rmse
[1] 0.8620102

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(P2Q2UM_reg_model_test_partB, P2Q2UM_reg_model_test)

# add RMSE result of P2Q2UM_reg_model_test to the RMSE table to compare the different models:
rmse_testresults <- rmse_testresults %>% add_row(method = "P2Q2U+Mreg model", RMSE = P2Q2UM_reg_test_rmse)

rmse_testresults
# A tibble: 4 × 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865
3 P1Q1U+Mreg model 0.862
4 P2Q2U+Mreg model 0.862

rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|
|P1Q1U+Mreg model | 0.8624939|
|P2Q2U+Mreg model | 0.8620102|


