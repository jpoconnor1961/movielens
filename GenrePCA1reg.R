library(tidyverse)
library(caret)
library(data.table)
library(ggrepel)

# DISTRIBUTION OF GENRES----

# Compute the average rating of the movie genres and plot the distribution of the ratings:
edx %>% group_by(genres) %>% 
  summarize(genre_mean = mean(rating)) %>% 
  ggplot(aes(genre_mean)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Average Rating by Movie Genre in the edx Training Data Set", 
       x = "Average of Movie Ratings", y = "Number of Genres",
       subtitle = "Plot shows a symmetrical normal distribution in the average ratings with tailing below 2.5 stars.")
# The plot shows a symmetrical normal distribution in the average movie ratings of the genres, with tailing below 2.5 stars.

edx %>% group_by(genres) %>% count() %>% arrange(desc(n))
# A tibble: 797 x 2
# Groups:   genres [797]
   genres                         n
   <chr>                      <int>
 1 Drama                     733296
 2 Comedy                    700889
 3 Comedy|Romance            365468
 4 Comedy|Drama              323637
 5 Comedy|Drama|Romance      261425
 6 Drama|Romance             259355
 7 Action|Adventure|Sci-Fi   219938
 8 Action|Adventure|Thriller 149091
 9 Drama|Thriller            145373
10 Crime|Drama               137387
# ... with 787 more rows

edx %>% group_by(genres) %>% count() %>% arrange(n)
# A tibble: 797 x 2
# Groups:   genres [797]
   genres                                         n
   <chr>                                      <int>
 1 Action|Animation|Comedy|Horror                 2
 2 Action|War|Western                             2
 3 Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi     2
 4 Adventure|Mystery                              2
 5 Crime|Drama|Horror|Sci-Fi                      2
 6 Documentary|Romance                            2
 7 Drama|Horror|Mystery|Sci-Fi|Thriller           2
 8 Fantasy|Mystery|Sci-Fi|War                     2
 9 Action|Adventure|Animation|Comedy|Sci-Fi       3
10 Horror|War|Western                             3
# ... with 787 more rows

# Histogram of low end of rating numbers per genre:
edx %>% group_by(genres) %>% count() %>% filter(n < 30) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30)
# histogram shows that there are 35 genres with less than 10 ratings.

length(unique(edx$genres))
[1] 797


edx_genreId <- data.frame(genreId = seq(1:797), genres = unique(edx$genres))

head(edx_genreId)

  genreId                        genres
1       1                Comedy|Romance
2       2         Action|Crime|Thriller
3       3  Action|Drama|Sci-Fi|Thriller
4       4       Action|Adventure|Sci-Fi
5       5 Action|Adventure|Drama|Sci-Fi
6       6       Children|Comedy|Fantasy


str(edx_genreId)

'data.frame':	797 obs. of  2 variables:
$ genreId: int  1 2 3 4 5 6 7 8 9 10 ...
$ genres : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...


edx <- left_join(edx, edx_genreId, by = 'genres')

edx
         userId movieId rating  timestamp                         title                        genres genreId
      1:      1     122    5.0  838985046              Boomerang (1992)                Comedy|Romance       1
      2:      1     185    5.0  838983525               Net, The (1995)         Action|Crime|Thriller       2
      3:      1     292    5.0  838983421               Outbreak (1995)  Action|Drama|Sci-Fi|Thriller       3
      4:      1     316    5.0  838983392               Stargate (1994)       Action|Adventure|Sci-Fi       4
      5:      1     329    5.0  838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi       5
---                                                                                                     
9000051:  32620   33140    3.5 1173562747         Down and Derby (2005)               Children|Comedy      61
9000052:  40976   61913    3.0 1227767528           Africa addio (1966)                   Documentary     128
9000053:  59269   63141    2.0 1226443318 Rockin' in the Rockies (1945)        Comedy|Musical|Western     597
9000054:  60713    4820    2.0 1119156754  Won't Anybody Listen? (2000)                   Documentary     128
9000055:  64621   39429    2.5 1201248182                Confess (2005)                Drama|Thriller      49

sum(is.na(edx$genreId) == TRUE)
[1] 0

sum(is.na(edx$genreId) == FALSE)
[1] 9000055


length(unique(validation$genres))
[1] 773

validation <- left_join(validation, edx_genreId, by = 'genres')

validation
        userId movieId rating timestamp                                             title                           genres genreId
     1:      1     231      5 838983392                              Dumb & Dumber (1994)                           Comedy      14
     2:      1     480      5 838983653                              Jurassic Park (1993) Action|Adventure|Sci-Fi|Thriller      31
     3:      1     586      5 838984068                                 Home Alone (1990)                  Children|Comedy      61
     4:      2     151      3 868246450                                    Rob Roy (1995)         Action|Drama|Romance|War      33
     5:      2     858      2 868245645                             Godfather, The (1972)                      Crime|Drama      66
---                                                                                                                           
999995:  71566     235      5 830341062                                    Ed Wood (1994)                     Comedy|Drama      43
999996:  71566     273      3 830341118 Frankenstein (Mary Shelley s Frankenstein) (1994)                     Drama|Horror      55
999997:  71566     434      3 830340953                                Cliffhanger (1993)        Action|Adventure|Thriller      24
999998:  71567     480      4 912580688                              Jurassic Park (1993) Action|Adventure|Sci-Fi|Thriller      31
999999:  71567     898      4 912649403                    Philadelphia Story, The (1940)                   Comedy|Romance       1

sum(is.na(validation$genreId) == TRUE)
[1] 0

sum(is.na(validation$genreId) == FALSE)
[1] 999999


# RESIDUALS FROM REGULARIZED BIASES ----

# Residuals from regularized biases, r_reg, are estimated by subtracting out mu_hat from each movie rating as well as 
# subtracting out the respective regularized user bias b_u_reg and regularized movie bias b_i_reg from each movie rating:
r_reg <- edx %>% left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId') %>% 
  mutate(r_reg = rating - mu_hat - b_u_reg - b_i_reg) %>% pull(r_reg)

r_reg[1:10]
[1]  0.7622408  0.4918782  0.2032327  0.2715620  0.2837785  1.1331644 -0.3915466  0.1662190 -0.1307774  0.6641133

# Add estimate of regularized residuals, r_reg, to edx data frame:
edx_r_reg <- mutate(edx, r_reg = r_reg)

edx_r_reg
         userId movieId rating  timestamp                         title                        genres genreId      r_reg
      1:      1     122    5.0  838985046              Boomerang (1992)                Comedy|Romance       1  0.7622408
      2:      1     185    5.0  838983525               Net, The (1995)         Action|Crime|Thriller       2  0.4918782
      3:      1     292    5.0  838983421               Outbreak (1995)  Action|Drama|Sci-Fi|Thriller       3  0.2032327
      4:      1     316    5.0  838983392               Stargate (1994)       Action|Adventure|Sci-Fi       4  0.2715620
      5:      1     329    5.0  838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi       5  0.2837785
---                                                                                                                
9000051:  32620   33140    3.5 1173562747         Down and Derby (2005)               Children|Comedy      61 -0.4209663
9000052:  40976   61913    3.0 1227767528           Africa addio (1966)                   Documentary     128 -0.1145383
9000053:  59269   63141    2.0 1226443318 Rockin' in the Rockies (1945)        Comedy|Musical|Western     597 -0.8717982
9000054:  60713    4820    2.0 1119156754  Won't Anybody Listen? (2000)                   Documentary     128 -0.4174039
9000055:  64621   39429    2.5 1201248182                Confess (2005)                Drama|Thriller      49 -0.6217796

# Remove large data files no longer needed (can always recalculate if needed again):
rm(r_reg)


# MATRIX FACTORIZATION ----

# REGULARIZATION PART 1 ----
# Filter for genres that have 10 or more ratings to avoid a PCA model that is over-trained to the uncertainty and noise
# of very low number genre observations.  This approach essentially regularizes genres with fewer than 10 observations, 
# because regularization mathematically shrinks the average toward a centered mean (zero) for cases with few observations. 
# See section "REGULARTZATION PART 2" below for point in code where zeros replace missing residuals (NA) of filtered genres:
edx_r10reg <- edx_r_reg %>% group_by(genreId) %>% filter(n() >= 10) %>% ungroup() %>% 
  select(userId, movieId, genreId, r_reg)

# edx_r10reg has a loss of 172 observations compared to edx_r_reg, due to 35 genres with 9 or fewer ratings.
edx_r10reg
# A tibble: 8,999,883 x 4
   userId movieId genreId  r_reg
    <int>   <dbl>   <int>  <dbl>
 1      1     122       1  0.762
 2      1     185       2  0.492
 3      1     292       3  0.203
 4      1     316       4  0.272
 5      1     329       5  0.284
 6      1     355       6  1.13 
 7      1     356       7 -0.392
 8      1     362       8  0.166
 9      1     364       9 -0.131
10      1     370      10  0.664
# ... with 8,999,873 more rows

rm(edx_r_reg)

# Group edx_r10reg by userId and genreId, and summarise n() and mean(r_reg) for each genre by userId:
gen_r10reg <- edx_r10reg %>% group_by(userId, genreId) %>% summarise(n = n(), R_reg = mean(r_reg))

gen_r10reg
# A tibble: 4,278,975 x 4
# Groups:   userId [69,878]
   userId genreId     n  R_reg
    <int>   <int> <int>  <dbl>
 1      1       1     1  0.762
 2      1       2     1  0.492
 3      1       3     1  0.203
 4      1       4     1  0.272
 5      1       5     1  0.284
 6      1       6     1  1.13 
 7      1       7     1 -0.392
 8      1       8     1  0.166
 9      1       9     1 -0.131
10      1      10     1  0.664
# ... with 4,278,965 more rows

gen_r10reg[90:100,]
# A tibble: 11 x 4
# Groups:   userId [2]
   userId genreId     n   R_reg
    <int>   <int> <int>   <dbl>
 1      4      64     1  0.743 
 2      5       1     3 -0.708 
 3      5      14     3 -0.0447
 4      5      15     5 -0.0592
 5      5      23     1 -0.467 
 6      5      25     1 -2.31  
 7      5      26     1 -2.46  
 8      5      28     8 -0.120 
 9      5      30     1  0.212 
10      5      32     1 -2.04  
11      5      34    20  0.259

rm(edx_r10reg)

# Select userId, genreId, and R_reg, then pivot wider and convert to 
# a matrix of residuals with userId rows and genreId columns:
GENr10reg <- gen_r10reg %>% 
  select(userId, genreId, R_reg) %>%
  pivot_wider(names_from = genreId, values_from = R_reg) %>%
  as.matrix()

GENr10reg[1:10, 1:10]
      userId          1         2           3           4         5          6          7        8          9
 [1,]      1  0.7622408 0.4918782  0.20323272  0.27156205 0.2837785  1.1331644 -0.3915466 0.166219 -0.1307774
 [2,]      2         NA        NA          NA  0.58128597        NA         NA         NA       NA         NA
 [3,]      3 -0.7349275        NA          NA          NA        NA         NA  0.2701505       NA         NA
 [4,]      4 -1.0724511 0.9344383 -1.00110971  0.31234641 1.0794361         NA         NA       NA  0.6648802
 [5,]      5 -0.7080745        NA          NA          NA        NA         NA         NA       NA         NA
 [6,]      6  0.4928571        NA          NA  0.36621397        NA         NA         NA       NA         NA
 [7,]      7  0.2543629        NA          NA -0.04569191        NA         NA         NA       NA         NA
 [8,]      8 -0.3047682 0.3370653 -0.06830501  0.20396049 0.6047634 -0.1681737         NA       NA -0.9532654
 [9,]      9         NA        NA          NA  0.41820735        NA         NA         NA       NA         NA
[10,]     10  0.3924235        NA          NA          NA        NA         NA -1.0932989       NA         NA

# Create row names for matrix GENr10reg with userIds (column 1):
rownames(GENr10reg)<- GENr10reg[ ,1]

GENr10reg[1:10, 1:10]
   userId          1         2           3           4         5          6          7        8          9
1       1  0.7622408 0.4918782  0.20323272  0.27156205 0.2837785  1.1331644 -0.3915466 0.166219 -0.1307774
2       2         NA        NA          NA  0.58128597        NA         NA         NA       NA         NA
3       3 -0.7349275        NA          NA          NA        NA         NA  0.2701505       NA         NA
4       4 -1.0724511 0.9344383 -1.00110971  0.31234641 1.0794361         NA         NA       NA  0.6648802
5       5 -0.7080745        NA          NA          NA        NA         NA         NA       NA         NA
6       6  0.4928571        NA          NA  0.36621397        NA         NA         NA       NA         NA
7       7  0.2543629        NA          NA -0.04569191        NA         NA         NA       NA         NA
8       8 -0.3047682 0.3370653 -0.06830501  0.20396049 0.6047634 -0.1681737         NA       NA -0.9532654
9       9         NA        NA          NA  0.41820735        NA         NA         NA       NA         NA
10     10  0.3924235        NA          NA          NA        NA         NA -1.0932989       NA         NA

# Delete redundant userId column from matrix GENr10reg:
GENr10reg <- GENr10reg[ ,-1]

GENr10reg[1:10, 1:10]
            1         2           3           4         5          6          7        8          9         10
1   0.7622408 0.4918782  0.20323272  0.27156205 0.2837785  1.1331644 -0.3915466 0.166219 -0.1307774  0.6641133
2          NA        NA          NA  0.58128597        NA         NA         NA       NA         NA         NA
3  -0.7349275        NA          NA          NA        NA         NA  0.2701505       NA         NA         NA
4  -1.0724511 0.9344383 -1.00110971  0.31234641 1.0794361         NA         NA       NA  0.6648802         NA
5  -0.7080745        NA          NA          NA        NA         NA         NA       NA         NA         NA
6   0.4928571        NA          NA  0.36621397        NA         NA         NA       NA         NA -0.7390318
7   0.2543629        NA          NA -0.04569191        NA         NA         NA       NA         NA         NA
8  -0.3047682 0.3370653 -0.06830501  0.20396049 0.6047634 -0.1681737         NA       NA -0.9532654  0.2533815
9          NA        NA          NA  0.41820735        NA         NA         NA       NA         NA         NA
10  0.3924235        NA          NA          NA        NA         NA -1.0932989       NA         NA         NA

# Check that Dimensions of matrix GENr10reg match expectations:
dim(GENr10reg)
[1] 69878   762

762 + 35 == 797
[1] TRUE

length(unique(edx$genres))
[1] 797

length(unique(edx$userId))
[1] 69878

rm(gen_r10reg)

# Replace all NAs in matrix GENr10reg with 0:
GENr10reg[is.na(GENr10reg)] <- 0

GENr10reg[1:10, 1:10]
            1         2           3           4         5          6          7        8          9         10
1   0.7622408 0.4918782  0.20323272  0.27156205 0.2837785  1.1331644 -0.3915466 0.166219 -0.1307774  0.6641133
2   0.0000000 0.0000000  0.00000000  0.58128597 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
3  -0.7349275 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000  0.2701505 0.000000  0.0000000  0.0000000
4  -1.0724511 0.9344383 -1.00110971  0.31234641 1.0794361  0.0000000  0.0000000 0.000000  0.6648802  0.0000000
5  -0.7080745 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
6   0.4928571 0.0000000  0.00000000  0.36621397 0.0000000  0.0000000  0.0000000 0.000000  0.0000000 -0.7390318
7   0.2543629 0.0000000  0.00000000 -0.04569191 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
8  -0.3047682 0.3370653 -0.06830501  0.20396049 0.6047634 -0.1681737  0.0000000 0.000000 -0.9532654  0.2533815
9   0.0000000 0.0000000  0.00000000  0.41820735 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
10  0.3924235 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000 -1.0932989 0.000000  0.0000000  0.0000000

# Matrix Factorization of Centered Residuals by Principle Component Analysis: ----
pcaGENr10reg <- prcomp(GENr10reg, center = TRUE)

# The q vectors (principal components) of the movie effects are stored in this matrix:
dim(pcaGENr10reg$rotation)
[1] 762 762

pcaGENr10reg$rotation[1:10, 1:9]
             PC1          PC2         PC3          PC4          PC5          PC6          PC7          PC8          PC9
1  -0.0208901733 -0.206271610  0.18877904 -0.001778290  0.132193362  0.171300034 -0.062288867  0.097091566 -0.162023705
2  -0.0653941404  0.186745832  0.05755474 -0.090655049  0.088920148  0.026541138  0.112661783 -0.395600905  0.142306998
3  -0.0795525371  0.004277106  0.03788560 -0.043283627 -0.020218379  0.007168940  0.024670786 -0.024549675 -0.014187505
4  -0.2319556836  0.169259971 -0.31591397  0.154227763 -0.335804527  0.365371716 -0.403147065 -0.159174411  0.147635547
5  -0.0829698438  0.017751373 -0.01547158 -0.008924534 -0.043910585  0.053590554 -0.016938199 -0.010501858 -0.026993495
6  -0.0405069913 -0.046885817  0.01999007  0.043036298  0.025247174  0.005096632  0.004069435  0.001995546  0.009672317
7  -0.0857021967 -0.165986582 -0.23053164 -0.353477444  0.264936819 -0.216344560 -0.515164633 -0.208956409 -0.305584341
8   0.0005392638 -0.020067341 -0.01112893  0.014461623  0.006969477 -0.001387141  0.010195068 -0.008040076  0.011654809
9  -0.0473383795 -0.165624991 -0.13771590  0.055881088  0.084245694 -0.055031358  0.083210798 -0.095044878  0.065412986
10 -0.0745798714  0.025976647  0.10609745  0.060888131  0.065677991 -0.011415142 -0.037065371  0.007328804  0.043564381

# The p vectors (principle components) of the user effects are stored in this matrix:
dim(pcaGENr10reg$x)
[1] 69878   762

pcaGENr10reg$x[1:10, 1:10]
           PC1         PC2        PC3         PC4         PC5         PC6         PC7         PC8        PC9          PC10
1  -0.31305438  0.08222765  0.5404829  0.35071032  0.20549700  0.20508653 -0.23736287  0.01437149  0.3371028  0.2040562882
2  -0.08299537  0.23921173 -0.6201370 -0.03581442 -0.09995748 -0.90450459 -0.67387666 -0.05473350  0.5975205  0.6302353209
3   0.13017815 -0.14358829 -0.2112351 -0.66221184 -0.08245728  0.33686529  0.09028072 -0.03706850  0.1471383 -0.8856192008
4  -0.09920045 -0.57809581 -1.4654954  0.08555973 -0.32958417 -0.07217559  0.27377154 -0.49796327 -0.3598605 -0.2915735659
5   1.74491106  0.70194519  0.2471835 -0.36499796 -1.17044860 -0.53604006 -0.52670468 -0.83315900  0.2744036 -0.7241213574
6   0.36463937 -0.39937914 -0.1419317 -0.07994977 -0.15966951  0.37972943 -0.50773834 -0.06843848 -0.3269233  0.4944989005
7   0.52563321 -0.23682609  0.3113771 -0.20840488 -0.39494679 -0.11690142  0.12629429 -0.27500103  0.2926854  0.0661749239
8  -1.32099085  2.33300325  1.6968615  1.13082079 -0.32078264  0.13984610 -0.21914221  0.15394717 -0.8433237 -0.0005177866
9  -0.06759668 -0.17425570 -0.4680100 -0.32940773 -0.26179209  1.15147689 -0.13241279 -0.65988893 -0.2355560  1.0011130401
10 -0.71045466 -1.05294001  0.6933001  0.19991344 -1.81847999  0.08231652  0.06784897  0.44899996  0.6651590  0.9978486763

# We can see the variability of each of the q vectors (principal components) of the genre effects:
qplot(1:ncol(pcaGENr10reg$rotation), pcaGENr10reg$sdev, xlab = "Principal Components (q vectors) of Genre Effects",
      main = "Variability of the Genre Principal Components from Regularized Residuals")

summary(pcaGENr10reg)$importance[,1:10]
                             PC1       PC2       PC3       PC4       PC5      PC6       PC7      PC8      PC9      PC10
Standard deviation     0.9191762 0.7827892 0.6961337 0.6916576 0.6375789 0.598429 0.5965195 0.565753 0.562506 0.5575937
Proportion of Variance 0.0217000 0.0157400 0.0124500 0.0122900 0.0104400 0.009200 0.0091400 0.008220 0.008130 0.0079900
Cumulative Proportion  0.0217000 0.0374400 0.0498900 0.0621800 0.0726300 0.081820 0.0909700 0.099190 0.107320 0.1153000

summary(pcaGENr10reg)$importance[,11:20]
                            PC11      PC12     PC13      PC14      PC15     PC16      PC17      PC18     PC19    PC20
Standard deviation     0.5531063 0.5454702 0.540199 0.5364294 0.5311331 0.524218 0.5205826 0.5203619 0.516068 0.51254
Proportion of Variance 0.0078600 0.0076400 0.007500 0.0073900 0.0072500 0.007060 0.0069600 0.0069600 0.006840 0.00675
Cumulative Proportion  0.1231600 0.1308000 0.138300 0.1456900 0.1529400 0.160000 0.1669600 0.1739200 0.180760 0.18751

summary(pcaGENr10reg)$importance[,c(30,40,50,60,70,80,90,100,200,250)]
                            PC30      PC40    PC50      PC60      PC70      PC80      PC90     PC100     PC200     PC250
Standard deviation     0.4844349 0.4622383 0.44289 0.4188192 0.3997632 0.3835615 0.3707885 0.3582423 0.2495719 0.2080936
Proportion of Variance 0.0060300 0.0054900 0.00504 0.0045100 0.0041100 0.0037800 0.0035300 0.0033000 0.0016000 0.0011100
Cumulative Proportion  0.2511800 0.3081500 0.36058 0.4080600 0.4507900 0.4899400 0.5263400 0.5603500 0.7979400 0.8653300

summary(pcaGENr10reg)$importance[,c(275,284,285,300,400,500,600,700,762)]
                           PC275    PC284     PC285     PC300     PC400     PC500      PC600     PC700     PC762
Standard deviation     0.1921038 0.186389 0.1863277 0.1771927 0.1151652 0.0714931 0.03954947 0.0190149 0.0047012
Proportion of Variance 0.0009500 0.000890 0.0008900 0.0008100 0.0003400 0.0001300 0.00004000 0.0000100 0.0000000
Cumulative Proportion  0.8908700 0.899120 0.9000100 0.9126600 0.9674600 0.9898000 0.99762000 0.9997000 1.0000000


# Calculate the diagonal matrix Sigma:
Sigma <- diag(pcaGENr10reg$sdev^2)

Sigma[1:12, 1:12]
           [,1]     [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]     [,9]     [,10]     [,11]     [,12]
 [1,] 0.8448848 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [2,] 0.0000000 0.612759 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.000000 0.4846021 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [4,] 0.0000000 0.000000 0.0000000 0.4783902 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [5,] 0.0000000 0.000000 0.0000000 0.0000000 0.4065068 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [6,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.3581173 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [7,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3558355 0.0000000 0.000000 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3200765 0.000000 0.0000000 0.0000000 0.0000000
 [9,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.316413 0.0000000 0.0000000 0.0000000
[10,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.3109107 0.0000000 0.0000000
[11,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.3059266 0.0000000
[12,] 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.2975378

dim(Sigma)
[1] 762 762


# Calculate regularized residual matrix GENr10reg_PQ from product of pcaGENr10reg$x %*% t(pcaGENr10reg$rotation), 
# and verify that the matrix GENr10reg_PQ values approximate the values in matrix GENr10reg:
GENr10reg_PQ <- pcaGENr10reg$x %*% t(pcaGENr10reg$rotation)

dim(GENr10reg_PQ)
[1] 69878   762

GENr10reg_PQ[1:10, 1:10]
             1          2           3          4          5           6           7           8           9           10
1   0.79476149 0.51565173  0.21335620 0.33814706 0.29538232  1.13926913 -0.37361318 0.170328448 -0.11666140  0.660885535
2   0.03252072 0.02377348  0.01012348 0.64787099 0.01160379  0.00610478  0.01793347 0.004109442  0.01411595 -0.003227775
3  -0.70240677 0.02377348  0.01012348 0.06658501 0.01160379  0.00610478  0.28808396 0.004109442  0.01411595 -0.003227775
4  -1.03993041 0.95821174 -0.99098623 0.37893142 1.09103989  0.00610478  0.01793347 0.004109442  0.67899617 -0.003227775
5  -0.67555378 0.02377348  0.01012348 0.06658501 0.01160379  0.00610478  0.01793347 0.004109442  0.01411595 -0.003227775
6   0.52537779 0.02377348  0.01012348 0.43279898 0.01160379  0.00610478  0.01793347 0.004109442  0.01411595 -0.742259614
7   0.28688365 0.02377348  0.01012348 0.02089310 0.01160379  0.00610478  0.01793347 0.004109442  0.01411595 -0.003227775
8  -0.27224749 0.36083880 -0.05818153 0.27054551 0.61636717 -0.16206892  0.01793347 0.004109442 -0.93914942  0.250153767
9   0.03252072 0.02377348  0.01012348 0.48479236 0.01160379  0.00610478  0.01793347 0.004109442  0.01411595 -0.003227775
10  0.42494424 0.02377348  0.01012348 0.06658501 0.01160379  0.00610478 -1.07536543 0.004109442  0.01411595 -0.003227775

GENr10reg[1:10, 1:10]
            1         2           3           4         5          6          7        8          9         10
1   0.7622408 0.4918782  0.20323272  0.27156205 0.2837785  1.1331644 -0.3915466 0.166219 -0.1307774  0.6641133
2   0.0000000 0.0000000  0.00000000  0.58128597 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
3  -0.7349275 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000  0.2701505 0.000000  0.0000000  0.0000000
4  -1.0724511 0.9344383 -1.00110971  0.31234641 1.0794361  0.0000000  0.0000000 0.000000  0.6648802  0.0000000
5  -0.7080745 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
6   0.4928571 0.0000000  0.00000000  0.36621397 0.0000000  0.0000000  0.0000000 0.000000  0.0000000 -0.7390318
7   0.2543629 0.0000000  0.00000000 -0.04569191 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
8  -0.3047682 0.3370653 -0.06830501  0.20396049 0.6047634 -0.1681737  0.0000000 0.000000 -0.9532654  0.2533815
9   0.0000000 0.0000000  0.00000000  0.41820735 0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000
10  0.3924235 0.0000000  0.00000000  0.00000000 0.0000000  0.0000000 -1.0932989 0.000000  0.0000000  0.0000000

dim(GENr10reg)
[1] 69878   762
# Out of the first 10 Principal Components, GENr10reg_PQ approximates GENr10reg well.

rm(GENr10reg_PQ)

# Calculate residual matrix GENr10reg_p1q1 from product of the 1st principle components of p (pcaGENr10reg$x) 
# and q (pcaGENr10reg$rotation):
GENr10reg_p1q1 <- pcaGENr10reg$x[,1] %*% t(pcaGENr10reg$rotation[,1])

dim(GENr10reg_p1q1)
[1] 69878   762

GENr10reg_p1q1[1:10, 1:9]
                 1            2            3           4            5            6            7             8            9
 [1,]  0.006539760  0.020471922  0.024904270  0.07261474  0.025974073  0.012680891  0.026829448 -1.688189e-04  0.014819487
 [2,]  0.001733788  0.005427411  0.006602492  0.01925125  0.006886113  0.003361893  0.007112886 -4.475640e-05  0.003928866
 [3,] -0.002719444 -0.008512888 -0.010356002 -0.03019556 -0.010800861 -0.005273125 -0.011156553  7.020036e-05 -0.006162423
 [4,]  0.002072315  0.006487128  0.007891647  0.02301011  0.008230646  0.004018312  0.008501696 -5.349521e-05  0.004695989
 [5,] -0.036451494 -0.114106959 -0.138812102 -0.40474204 -0.144774998 -0.070681097 -0.149542711  9.409674e-04 -0.082601262
 [6,] -0.007617380 -0.023845278 -0.029007987 -0.08458017 -0.030254071 -0.014770444 -0.031250395  1.966368e-04 -0.017261437
 [7,] -0.010980569 -0.034373332 -0.041815456 -0.12192361 -0.043611706 -0.021291820 -0.045047921  2.834550e-04 -0.024882624
 [8,]  0.027595728  0.086385061  0.105088174  0.30641134  0.109602405  0.053509365  0.113211818 -7.123626e-04  0.062533566
 [9,]  0.001412106  0.004420427  0.005377488  0.01567943  0.005608486  0.002738138  0.005793184 -3.645245e-05  0.003199917
[10,]  0.014841521  0.046459572  0.056518471  0.16479400  0.058946312  0.028778381  0.060887525 -3.831225e-04  0.033631772

# Convert matrix GENr10reg_p1q1 to a data frame:
GENr10reg_p1q1_df <- as.data.frame(GENr10reg_p1q1)

GENr10reg_p1q1_df[1:10, 1:9]
              1            2            3           4            5            6            7             8            9
1   0.006539760  0.020471922  0.024904270  0.07261474  0.025974073  0.012680891  0.026829448 -1.688189e-04  0.014819487
2   0.001733788  0.005427411  0.006602492  0.01925125  0.006886113  0.003361893  0.007112886 -4.475640e-05  0.003928866
3  -0.002719444 -0.008512888 -0.010356002 -0.03019556 -0.010800861 -0.005273125 -0.011156553  7.020036e-05 -0.006162423
4   0.002072315  0.006487128  0.007891647  0.02301011  0.008230646  0.004018312  0.008501696 -5.349521e-05  0.004695989
5  -0.036451494 -0.114106959 -0.138812102 -0.40474204 -0.144774998 -0.070681097 -0.149542711  9.409674e-04 -0.082601262
6  -0.007617380 -0.023845278 -0.029007987 -0.08458017 -0.030254071 -0.014770444 -0.031250395  1.966368e-04 -0.017261437
7  -0.010980569 -0.034373332 -0.041815456 -0.12192361 -0.043611706 -0.021291820 -0.045047921  2.834550e-04 -0.024882624
8   0.027595728  0.086385061  0.105088174  0.30641134  0.109602405  0.053509365  0.113211818 -7.123626e-04  0.062533566
9   0.001412106  0.004420427  0.005377488  0.01567943  0.005608486  0.002738138  0.005793184 -3.645245e-05  0.003199917
10  0.014841521  0.046459572  0.056518471  0.16479400  0.058946312  0.028778381  0.060887525 -3.831225e-04  0.033631772

rm(GENr10reg_p1q1)

# Add userId column to GENr10reg_p1q1_df data frame and save as data frame object x:
x <- GENr10reg_p1q1_df %>% mutate(userId = rownames(GENr10reg))

x[1:10, 755:763]
             771           774           776           779           782           783           784           789 userId
1  -1.744276e-04  1.811757e-05 -1.167228e-04 -4.700574e-05 -2.126078e-05 -6.666071e-05 -6.506064e-05  2.463258e-05      1
2  -4.624334e-05  4.803236e-06 -3.094495e-05 -1.246192e-05 -5.636549e-06 -1.767275e-05 -1.724854e-05  6.530463e-06      2
3   7.253262e-05 -7.533871e-06  4.853713e-05  1.954651e-05  8.840921e-06  2.771968e-05  2.705432e-05 -1.024302e-05      3
4  -5.527248e-05  5.741082e-06 -3.698704e-05 -1.489514e-05 -6.737101e-06 -2.112340e-05 -2.061637e-05  7.805554e-06      4
5   9.722290e-04 -1.009842e-04  6.505928e-04  2.620019e-04  1.185039e-04  3.715553e-04  3.626368e-04 -1.372977e-04      5
6   2.031697e-04 -2.110297e-05  1.359564e-04  5.475132e-05  2.476412e-05  7.764504e-05  7.578131e-05 -2.869153e-05      6
7   2.928722e-04 -3.042026e-05  1.959832e-04  7.892487e-05  3.569786e-05  1.119265e-04  1.092399e-04 -4.135927e-05      7
8  -7.360293e-04  7.645042e-05 -4.925335e-04 -1.983494e-04 -8.971379e-05 -2.812872e-04 -2.745354e-04  1.039417e-04      8
9  -3.766350e-05  3.912060e-06 -2.520353e-05 -1.014978e-05 -4.590762e-06 -1.439380e-05 -1.404831e-05  5.318822e-06      9
10 -3.958509e-04  4.111653e-05 -2.648941e-04 -1.066762e-04 -4.824983e-05 -1.512817e-04 -1.476505e-04  5.590189e-05     10

rm(GENr10reg_p1q1_df)

# Create data frame object y with a 1st column of userIds from row names of matrix GENr10reg:
y <- data.frame(userId = rownames(GENr10reg))

  head(y)
  userId
1      1
2      2
3      3
4      4
5      5
6      6

rm(GENr10reg)

# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
GENr10reg_p1q1_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

GENr10reg_p1q1_dfw[1:10, 1:9]
   userId            1            2            3           4            5            6            7             8
1       1  0.006539760  0.020471922  0.024904270  0.07261474  0.025974073  0.012680891  0.026829448 -1.688189e-04
2       2  0.001733788  0.005427411  0.006602492  0.01925125  0.006886113  0.003361893  0.007112886 -4.475640e-05
3       3 -0.002719444 -0.008512888 -0.010356002 -0.03019556 -0.010800861 -0.005273125 -0.011156553  7.020036e-05
4       4  0.002072315  0.006487128  0.007891647  0.02301011  0.008230646  0.004018312  0.008501696 -5.349521e-05
5       5 -0.036451494 -0.114106959 -0.138812102 -0.40474204 -0.144774998 -0.070681097 -0.149542711  9.409674e-04
6       6 -0.007617380 -0.023845278 -0.029007987 -0.08458017 -0.030254071 -0.014770444 -0.031250395  1.966368e-04
7       7 -0.010980569 -0.034373332 -0.041815456 -0.12192361 -0.043611706 -0.021291820 -0.045047921  2.834550e-04
8       8  0.027595728  0.086385061  0.105088174  0.30641134  0.109602405  0.053509365  0.113211818 -7.123626e-04
9       9  0.001412106  0.004420427  0.005377488  0.01567943  0.005608486  0.002738138  0.005793184 -3.645245e-05
10     10  0.014841521  0.046459572  0.056518471  0.16479400  0.058946312  0.028778381  0.060887525 -3.831225e-04

GENr10reg_p1q1_dfw[1:10, 756:763]
             771           774           776           779           782           783           784           789
1  -1.744276e-04  1.811757e-05 -1.167228e-04 -4.700574e-05 -2.126078e-05 -6.666071e-05 -6.506064e-05  2.463258e-05
2  -4.624334e-05  4.803236e-06 -3.094495e-05 -1.246192e-05 -5.636549e-06 -1.767275e-05 -1.724854e-05  6.530463e-06
3   7.253262e-05 -7.533871e-06  4.853713e-05  1.954651e-05  8.840921e-06  2.771968e-05  2.705432e-05 -1.024302e-05
4  -5.527248e-05  5.741082e-06 -3.698704e-05 -1.489514e-05 -6.737101e-06 -2.112340e-05 -2.061637e-05  7.805554e-06
5   9.722290e-04 -1.009842e-04  6.505928e-04  2.620019e-04  1.185039e-04  3.715553e-04  3.626368e-04 -1.372977e-04
6   2.031697e-04 -2.110297e-05  1.359564e-04  5.475132e-05  2.476412e-05  7.764504e-05  7.578131e-05 -2.869153e-05
7   2.928722e-04 -3.042026e-05  1.959832e-04  7.892487e-05  3.569786e-05  1.119265e-04  1.092399e-04 -4.135927e-05
8  -7.360293e-04  7.645042e-05 -4.925335e-04 -1.983494e-04 -8.971379e-05 -2.812872e-04 -2.745354e-04  1.039417e-04
9  -3.766350e-05  3.912060e-06 -2.520353e-05 -1.014978e-05 -4.590762e-06 -1.439380e-05 -1.404831e-05  5.318822e-06
10 -3.958509e-04  4.111653e-05 -2.648941e-04 -1.066762e-04 -4.824983e-05 -1.512817e-04 -1.476505e-04  5.590189e-05


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y)

# Create long data frame GENr10reg_p1q1_dfl from the wide data frame to arrange p1q1 data into a table joining format:
GENr10reg_p1q1_dfl <- pivot_longer(data = GENr10reg_p1q1_dfw, cols = 2:763, names_to = "genreId", values_to = "p1q1")

GENr10reg_p1q1_dfl
# A tibble: 53,247,036 x 3
  userId genreId      p1q1
  <chr>  <chr>       <dbl>
 1 1      1        0.00654 
 2 1      2        0.0205  
 3 1      3        0.0249  
 4 1      4        0.0726  
 5 1      5        0.0260  
 6 1      6        0.0127  
 7 1      7        0.0268  
 8 1      8       -0.000169
 9 1      9        0.0148  
10 1      10       0.0233  
# ... with 53,247,026 more rows


rm(GENr10reg_p1q1_dfw)


# Convert userId and genreId columns from <chr> to <int> values in the long data frame:
GENr10reg_p1q1_dfl <- GENr10reg_p1q1_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

GENr10reg_p1q1_dfl
# A tibble: 53,247,036 x 3
   userId genreId      p1q1
    <int>   <int>     <dbl>
 1      1       1  0.00654 
 2      1       2  0.0205  
 3      1       3  0.0249  
 4      1       4  0.0726  
 5      1       5  0.0260  
 6      1       6  0.0127  
 7      1       7  0.0268  
 8      1       8 -0.000169
 9      1       9  0.0148  
10      1      10  0.0233  
# ... with 53,247,026 more rows


# p1q1_U.M_reg_model adds the corresponding p1q1 genre factor, regularized user bias, and regularized movie bias 
# to the mean rating of all edx movies (model built in stages to avoid crashing R/RStudio):

p1q1_U.M_reg_partA <- edx %>% left_join(user_bias_regularized, by = 'userId') %>% 
  left_join(movie_bias_regularized, by = 'movieId') %>% select(-rating, -timestamp, -title)

p1q1_U.M_reg_partA
         userId movieId                        genres genreId     b_u_reg     b_i_reg
      1:      1     122                Comedy|Romance       1  1.37874734 -0.65345331
      2:      1     185         Action|Crime|Thriller       2  1.37874734 -0.38309079
      3:      1     292  Action|Drama|Sci-Fi|Thriller       3  1.37874734 -0.09444526
      4:      1     316       Action|Adventure|Sci-Fi       4  1.37874734 -0.16277459
      5:      1     329 Action|Adventure|Drama|Sci-Fi       5  1.37874734 -0.17499108
---                                                                             
9000051:  32620   33140               Children|Comedy      61  0.41365199 -0.00515091
9000052:  40976   61913                   Documentary     128 -0.18616444 -0.21176248
9000053:  59269   63141        Comedy|Musical|Western     597 -0.01568136 -0.62498562
9000054:  60713    4820                   Documentary     128 -0.47007568 -0.62498562
9000055:  64621   39429                Drama|Thriller      49  0.02768848 -0.41837405


p1q1_U.M_reg_partB <- p1q1_U.M_reg_partA %>% left_join(GENr10reg_p1q1_dfl, by = c('userId', 'genreId'))

p1q1_U.M_reg_partB
         userId movieId                        genres genreId     b_u_reg     b_i_reg          p1q1
      1:      1     122                Comedy|Romance       1  1.37874734 -0.65345331  0.0065397603
      2:      1     185         Action|Crime|Thriller       2  1.37874734 -0.38309079  0.0204719222
      3:      1     292  Action|Drama|Sci-Fi|Thriller       3  1.37874734 -0.09444526  0.0249042703
      4:      1     316       Action|Adventure|Sci-Fi       4  1.37874734 -0.16277459  0.0726147432
      5:      1     329 Action|Adventure|Drama|Sci-Fi       5  1.37874734 -0.17499108  0.0259740732
---                                                                                           
9000051:  32620   33140               Children|Comedy      61  0.41365199 -0.00515091 -0.1426287970
9000052:  40976   61913                   Documentary     128 -0.18616444 -0.21176248  0.0700127182
9000053:  59269   63141        Comedy|Musical|Western     597 -0.01568136 -0.62498562  0.0003446536
9000054:  60713    4820                   Documentary     128 -0.47007568 -0.62498562  0.0531311241
9000055:  64621   39429                Drama|Thriller      49  0.02768848 -0.41837405  0.0002787500


rm(GENr10reg_p1q1_dfl, p1q1_U.M_reg_partA)

# 172 observations with NA due to 35 genres with 9 or fewer ratings:
sum(is.na(p1q1_U.M_reg_partB$p1q1) == TRUE)
[1] 172

sum(is.na(p1q1_U.M_reg_partB$p1q1) == FALSE)
[1] 8999883

172 + 8999883 == 9000055
[1] TRUE

# REGULARIZATION PART 2 ----
# Replace NA with zero (0) in the 172 observations of p1q1_U.M_reg_partB$p1q1, and verify that no NA remains. 
# Also see comments in "REGULARIZATION PART 1" at the beginning of the section above in "MATRIX FACTORIZATION":
p1q1_U.M_reg_partB$p1q1[is.na(p1q1_U.M_reg_partB$p1q1)] <- 0

sum(is.na(p1q1_U.M_reg_partB$p1q1) == TRUE)
[1] 0

sum(is.na(p1q1_U.M_reg_partB$p1q1) == FALSE)
[1] 9000055

# Create P1Q1_regularized data frame for future use in calculations:
P1Q1_regularized <- p1q1_U.M_reg_partB %>% select(userId, genreId, p1q1)

P1Q1_regularized
         userId genreId          p1q1
      1:      1       1  0.0065397603
      2:      1       2  0.0204719222
      3:      1       3  0.0249042703
      4:      1       4  0.0726147432
      5:      1       5  0.0259740732
---                             
9000051:  32620      61 -0.1426287970
9000052:  40976     128  0.0700127182
9000053:  59269     597  0.0003446536
9000054:  60713     128  0.0531311241
9000055:  64621      49  0.0002787500


P1Q1UM_reg_model <- p1q1_U.M_reg_partB %>% mutate(p1q1um_reg = mu_hat + b_u_reg + b_i_reg + p1q1) %>% 
  pull(p1q1um_reg)

P1Q1UM_reg_model[1:10]
[1] 4.244299 4.528594 4.821672 4.801053 4.742196 3.879517 5.418376 4.833612 5.145597 4.359234

length(P1Q1UM_reg_model)
[1] 9000055

rm(p1q1_U.M_reg_partB)

# RMSE of the P1Q1UM_reg_model where p1q1um_reg = mu_hat + b_u_reg + b_i_reg + p1q1:
P1Q1UM_reg_rmse <- RMSE(edx$rating, P1Q1UM_reg_model)

P1Q1UM_reg_rmse
[1] 0.8493764

# add RMSE result of P1Q1UM_reg_model to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "P1Q1U+Mreg model", RMSE = P1Q1UM_reg_rmse)

rmse_results
# A tibble: 7 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 RegMovBias model 0.942
4 U + M bias model 0.857
5 RegU+Mbias model 0.857
6 P1Q1 + U+M model 0.849
7 P1Q1U+Mreg model 0.849


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


rm(P1Q1UM_reg_model)


# TEST OF THE REGULARIZED USER + MOVIE BIAS + P1Q1 MODEL ON THE VALIDATION DATA SET: ----
P1Q1UM_reg_model_test_partA <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias_regularized, by='userId') %>% left_join(movie_bias_regularized, by='movieId')

P1Q1UM_reg_model_test_partA
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


P1Q1UM_reg_model_test_partB <- plyr::join(P1Q1UM_reg_model_test_partA, P1Q1_regularized, 
                                      by = c('userId', 'genreId'), type = "left", match = "first")

P1Q1UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1
     1:      1      14     231  1.3787473 -0.57729297  0.002264051
     2:      1      31     480  1.3787473  0.15104929           NA
     3:      1      61     586  1.3787473 -0.45676603           NA
     4:      2      33     151 -0.1901245  0.01758977           NA
     5:      2      66     858 -0.1901245  0.90282854           NA
---                                                           
999995:  71566      43     235  0.2366598  0.15098277           NA
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787
999999:  71567       1     898 -0.0694971  0.70352257           NA


sum(is.na(P1Q1UM_reg_model_test_partB$p1q1) == TRUE)
[1] 336086

sum(is.na(P1Q1UM_reg_model_test_partB$p1q1) == FALSE)
[1] 663913

336086 + 663913 == 999999
[1] TRUE

# Replace NA with zero (0) in P1Q1UM_reg_model_test_partB$p1q1 and verify that no NA remains:
P1Q1UM_reg_model_test_partB$p1q1[is.na(P1Q1UM_reg_model_test_partB$p1q1)] <- 0

P1Q1UM_reg_model_test_partB
        userId genreId movieId    b_u_reg     b_i_reg         p1q1
     1:      1      14     231  1.3787473 -0.57729297  0.002264051
     2:      1      31     480  1.3787473  0.15104929  0.000000000
     3:      1      61     586  1.3787473 -0.45676603  0.000000000
     4:      2      33     151 -0.1901245  0.01758977  0.000000000
     5:      2      66     858 -0.1901245  0.90282854  0.000000000
---                                                           
999995:  71566      43     235  0.2366598  0.15098277  0.000000000
999996:  71566      55     273  0.2366598 -0.36744915  0.012570821
999997:  71566      24     434  0.2366598 -0.43687885 -0.055663159
999998:  71567      31     480 -0.0694971  0.15104929  0.183509787
999999:  71567       1     898 -0.0694971  0.70352257  0.000000000

sum(is.na(P1Q1UM_reg_model_test_partB$p1q1) == TRUE)
[1] 0

sum(is.na(P1Q1UM_reg_model_test_partB$p1q1) == FALSE)
[1] 999999

rm(P1Q1UM_reg_model_test_partA)

P1Q1UM_reg_model_test <- P1Q1UM_reg_model_test_partB %>% 
  mutate(p1q1um_reg = mu_hat + b_u_reg + b_i_reg + p1q1) %>% 
  pull(p1q1um_reg)

P1Q1UM_reg_model_test[1:10]
[1] 4.316184 5.042262 4.434447 3.339930 4.225169 2.755264 3.975956 4.139397 4.288728 3.317691

length(P1Q1UM_reg_model_test)
[1] 999999

P1Q1UM_reg_test_rmse <- RMSE(validation$rating, P1Q1UM_reg_model_test)
P1Q1UM_reg_test_rmse
[1] 0.8624939


rm(P1Q1UM_reg_model_test_partB, P1Q1UM_reg_model_test)


# add RMSE result of P1Q1UM_reg_model_test to the RMSE table to compare the different models:
rmse_testresults <- rmse_testresults %>% add_row(method = "P1Q1U+Mreg model", RMSE = P1Q1UM_reg_test_rmse)

rmse_testresults
# A tibble: 3 x 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 RegU+Mbias model 0.865
3 P1Q1U+Mreg model 0.862

rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|RegU+Mbias model | 0.8648873|
|P1Q1U+Mreg model | 0.8624939|
  
  
  