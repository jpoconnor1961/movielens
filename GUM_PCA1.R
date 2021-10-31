library(tidyverse)
library(caret)
library(data.table)
library(stringr)
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


tail(edx_genreId)
    genreId                                     genres
792     792 Adventure|Fantasy|Film-Noir|Mystery|Sci-Fi
793     793                  Animation|Documentary|War
794     794         Adventure|Animation|Musical|Sci-Fi
795     795                 Fantasy|Mystery|Sci-Fi|War
796     796                         Action|War|Western
797     797                          Adventure|Mystery


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

# Revise genre_bias as genreId and the corresponding b_g_hat (without listing the actual genres):
genre_bias <- left_join(edx_genreId, genre_bias, by = "genres") %>% select(genreId, b_g_hat)

head(genre_bias)
  genreId     b_g_hat
1       1 -0.01544456
2       2 -0.02647697
3       3 -0.04619923
4       4 -0.02323825
5       5 -0.04772954
6       6 -0.04164348

tail(genre_bias)
    genreId    b_g_hat
792     792 0.41427612
793     793 0.25852930
794     794 0.03361837
795     795 0.64202085
796     796 0.37310835
797     797 0.14318101


# GUM RESIDUALS ----

# GUM Residuals, r_hat, are estimated by subtracting out mu_hat from each movie rating as well as  
# subtracting out the respective b_u_hat, b_i_hat, and b_g_hat from each movie rating:

r_hat <- edx %>% left_join(user_bias, by='userId') %>% 
                 left_join(movie_bias, by='movieId') %>% 
                 left_join(genre_bias, by='genreId') %>% 
                 mutate(r_hat = rating - mu_hat - b_u_hat - b_i_hat - b_g_hat) %>% pull(r_hat)

r_hat[1:10]
[1]  0.477623994  0.217908235 -0.051046139 -0.005673499  0.031037791
[6]  0.874621567 -0.659021393 -0.049998650 -0.376095220  0.356577672


# Add estimate of the gum residuals, r_hat, to the edx data frame and select variables for matrix factorization:

gum_r_hat <- edx %>% mutate(r_hat = r_hat) %>% select(userId, movieId, r_hat)

gum_r_hat
         userId movieId        r_hat
      1:      1     122  0.477623994
      2:      1     185  0.217908235
      3:      1     292 -0.051046139
      4:      1     316 -0.005673499
      5:      1     329  0.031037791
---                            
9000051:  32620   33140 -0.433600320
9000052:  40976   61913  0.117403056
9000053:  59269   63141  0.006698242
9000054:  60713    4820  0.400206035
9000055:  64621   39429 -0.029096887


str(gum_r_hat)

Classes ‘data.table’ and 'data.frame':	9000055 obs. of  3 variables:
$ userId : int  1 1 1 1 1 1 1 1 1 1 ...
$ movieId: num  122 185 292 316 329 355 356 362 364 370 ...
$ r_hat  : num  0.47762 0.21791 -0.05105 -0.00567 0.03104 ...
- attr(*, ".internal.selfref")=<externalptr>


# Remove large data files no longer needed (can always recalculate if needed again):
rm(r_hat)


# count the number of movies rated by each user in the edx training data set and plot the lower end of the distribution:
edx %>% group_by(userId) %>%  
  summarize(count = n()) %>%
  as.data.frame() %>%
  filter(count <= 100) %>% 
  ggplot(aes(count)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Number of Movies Rated around Peak of Users in the edx Training Data Set", 
       x = "Number of Movies Rated", y = "Number of Users", 
       subtitle = "Plot shows that, of the peak number of users in the distribution, each user rated 25 or fewer movies.")
# The plot shows that of the peak number of users in the distribution, each user rated 25 or fewer movies. 


# count the number of ratings for each movie in the edx training data set and plot the lower end of the distribution:
edx %>% group_by(movieId) %>%  
  summarize(count = n()) %>%
  as.data.frame() %>%
  filter(count <= 100) %>% 
  ggplot(aes(count)) + 
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Number of Ratings around Peak of Movies in the edx Training Data Set", 
       x = "Number of Ratings", y = "Number of Movies", 
       subtitle = "Plot shows that, of the peak number of movies in the distribution, each was rated 25 times or less.")
# The plot shows that of the peak number of movies in the distribution, each was rated 25 times or less. 


# Matrix Factorization ----

# To reduce noise, filter for users and movies that have 50 or more ratings, and determine if order makes a difference:

gum_r_50 <- gum_r_hat %>% group_by(userId) %>% filter(n() >= 50) %>% ungroup() %>% 
                          group_by(movieId) %>% filter(n() >= 50) %>% ungroup()
dim(gum_r_50)
[1] 8041064       3

mug_r_50 <- gum_r_hat %>% group_by(movieId) %>% filter(n() >= 50) %>% ungroup() %>% 
                          group_by(userId) %>% filter(n() >= 50) %>% ungroup()
dim(mug_r_50)
[1] 8041858       3

# The results show that filtering by movieId before userId yields 794 more observations, 
# therefore mug_r_50 will be used for matrix factorization.
8041858 - 8041064
[1] 794


mug_r_50
# A tibble: 8,041,858 × 3
   userId movieId  r_hat
    <int>   <dbl>  <dbl>
 1      5       1 -2.98 
 2      5       7 -0.428
 3      5      25 -0.774
 4      5      28 -1.18 
 5      5      30  1.23 
 6      5      32  1.05 
 7      5      47  0.887
 8      5      52  0.370
 9      5      57 -0.429
10      5      58 -1.10 
# … with 8,041,848 more rows


rm(gum_r_50)


# mug_r_50 has a loss of 958,197 observations compared to gum_r_hat:
dim(gum_r_hat)
[1] 9000055       3

9000055 - 8041858
[1] 958197


# Use pivot_wider and convert mug_r_50 to a matrix of gum residuals, r_hat, with userId rows and movieId columns:
mug50r <- mug_r_50 %>% pivot_wider(names_from = movieId, values_from = r_hat) %>% as.matrix()

mug50r[1:10, 1:10]
      userId          1          7         25         28       30         32         47        52         57
 [1,]      5 -2.9765241 -0.4284125 -0.7744274 -1.1765053 1.231397  1.0541062  0.8867561 0.3702443 -0.4289389
 [2,]      7         NA         NA         NA         NA       NA  0.1155526         NA        NA         NA
 [3,]      8         NA         NA         NA         NA       NA         NA -0.2310719        NA         NA
 [4,]     10         NA -0.4264759 -0.7724907         NA       NA         NA         NA        NA         NA
 [5,]     11         NA         NA         NA         NA       NA         NA         NA        NA         NA
 [6,]     13         NA         NA         NA         NA       NA         NA         NA        NA         NA
 [7,]     14 -0.7865207         NA         NA         NA       NA         NA         NA        NA         NA
 [8,]     17         NA         NA         NA         NA       NA -1.1020162         NA        NA         NA
 [9,]     18 -0.9914650 -0.9433534  0.2106317         NA       NA         NA  0.3718153        NA         NA
[10,]     19         NA         NA         NA  0.8405613       NA         NA         NA        NA         NA

# Create row names for matrix mug50r with userIds:
rownames(mug50r)<- mug50r[ ,1]

mug50r[1:10, 1:10]
   userId          1          7         25         28       30         32         47        52         57
5       5 -2.9765241 -0.4284125 -0.7744274 -1.1765053 1.231397  1.0541062  0.8867561 0.3702443 -0.4289389
7       7         NA         NA         NA         NA       NA  0.1155526         NA        NA         NA
8       8         NA         NA         NA         NA       NA         NA -0.2310719        NA         NA
10     10         NA -0.4264759 -0.7724907         NA       NA         NA         NA        NA         NA
11     11         NA         NA         NA         NA       NA         NA         NA        NA         NA
13     13         NA         NA         NA         NA       NA         NA         NA        NA         NA
14     14 -0.7865207         NA         NA         NA       NA         NA         NA        NA         NA
17     17         NA         NA         NA         NA       NA -1.1020162         NA        NA         NA
18     18 -0.9914650 -0.9433534  0.2106317         NA       NA         NA  0.3718153        NA         NA
19     19         NA         NA         NA  0.8405613       NA         NA         NA        NA         NA

# Delete redundant userId column from matrix mug50r:
mug50r <- mug50r[ ,-1]

mug50r[1:10, 1:10]
            1          7         25         28       30         32         47        52         57        58
5  -2.9765241 -0.4284125 -0.7744274 -1.1765053 1.231397  1.0541062  0.8867561 0.3702443 -0.4289389 -1.102863
7          NA         NA         NA         NA       NA  0.1155526         NA        NA         NA        NA
8          NA         NA         NA         NA       NA         NA -0.2310719        NA         NA        NA
10         NA -0.4264759 -0.7724907         NA       NA         NA         NA        NA         NA -1.100926
11         NA         NA         NA         NA       NA         NA         NA        NA         NA        NA
13         NA         NA         NA         NA       NA         NA         NA        NA         NA        NA
14 -0.7865207         NA         NA         NA       NA         NA         NA        NA         NA        NA
17         NA         NA         NA         NA       NA -1.1020162         NA        NA         NA        NA
18 -0.9914650 -0.9433534  0.2106317         NA       NA         NA  0.3718153        NA         NA        NA
19         NA         NA         NA  0.8405613       NA         NA         NA        NA         NA        NA

# Dimensions of matrix mug50r:
dim(mug50r)
[1] 40595  7081

length(unique(edx$userId))
[1] 69878

69878 - 40595
[1] 29283 # fewer users in the matrix factorization 

length(unique(edx$movieId))
[1] 10677

10677 - 7081
[1] 3596 # fewer movies in the matrix factorization

# Replace all NAs in matrix mug50r with 0:
mug50r[is.na(mug50r)] <- 0

mug50r[1:10, 1:10]
            1          7         25         28       30         32         47        52         57        58
5  -2.9765241 -0.4284125 -0.7744274 -1.1765053 1.231397  1.0541062  0.8867561 0.3702443 -0.4289389 -1.102863
7   0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.1155526  0.0000000 0.0000000  0.0000000  0.000000
8   0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000 -0.2310719 0.0000000  0.0000000  0.000000
10  0.0000000 -0.4264759 -0.7724907  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000 -1.100926
11  0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
13  0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
14 -0.7865207  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
17  0.0000000  0.0000000  0.0000000  0.0000000 0.000000 -1.1020162  0.0000000 0.0000000  0.0000000  0.000000
18 -0.9914650 -0.9433534  0.2106317  0.0000000 0.000000  0.0000000  0.3718153 0.0000000  0.0000000  0.000000
19  0.0000000  0.0000000  0.0000000  0.8405613 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000


rm(mug_r_50)


# Matrix Factorization by Principle Component Analysis: ----
pca_mug50r <- prcomp(mug50r)


# The q vectors (principal components) of the movie effects are stored in this matrix:
dim(pca_mug50r$rotation)
[1] 7081 7081

pca_mug50r$rotation[1:10, 1:9]
            PC1          PC2           PC3          PC4          PC5           PC6           PC7          PC8           PC9
1  -0.009210354  0.150404654  0.0899007339 -0.074081346  0.008217514 -0.0949622524  0.1075646728 -0.043060184  2.071728e-02
7  -0.024830438 -0.009447657  0.0205855801  0.012052150  0.018086420  0.0020840625 -0.0142571721 -0.011046553 -8.983116e-03
25  0.068022138  0.004243391 -0.0039295963  0.053582522 -0.012499885 -0.0173913298  0.0535569752  0.011177005  6.997228e-02
28  0.001377849 -0.002920135  0.0026184659 -0.003143923  0.005948378 -0.0046246053  0.0032467083  0.001017347 -1.931157e-03
30  0.003409409 -0.003239091  0.0006479528 -0.002204609  0.001021649  0.0017043885  0.0003888973  0.003553432 -4.665541e-05
32  0.090769563  0.020957415 -0.0889767195  0.050685441  0.049604071 -0.0298067480  0.0290859873 -0.140909859  1.526584e-02
47  0.061056573  0.045656401 -0.1132065845  0.131108485 -0.027268234 -0.0004172563  0.0065741964 -0.080738671  1.068137e-01
52  0.017409640 -0.002740593  0.0036296398  0.009225475  0.001189734 -0.0063284979  0.0118207225  0.004572194  9.869523e-03
57  0.004422033 -0.004732987  0.0012114374  0.004215804  0.006359914 -0.0031099777  0.0041895012 -0.003608721 -8.159627e-04
58  0.010867177 -0.001543884  0.0176060344  0.020788891  0.015114299 -0.0153584772  0.0034284234  0.005151869 -6.896316e-03


# The p vectors (principle components) of the user effects are stored in this matrix:
dim(pca_mug50r$x)
[1] 40595  7081

pca_mug50r$x[1:10, 1:10]
           PC1         PC2         PC3           PC4         PC5         PC6         PC7        PC8         PC9        PC10
5   1.13794400 -0.41907894  0.02315635  0.2225642501 -0.02978516 -0.17093232 -0.80604226  0.1588801  0.35195988  0.48933642
7   0.43092793 -0.42627452  0.59404777  0.0713279127 -0.62496983  0.29427637  0.11937539  0.2269217 -0.29667344 -0.36760886
8  -0.26361555 -3.81168728 -3.65196613 -1.5284428975  0.70662633 -0.68588821  1.56622585 -0.5720852 -0.15541819  0.35324768
10 -0.57627568 -0.63379632  0.80266179 -0.7127153497  0.66009026 -0.11736433 -0.16132941 -0.1252867 -1.27988438 -0.03968345
11 -1.04290421  0.32736645  0.70334070 -0.3338654165  0.71467532 -0.20075022 -0.66537365 -0.3115773 -1.01564861  0.11897071
13 -0.52993573 -0.37917518 -0.44064329 -0.0007783951 -0.22074716  0.01785301  0.08477686  0.3680456 -0.36944257 -0.27883114
14 -0.18945992  0.19940208  0.34893980 -0.0808984437  0.65213742  0.09019343 -0.16064552 -0.1253320  0.11508906  0.22239226
17 -0.05765358 -0.04709548  0.79914773 -0.0650517649 -0.31893222  0.24694240 -0.48327155  1.0967612  0.11688069  0.33365802
18  0.83191685 -1.66679723 -2.47905234  0.8726579637  0.06249052  1.19007486  1.93734308 -0.1973805  0.23418138  0.66177186
19 -1.68483010 -0.19430809  0.05525708 -0.2118299918  1.99557577 -2.81579148 -0.52226927 -1.4651322  0.04170323  0.74882058


rm(mug50r)


# We can see the variability of each of the q vectors (principal components) of the movie effects:
qplot(1:ncol(pca_mug50r$rotation), pca_mug50r$sdev, xlab = "Principal Components (q vectors) of Movie Effects",
      main = "Variability (standard deviation) of the Movie Principal Components")

summary(pca_mug50r)$importance[,1:10]
                            PC1      PC2      PC3       PC4       PC5       PC6       PC7      PC8      PC9     PC10
Standard deviation     1.408121 1.122604 1.028423 0.9000563 0.8553758 0.7880584 0.7598416 0.745612 0.726065 0.707945
Proportion of Variance 0.013830 0.008790 0.007370 0.0056500 0.0051000 0.0043300 0.0040300 0.003880 0.003680 0.003490
Cumulative Proportion  0.013830 0.022610 0.029990 0.0356400 0.0407400 0.0450700 0.0490900 0.052970 0.056650 0.060140

summary(pca_mug50r)$importance[,11:20]
                            PC11      PC12      PC13      PC14      PC15      PC16      PC17      PC18      PC19      PC20
Standard deviation     0.6856479 0.6574896 0.6523656 0.6411314 0.6339477 0.6233606 0.6192485 0.6131635 0.5982004 0.5885285
Proportion of Variance 0.0032800 0.0030100 0.0029700 0.0028700 0.0028000 0.0027100 0.0026700 0.0026200 0.0025000 0.0024200
Cumulative Proportion  0.0634200 0.0664300 0.0694000 0.0722700 0.0750700 0.0777800 0.0804500 0.0830800 0.0855700 0.0879900

summary(pca_mug50r)$importance[,c(30,40,50,60,70,80,90,100,200,300)]
                            PC30      PC40      PC50      PC60      PC70      PC80      PC90     PC100     PC200     PC300
Standard deviation     0.5486335 0.5197008 0.4949644 0.4767253 0.4633365 0.4518611 0.4421757 0.4330717 0.3637378 0.3250069
Proportion of Variance 0.0021000 0.0018800 0.0017100 0.0015800 0.0015000 0.0014200 0.0013600 0.0013100 0.0009200 0.0007400
Cumulative Proportion  0.1102500 0.1299200 0.1477300 0.1641300 0.1794500 0.1939900 0.2078800 0.2212400 0.3294100 0.4115200

summary(pca_mug50r)$importance[,c(400,500,1000,1500,2000,3000,4000,5000,6000,7081)]
                           PC400     PC500    PC1000    PC1500   PC2000     PC3000     PC4000     PC5000     PC6000      PC7081
Standard deviation     0.2935257 0.2691965 0.1945643 0.1502549 0.119642 0.07879433 0.05335311 0.03620976 0.02362785 0.007141173
Proportion of Variance 0.0006000 0.0005100 0.0002600 0.0001600 0.000100 0.00004000 0.00002000 0.00001000 0.00000000 0.000000000
Cumulative Proportion  0.4779900 0.5329900 0.7150800 0.8174600 0.880410 0.94763000 0.97758000 0.99141000 0.99763000 1.000000000


# Calculate the diagonal matrix Sigma:
sigma <- diag(pca_mug50r$sdev^2)

sigma[1:12, 1:12]
          [,1]    [,2]     [,3]      [,4]      [,5]     [,6]      [,7]      [,8]      [,9]     [,10]    [,11]     [,12]
 [1,] 1.982804 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [2,] 0.000000 1.26024 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [3,] 0.000000 0.00000 1.057654 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [4,] 0.000000 0.00000 0.000000 0.8101014 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [5,] 0.000000 0.00000 0.000000 0.0000000 0.7316678 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [6,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.621036 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [7,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.5773593 0.0000000 0.0000000 0.0000000 0.000000 0.0000000
 [8,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.5559372 0.0000000 0.0000000 0.000000 0.0000000
 [9,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.5271704 0.0000000 0.000000 0.0000000
[10,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.5011861 0.000000 0.0000000
[11,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.470113 0.0000000
[12,] 0.000000 0.00000 0.000000 0.0000000 0.0000000 0.000000 0.0000000 0.0000000 0.0000000 0.0000000 0.000000 0.4322925

dim(sigma)
[1] 7081 7081


rm(sigma)


# Calculate residual matrix mug50r_pq from product of pca_mug50r$x %*% t(pca_mug50r$rotation), 
# and verify that the matrix mug50r_pq values approximate the values in matrix mug50r:
mug50r_pq <- pca_mug50r$x %*% t(pca_mug50r$rotation)

dim(mug50r_pq)
[1] 40595  7081

mug50r_pq[1:10, 1:10]
              1            7           25           28            30          32           47           52           57           58
5  -2.972181427 -0.421108583 -0.775597214 -1.175381611  1.2310981986  1.03723391  0.884931632  0.367796611 -0.427763411 -1.100501253
7   0.004342696  0.007303919 -0.001169826  0.001123668 -0.0002991337  0.09868023 -0.001824492 -0.002447707  0.001175466  0.002361292
8   0.004342696  0.007303919 -0.001169826  0.001123668 -0.0002991337 -0.01687233 -0.232896381 -0.002447707  0.001175466  0.002361292
10  0.004342696 -0.419171933 -0.773660564  0.001123668 -0.0002991337 -0.01687233 -0.001824492 -0.002447707  0.001175466 -1.098564602
11  0.004342696  0.007303919 -0.001169826  0.001123668 -0.0002991337 -0.01687233 -0.001824492 -0.002447707  0.001175466  0.002361292
13  0.004342696  0.007303919 -0.001169826  0.001123668 -0.0002991337 -0.01687233 -0.001824492 -0.002447707  0.001175466  0.002361292
14 -0.782178030  0.007303919 -0.001169826  0.001123668 -0.0002991337 -0.01687233 -0.001824492 -0.002447707  0.001175466  0.002361292
17  0.004342696  0.007303919 -0.001169826  0.001123668 -0.0002991337 -1.11888850 -0.001824492 -0.002447707  0.001175466  0.002361292
18 -0.987122295 -0.936049451  0.209461918  0.001123668 -0.0002991337 -0.01687233  0.369990765 -0.002447707  0.001175466  0.002361292
19  0.004342696  0.007303919 -0.001169826  0.841684941 -0.0002991337 -0.01687233 -0.001824492 -0.002447707  0.001175466  0.002361292

mug50r[1:10, 1:10]
            1          7         25         28       30         32         47        52         57        58
5  -2.9765241 -0.4284125 -0.7744274 -1.1765053 1.231397  1.0541062  0.8867561 0.3702443 -0.4289389 -1.102863
7   0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.1155526  0.0000000 0.0000000  0.0000000  0.000000
8   0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000 -0.2310719 0.0000000  0.0000000  0.000000
10  0.0000000 -0.4264759 -0.7724907  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000 -1.100926
11  0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
13  0.0000000  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
14 -0.7865207  0.0000000  0.0000000  0.0000000 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000
17  0.0000000  0.0000000  0.0000000  0.0000000 0.000000 -1.1020162  0.0000000 0.0000000  0.0000000  0.000000
18 -0.9914650 -0.9433534  0.2106317  0.0000000 0.000000  0.0000000  0.3718153 0.0000000  0.0000000  0.000000
19  0.0000000  0.0000000  0.0000000  0.8405613 0.000000  0.0000000  0.0000000 0.0000000  0.0000000  0.000000

dim(mug50r)
[1] 40595  7081
# Out of the first 10 Principal Components, mug50r_pq approximates mug50r quite well.


rm(mug50r_pq)


# Calculate residual matrix mug50r_p1q1 from product of the 1st principle components of p (pca_mug50r$x) and q (pca_mug50r$rotation):

mug50r_p1q1 <- pca_mug50r$x[, 1] %*% t(pca_mug50r$rotation[, 1])


dim(mug50r_p1q1)
[1] 40595  7081

mug50r_p1q1[1:10, 1:9]
                  1            7          25            28            30          32          47           52           57
 [1,] -0.0104808675 -0.028255648  0.07740538  1.567915e-03  0.0038797167  0.10329068  0.06947896  0.019811195  0.005032026
 [2,] -0.0039689990 -0.010700129  0.02931264  5.937535e-04  0.0014692096  0.03911514  0.02631098  0.007502300  0.001905577
 [3,]  0.0024279926  0.006545690 -0.01793169 -3.632224e-04 -0.0008987733 -0.02392827 -0.01609546 -0.004589452 -0.001165717
 [4,]  0.0053077032  0.014309178 -0.03919950 -7.940207e-04 -0.0019647596 -0.05230829 -0.03518542 -0.010032752 -0.002548310
 [5,]  0.0096055174  0.025895769 -0.07094057 -1.436964e-03 -0.0035556871 -0.09466396 -0.06367616 -0.018156587 -0.004611757
 [6,]  0.0048808959  0.013158536 -0.03604736 -7.301713e-04 -0.0018067677 -0.04810203 -0.03235606 -0.009225990 -0.002343393
 [7,]  0.0017449930  0.004704373 -0.01288747 -2.610471e-04 -0.0006459464 -0.01719719 -0.01156777 -0.003298429 -0.000837798
 [8,]  0.0005310099  0.001431564 -0.00392172 -7.943792e-05 -0.0001965646 -0.00523319 -0.00352013 -0.001003728 -0.000254946
 [9,] -0.0076622491 -0.020656860  0.05658876  1.146256e-03  0.0028363449  0.07551273  0.05079399  0.014483373  0.003678764
[10,]  0.0155178824  0.041835070 -0.11460574 -2.321441e-03 -0.0057442751 -0.15293129 -0.10286995 -0.029332285 -0.007450374

# Convert matrix mug50r_p1q1 to a data frame:
mug50r_p1q1_df <- as.data.frame(mug50r_p1q1)

mug50r_p1q1_df[1:10, 1:9]
               1            7          25            28            30          32          47           52           57
1  -0.0104808675 -0.028255648  0.07740538  1.567915e-03  0.0038797167  0.10329068  0.06947896  0.019811195  0.005032026
2  -0.0039689990 -0.010700129  0.02931264  5.937535e-04  0.0014692096  0.03911514  0.02631098  0.007502300  0.001905577
3   0.0024279926  0.006545690 -0.01793169 -3.632224e-04 -0.0008987733 -0.02392827 -0.01609546 -0.004589452 -0.001165717
4   0.0053077032  0.014309178 -0.03919950 -7.940207e-04 -0.0019647596 -0.05230829 -0.03518542 -0.010032752 -0.002548310
5   0.0096055174  0.025895769 -0.07094057 -1.436964e-03 -0.0035556871 -0.09466396 -0.06367616 -0.018156587 -0.004611757
6   0.0048808959  0.013158536 -0.03604736 -7.301713e-04 -0.0018067677 -0.04810203 -0.03235606 -0.009225990 -0.002343393
7   0.0017449930  0.004704373 -0.01288747 -2.610471e-04 -0.0006459464 -0.01719719 -0.01156777 -0.003298429 -0.000837798
8   0.0005310099  0.001431564 -0.00392172 -7.943792e-05 -0.0001965646 -0.00523319 -0.00352013 -0.001003728 -0.000254946
9  -0.0076622491 -0.020656860  0.05658876  1.146256e-03  0.0028363449  0.07551273  0.05079399  0.014483373  0.003678764
10  0.0155178824  0.041835070 -0.11460574 -2.321441e-03 -0.0057442751 -0.15293129 -0.10286995 -0.029332285 -0.007450374

dim(mug50r_p1q1_df)
[1] 40595  7081


rm(mug50r_p1q1)


# Add userId column to mug50r_p1q1_df data frame and save as data frame object x:
x <- mug50r_p1q1_df %>% mutate(userId = rownames(pca_mug50r$x))

x[1:10, 7074:7082]
           59118         62155         26249          6527          6318         32460          5243         62336 userId
1   5.108775e-05  7.918432e-05 -2.345380e-04  1.726868e-05 -4.018020e-04 -8.678319e-05  2.770286e-04  5.180936e-04      5
2   1.934642e-05  2.998631e-05 -8.881717e-05  6.539476e-06 -1.521584e-04 -3.286392e-05  1.049079e-04  1.961968e-04      7
3  -1.183496e-05 -1.834380e-05  5.433295e-05 -4.000454e-06  9.308125e-05  2.010415e-05 -6.417630e-05 -1.200213e-04      8
4  -2.587177e-05 -4.010039e-05  1.187743e-04 -8.745176e-06  2.034799e-04  4.394860e-05 -1.402923e-04 -2.623721e-04     10
5  -4.682096e-05 -7.257094e-05  2.149496e-04 -1.582642e-05  3.682439e-04  7.953516e-05 -2.538915e-04 -4.748230e-04     11
6  -2.379135e-05 -3.687581e-05  1.092234e-04 -8.041952e-06  1.871175e-04  4.041457e-05 -1.290110e-04 -2.412740e-04     13
7  -8.505762e-06 -1.318365e-05  3.904898e-05 -2.875118e-06  6.689729e-05  1.444881e-05 -4.612337e-05 -8.625906e-05     14
8  -2.588345e-06 -4.011849e-06  1.188279e-05 -8.749124e-07  2.035717e-05  4.396843e-06 -1.403557e-05 -2.624905e-05     17
9   3.734873e-05  5.788929e-05 -1.714637e-04  1.262462e-05 -2.937454e-04 -6.344460e-05  2.025273e-04  3.787628e-04     18
10 -7.564008e-05 -1.172396e-04  3.472549e-04 -2.556786e-05  5.949045e-04  1.284904e-04 -4.101661e-04 -7.670849e-04     19

# Create data frame object y with a 1st column of userIds from row names of user effects matrix pca_mug50r$x:
y <- data.frame(userId = rownames(pca_mug50r$x))

head(y)
  userId
1      5
2      7
3      8
4     10
5     11
6     13

tail(y)
      userId
40590  71559
40591  71560
40592  71562
40593  71564
40594  71565
40595  71567


rm(mug50r_p1q1_df)


# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
mug50r_p1q1_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

mug50r_p1q1_dfw[1:10, 1:9]
   userId             1            7          25            28            30          32          47           52
1       5 -0.0104808675 -0.028255648  0.07740538  1.567915e-03  0.0038797167  0.10329068  0.06947896  0.019811195
2       7 -0.0039689990 -0.010700129  0.02931264  5.937535e-04  0.0014692096  0.03911514  0.02631098  0.007502300
3       8  0.0024279926  0.006545690 -0.01793169 -3.632224e-04 -0.0008987733 -0.02392827 -0.01609546 -0.004589452
4      10  0.0053077032  0.014309178 -0.03919950 -7.940207e-04 -0.0019647596 -0.05230829 -0.03518542 -0.010032752
5      11  0.0096055174  0.025895769 -0.07094057 -1.436964e-03 -0.0035556871 -0.09466396 -0.06367616 -0.018156587
6      13  0.0048808959  0.013158536 -0.03604736 -7.301713e-04 -0.0018067677 -0.04810203 -0.03235606 -0.009225990
7      14  0.0017449930  0.004704373 -0.01288747 -2.610471e-04 -0.0006459464 -0.01719719 -0.01156777 -0.003298429
8      17  0.0005310099  0.001431564 -0.00392172 -7.943792e-05 -0.0001965646 -0.00523319 -0.00352013 -0.001003728
9      18 -0.0076622491 -0.020656860  0.05658876  1.146256e-03  0.0028363449  0.07551273  0.05079399  0.014483373
10     19  0.0155178824  0.041835070 -0.11460574 -2.321441e-03 -0.0057442751 -0.15293129 -0.10286995 -0.029332285

mug50r_p1q1_dfw[1:10, 7075:7082]
           59118         62155         26249          6527          6318         32460          5243         62336
1   5.108775e-05  7.918432e-05 -2.345380e-04  1.726868e-05 -4.018020e-04 -8.678319e-05  2.770286e-04  5.180936e-04
2   1.934642e-05  2.998631e-05 -8.881717e-05  6.539476e-06 -1.521584e-04 -3.286392e-05  1.049079e-04  1.961968e-04
3  -1.183496e-05 -1.834380e-05  5.433295e-05 -4.000454e-06  9.308125e-05  2.010415e-05 -6.417630e-05 -1.200213e-04
4  -2.587177e-05 -4.010039e-05  1.187743e-04 -8.745176e-06  2.034799e-04  4.394860e-05 -1.402923e-04 -2.623721e-04
5  -4.682096e-05 -7.257094e-05  2.149496e-04 -1.582642e-05  3.682439e-04  7.953516e-05 -2.538915e-04 -4.748230e-04
6  -2.379135e-05 -3.687581e-05  1.092234e-04 -8.041952e-06  1.871175e-04  4.041457e-05 -1.290110e-04 -2.412740e-04
7  -8.505762e-06 -1.318365e-05  3.904898e-05 -2.875118e-06  6.689729e-05  1.444881e-05 -4.612337e-05 -8.625906e-05
8  -2.588345e-06 -4.011849e-06  1.188279e-05 -8.749124e-07  2.035717e-05  4.396843e-06 -1.403557e-05 -2.624905e-05
9   3.734873e-05  5.788929e-05 -1.714637e-04  1.262462e-05 -2.937454e-04 -6.344460e-05  2.025273e-04  3.787628e-04
10 -7.564008e-05 -1.172396e-04  3.472549e-04 -2.556786e-05  5.949045e-04  1.284904e-04 -4.101661e-04 -7.670849e-04


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y)

# Create a long data frame mug50r_p1q1_dfl from the wide data frame to get all rows into the edx data frame order and format:
mug50r_p1q1_dfl <- pivot_longer(data = mug50r_p1q1_dfw, cols = 2:7082, names_to = "movieId", values_to = "p1q1")

mug50r_p1q1_dfl
# A tibble: 287,453,195 × 3
   userId movieId     p1q1
   <chr>  <chr>      <dbl>
 1 5      1       -0.0105 
 2 5      7       -0.0283 
 3 5      25       0.0774 
 4 5      28       0.00157
 5 5      30       0.00388
 6 5      32       0.103  
 7 5      47       0.0695 
 8 5      52       0.0198 
 9 5      57       0.00503
10 5      58       0.0124 
# … with 287,453,185 more rows


# Convert userId and movieId columns from <chr> to <int> values in the long data frame:
mug50r_p1q1_dfl <- mug50r_p1q1_dfl %>% mutate(userId = as.integer(userId), movieId = as.integer(movieId))

mug50r_p1q1_dfl
# A tibble: 287,453,195 × 3
   userId movieId     p1q1
    <int>   <int>    <dbl>
 1      5       1 -0.0105 
 2      5       7 -0.0283 
 3      5      25  0.0774 
 4      5      28  0.00157
 5      5      30  0.00388
 6      5      32  0.103  
 7      5      47  0.0695 
 8      5      52  0.0198 
 9      5      57  0.00503
10      5      58  0.0124 
# … with 287,453,185 more rows

sum(is.na(mug50r_p1q1_dfl$p1q1) == TRUE)
[1] 0


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(mug50r_p1q1_dfw)


# mug50r_p1q1_edx organizes the p1q1 elements of the long data frame, mug50r_p1q1_dfl, to the corresponding
# userId and movieId of edx:

mug50r_p1q1_edx <- edx %>% select(userId, movieId) %>% left_join(mug50r_p1q1_dfl, by = c('userId', 'movieId'))

mug50r_p1q1_edx
         userId movieId p1q1
      1:      1     122   NA
      2:      1     185   NA
      3:      1     292   NA
      4:      1     316   NA
      5:      1     329   NA
     ---                    
9000051:  32620   33140   NA
9000052:  40976   61913   NA
9000053:  59269   63141   NA
9000054:  60713    4820   NA
9000055:  64621   39429   NA


mug50r_p1q1_edx[100:110]
    userId movieId         p1q1
 1:      4     592           NA
 2:      4     595           NA
 3:      4     597           NA
 4:      5       1 -0.010480868
 5:      5       7 -0.028255648
 6:      5      25  0.077405383
 7:      5      28  0.001567915
 8:      5      30  0.003879717
 9:      5      32  0.103290679
10:      5      47  0.069478961
11:      5      52  0.019811195


# 958,197 observations with NA due to 29283 fewer users and 3596 fewer movies in the matrix factorization:
sum(is.na(mug50r_p1q1_edx$p1q1) == TRUE)
[1] 958197

sum(is.na(mug50r_p1q1_edx$p1q1) == FALSE)
[1] 8041858

958197 + 8041858 == 9000055
[1] TRUE

# Replace NA with zero (0) in the 958,197 observations of mug50r_p1q1_edx$p1q1, and verify that no NA remains:
mug50r_p1q1_edx$p1q1[is.na(mug50r_p1q1_edx$p1q1)] <- 0

sum(is.na(mug50r_p1q1_edx$p1q1) == TRUE)
[1] 0

sum(is.na(mug50r_p1q1_edx$p1q1) == FALSE)
[1] 9000055

mug50r_p1q1_edx[100:110]
    userId movieId         p1q1
 1:      4     592  0.000000000
 2:      4     595  0.000000000
 3:      4     597  0.000000000
 4:      5       1 -0.010480868
 5:      5       7 -0.028255648
 6:      5      25  0.077405383
 7:      5      28  0.001567915
 8:      5      30  0.003879717
 9:      5      32  0.103290679
10:      5      47  0.069478961
11:      5      52  0.019811195


# mug50r_p1q1_val organizes the p1q1 elements of the long data frame, mug50r_p1q1_dfl, to the corresponding
# userId and movieId of validation:

mug50r_p1q1_val <- validation %>% select(userId, movieId) %>% left_join(mug50r_p1q1_dfl, by = c('userId', 'movieId'))

mug50r_p1q1_val
        userId movieId        p1q1
     1:      1     231          NA
     2:      1     480          NA
     3:      1     586          NA
     4:      2     151          NA
     5:      2     858          NA
    ---                           
999995:  71566     235          NA
999996:  71566     273          NA
999997:  71566     434          NA
999998:  71567     480  0.05222851
999999:  71567     898 -0.00495711


sum(is.na(mug50r_p1q1_val$p1q1) == TRUE)
[1] 109507

sum(is.na(mug50r_p1q1_val$p1q1) == FALSE)
[1] 890492

109507 + 890492 == 999999
[1] TRUE

# Replace NA with zero (0) in the 958,197 observations of mug50r_p1q1_val$p1q1, and verify that no NA remains:
mug50r_p1q1_val$p1q1[is.na(mug50r_p1q1_val$p1q1)] <- 0

sum(is.na(mug50r_p1q1_val$p1q1) == TRUE)
[1] 0

sum(is.na(mug50r_p1q1_val$p1q1) == FALSE)
[1] 999999

mug50r_p1q1_val
        userId movieId        p1q1
     1:      1     231  0.00000000
     2:      1     480  0.00000000
     3:      1     586  0.00000000
     4:      2     151  0.00000000
     5:      2     858  0.00000000
    ---                           
999995:  71566     235  0.00000000
999996:  71566     273  0.00000000
999997:  71566     434  0.00000000
999998:  71567     480  0.05222851
999999:  71567     898 -0.00495711


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(mug50r_p1q1_dfl)


# mug50r_p1q1_model adds the corresponding movie bias, user bias, genre bias, and mug50r_p1q1_edx factor 
# to the mean rating of all movies:
mug50r_p1q1_model <- edx %>% select(userId, movieId, genreId) %>% 
                     left_join(user_bias, by = 'userId') %>%
                     left_join(movie_bias, by = 'movieId') %>%
                     left_join(genre_bias, by = 'genreId') %>%
                     left_join(mug50r_p1q1_edx, by = c('userId', 'movieId')) %>% 
                     mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1) %>% 
                     pull(y_hat)

mug50r_p1q1_model[1:10]
[1] 4.522376 4.782092 5.051046 5.005673 4.968962 4.125378 5.659021 5.049999 5.376095 4.643422

length(mug50r_p1q1_model)
[1] 9000055

# RMSE of the mug50r_p1q1_model where y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1:
mug50r_p1q1_rmse <- RMSE(edx$rating, mug50r_p1q1_model)

mug50r_p1q1_rmse
[1] 0.8482445

# add RMSE result of mug50r_p1q1_model to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "GUM + P1Q1 model", RMSE = mug50r_p1q1_rmse)

rmse_results
# A tibble: 5 × 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 G+U+M bias model 0.856
5 GUM + P1Q1 model 0.848


rmse_results %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|naive mean model | 1.0603313|
|movie bias model | 0.9423475|
|U + M bias model | 0.8567039|
|G+U+M bias model | 0.8563595|
|GUM + P1Q1 model | 0.8482445|


rm(mug50r_p1q1_model)


# TEST OF THE mug50r_p1q1_model ON THE VALIDATION DATA SET: ----

# mug50r_p1q1_model_test adds the corresponding movie bias, user bias, genre bias, 
# and mug50r_p1q1_val factor to the mean rating of all movies:
mug50r_p1q1_model_test <- validation %>% select(userId, movieId, genreId) %>% 
                          left_join(user_bias, by = 'userId') %>%
                          left_join(movie_bias, by = 'movieId') %>%
                          left_join(genre_bias, by = 'genreId') %>%
                          left_join(mug50r_p1q1_val, by = c('userId', 'movieId')) %>% 
                          mutate(y_hat = mu_hat + b_u_hat + b_i_hat + b_g_hat + p1q1) %>% 
                          pull(y_hat)

mug50r_p1q1_model_test[1:10]
[1] 4.608598 5.318447 4.712710 3.297101 4.201671 2.689968 3.959558 4.203180 4.318585 3.351185

length(mug50r_p1q1_model_test)
[1] 999999

mug50r_p1q1_test_rmse <- RMSE(validation$rating, mug50r_p1q1_model_test)
mug50r_p1q1_test_rmse
[1] 0.8578414

rmse_testresults <- rmse_testresults %>% add_row(method = "GUM + P1Q1 model", RMSE = mug50r_p1q1_test_rmse)
rmse_testresults
# A tibble: 3 × 2
  method            RMSE
  <chr>            <dbl>
1 U + M bias model 0.865
2 G+U+M bias model 0.865
3 GUM + P1Q1 model 0.858


rmse_testresults %>% knitr::kable()

|method           |      RMSE|
|:----------------|---------:|
|U + M bias model | 0.8653488|
|G+U+M bias model | 0.8649469|
|GUM + P1Q1 model | 0.8578414|


rm(mug50r_p1q1_model_test)

