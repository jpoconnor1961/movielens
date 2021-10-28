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


# RESIDUALS ----

# Residuals, r_hat, are estimated by subtracting out mu_hat from each movie rating as well as subtracting out the respective
# b_u_hat and b_i_hat from each movie rating:
r_hat <- edx %>% left_join(user_bias, by='userId') %>% left_join(movie_bias, by='movieId') %>% 
  mutate(r_hat = rating - mu_hat - b_u_hat - b_i_hat) %>% pull(r_hat)

r_hat[1:10]
[1]  0.46217944  0.19143127 -0.09724537 -0.02891175 -0.01669175  0.83297809 -0.69205687 -0.13424578 -0.43128269  0.36373351

# Add estimate of residuals, r_hat, to edx data frame:
edx_r <- mutate(edx, r_hat = r_hat)

edx_r
         userId movieId rating  timestamp                         title                        genres genreId        r_hat
      1:      1     122    5.0  838985046              Boomerang (1992)                Comedy|Romance       1  0.462179436
      2:      1     185    5.0  838983525               Net, The (1995)         Action|Crime|Thriller       2  0.191431268
      3:      1     292    5.0  838983421               Outbreak (1995)  Action|Drama|Sci-Fi|Thriller       3 -0.097245365
      4:      1     316    5.0  838983392               Stargate (1994)       Action|Adventure|Sci-Fi       4 -0.028911746
      5:      1     329    5.0  838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi       5 -0.016691750
---                                                                                                                  
9000051:  32620   33140    3.5 1173562747         Down and Derby (2005)               Children|Comedy      61 -0.455777291
9000052:  40976   61913    3.0 1227767528           Africa addio (1966)                   Documentary     128  0.185417866
9000053:  59269   63141    2.0 1226443318 Rockin' in the Rockies (1945)        Comedy|Musical|Western     597  0.004039963
9000054:  60713    4820    2.0 1119156754  Won't Anybody Listen? (2000)                   Documentary     128  0.468220844
9000055:  64621   39429    2.5 1201248182                Confess (2005)                Drama|Thriller      49 -0.030529561

# Remove large data files no longer needed (can always recalculate if needed again):
rm(r_hat)


# Matrix Factorization ----

# To reduce noise, filter for genres that have 10 or more ratings:
edx_r_10 <- edx_r %>% group_by(genreId) %>% filter(n() >= 10) %>% ungroup() %>% select(userId, movieId, genreId, r_hat)

# edx_r_10 has a loss of 172 observations compared to edx_r, due to 35 genres with 9 or fewer ratings.
edx_r_10
# A tibble: 8,999,883 x 4
   userId movieId genreId   r_hat
    <int>   <dbl>   <int>   <dbl>
 1      1     122       1  0.462 
 2      1     185       2  0.191 
 3      1     292       3 -0.0972
 4      1     316       4 -0.0289
 5      1     329       5 -0.0167
 6      1     355       6  0.833 
 7      1     356       7 -0.692 
 8      1     362       8 -0.134 
 9      1     364       9 -0.431 
10      1     370      10  0.364 
# ... with 8,999,873 more rows

# Group edx_r_10 by userId and genreId, and summarise n() and mean(r_hat) for each genre by userId:
gr_10 <- edx_r_10 %>% group_by(userId, genreId) %>% summarise(n = n(), R_hat = mean(r_hat))

gr_10
# A tibble: 4,278,975 x 4
# Groups:   userId [69,878]
   userId genreId     n   R_hat
    <int>   <int> <int>   <dbl>
 1      1       1     1  0.462 
 2      1       2     1  0.191 
 3      1       3     1 -0.0972
 4      1       4     1 -0.0289
 5      1       5     1 -0.0167
 6      1       6     1  0.833 
 7      1       7     1 -0.692 
 8      1       8     1 -0.134 
 9      1       9     1 -0.431 
10      1      10     1  0.364 
# ... with 4,278,965 more rows

gr_10[90:100,]
# A tibble: 11 x 4
# Groups:   userId [2]
   userId genreId     n   R_hat
    <int>   <int> <int>   <dbl>
 1      4      64     1  0.674 
 2      5       1     3 -0.712 
 3      5      14     3 -0.0488
 4      5      15     5 -0.0637
 5      5      23     1 -0.471 
 6      5      25     1 -2.31  
 7      5      26     1 -2.46  
 8      5      28     8 -0.124 
 9      5      30     1  0.207 
10      5      32     1 -2.04  
11      5      34    20  0.255 

# Select userId, genreId, and R_hat, then pivot wider and convert to 
# a matrix of residuals with userId rows and genreId columns:
Gr10 <- gr_10 %>% 
  select(userId, genreId, R_hat) %>%
  pivot_wider(names_from = genreId, values_from = R_hat) %>%
  as.matrix()

Gr10[1:10, 1:10]
      userId          1         2           3           4           5          6          7          8          9
 [1,]      1  0.4621794 0.1914313 -0.09724537 -0.02891175 -0.01669175  0.8329781 -0.6920569 -0.1342458 -0.4312827
 [2,]      2         NA        NA          NA  0.62753522          NA         NA         NA         NA         NA
 [3,]      3 -0.7659126        NA          NA          NA          NA         NA  0.2391727         NA         NA
 [4,]      4 -1.1414386 0.8654524 -1.07008872  0.24339700  1.01046489         NA         NA         NA  0.5958739
 [5,]      5 -0.7124970        NA          NA          NA          NA         NA         NA         NA         NA
 [6,]      6  0.4603310        NA          NA  0.33280628          NA         NA         NA         NA         NA
 [7,]      7  0.2528647        NA          NA -0.04669991          NA         NA         NA         NA         NA
 [8,]      8 -0.3061964 0.3356187 -0.06942945  0.20233890  0.60335007 -0.1696456         NA         NA -0.9551437
 [9,]      9         NA        NA          NA  0.38015693          NA         NA         NA         NA         NA
[10,]     10  0.3895989        NA          NA          NA          NA         NA -1.0961532         NA         NA

# Create row names for matrix Gr10 with userIds:
rownames(Gr10)<- Gr10[ ,1]

Gr10[1:10, 1:10]
   userId          1         2           3           4           5          6          7          8          9
1       1  0.4621794 0.1914313 -0.09724537 -0.02891175 -0.01669175  0.8329781 -0.6920569 -0.1342458 -0.4312827
2       2         NA        NA          NA  0.62753522          NA         NA         NA         NA         NA
3       3 -0.7659126        NA          NA          NA          NA         NA  0.2391727         NA         NA
4       4 -1.1414386 0.8654524 -1.07008872  0.24339700  1.01046489         NA         NA         NA  0.5958739
5       5 -0.7124970        NA          NA          NA          NA         NA         NA         NA         NA
6       6  0.4603310        NA          NA  0.33280628          NA         NA         NA         NA         NA
7       7  0.2528647        NA          NA -0.04669991          NA         NA         NA         NA         NA
8       8 -0.3061964 0.3356187 -0.06942945  0.20233890  0.60335007 -0.1696456         NA         NA -0.9551437
9       9         NA        NA          NA  0.38015693          NA         NA         NA         NA         NA
10     10  0.3895989        NA          NA          NA          NA         NA -1.0961532         NA         NA

# Delete redundant userId column from matrix Gr10:
Gr10 <- Gr10[ ,-1]

Gr10[1:10, 1:10]
            1         2           3           4           5          6          7          8          9         10
1   0.4621794 0.1914313 -0.09724537 -0.02891175 -0.01669175  0.8329781 -0.6920569 -0.1342458 -0.4312827  0.3637335
2          NA        NA          NA  0.62753522          NA         NA         NA         NA         NA         NA
3  -0.7659126        NA          NA          NA          NA         NA  0.2391727         NA         NA         NA
4  -1.1414386 0.8654524 -1.07008872  0.24339700  1.01046489         NA         NA         NA  0.5958739         NA
5  -0.7124970        NA          NA          NA          NA         NA         NA         NA         NA         NA
6   0.4603310        NA          NA  0.33280628          NA         NA         NA         NA         NA -0.7694597
7   0.2528647        NA          NA -0.04669991          NA         NA         NA         NA         NA         NA
8  -0.3061964 0.3356187 -0.06942945  0.20233890  0.60335007 -0.1696456         NA         NA -0.9551437  0.2529476
9          NA        NA          NA  0.38015693          NA         NA         NA         NA         NA         NA
10  0.3895989        NA          NA          NA          NA         NA -1.0961532         NA         NA         NA

# Dimensions of matrix Gr10:
dim(Gr10)
[1] 69878   762

762 + 35 == 797
[1] TRUE

length(unique(edx$genres))
[1] 797

length(unique(edx$userId))
[1] 69878

# Replace all NAs in matrix Gr10 with 0:
Gr10[is.na(Gr10)] <- 0

Gr10[1:10, 1:10]
            1         2           3           4           5          6          7          8          9         10
1   0.4621794 0.1914313 -0.09724537 -0.02891175 -0.01669175  0.8329781 -0.6920569 -0.1342458 -0.4312827  0.3637335
2   0.0000000 0.0000000  0.00000000  0.62753522  0.00000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
3  -0.7659126 0.0000000  0.00000000  0.00000000  0.00000000  0.0000000  0.2391727  0.0000000  0.0000000  0.0000000
4  -1.1414386 0.8654524 -1.07008872  0.24339700  1.01046489  0.0000000  0.0000000  0.0000000  0.5958739  0.0000000
5  -0.7124970 0.0000000  0.00000000  0.00000000  0.00000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
6   0.4603310 0.0000000  0.00000000  0.33280628  0.00000000  0.0000000  0.0000000  0.0000000  0.0000000 -0.7694597
7   0.2528647 0.0000000  0.00000000 -0.04669991  0.00000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
8  -0.3061964 0.3356187 -0.06942945  0.20233890  0.60335007 -0.1696456  0.0000000  0.0000000 -0.9551437  0.2529476
9   0.0000000 0.0000000  0.00000000  0.38015693  0.00000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
10  0.3895989 0.0000000  0.00000000  0.00000000  0.00000000  0.0000000 -1.0961532  0.0000000  0.0000000  0.0000000

# Matrix Factorization by Principle Component Analysis:
pcaGr10 <- prcomp(Gr10)

# The q vectors (principal components) of the movie effects are stored in this matrix:
dim(pcaGr10$rotation)
[1] 762 762

pcaGr10$rotation[1:10, 1:9]
             PC1         PC2          PC3         PC4          PC5          PC6          PC7          PC8          PC9
1  -0.0167597944  0.20373308 -0.180741238  0.02016690 -0.139033615  0.171268811 -0.052650563  0.085097343 -0.172023642
2  -0.0606785575 -0.18937104 -0.047206518  0.09412923 -0.090826776  0.024641120  0.115689927 -0.384346711  0.176371128
3  -0.0778779354 -0.00508102 -0.033377247  0.04638668  0.018127412  0.007588198  0.025696103 -0.024073032 -0.012335629
4  -0.2277482894 -0.17163264  0.297964124 -0.18485431  0.335532110  0.387990302 -0.387346185 -0.148310935  0.163443082
5  -0.0812518063 -0.01851859  0.016001731  0.00779141  0.040855960  0.054582844 -0.014111113 -0.010495127 -0.026072016
6  -0.0399815234  0.04643545 -0.022158274 -0.04006618 -0.025948139  0.005152937  0.003871118  0.002296998  0.009620698
7  -0.0818271894  0.16177207  0.266514353  0.33288225 -0.253339032 -0.201118828 -0.527732038 -0.231079988 -0.287614329
8   0.0007458081  0.01994516  0.009984366 -0.01534014 -0.006968677 -0.001845256  0.009930365 -0.007343052  0.012248087
9  -0.0456122411  0.16393296  0.134361546 -0.06763864 -0.082793964 -0.059108486  0.079959698 -0.091147702  0.073395243
10 -0.0740715828 -0.02590746 -0.108487895 -0.04926345 -0.067443479 -0.009739347 -0.036769502  0.010590637  0.041881602

# The p vectors (principle components) of the user effects are stored in this matrix:
dim(pcaGr10$x)
[1] 69878   762

pcaGr10$x[1:10, 1:10]
            PC1        PC2        PC3           PC4        PC5        PC6         PC7         PC8        PC9        PC10
1  -0.002250367 -0.1996245 -0.5949504 -0.2166067697 0.01018279  0.1501558  0.03480262  0.23711958  0.3683382  0.24445025
2  -0.146615470 -0.2407114  0.6516307  0.0001996551 0.10848236 -0.8460623 -0.72180763  0.05811136  0.6119041  0.63364262
3   0.113777314  0.1130408  0.2667589  0.6045507509 0.08414690  0.3262517  0.13237604 -0.05807179  0.1458262 -0.91115274
4   0.023988124  0.5429333  1.3961028 -0.2072873639 0.38180794 -0.0577409  0.27952559 -0.51699010 -0.3026426 -0.32120288
5   1.727548961 -0.6923070 -0.2526611  0.3737888049 1.16773553 -0.5011249 -0.53285916 -0.80308429  0.3552775 -0.74533288
6   0.399278852  0.4082252  0.1376121  0.0375546419 0.14442722  0.3876461 -0.47430956 -0.08739242 -0.3237855  0.44020378
7   0.516373887  0.2409702 -0.3008987  0.2315078215 0.39002558 -0.1065010  0.11868676 -0.25074437  0.3235634  0.06906183
8  -1.361209904 -2.3045201 -1.8116605 -0.9614674815 0.30457021  0.1538832 -0.22112236  0.09777966 -0.8700238  0.03306856
9  -0.097243262  0.1751937  0.5041011  0.2934458221 0.23032841  1.1663678 -0.05757955 -0.65615712 -0.2051250  1.01045694
10 -0.743830090  1.0655817 -0.7289608 -0.1296110167 1.78976889  0.1082901  0.09139864  0.51157551  0.6182758  0.98885069

# We can see the variability of each of the q vectors (principal components) of the genre effects:
qplot(1:ncol(pcaGr10$rotation), pcaGr10$sdev, xlab = "Principal Components (q vectors) of Genre Effects",
      main = "Variability (standard deviation) of the Genre Principal Components")

summary(pcaGr10)$importance[,1:10]
                             PC1       PC2       PC3       PC4       PC5      PC6       PC7       PC8       PC9      PC10
Standard deviation     0.9158645 0.7820438 0.6963746 0.6909147 0.6388125 0.598668 0.5960267 0.5650854 0.5619785 0.5575469
Proportion of Variance 0.0215700 0.0157200 0.0124700 0.0122700 0.0104900 0.009210 0.0091300 0.0082100 0.0081200 0.0079900
Cumulative Proportion  0.0215700 0.0372900 0.0497600 0.0620300 0.0725200 0.081740 0.0908700 0.0990800 0.1072000 0.1152000

summary(pcaGr10)$importance[,11:20]
                            PC11      PC12     PC13      PC14      PC15      PC16     PC17      PC18      PC19     PC20
Standard deviation     0.5525487 0.5455556 0.540253 0.5367544 0.5312007 0.5241849 0.520642 0.5203514 0.5161994 0.511279
Proportion of Variance 0.0078500 0.0076500 0.007500 0.0074100 0.0072500 0.0070600 0.006970 0.0069600 0.0068500 0.006720
Cumulative Proportion  0.1230400 0.1307000 0.138200 0.1456100 0.1528600 0.1599300 0.166900 0.1738600 0.1807100 0.187430

summary(pcaGr10)$importance[,c(30,40,50,60,70,80,90,100,200,250)]
                            PC30      PC40      PC50     PC60      PC70      PC80      PC90     PC100     PC200    PC250
Standard deviation     0.4843564 0.4623656 0.4426079 0.418426 0.3995561 0.3832152 0.3708453 0.3581764 0.2494281 0.208017
Proportion of Variance 0.0060300 0.0055000 0.0050400 0.004500 0.0041000 0.0037800 0.0035400 0.0033000 0.0016000 0.001110
Cumulative Proportion  0.2511400 0.3081600 0.3605700 0.408020 0.4507400 0.4898800 0.5263000 0.5603300 0.7978500 0.865260

summary(pcaGr10)$importance[,c(275,285,286,300,400,500,600,700,762)]
                           PC275     PC285     PC286     PC300     PC400      PC500      PC600     PC700      PC762
Standard deviation     0.1920218 0.1863187 0.1853277 0.1771422 0.1151474 0.07149256 0.03957452 0.0190338 0.00483813
Proportion of Variance 0.0009500 0.0008900 0.0008800 0.0008100 0.0003400 0.00013000 0.00004000 0.0000100 0.00000000
Cumulative Proportion  0.8908100 0.8999700 0.9008500 0.9126200 0.9674400 0.98978000 0.99762000 0.9997000 1.00000000


# Calculate the diagonal matrix Sigma:
Sigma <- diag(pcaGr10$sdev^2)

Sigma[1:12, 1:12]
           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]   [,11]     [,12]
 [1,] 0.8388078 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [2,] 0.0000000 0.6115925 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [3,] 0.0000000 0.0000000 0.4849375 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [4,] 0.0000000 0.0000000 0.0000000 0.4773632 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [5,] 0.0000000 0.0000000 0.0000000 0.0000000 0.4080814 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [6,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3584033 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [7,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3552479 0.0000000 0.0000000 0.0000000 0.00000 0.0000000
 [8,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3193215 0.0000000 0.0000000 0.00000 0.0000000
 [9,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3158199 0.0000000 0.00000 0.0000000
[10,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.3108585 0.00000 0.0000000
[11,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.30531 0.0000000
[12,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.00000 0.2976309

dim(Sigma)
[1] 762 762


# Calculate residual matrix Gr10_PQ from product of pcaGr10$x %*% t(pcaGr10$rotation), 
# and verify that the matrix Gr10_PQ values approximate the values in matrix Gr10:
Gr10_PQ <- pcaGr10$x %*% t(pcaGr10$rotation)

dim(Gr10_PQ)
[1] 69878   762

Gr10_PQ[1:10, 1:10]
             1          2           3          4            5            6           7            8           9           10
1   0.49884179 0.21838935 -0.08575805 0.04173818 -0.003498882  0.839974502 -0.67187015 -0.129882599 -0.41563919  0.361135019
2   0.03666236 0.02695808  0.01148732 0.69818514  0.013192868  0.006996416  0.02018672  0.004363181  0.01564351 -0.002598492
3  -0.72925024 0.02695808  0.01148732 0.07064992  0.013192868  0.006996416  0.25935944  0.004363181  0.01564351 -0.002598492
4  -1.10477626 0.89241047 -1.05860140 0.31404692  1.023657761  0.006996416  0.02018672  0.004363181  0.61151745 -0.002598492
5  -0.67583468 0.02695808  0.01148732 0.07064992  0.013192868  0.006996416  0.02018672  0.004363181  0.01564351 -0.002598492
6   0.49699333 0.02695808  0.01148732 0.40345620  0.013192868  0.006996416  0.02018672  0.004363181  0.01564351 -0.772058203
7   0.28952709 0.02695808  0.01148732 0.02395001  0.013192868  0.006996416  0.02018672  0.004363181  0.01564351 -0.002598492
8  -0.26953403 0.36257682 -0.05794213 0.27298882  0.616542940 -0.162649142  0.02018672  0.004363181 -0.93950022  0.250349094
9   0.03666236 0.02695808  0.01148732 0.45080685  0.013192868  0.006996416  0.02018672  0.004363181  0.01564351 -0.002598492
10  0.42626128 0.02695808  0.01148732 0.07064992  0.013192868  0.006996416 -1.07596652  0.004363181  0.01564351 -0.002598492

Gr10[1:10, 1:10]
            1          2            3           4           5          6            7           8            9          10
1   0.4621794  0.1914313  -0.09724537 -0.02891175 -0.01669175  0.8329781   -0.6920569  -0.1342458   -0.4312827   0.3637335
2   0.0000000  0.0000000   0.00000000  0.62753522  0.00000000  0.0000000    0.0000000   0.0000000    0.0000000   0.0000000
3  -0.7659126  0.0000000   0.00000000  0.00000000  0.00000000  0.0000000    0.2391727   0.0000000    0.0000000   0.0000000
4  -1.1414386  0.8654524  -1.07008872  0.24339700  1.01046489  0.0000000    0.0000000   0.0000000    0.5958739   0.0000000
5  -0.7124970  0.0000000   0.00000000  0.00000000  0.00000000  0.0000000    0.0000000   0.0000000    0.0000000   0.0000000
6   0.4603310  0.0000000   0.00000000  0.33280628  0.00000000  0.0000000    0.0000000   0.0000000    0.0000000  -0.7694597
7   0.2528647  0.0000000   0.00000000 -0.04669991  0.00000000  0.0000000    0.0000000   0.0000000    0.0000000   0.0000000
8  -0.3061964  0.3356187  -0.06942945  0.20233890  0.60335007 -0.1696456    0.0000000   0.0000000   -0.9551437   0.2529476
9   0.0000000  0.0000000   0.00000000  0.38015693  0.00000000  0.0000000    0.0000000   0.0000000    0.0000000   0.0000000
10  0.3895989  0.0000000   0.00000000  0.00000000  0.00000000  0.0000000   -1.0961532   0.0000000    0.0000000   0.0000000

dim(Gr10)
[1] 69878   762
# Out of the first 10 Principal Components, Gr10_PQ approximates Gr10 well with a couple of minor differences in PC4.


# Calculate residual matrix Gr10_p1q1 from product of the 1st principle components of p (pcaGr10$x) and q (pcaGr10$rotation):
Gr10_p1q1 <- pcaGr10$x[,1] %*% t(pcaGr10$rotation[,1])

dim(Gr10_p1q1)
[1] 69878   762

Gr10_p1q1[1:10, 1:8]
                  1            2             3             4             5             6             7             8
 [1,]  3.771568e-05  0.000136549  0.0001752539  0.0005125171  0.0001828464  8.997309e-05  0.0001841412 -1.678342e-06
 [2,]  2.457245e-03  0.008896415  0.0114181101  0.0333914225  0.0119127718  5.861910e-03  0.0119971318 -1.093470e-04
 [3,] -1.906884e-03 -0.006903843 -0.0088607423 -0.0259125887 -0.0092446123 -4.548990e-03 -0.0093100778  8.485604e-05
 [4,] -4.020360e-04 -0.001455565 -0.0018681456 -0.0054632542 -0.0019490784 -9.590817e-04 -0.0019628808  1.789054e-05
 [5,] -2.895337e-02 -0.104825179 -0.1345379464 -0.3934463208 -0.1403664735 -6.907004e-02 -0.1413604760  1.288420e-03
 [6,] -6.691831e-03 -0.024227665 -0.0310950127 -0.0909350756 -0.0324421279 -1.596378e-02 -0.0326718663  2.977854e-04
 [7,] -8.654320e-03 -0.031332823 -0.0402141323 -0.1176032696 -0.0419563111 -2.064541e-02 -0.0422534239  3.851158e-04
 [8,]  2.281360e-02  0.082596253  0.1060082170  0.3100132272  0.1106007634  5.442325e-02  0.1113839806 -1.015201e-03
 [9,]  1.629777e-03  0.005900581  0.0075731045  0.0221469867  0.0079011907  3.887934e-03  0.0079571428 -7.252481e-05
[10,]  1.246644e-02  0.045134537  0.0579279517  0.1694060306  0.0604375384  2.973946e-02  0.0608655256 -5.547545e-04

# Convert matrix Gr10_p1q1 to a data frame:
Gr10_p1q1_df <- as.data.frame(Gr10_p1q1)

Gr10_p1q1_df[1:10, 1:8]
               1            2             3             4             5             6             7             8
1   3.771568e-05  0.000136549  0.0001752539  0.0005125171  0.0001828464  8.997309e-05  0.0001841412 -1.678342e-06
2   2.457245e-03  0.008896415  0.0114181101  0.0333914225  0.0119127718  5.861910e-03  0.0119971318 -1.093470e-04
3  -1.906884e-03 -0.006903843 -0.0088607423 -0.0259125887 -0.0092446123 -4.548990e-03 -0.0093100778  8.485604e-05
4  -4.020360e-04 -0.001455565 -0.0018681456 -0.0054632542 -0.0019490784 -9.590817e-04 -0.0019628808  1.789054e-05
5  -2.895337e-02 -0.104825179 -0.1345379464 -0.3934463208 -0.1403664735 -6.907004e-02 -0.1413604760  1.288420e-03
6  -6.691831e-03 -0.024227665 -0.0310950127 -0.0909350756 -0.0324421279 -1.596378e-02 -0.0326718663  2.977854e-04
7  -8.654320e-03 -0.031332823 -0.0402141323 -0.1176032696 -0.0419563111 -2.064541e-02 -0.0422534239  3.851158e-04
8   2.281360e-02  0.082596253  0.1060082170  0.3100132272  0.1106007634  5.442325e-02  0.1113839806 -1.015201e-03
9   1.629777e-03  0.005900581  0.0075731045  0.0221469867  0.0079011907  3.887934e-03  0.0079571428 -7.252481e-05
10  1.246644e-02  0.045134537  0.0579279517  0.1694060306  0.0604375384  2.973946e-02  0.0608655256 -5.547545e-04

# Add userId column to Gr10_p1q1_df data frame and save as data frame object x:
x <- Gr10_p1q1_df %>% mutate(userId = rownames(Gr10))

x[1:10, 755:763]
             771           774           776           779           782           783           784           789 userId
1  -1.302132e-06  1.291021e-07 -8.507780e-07 -3.539761e-07 -1.533079e-07 -4.922756e-07 -4.885021e-07  1.720116e-07      1
2  -8.483623e-05  8.411239e-06 -5.542973e-05 -2.306219e-05 -9.988286e-06 -3.207265e-05 -3.182680e-05  1.120687e-05      2
3   6.583506e-05 -6.527334e-06  4.301488e-05  1.789684e-05  7.751163e-06  2.488918e-05  2.469840e-05 -8.696811e-06      3
4   1.388027e-05 -1.376184e-06  9.068998e-06  3.773262e-06  1.634209e-06  5.247486e-06  5.207263e-06 -1.833583e-06      4
5   9.996131e-04 -9.910842e-05  6.531206e-04  2.717384e-04  1.176905e-04  3.779074e-04  3.750106e-04 -1.320489e-04      5
6   2.310351e-04 -2.290638e-05  1.509522e-04  6.280541e-05  2.720116e-05  8.734364e-05  8.667413e-05 -3.051973e-05      6
7   2.987899e-04 -2.962405e-05  1.952213e-04  8.122412e-05  3.517835e-05  1.129586e-04  1.120927e-04 -3.947014e-05      7
8  -7.876380e-04  7.809177e-05 -5.146217e-04 -2.141144e-04 -9.273342e-05 -2.977694e-04 -2.954869e-04  1.040470e-04      8
9  -5.626795e-05  5.578786e-06 -3.676398e-05 -1.529608e-05 -6.624769e-06 -2.127230e-05 -2.110925e-05  7.432996e-06      9
10 -4.304030e-04  4.267307e-05 -2.812139e-04 -1.170023e-04 -5.067397e-05 -1.627154e-04 -1.614682e-04  5.685624e-05     10

# Create data frame object y with a 1st column of userIds from row names of matrix Gr10:
y <- data.frame(userId = rownames(Gr10))

head(y)
  userId
1      1
2      2
3      3
4      4
5      5
6      6

# Create wide data frame with userId from y as 1st column and join all columns from x by matching rows of userId:
Gr10_p1q1_dfw <- plyr::join(x, y, by = "userId", type = "right", match = "all")

Gr10_p1q1_dfw[1:10, 1:9]
   userId             1            2             3             4             5             6             7             8
1       1  3.771568e-05  0.000136549  0.0001752539  0.0005125171  0.0001828464  8.997309e-05  0.0001841412 -1.678342e-06
2       2  2.457245e-03  0.008896415  0.0114181101  0.0333914225  0.0119127718  5.861910e-03  0.0119971318 -1.093470e-04
3       3 -1.906884e-03 -0.006903843 -0.0088607423 -0.0259125887 -0.0092446123 -4.548990e-03 -0.0093100778  8.485604e-05
4       4 -4.020360e-04 -0.001455565 -0.0018681456 -0.0054632542 -0.0019490784 -9.590817e-04 -0.0019628808  1.789054e-05
5       5 -2.895337e-02 -0.104825179 -0.1345379464 -0.3934463208 -0.1403664735 -6.907004e-02 -0.1413604760  1.288420e-03
6       6 -6.691831e-03 -0.024227665 -0.0310950127 -0.0909350756 -0.0324421279 -1.596378e-02 -0.0326718663  2.977854e-04
7       7 -8.654320e-03 -0.031332823 -0.0402141323 -0.1176032696 -0.0419563111 -2.064541e-02 -0.0422534239  3.851158e-04
8       8  2.281360e-02  0.082596253  0.1060082170  0.3100132272  0.1106007634  5.442325e-02  0.1113839806 -1.015201e-03
9       9  1.629777e-03  0.005900581  0.0075731045  0.0221469867  0.0079011907  3.887934e-03  0.0079571428 -7.252481e-05
10     10  1.246644e-02  0.045134537  0.0579279517  0.1694060306  0.0604375384  2.973946e-02  0.0608655256 -5.547545e-04

Gr10_p1q1_dfw[1:10, 756:763]
             771           774           776           779           782           783           784           789
1  -1.302132e-06  1.291021e-07 -8.507780e-07 -3.539761e-07 -1.533079e-07 -4.922756e-07 -4.885021e-07  1.720116e-07
2  -8.483623e-05  8.411239e-06 -5.542973e-05 -2.306219e-05 -9.988286e-06 -3.207265e-05 -3.182680e-05  1.120687e-05
3   6.583506e-05 -6.527334e-06  4.301488e-05  1.789684e-05  7.751163e-06  2.488918e-05  2.469840e-05 -8.696811e-06
4   1.388027e-05 -1.376184e-06  9.068998e-06  3.773262e-06  1.634209e-06  5.247486e-06  5.207263e-06 -1.833583e-06
5   9.996131e-04 -9.910842e-05  6.531206e-04  2.717384e-04  1.176905e-04  3.779074e-04  3.750106e-04 -1.320489e-04
6   2.310351e-04 -2.290638e-05  1.509522e-04  6.280541e-05  2.720116e-05  8.734364e-05  8.667413e-05 -3.051973e-05
7   2.987899e-04 -2.962405e-05  1.952213e-04  8.122412e-05  3.517835e-05  1.129586e-04  1.120927e-04 -3.947014e-05
8  -7.876380e-04  7.809177e-05 -5.146217e-04 -2.141144e-04 -9.273342e-05 -2.977694e-04 -2.954869e-04  1.040470e-04
9  -5.626795e-05  5.578786e-06 -3.676398e-05 -1.529608e-05 -6.624769e-06 -2.127230e-05 -2.110925e-05  7.432996e-06
10 -4.304030e-04  4.267307e-05 -2.812139e-04 -1.170023e-04 -5.067397e-05 -1.627154e-04 -1.614682e-04  5.685624e-05


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(x, y, Sigma, gr_10, edx_r, edx_r_10, Gr10_PQ, Gr10_p1q1)

# Create a long data frame Gr10_p1q1_dfl from the wide data frame to get all rows into the edx data frame order and format:
Gr10_p1q1_dfl <- pivot_longer(data = Gr10_p1q1_dfw, cols = 2:763, names_to = "genreId", values_to = "p1q1")

head(Gr10_p1q1_dfl)
# A tibble: 6 x 3
  userId genreId      p1q1
  <chr>  <chr>       <dbl>
1 1      1       0.0000377
2 1      2       0.000137 
3 1      3       0.000175 
4 1      4       0.000513 
5 1      5       0.000183 
6 1      6       0.0000900


# Convert userId and genreId columns from <chr> to <int> values in the long data frame:
Gr10_p1q1_dfl <- Gr10_p1q1_dfl %>% mutate(userId = as.integer(userId), genreId = as.integer(genreId))

Gr10_p1q1_dfl
# A tibble: 53,247,036 x 3
   userId genreId        p1q1
    <int>   <int>       <dbl>
 1      1       1  0.0000377 
 2      1       2  0.000137  
 3      1       3  0.000175  
 4      1       4  0.000513  
 5      1       5  0.000183  
 6      1       6  0.0000900 
 7      1       7  0.000184  
 8      1       8 -0.00000168
 9      1       9  0.000103  
10      1      10  0.000167  
# ... with 53,247,026 more rows

# Remove large data files that are no longer needed to free up memory for further calculations:
rm(Gr10_p1q1_dfw, Gr10_p1q1_df, Gr10)

# p1q1_U.M_model adds the corresponding p1q1 genre factor, user bias, and movie bias to the mean rating of all movies
# (model built in stages to avoid crashing R/RStudio):

p1q1_U.M_partA <- edx %>% left_join(user_bias, by = 'userId') %>% left_join(movie_bias, by = 'movieId') %>% 
                          select(-rating, -timestamp, -title)

head(p1q1_U.M_partA)
   userId movieId                        genres genreId  b_u_hat     b_i_hat
1:      1     122                Comedy|Romance       1 1.679235 -0.65387934
2:      1     185         Action|Crime|Thriller       2 1.679235 -0.38313118
3:      1     292  Action|Drama|Sci-Fi|Thriller       3 1.679235 -0.09445454
4:      1     316       Action|Adventure|Sci-Fi       4 1.679235 -0.16278816
5:      1     329 Action|Adventure|Drama|Sci-Fi       5 1.679235 -0.17500816
6:      1     355       Children|Comedy|Fantasy       6 1.679235 -1.02467799


p1q1_U.M_partB <- p1q1_U.M_partA %>% left_join(Gr10_p1q1_dfl, by = c('userId', 'genreId'))

p1q1_U.M_partB
         userId movieId                        genres genreId      b_u_hat     b_i_hat          p1q1
      1:      1     122                Comedy|Romance       1  1.679234705 -0.65387934  3.771568e-05
      2:      1     185         Action|Crime|Thriller       2  1.679234705 -0.38313118  1.365490e-04
      3:      1     292  Action|Drama|Sci-Fi|Thriller       3  1.679234705 -0.09445454  1.752539e-04
      4:      1     316       Action|Adventure|Sci-Fi       4  1.679234705 -0.16278816  5.125171e-04
      5:      1     329 Action|Adventure|Drama|Sci-Fi       5  1.679234705 -0.17500816  1.828464e-04
---                                                                                            
9000051:  32620   33140               Children|Comedy      61  0.455777291 -0.01246520 -1.384048e-01
9000052:  40976   61913                   Documentary     128 -0.185417866 -0.51246520  6.874807e-02
9000053:  59269   63141        Comedy|Musical|Western     597 -0.004039963 -1.51246520  3.510948e-04
9000054:  60713    4820                   Documentary     128 -0.468220844 -1.51246520  4.846595e-02
9000055:  64621   39429                Drama|Thriller      49  0.030529561 -1.01246520 -5.988840e-04


# Remove large data files that are no longer needed to free up memory for further calculations:
rm(Gr10_p1q1_dfl, p1q1_U.M_partA)

# 172 observations with NA due to 35 genres with 9 or fewer ratings:
sum(is.na(p1q1_U.M_partB$p1q1) == TRUE)
[1] 172

sum(is.na(p1q1_U.M_partB$p1q1) == FALSE)
[1] 8999883

172 + 8999883 == 9000055
[1] TRUE

# Replace NA with zero (0) in the 172 observations of p1q1_U.M_partB$p1q1, and verify that no NA remains:
p1q1_U.M_partB$p1q1[is.na(p1q1_U.M_partB$p1q1)] <- 0

sum(is.na(p1q1_U.M_partB$p1q1) == TRUE)
[1] 0

sum(is.na(p1q1_U.M_partB$p1q1) == FALSE)
[1] 9000055


P1Q1 <- p1q1_U.M_partB %>% select(userId, genreId, p1q1)

P1Q1
         userId genreId          p1q1
      1:      1       1  3.771568e-05
      2:      1       2  1.365490e-04
      3:      1       3  1.752539e-04
      4:      1       4  5.125171e-04
      5:      1       5  1.828464e-04
---                             
9000051:  32620      61 -1.384048e-01
9000052:  40976     128  6.874807e-02
9000053:  59269     597  3.510948e-04
9000054:  60713     128  4.846595e-02
9000055:  64621      49 -5.988840e-04


P1Q1UM_model <- p1q1_U.M_partB %>% mutate(p1q1um_hat = mu_hat + b_u_hat + b_i_hat + p1q1) %>% 
  pull(p1q1um_hat)

P1Q1UM_model[1:10]
[1] 4.537858 4.808705 5.097421 5.029424 5.016875 4.167112 5.692241 5.134244 5.431385 4.636433

length(P1Q1UM_model)
[1] 9000055

# RMSE of the P1Q1UM_model where p1q1um_hat = mu_hat + b_u_hat + b_i_hat + p1q1:
P1Q1UM_rmse <- RMSE(edx$rating, P1Q1UM_model)

P1Q1UM_rmse
[1] 0.8493028

# add RMSE result of P1Q1UM_model to the RMSE table to compare the different models:
rmse_results <- rmse_results %>% add_row(method = "P1Q1 + U+M model", RMSE = P1Q1UM_rmse)

rmse_results
# A tibble: 4 x 2
  method            RMSE
  <chr>            <dbl>
1 naive mean model 1.06 
2 movie bias model 0.942
3 U + M bias model 0.857
4 P1Q1 + U+M model 0.849


# TEST OF THE USER BIAS + MOVIE BIAS + P1Q1 MODEL ON THE VALIDATION DATA SET: ----
P1Q1UM_model_test_partA <- validation %>% select(userId, movieId, genreId) %>%
  left_join(user_bias, by='userId') %>% left_join(movie_bias, by='movieId')

P1Q1UM_model_test_partA
        userId movieId genreId     b_u_hat     b_i_hat
     1:      1     231      14  1.67923471 -0.57734404
     2:      1     480      31  1.67923471  0.15105660
     3:      1     586      61  1.67923471 -0.45681303
     4:      2     151      33 -0.23640856  0.01759325
     5:      2     858      66 -0.23640856  0.90290078
---                                               
999995:  71566     235      43  0.25943697  0.15100737
999996:  71566     273      55  0.25943697 -0.36758474
999997:  71566     434      24  0.25943697 -0.43692406
999998:  71567     480      31 -0.07467827  0.15105660
999999:  71567     898       1 -0.07467827  0.70383574


P1Q1UM_model_test_partB <- plyr::join(P1Q1UM_model_test_partA, P1Q1, by = c('userId', 'genreId'), 
                                      type = "left", match = "first")

P1Q1UM_model_test_partB
        userId genreId movieId     b_u_hat     b_i_hat          p1q1
     1:      1      14     231  1.67923471 -0.57734404  6.135062e-06
     2:      1      31     480  1.67923471  0.15105660            NA
     3:      1      61     586  1.67923471 -0.45681303            NA
     4:      2      33     151 -0.23640856  0.01759325            NA
     5:      2      66     858 -0.23640856  0.90290078            NA
---                                                             
999995:  71566      43     235  0.25943697  0.15100737            NA
999996:  71566      55     273  0.25943697 -0.36758474  1.264813e-02
999997:  71566      24     434  0.25943697 -0.43692406 -5.281072e-02
999998:  71567      31     480 -0.07467827  0.15105660  1.777941e-01
999999:  71567       1     898 -0.07467827  0.70383574            NA


sum(is.na(P1Q1UM_model_test_partB$p1q1) == TRUE)
[1] 336086

sum(is.na(P1Q1UM_model_test_partB$p1q1) == FALSE)
[1] 663913

336086 + 663913 == 999999
[1] TRUE

# Replace NA with zero (0) in P1Q1UM_model_test_partB$p1q1 and verify that no NA remains:
P1Q1UM_model_test_partB$p1q1[is.na(P1Q1UM_model_test_partB$p1q1)] <- 0

P1Q1UM_model_test_partB
        userId genreId movieId     b_u_hat     b_i_hat          p1q1
     1:      1      14     231  1.67923471 -0.57734404  6.135062e-06
     2:      1      31     480  1.67923471  0.15105660  0.000000e+00
     3:      1      61     586  1.67923471 -0.45681303  0.000000e+00
     4:      2      33     151 -0.23640856  0.01759325  0.000000e+00
     5:      2      66     858 -0.23640856  0.90290078  0.000000e+00
---                                                             
999995:  71566      43     235  0.25943697  0.15100737  0.000000e+00
999996:  71566      55     273  0.25943697 -0.36758474  1.264813e-02
999997:  71566      24     434  0.25943697 -0.43692406 -5.281072e-02
999998:  71567      31     480 -0.07467827  0.15105660  1.777941e-01
999999:  71567       1     898 -0.07467827  0.70383574  0.000000e+00

sum(is.na(P1Q1UM_model_test_partB$p1q1) == TRUE)
[1] 0

sum(is.na(P1Q1UM_model_test_partB$p1q1) == FALSE)
[1] 999999

rm(P1Q1UM_model_test_partA)

P1Q1UM_model_test <- P1Q1UM_model_test_partB %>% 
  mutate(p1q1um_hat = mu_hat + b_u_hat + b_i_hat + p1q1) %>% 
  pull(p1q1um_hat)

P1Q1UM_model_test[1:10]
[1] 4.614362 5.342757 4.734887 3.293650 4.178957 2.708870 4.006959 4.170450 4.357732 3.386527

length(P1Q1UM_model_test)
[1] 999999

P1Q1UM_model_test_rmse <- RMSE(validation$rating, P1Q1UM_model_test)
P1Q1UM_model_test_rmse
[1] 0.8630654


rm(p1q1_U.M_partB, P1Q1UM_model, P1Q1UM_model_test, P1Q1UM_model_test_partB)

