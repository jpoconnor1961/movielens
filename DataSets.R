##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

library(tidyverse)
library(caret)
library(data.table)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),   # gsub replaces :: with a tab
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)   # split at :: and return 3 pieces
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
dim(temp)
[1] 1000007       6

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
dim(validation)
[1] 999999      6

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)

removed
   userId movieId rating  timestamp                         title                 genres
1:  16929   39412    3.0 1221160134    Living 'til the End (2005)                  Drama
2:  20306   63826    4.0 1228431590               Splinter (2008) Action|Horror|Thriller
3:  30445    8394    0.5 1200074027           Hi-Line, The (1999)                  Drama
4:  32620   33140    3.5 1173562747         Down and Derby (2005)        Children|Comedy
5:  40976   61913    3.0 1227767528           Africa addio (1966)            Documentary
6:  59269   63141    2.0 1226443318 Rockin' in the Rockies (1945) Comedy|Musical|Western
7:  60713    4820    2.0 1119156754  Won't Anybody Listen? (2000)            Documentary
8:  64621   39429    2.5 1201248182                Confess (2005)         Drama|Thriller   '

edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

str(edx)
Classes ‘data.table’ and 'data.frame':	9000055 obs. of  6 variables:
$ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
$ movieId  : num  122 185 292 316 329 355 356 362 364 370 ...
$ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
$ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
$ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
$ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...
- attr(*, ".internal.selfref")=<externalptr>

summary(edx)
    userId         movieId          rating        timestamp            title              genres         
Min.   :    1   Min.   :    1   Min.   :0.500   Min.   :7.897e+08   Length:9000055     Length:9000055    
1st Qu.:18124   1st Qu.:  648   1st Qu.:3.000   1st Qu.:9.468e+08   Class :character   Class :character  
Median :35738   Median : 1834   Median :4.000   Median :1.035e+09   Mode  :character   Mode  :character  
Mean   :35870   Mean   : 4122   Mean   :3.512   Mean   :1.033e+09                                        
3rd Qu.:53607   3rd Qu.: 3626   3rd Qu.:4.000   3rd Qu.:1.127e+09                                        
Max.   :71567   Max.   :65133   Max.   :5.000   Max.   :1.231e+09

length(unique(edx$movieId))
[1] 10677

length(unique(edx$title))
[1] 10676

length(unique(edx$genres))
[1] 797

length(unique(edx$userId))
[1] 69878

length(unique(validation$movieId))
[1] 9809

length(unique(validation$title))
[1] 9808

length(unique(validation$genres))
[1] 773

length(unique(validation$userId))
68534

# edx_test set will be 10% of edx data
# edx_train set will be 90% of edx data
set.seed(1, sample.kind="Rounding")
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-edx_test_index,]
edx_temp <- edx[edx_test_index,]

dim(edx_train)
[1] 8100048       7

dim(edx_temp)
[1] [1] 900007      7

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- edx_temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

dim(edx_test)
[1] 899990      7

# Add rows removed from edx_test set back into edx_train set
edx_removed <- anti_join(edx_temp, edx_test)

edx_removed
    userId movieId rating  timestamp                                                    title              genres genreId
 1:   7304   38435    3.5 1230589804                              Forty Shades of Blue (2005)               Drama      34
 2:  11680    6634    2.5 1060853936          Rowing with the Wind (Remando al viento) (1988)       Drama|Romance      28
 3:  23140   39439    3.5 1159527126                            God s Sandbox (Tahara) (2002)               Drama      34
 4:  27006   63840    4.0 1227813019                                      Kanak Attack (2000)               Drama      34
 5:  30840   61862    2.5 1229365440                               In Bed (En la cama) (2005)        Comedy|Drama      43
 6:  36663   63806    3.5 1227794190                                  Ring of Darkness (2004)     Horror|Thriller      80
 7:  40215    4075    1.0 1176305292        Monkey s Tale, A (Les ChÃ¢teau des singes) (1999)  Animation|Children      19
 8:  47976    5676    2.5 1152215168                               Young Unknowns, The (2000)               Drama      34
 9:  48159   61279    4.0 1224073164                Variety Lights (Luci del varietÃ ) (1950)               Drama      34
10:  53315    6634    3.0 1110686356          Rowing with the Wind (Remando al viento) (1988)       Drama|Romance      28
11:  56915   60336    4.5 1222800223                          Bad Blood (Mauvais sang) (1986) Crime|Drama|Romance     284
12:  59269    3383    3.0 1106423259                                         Big Fella (1937)       Drama|Musical      70
13:  59269   64897    3.0 1230162557                                            Mr. Wu (1927)               Drama      34
14:  59342   61768    0.5 1230070861                                Accused (Anklaget) (2005)               Drama      34
15:  61632   50477    4.5 1227906880   Testament of Orpheus, The (Testament d OrphÃ©e) (1960)               Drama      34
16:  63134   54318    2.5 1222631928 Cruel Story of Youth (Seishun zankoku monogatari) (1960)               Drama      34
17:  67385    7537    2.5 1188277406                             Du cÃ´tÃ© de la cÃ´te (1958)         Documentary     128

edx_train <- rbind(edx_train, edx_removed)

dim(edx_train)
[1] 8100065       7

rm(edx_test_index, edx_temp, edx_removed)

str(edx_train)
Classes ‘data.table’ and 'data.frame':	8100065 obs. of  7 variables:
$ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
$ movieId  : num  122 292 316 329 355 356 362 364 370 377 ...
$ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
$ timestamp: int  838985046 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 838983834 ...
$ title    : chr  "Boomerang (1992)" "Outbreak (1995)" "Stargate (1994)" "Star Trek: Generations (1994)" ...
$ genres   : chr  "Comedy|Romance" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" "Action|Adventure|Drama|Sci-Fi" ...
$ genreId  : int  1 3 4 5 6 7 8 9 10 11 ...
- attr(*, ".internal.selfref")=<externalptr>

length(unique(edx_train$movieId))
[1] 10677

length(unique(edx_train$title))
[1] 10676

length(unique(edx_train$genres))
[1] 797

length(unique(edx_train$userId))
[1] 69878

str(edx_test)
Classes ‘data.table’ and 'data.frame':	899990 obs. of  7 variables:
$ userId   : int  1 2 2 2 2 3 3 3 3 4 ...
$ movieId  : num  185 260 590 1049 1210 ...
$ rating   : num  5 5 5 3 4 4 2 4.5 5 3 ...
$ timestamp: int  838983525 868244562 868245608 868245920 868245644 1133571121 1133571139 1136075915 1133571238 844417070 ...
$ title    : chr  "Net, The (1995)" "Star Wars: Episode IV - A New Hope (a.k.a. Star Wars) (1977)" "Dances with Wolves (1990)" "Ghost and the Darkness, The (1996)" ...
$ genres   : chr  "Action|Crime|Thriller" "Action|Adventure|Sci-Fi" "Adventure|Drama|Western" "Action|Adventure" ...
$ genreId  : int  2 4 22 29 4 35 24 28 46 60 ...
- attr(*, ".internal.selfref")=<externalptr>

length(unique(edx_test$movieId))
[1] 9701

length(unique(edx_test$title))
[1] 9700

length(unique(edx_test$genres))
[1] 764

length(unique(edx_test$userId))
[1] 68159



