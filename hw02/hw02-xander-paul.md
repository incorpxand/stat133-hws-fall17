HW 02 - Basics of Data Frames
================
Xander Paul

``` r
chooseCRANmirror(ind = 50)
require(readr)
```

    ## Loading required package: readr

    ## Warning: package 'readr' was built under R version 3.4.2

``` r
install.packages("readr")
```

    ## Installing package into 'C:/Users/Xander/Documents/R/win-library/3.4'
    ## (as 'lib' is unspecified)

    ## Warning: package 'readr' is in use and will not be installed

``` r
library(readr)
install.packages('ggplot2')
```

    ## Installing package into 'C:/Users/Xander/Documents/R/win-library/3.4'
    ## (as 'lib' is unspecified)

    ## package 'ggplot2' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\Xander\AppData\Local\Temp\Rtmpwvf5cy\downloaded_packages

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.2

``` r
install.packages('dplyr')
```

    ## Installing package into 'C:/Users/Xander/Documents/R/win-library/3.4'
    ## (as 'lib' is unspecified)

    ## package 'dplyr' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\Xander\AppData\Local\Temp\Rtmpwvf5cy\downloaded_packages

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
github <- "https://github.com/ucb-stat133/stat133-fall-2017/raw/master/"
file <- "data/nba2017-player-statistics.csv"
csv <- paste0(github, file)
download.file(url = csv, destfile = 'nba2017-player-statistics.csv')
```

``` r
base <- read.csv("nba2017-player-statistics.csv", header = TRUE, colClasses = c('character', 'character', 'factor', 'character', 'double', rep('integer', 19) ))
str(base)
```

    ## 'data.frame':    441 obs. of  24 variables:
    ##  $ Player      : chr  "Al Horford" "Amir Johnson" "Avery Bradley" "Demetrius Jackson" ...
    ##  $ Team        : chr  "BOS" "BOS" "BOS" "BOS" ...
    ##  $ Position    : Factor w/ 5 levels "C","PF","PG",..: 1 2 5 3 4 3 4 5 4 2 ...
    ##  $ Experience  : chr  "9" "11" "6" "R" ...
    ##  $ Salary      : num  26540100 12000000 8269663 1450000 1410598 ...
    ##  $ Rank        : int  4 6 5 15 11 1 3 13 8 10 ...
    ##  $ Age         : int  30 29 26 22 31 27 26 21 20 29 ...
    ##  $ GP          : int  68 80 55 5 47 76 72 29 78 78 ...
    ##  $ GS          : int  68 77 55 0 0 76 72 0 20 6 ...
    ##  $ MIN         : int  2193 1608 1835 17 538 2569 2335 220 1341 1232 ...
    ##  $ FGM         : int  379 213 359 3 95 682 333 25 192 114 ...
    ##  $ FGA         : int  801 370 775 4 232 1473 720 58 423 262 ...
    ##  $ Points3     : int  86 27 108 1 39 245 157 12 46 45 ...
    ##  $ Points3_atts: int  242 66 277 1 111 646 394 35 135 130 ...
    ##  $ Points2     : int  293 186 251 2 56 437 176 13 146 69 ...
    ##  $ Points2_atts: int  559 304 498 3 121 827 326 23 288 132 ...
    ##  $ FTM         : int  108 67 68 3 33 590 176 6 85 26 ...
    ##  $ FTA         : int  135 100 93 6 41 649 217 9 124 37 ...
    ##  $ OREB        : int  95 117 65 2 17 43 48 6 45 60 ...
    ##  $ DREB        : int  369 248 269 2 68 162 367 20 175 213 ...
    ##  $ AST         : int  337 140 121 3 33 449 155 4 64 71 ...
    ##  $ STL         : int  52 52 68 0 9 70 72 10 35 26 ...
    ##  $ BLK         : int  87 62 11 0 7 13 23 2 18 17 ...
    ##  $ TO          : int  116 77 88 0 25 210 79 4 68 39 ...

``` r
readr <- read_csv("nba2017-player-statistics.csv", col_types = list(Player = 'c', Team = 'c', Position = col_factor(c('C', 'PF', 'PG', 'SF', 'SG')), Experience = 'c', Salary = 'n', Rank = 'i', Age = 'i', GP = 'i', GS = 'i', MIN = 'i', FGM = 'i', FGA = 'i', Points3 = 'i', Points3_atts = 'i', Points2 = 'i', Points2_atts = 'i', FTM = 'i', FTA = 'i', OREB = 'i', DREB = 'i', AST = 'i', STL = 'i', BLK = 'i', TO = 'i'))
str(readr)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    441 obs. of  24 variables:
    ##  $ Player      : chr  "Al Horford" "Amir Johnson" "Avery Bradley" "Demetrius Jackson" ...
    ##  $ Team        : chr  "BOS" "BOS" "BOS" "BOS" ...
    ##  $ Position    : Factor w/ 5 levels "C","PF","PG",..: 1 2 5 3 4 3 4 5 4 2 ...
    ##  $ Experience  : chr  "9" "11" "6" "R" ...
    ##  $ Salary      : num  2.65e+07 1.20 8.27e+06 1.45e+06 1.41e+06 ...
    ##  $ Rank        : int  4 6 5 15 11 1 3 13 8 10 ...
    ##  $ Age         : int  30 29 26 22 31 27 26 21 20 29 ...
    ##  $ GP          : int  68 80 55 5 47 76 72 29 78 78 ...
    ##  $ GS          : int  68 77 55 0 0 76 72 0 20 6 ...
    ##  $ MIN         : int  2193 1608 1835 17 538 2569 2335 220 1341 1232 ...
    ##  $ FGM         : int  379 213 359 3 95 682 333 25 192 114 ...
    ##  $ FGA         : int  801 370 775 4 232 1473 720 58 423 262 ...
    ##  $ Points3     : int  86 27 108 1 39 245 157 12 46 45 ...
    ##  $ Points3_atts: int  242 66 277 1 111 646 394 35 135 130 ...
    ##  $ Points2     : int  293 186 251 2 56 437 176 13 146 69 ...
    ##  $ Points2_atts: int  559 304 498 3 121 827 326 23 288 132 ...
    ##  $ FTM         : int  108 67 68 3 33 590 176 6 85 26 ...
    ##  $ FTA         : int  135 100 93 6 41 649 217 9 124 37 ...
    ##  $ OREB        : int  95 117 65 2 17 43 48 6 45 60 ...
    ##  $ DREB        : int  369 248 269 2 68 162 367 20 175 213 ...
    ##  $ AST         : int  337 140 121 3 33 449 155 4 64 71 ...
    ##  $ STL         : int  52 52 68 0 9 70 72 10 35 26 ...
    ##  $ BLK         : int  87 62 11 0 7 13 23 2 18 17 ...
    ##  $ TO          : int  116 77 88 0 25 210 79 4 68 39 ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 24
    ##   .. ..$ Player      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Team        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Position    :List of 3
    ##   .. .. ..$ levels    : chr  "C" "PF" "PG" "SF" ...
    ##   .. .. ..$ ordered   : logi FALSE
    ##   .. .. ..$ include_na: logi FALSE
    ##   .. .. ..- attr(*, "class")= chr  "collector_factor" "collector"
    ##   .. ..$ Experience  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ Salary      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_number" "collector"
    ##   .. ..$ Rank        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Age         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ GP          : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ GS          : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ MIN         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ FGM         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ FGA         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Points3     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Points3_atts: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Points2     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ Points2_atts: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ FTM         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ FTA         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ OREB        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ DREB        : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ AST         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ STL         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ BLK         : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ TO          : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

``` r
experience <- replace(base$experience, base$experience=="R", 0)
experience <- as.integer(base$experience)
experience
```

    ## integer(0)

``` r
base$Experience[base$Experience=='R'] <- 0
readr$Experience[readr$Experience=='R'] <- 0
base$Experience <- as.integer(base$Experience)
readr$Experience <- as.integer(readr$Experience)
```

``` r
Missed_FG <- base$FGA - base$FGM
base <- mutate(base, Missed_FG = Missed_FG)
Missed_FT <- base$FTA - base$FTM
base <- mutate(base, Missed_FT = Missed_FT)
pts <- 2 * base$Points2 + 3 * base$Points3 + base$FTM
base <- mutate(base, PTS = pts)
reb <- base$OREB + base$DREB
base <- mutate(base, REB = reb)
mpg <- base$MIN / base$GP
base <- mutate(base, MPG = mpg)
eff <- (pts + reb + base$AST + base$STL + base$BLK - Missed_FG - Missed_FT - base$TO) / base$GP
base <- mutate(base, EFF = eff)
summary(eff)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -0.600   5.452   9.090  10.137  13.247  33.840

``` r
ggplot(data = base, aes(x = EFF)) + geom_histogram(binwidth = 5)
```

![](hw02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

``` r
base_some <- select(base, Player, Team, Salary, EFF)
base_some <- arrange(base_some, desc(EFF))
slice(base_some, 1:10)
```

    ## # A tibble: 10 x 4
    ##                   Player  Team   Salary      EFF
    ##                    <chr> <chr>    <dbl>    <dbl>
    ##  1     Russell Westbrook   OKC 26540100 33.83951
    ##  2          James Harden   HOU 26540100 32.34568
    ##  3         Anthony Davis   NOP 22116750 31.16000
    ##  4          LeBron James   CLE 30963450 30.97297
    ##  5    Karl-Anthony Towns   MIN  5960160 30.32927
    ##  6          Kevin Durant   GSW 26540100 30.19355
    ##  7 Giannis Antetokounmpo   MIL  2995421 28.37500
    ##  8      DeMarcus Cousins   NOP 16957900 27.94118
    ##  9          Jimmy Butler   CHI 17552209 25.60526
    ## 10      Hassan Whiteside   MIA 22116750 25.36364

``` r
low_guys <- filter(base, EFF < 0)
select(low_guys, Player)
```

    ##            Player
    ## 1 Patricio Garino

``` r
cor(eff, base[c('PTS', 'REB', 'AST', 'STL', 'BLK', 'Missed_FG', 'Missed_FT', 'TO')])
```

    ##            PTS       REB       AST       STL       BLK Missed_FG Missed_FT
    ## [1,] 0.8588644 0.7634501 0.6689232 0.6957286 0.5679571 0.7722477 0.7271456
    ##             TO
    ## [1,] 0.8003289

``` r
cor_vector <- c(0.8588644, 0.7634501, 0.6957286, 0.6689232, 0.5679571, -0.7271456, -0.7722477, -0.8003289)
barplot(cor_vector, main = "Correlations between Player Stats and EFF", names.arg = c('PTS', 'REB', 'STL', 'AST', 'BLK', 'Missed_FT', 'Missed_FG', 'TO'), cex.names = 0.6)
```

![](hw02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

``` r
ggplot(data = base, aes(x = EFF, y = Salary)) + geom_point() + geom_smooth(method = 'loess')
```

![](hw02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

``` r
cor(eff, base[c('Salary')])
```

    ##        Salary
    ## [1,] 0.655624

``` r
players2 <- filter(base, MPG > 20)
ggplot(data = players2, aes(x = EFF, y = Salary)) + geom_point() + geom_smooth(method = 'loess')
```

![](hw02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)

``` r
cor(players2[c('EFF')], players2[c('Salary')])
```

    ##        Salary
    ## EFF 0.5367224

Between more established players, EFF is not as strong a predictor of salary. That is, EFF is more important for determining the salary of a rookie.

Comments
========

Importing the csv file was difficult and required a lot of internet searching. Downloading the packages and installing them proved to be a headache as well.

I looked up many different R guides on the internet to help me with this assignment. The homework took me about 3 hours to complete. The most time consuming part was ensuring that the packages installed correctly, and this was highly frustrating.
