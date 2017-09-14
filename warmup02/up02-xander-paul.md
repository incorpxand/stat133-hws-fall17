Warmup 2
================
Xander Paul
September 14, 2017

``` r
load("nba2017-salary-points.RData")
ls()
```

    ## [1] "player"   "points"   "points1"  "points2"  "points3"  "position"
    ## [7] "salary"   "team"

``` r
class(salary)
```

    ## [1] "numeric"

``` r
class(team)
```

    ## [1] "factor"

``` r
length(team)
```

    ## [1] 441

``` r
typeof(team)
```

    ## [1] "integer"

``` r
summary(points)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0   156.0   432.0   546.6   780.0  2558.0

``` r
mean(points)
```

    ## [1] 546.6054

``` r
sd(points)
```

    ## [1] 489.0156

``` r
min(points)
```

    ## [1] 0

``` r
max(points)
```

    ## [1] 2558

``` r
median(points)
```

    ## [1] 432

``` r
mode(points)
```

    ## [1] "numeric"

``` r
range(points)
```

    ## [1]    0 2558

``` r
hist(points)
```

![](up02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

``` r
boxplot(points)
```

![](up02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-2.png)

``` r
hist(points)
```

![](up02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-3.png)

``` r
table(team)
```

    ## team
    ## ATL BOS BRK CHI CHO CLE DAL DEN DET GSW HOU IND LAC LAL MEM MIA MIL MIN 
    ##  14  15  15  15  15  15  15  15  15  15  14  14  15  15  15  14  14  14 
    ## NOP NYK OKC ORL PHI PHO POR SAC SAS TOR UTA WAS 
    ##  14  15  15  15  15  15  14  15  15  15  15  14

``` r
props <- prop.table(table(team))
props
```

    ## team
    ##        ATL        BOS        BRK        CHI        CHO        CLE 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03401361 
    ##        DAL        DEN        DET        GSW        HOU        IND 
    ## 0.03401361 0.03401361 0.03401361 0.03401361 0.03174603 0.03174603 
    ##        LAC        LAL        MEM        MIA        MIL        MIN 
    ## 0.03401361 0.03401361 0.03401361 0.03174603 0.03174603 0.03174603 
    ##        NOP        NYK        OKC        ORL        PHI        PHO 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03401361 
    ##        POR        SAC        SAS        TOR        UTA        WAS 
    ## 0.03174603 0.03401361 0.03401361 0.03401361 0.03401361 0.03174603

``` r
barplot(props)
```

![](up02-xander-paul_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
summary(props)
```

    ## Number of cases in table: 1 
    ## Number of factors: 1
