Visualization
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library (ggridges)
```

\##Load the weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = case_match(
      id, 
      "USW00094728" ~ "CentralPark_NY", 
      "USW00022534" ~ "Molokai_HI",
      "USS0023B17S" ~ "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 20:34:18.576194 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 20:34:36.21331 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/Tammy/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 20:34:42.357773 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

``` r
weather_df
```

    ## # A tibble: 2,190 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2021-01-01   157   4.4   0.6
    ##  2 CentralPark_NY USW00094728 2021-01-02    13  10.6   2.2
    ##  3 CentralPark_NY USW00094728 2021-01-03    56   3.3   1.1
    ##  4 CentralPark_NY USW00094728 2021-01-04     5   6.1   1.7
    ##  5 CentralPark_NY USW00094728 2021-01-05     0   5.6   2.2
    ##  6 CentralPark_NY USW00094728 2021-01-06     0   5     1.1
    ##  7 CentralPark_NY USW00094728 2021-01-07     0   5    -1  
    ##  8 CentralPark_NY USW00094728 2021-01-08     0   2.8  -2.7
    ##  9 CentralPark_NY USW00094728 2021-01-09     0   2.8  -4.3
    ## 10 CentralPark_NY USW00094728 2021-01-10     0   5    -1.6
    ## # ℹ 2,180 more rows

\##Scatterplots Create my first scatterplot ever.

``` r
ggplot(weather_df,aes(x=tmin, y=tmax)) + 
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

New approach, same plot.

``` r
weather_df %>% 
  ggplot(aes(x=tmin, y=tmax)) +
  geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Save and edit plot object.

``` r
weather_plot =
  weather_df %>% 
  ggplot(aes(x=tmin, y=tmax))

weather_plot + geom_point()
```

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Advanced scatterplot

Start with same one and make it fancy

``` r
weather_df %>% 
  ggplot(aes(x=tmin, y=tmax, color=name)) +
  geom_point() + 
  geom_smooth(se=FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

What about the ‘aes’ placement

``` r
weather_df %>% 
  ggplot(aes(x=tmin, y=tmax)) +
  geom_point(aes(color=name)) +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Let’s facet some things

``` r
weather_df %>% 
  ggplot(aes(x=tmin, y=tmax,color=name)) +
  geom_point(alpha= 0.2) + 
  geom_smooth(se=FALSE) +
  facet_grid(.~ name)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Let’s combine some elements and try a new plot

``` r
weather_df %>% 
  ggplot(aes(x=date, y=tmax, color=name)) +
  geom_point(aes(size=prcp),alpha = 0.5)+
  geom_smooth(se=FALSE) +
  facet_grid(.~name)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 19 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Some small notes

Home many geoms have to exist? You can have whatever geoms you want

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax, color=name)) +
  geom_smooth(se=FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

![](viz_i_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

You can use a neat geom!

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax)) +
  geom_hex()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_binhex()`).

![](viz_i_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax)) +
  geom_density2d()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density2d()`).

![](viz_i_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x=tmin,y=tmax)) +
    geom_density2d()+
  geom_point(alpha= 0.3)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density2d()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](viz_i_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Univariate plots

Histograms are really great

``` r
weather_df %>% 
  ggplot(aes(x=tmin)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](viz_i_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Can we add color…

``` r
weather_df %>% 
  ggplot(aes(x=tmin, fill=name)) +
  geom_histogram(position = "dodge") +
  facet_grid(.~name)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](viz_i_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Let’s try a new geometry

``` r
weather_df %>% 
  ggplot(aes(x=tmin,fill=name)) +
  geom_density(alpha=0.3)
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](viz_i_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

What about box plots

``` r
weather_df %>% 
  ggplot(aes(x=name, y=tmin,)) +
  geom_boxplot()
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

![](viz_i_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Trendy plots

``` r
weather_df %>% 
  ggplot(aes(x=name, y=tmin, fill=name)) +
  geom_violin(alpha =0.5) +
  stat_summary(fun = "median")
```

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_ydensity()`).

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_summary()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_segment()`).

![](viz_i_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Ridge plots… the most popular plot of 2017

``` r
weather_df %>% 
  ggplot(aes(x=tmin, y=name)) +
  geom_density_ridges()
```

    ## Picking joint bandwidth of 1.41

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_density_ridges()`).

![](viz_i_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
