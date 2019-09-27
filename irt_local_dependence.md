irt\_local\_dependence
================

load packages

``` r
library(tidyverse)
library(mirt)
```

simulate data with a dependency. we’ll have 10000 students and 10 items.
all items will have difficulty 0 and discrimination 1. the first 9 items
will be as usual. the order dependence comes from students who got the
9rd item correct get a 1-unit boost to their ability for the 10th item.

``` r
# simulate first 9 items
set.seed(1)
n_students <- 100000
dim <- 1
n_items <- 9

data <- 
    simdata(
        a = matrix(rep(1, n_items), nrow = n_items, ncol = dim),
        d = matrix(rep(0, n_items), ncol = 1),
        guess = rep(0, n_items),
        N = n_students,
        sigma = diag(dim),
        mu = 0,
        itemtype = rep("3PL", n_items),
        returnList = TRUE
    )

# clean up results
results <- 
    data$data %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(theta_1to9 = data$Theta[ , 1]) %>% 
    mutate(theta_10 = ifelse(item_9 == 1, theta_1to9 + 1, theta_1to9)) %>% 
    select(starts_with("theta"), everything())

# add in 10th item
set.seed(2)

results$item_10 <- 
    simdata(
        Theta = matrix(results$theta_10, nrow = n_students, ncol = dim),
        a = matrix(1, nrow = 1),
        d = matrix(0, ncol = 1),
        guess = 0,
        itemtype = "3PL",
        returnList = FALSE
    )[, 1]

# check it out
results %>% select(starts_with("theta"), item_9, item_10)
```

    ## # A tibble: 100,000 x 4
    ##    theta_1to9 theta_10 item_9 item_10
    ##         <dbl>    <dbl>  <dbl>   <dbl>
    ##  1     -0.626   -0.626      0       0
    ##  2      0.184    1.18       1       1
    ##  3     -0.836    0.164      1       1
    ##  4      1.60     1.60       0       0
    ##  5      0.330    0.330      0       1
    ##  6     -0.820   -0.820      0       1
    ##  7      0.487    1.49       1       0
    ##  8      0.738    1.74       1       1
    ##  9      0.576    1.58       1       1
    ## 10     -0.305    0.695      1       1
    ## # … with 99,990 more rows

first, let’s verify that a model fit just to the first 9 items is as
expected

``` r
model_first_9 <- mirt(results %>% select(item_1:item_9), 1, "2PL", verbose = FALSE)

# indeed, parameter recovery is good
coef(model_first_9, simplify = TRUE)$items %>% 
    as_tibble() %>% 
    select(disc = 1, easy = 2)
```

    ## # A tibble: 9 x 2
    ##    disc        easy
    ##   <dbl>       <dbl>
    ## 1 0.995 -0.00276   
    ## 2 1.01  -0.000256  
    ## 3 1.00  -0.0168    
    ## 4 1.01  -0.00263   
    ## 5 0.991 -0.00590   
    ## 6 1.03  -0.00436   
    ## 7 0.979 -0.00000833
    ## 8 0.998  0.00163   
    ## 9 1.01   0.00144

``` r
# indeed, we fail to reject goodness of fit
M2(model_first_9)
```

    ##             M2 df         p RMSEA RMSEA_5    RMSEA_95       SRMSR      TLI
    ## stats 21.62436 27 0.7562121     0       0 0.001786952 0.002074968 1.000066
    ##       CFI
    ## stats   1

second, let’s see that a model fit to all items has some quirks

``` r
model_all <- mirt(results %>% select(item_1:item_10), 1, "2PL", verbose = FALSE)

# as makes sense, the discrimination of 9 and 10 are boosted and the 10th item is easier 
coef(model_all, simplify = TRUE)$items %>% 
    as_tibble() %>% 
    select(disc = 1, easy = 2)
```

    ## # A tibble: 10 x 2
    ##     disc       easy
    ##    <dbl>      <dbl>
    ##  1 0.963 -0.00276  
    ##  2 0.980 -0.000276 
    ##  3 0.976 -0.0167   
    ##  4 0.978 -0.00263  
    ##  5 0.967 -0.00588  
    ##  6 0.997 -0.00434  
    ##  7 0.954 -0.0000299
    ##  8 0.969  0.00160  
    ##  9 1.23   0.00147  
    ## 10 1.42   0.518

``` r
# indeed, we reject goodness of fit
M2(model_all)
```

    ##             M2 df p      RMSEA    RMSEA_5   RMSEA_95      SRMSR       TLI
    ## stats 3231.093 35 0 0.03021884 0.02934182 0.03110491 0.02142353 0.9729678
    ##             CFI
    ## stats 0.9789749
