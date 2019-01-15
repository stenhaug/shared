ben\_columns\_example
================
Ben Stenhaug

Let’s make some simple data to work with:

``` r
library(tidyverse)

simple_data_frame <- 
   tibble(
      x = 1:5,
      ben1 = rnorm(5),
      ben2 = runif(5),
      ben3 = rpois(5, 1)
   ) %>% 
   print()
```

    ## # A tibble: 5 x 4
    ##       x   ben1  ben2  ben3
    ##   <int>  <dbl> <dbl> <int>
    ## 1     1 -0.230 0.819     1
    ## 2     2  0.376 0.523     0
    ## 3     3  0.271 0.160     0
    ## 4     4  1.32  0.784     0
    ## 5     5 -0.386 0.226     0

Here’s the base R way of changing columns with the form ben\# to be
equal to
1:

``` r
simple_data_frame[ , str_detect(names(simple_data_frame), "^ben\\d+")] <- 1
simple_data_frame
```

    ## # A tibble: 5 x 4
    ##       x  ben1  ben2  ben3
    ##   <int> <dbl> <dbl> <dbl>
    ## 1     1     1     1     1
    ## 2     2     1     1     1
    ## 3     3     1     1     1
    ## 4     4     1     1     1
    ## 5     5     1     1     1

Here’s the tidyverse way of doing the same
thing

``` r
simple_data_frame %>% mutate_at(vars(starts_with("ben")), function(x){x = 1})
```

    ## # A tibble: 5 x 4
    ##       x  ben1  ben2  ben3
    ##   <int> <dbl> <dbl> <dbl>
    ## 1     1     1     1     1
    ## 2     2     1     1     1
    ## 3     3     1     1     1
    ## 4     4     1     1     1
    ## 5     5     1     1     1
