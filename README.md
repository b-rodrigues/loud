
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loud

<!-- badges: start -->
<!-- badges: end -->

Easily add logs to your functions.

## Installation

You can install the development version of loud from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("b-rodrigues/loud")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(loud)
#> Loading required package: rlang

loud_sqrt <- loudly(sqrt)

loud_sqrt(1:5)
#> $result
#> [1] 1.000000 1.414214 1.732051 2.000000 2.236068
#> 
#> $log
#> [1] "Log start..."                                                             
#> [2] "sqrt(1:5) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"
```

You can also use the native R pipe:

``` r
loud_sqrt <- loudly(sqrt)
loud_exp <- loudly(exp)
loud_mean <- loudly(mean)

1:10 |>
  loud_sqrt() |>
  bind_loudly(loud_exp) |>
  bind_loudly(loud_mean)
#> $result
#> [1] 11.55345
#> 
#> $log
#> [1] "Log start..."                                                                   
#> [2] "sqrt(1:10) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"     
#> [3] "exp(.l$result) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58" 
#> [4] "mean(.l$result) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"
```

`bind_loudly()` is used to pass the output from one decorated function
to the next.

`loudly()` works with any function:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

loud_group_by <- loudly(group_by)
loud_select <- loudly(select)
loud_summarise <- loudly(summarise)
loud_filter <- loudly(filter)

starwars %>%
  loud_select(height, mass, species, sex) %>%
  bind_loudly(loud_group_by, species, sex) %>%
  bind_loudly(loud_filter, sex != "male") %>%  
  bind_loudly(loud_summarise,
              mass = mean(mass, na.rm = TRUE)
              )
#> $result
#> # A tibble: 9 x 3
#> # Groups:   species [9]
#>   species    sex              mass
#>   <chr>      <chr>           <dbl>
#> 1 Clawdite   female           55  
#> 2 Droid      none             69.8
#> 3 Human      female           56.3
#> 4 Hutt       hermaphroditic 1358  
#> 5 Kaminoan   female          NaN  
#> 6 Mirialan   female           53.1
#> 7 Tholothian female           50  
#> 8 Togruta    female           57  
#> 9 Twi'lek    female           55  
#> 
#> $log
#> [1] "Log start..."                                                                                                 
#> [2] "select(.,height,mass,species,sex) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"            
#> [3] "group_by(.l$result,species,sex) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"              
#> [4] "filter(.l$result,sex != \"male\") started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"            
#> [5] "summarise(.l$result,mean(mass, na.rm = TRUE)) started at 2022-02-18 16:03:58 and ended at 2022-02-18 16:03:58"
```

You could also use the `%>=%` pipe instead of `bind_loudly()`, once I
get it working properly. For now it does, but I plan on making things
like this work:

``` r
starwars %>%
  loud_select(height, mass, species, sex) %>=%
  loud_group_by(species, sex) %>=%
  loud_filter(sex != "male") %>=%  
  loud_summarise(mass = mean(mass, na.rm = TRUE)) 
```

## Caution

This packages is in early development, has not a single test and
basically is held together with string and j\*zz. Use at your own peril.

## Thanks

I’d like to thank [armcn](https://github.com/armcn),
[Kupac](https://github.com/Kupac) for their blog posts
([here](https://kupac.gitlab.io/biofunctor/2019/05/25/maybe-monad-in-r/))
and packages ([maybe](https://armcn.github.io/maybe/)) which inspired me
to build this package.
