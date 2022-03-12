
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

loud_sqrt <- loudly(sqrt)

loud_sqrt(1:5)
#> $result
#> [1] 1.000000 1.414214 1.732051 2.000000 2.236068
#> 
#> $log
#> [1] "Log start..."                                                               
#> [2] "✔ sqrt(1:5) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40"
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
#> [2] "✔ sqrt(1:10) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40"     
#> [3] "✔ exp(.l$result) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40" 
#> [4] "✔ mean(.l$result) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40"
```

`bind_loudly()` is used to pass the output from one decorated function
to the next.

`loudly()` works with any function:

``` r
library(dplyr)

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
#> NULL
#> 
#> $log
#> [1] "Log start..."                                                                                                                                                                                                                         
#> [2] "✔ select(.,height,mass,species,sex) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40"                                                                                                                                  
#> [3] "✖ CAUTION - ERROR: group_by(.l$result,species,sex) started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: "                                                                                         
#> [4] "✖ CAUTION - ERROR: filter(.l$result,sex != \"male\") started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: no applicable method for 'filter' applied to an object of class \"NULL\""               
#> [5] "✖ CAUTION - ERROR: summarise(.l$result,mean(mass, na.rm = TRUE)) started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: no applicable method for 'summarise' applied to an object of class \"NULL\""
```

You could also use the `%>=%` pipe instead of `bind_loudly()`:

``` r
starwars %>%
  loud_value() %>=%
  loud_select(height, mass, species, sex) %>=%
  loud_group_by(species, sex) %>=%
  loud_filter(sex != "male") %>=%
  loud_summarise(mass = mean(mass, na.rm = TRUE))
#> $result
#> NULL
#> 
#> $log
#> [1] "Created loud value..."                                                                                                                                                                                                                
#> [2] "✔ select(.l$result,height,mass,species,sex) started at 2022-03-12 21:08:40 and ended at 2022-03-12 21:08:40"                                                                                                                          
#> [3] "✖ CAUTION - ERROR: group_by(.l$result,species,sex) started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: "                                                                                         
#> [4] "✖ CAUTION - ERROR: filter(.l$result,sex != \"male\") started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: no applicable method for 'filter' applied to an object of class \"NULL\""               
#> [5] "✖ CAUTION - ERROR: summarise(.l$result,mean(mass, na.rm = TRUE)) started at 2022-03-12 21:08:40 and failed at 2022-03-12 21:08:40 with following message: no applicable method for 'summarise' applied to an object of class \"NULL\""
```

## Caution

This packages is in early development and basically is held together
with string and j\*zz. Use at your own peril.

## Thanks

I’d like to thank [armcn](https://github.com/armcn),
[Kupac](https://github.com/Kupac) for their blog posts
([here](https://kupac.gitlab.io/biofunctor/2019/05/25/maybe-monad-in-r/))
and packages ([maybe](https://armcn.github.io/maybe/)) which inspired me
to build this package.
