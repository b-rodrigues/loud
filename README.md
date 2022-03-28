
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
devtools::install_github("b-rodrigues/loud@release_0.1.1")
```

## Introduction

{loud} allows you to decorate functions make them provide enhanced
output:

``` r
library(loud)

loud_sqrt <- loudly(sqrt)

a <- loud_sqrt(1:5)
```

Object `a` is now an object of class `loud`. The value of the `sqrt()`
function applied to its arguments can be obtained using `pick()`:

``` r
pick(a, "value")
#> [1] 1.000000 1.414214 1.732051 2.000000 2.236068
```

A log also gets generated and can be read using `read_log()`:

``` r
read_log(a)
#> [1] "Complete log:"                                      
#> [2] "✔ sqrt(1:5) ran successfully at 2022-03-28 22:05:27"
#> [3] "Total running time: 0.000116825103759766 secs"
```

This is especially useful for objects that get created using multiple
calls:

``` r
loud_sqrt <- loudly(sqrt)
loud_exp <- loudly(exp)
loud_mean <- loudly(mean)

b <- 1:10 |>
  loud_sqrt() |>
  bind_loudly(loud_exp) |>
  bind_loudly(loud_mean)
```

``` r
read_log(b)
#> [1] "Complete log:"                                           
#> [2] "✔ sqrt(1:10) ran successfully at 2022-03-28 22:05:27"    
#> [3] "✔ exp(.l$value) ran successfully at 2022-03-28 22:05:27" 
#> [4] "✔ mean(.l$value) ran successfully at 2022-03-28 22:05:27"
#> [5] "Total running time: 0.0154945850372314 secs"

pick(b, "value")
#> [1] 11.55345
```

## Composing decorated functions

`bind_loudly()` is used to pass the output from one decorated function
to the next.

`loudly()` works with any function:

``` r
library(dplyr)

loud_group_by <- loudly(group_by)
loud_select <- loudly(select)
loud_summarise <- loudly(summarise)
loud_filter <- loudly(filter)

output <- starwars %>%
  loud_select(height, mass, species, sex) %>%
  bind_loudly(loud_group_by, species, sex) %>%
  bind_loudly(loud_filter, sex != "male") %>%
  bind_loudly(loud_summarise,
              mass = mean(mass, na.rm = TRUE)
              )
```

``` r
read_log(output)
#> [1] "Complete log:"                                                                         
#> [2] "✔ select(.,height,mass,species,sex) ran successfully at 2022-03-28 22:05:27"           
#> [3] "✔ group_by(.l$value,species,sex) ran successfully at 2022-03-28 22:05:27"              
#> [4] "✔ filter(.l$value,sex != \"male\") ran successfully at 2022-03-28 22:05:27"            
#> [5] "✔ summarise(.l$value,mean(mass, na.rm = TRUE)) ran successfully at 2022-03-28 22:05:27"
#> [6] "Total running time: 0.0635149478912354 secs"
```

The value can then be accessed and worked on as usual using `pick()`:

``` r
pick(output, "value")
#> tibble [9, 3] 
#> grouped by: species [9] 
#> species chr Clawdite Droid Human Hutt Kaminoan Mirialan
#> sex     chr female none female hermaphroditic female female
#> mass    dbl 55 69.75 56.333333 1358 NaN 53.1
```

This package also ships with a dedicated pipe, `%>=%` which you can use
instead of `bind_loudly()`:

``` r
output_pipe <- starwars %>%
  loud_value() %>=%
  loud_select(height, mass, species, sex) %>=%
  loud_group_by(species, sex) %>=%
  loud_filter(sex != "male") %>=%
  loud_summarise(mass = mean(mass, na.rm = TRUE))
```

``` r
pick(output_pipe, "value")
#> tibble [9, 3] 
#> grouped by: species [9] 
#> species chr Clawdite Droid Human Hutt Kaminoan Mirialan
#> sex     chr female none female hermaphroditic female female
#> mass    dbl 55 69.75 56.333333 1358 NaN 53.1
```

Objects of class `loud` have their own print method:

``` r
output_pipe
#> Value
#> ---------------
#> tibble [9, 3] 
#> grouped by: species [9] 
#> species chr Clawdite Droid Human Hutt Kaminoan Mirialan
#> sex     chr female none female hermaphroditic female female
#> mass    dbl 55 69.75 56.333333 1358 NaN 53.1 
#> 
#> ---------------
#> This is an object of type `loud`.
#> Retrieve the value of this object with pick(x, "value").
#> To read the log of this object, call read_log().
```

## Condition handling

Errors, warnings, and messages also get caught and composed in the log:

``` r
errord_output <- starwars %>%
  loud_value() %>=%
  loud_select(height, mass, species, sex) %>=% 
  loud_group_by(species, sx) %>=% # type, "sx" instead of "sex"
  loud_filter(sex != "male") %>=%
  loud_summarise(mass = mean(mass, na.rm = TRUE))
```

``` r
errord_output
#> Value
#> ---------------
#> [1] NA
#> 
#> ---------------
#> This is an object of type `loud`.
#> Retrieve the value of this object with pick(x, "value").
#> To read the log of this object, call read_log().
```

Reading the log tells you which function failed, and with which error
message:

``` r
read_log(errord_output)
#> [1] "Complete log:"                                                                                                                                                                                    
#> [2] "✔ as_loud(NA) ran successfully at 2022-03-28 22:05:27"                                                                                                                                            
#> [3] "✔ select(.l$value,height,mass,species,sex) ran successfully at 2022-03-28 22:05:27"                                                                                                               
#> [4] "✖ group_by(.l$value,species,sx) ran unsuccessfully with following exception: Must group by variables found in `.data`.\n✖ Column `sx` is not found. at 2022-03-28 22:05:27"                       
#> [5] "✖ filter(.l$value,sex != \"male\") ran unsuccessfully with following exception: no applicable method for 'filter' applied to an object of class \"logical\" at 2022-03-28 22:05:27"               
#> [6] "✖ summarise(.l$value,mean(mass, na.rm = TRUE)) ran unsuccessfully with following exception: no applicable method for 'summarise' applied to an object of class \"logical\" at 2022-03-28 22:05:27"
#> [7] "Total running time: 0.120630502700806 secs"
```

## Caution

This packages is in early development and basically is held together
with string and j\*zz. Use at your own peril, but it has some tests now
that pass, so it shouldn’t be too bad. That being said, if you used it
for serious work and it turns out that you house caught on fire, that’s
on you.

## Thanks

I’d like to thank [armcn](https://github.com/armcn),
[Kupac](https://github.com/Kupac) for their blog posts
([here](https://kupac.gitlab.io/biofunctor/2019/05/25/maybe-monad-in-r/))
and packages ([maybe](https://armcn.github.io/maybe/)) which inspired me
to build this package.
