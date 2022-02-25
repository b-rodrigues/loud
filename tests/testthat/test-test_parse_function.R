test_that("parse simple function", {
  expect_equal("loud_sqrt", parse_function(
                              deparse1(substitute(
                                loud_sqrt())
                                ))$func)
  expect_equal("", parse_function(
                     deparse1(substitute(
                       loud_sqrt())
                       ))$args)
})

test_that("parse simple function", {
  expect_equal("loud_sqrt", parse_function(
                              deparse1(substitute(
                                loud_sqrt(any_arg))
                                ))$func)
  expect_equal("any_arg, ", parse_function(
                              deparse1(substitute(
                                loud_sqrt(any_arg))
                                ))$args)
})

test_that("parse dplyr function", {
  expect_equal("loud_summarise",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, average = mean(cyl)))
                   ))$func)
  expect_equal("mtcars, average = mean(cyl), ",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, average = mean(cyl))
                 )))$args)
})

test_that("parse summarise, with two functions", {
  expect_equal("loud_summarise",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, mean_cyl = mean(cyl), mean_hp = mean(hp)))
                   ))$func)
  expect_equal("mtcars, mean_cyl = mean(cyl), mean_hp = mean(hp), ",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, mean_cyl = mean(cyl), mean_hp = mean(hp)))
                 ))$args)
})

test_that("parse summarise, with two functions and several arguments", {
  expect_equal("loud_summarise",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, mean_cyl = mean(cyl, na.rm = TRUE), mean_hp = mean(hp, na.rm = TRUE)))
                   ))$func)
  expect_equal("mtcars, mean_cyl = mean(cyl, na.rm = TRUE), mean_hp = mean(hp, na.rm = TRUE), ",
               parse_function(
                 deparse1(substitute(
                   loud_summarise(mtcars, mean_cyl = mean(cyl, na.rm = TRUE), mean_hp = mean(hp, na.rm = TRUE)))
                   ))$args)
})

test_that("make simple command", {
  expect_equal(".l$result |> loud_exp(.log = .l$log)",
               make_command(
                 parse_function(
                   deparse1(substitute(
                     loud_exp()
                     ))
                               )
                            )
               )
})

test_that("make command with any args", {
  expect_equal(".l$result |> loud_exp(ha, hu, .log = .l$log)",
               make_command(
                 parse_function(
                   deparse1(substitute(
                   loud_exp(ha, hu)
                   ))
                 )
               )
               )
})

test_that("make dplyr command", {
  expect_equal(".l$result |> loud_summarise(mtcars, average = mean(cyl), .log = .l$log)",
               make_command(
                 parse_function(
                   deparse1(substitute(
                   loud_summarise(mtcars, average = mean(cyl))
                   ))
                 )
               )
               )
})


test_that("test that pipe and bind_loud give same results", {

loud_sqrt <- loudly(sqrt)
loud_mean <- loudly(mean)
loud_exp <- loudly(exp)

  result_pipe <- 1:10 |>
    loud_sqrt() %>=%
    loud_exp() %>=%
    loud_mean()

  result_bind <- 1:10 |>
    loud_sqrt() |>
    bind_loudly(loud_exp) |>
    bind_loudly(loud_mean)

  expect_equal(result_pipe, result_bind)
})

