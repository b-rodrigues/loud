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


test_that("errors get captured and logs composed", {

  loud_sqrt <- loudly(sqrt)
  loud_mean <- loudly(mean)
  loud_exp <- loudly(exp)

  result_pipe <- -1:-10 |>
    loud_mean() %>=%
    loud_sqrt() %>=%
    loud_exp()

  expect_length(result_pipe$log, 4)

})


test_that("test that pipe and bind_loud give same results for dplyr functions", {

  loud_select <- loudly(dplyr::select)
  loud_filter <- loudly(dplyr::filter)
  loud_group_by <- loudly(dplyr::group_by)
  loud_summarise <- loudly(dplyr::summarise)

  result_pipe <- mtcars |>
    loud_value() %>=%
    loud_select(am, starts_with("c")) %>=%
    loud_filter(am == 1) %>=%
    loud_group_by(carb) %>=%
    loud_summarise(mean_cyl = mean(cyl))

  result_bind <- mtcars |>
    loud_value() |>
    bind_loudly(loud_select, am, starts_with("c")) |>
    bind_loudly(loud_filter, am == 1) |>
    bind_loudly(loud_group_by, carb) |>
    bind_loudly(loud_summarise, mean_cyl = mean(cyl))

  expect_equal(result_pipe, result_bind)
})
