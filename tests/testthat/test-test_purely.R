test_that("purely decorated function provides correct result", {
  expect_equal((purely(log)(10))$result, log(10))
})


test_that("purely decorated function provides right result", {
  expect_equal((purely(log)(seq(1, 10)))$result, log(seq(1, 10)))
})

test_that("purely decorated function provides NA if problem", {
  expect_equal((purely(log)(-10))$result, NA)
})

test_that("purely decorated function log", {
  expect_type((purely(log)(-10))$log, "character")
})

test_that("compose purely decorated functions", {

  pure_sqrt <- purely(sqrt)
  pure_mean <- purely(mean)
  pure_exp <- purely(exp)

  result_pipe <- 1:10 |>
    pure_sqrt() %>=%
    pure_exp() %>=%
    pure_mean()

  expect_equal(result_pipe$result, mean(exp(sqrt(1:10))))

})
