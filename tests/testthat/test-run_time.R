test_that("test running time", {

  sleeping <- function(x, y = 0){

    Sys.sleep(x)
    x + y

  }

  loud_sleep <- loudly(sleeping)

  result_pipe <- loud_sleep(1) %>=%
    loud_sleep(2)

  expect_equal(as.integer(result_pipe$run_time), 3)
})
