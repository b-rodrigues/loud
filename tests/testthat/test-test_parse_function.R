test_that("parse simple function", {
  expect_equal("loud_sqrt", parse_function(loud_sqrt())$func)
  expect_equal("", parse_function(loud_sqrt())$args)
})

test_that("parse dplyr function", {
  expect_equal("loud_summarise",
               parse_function(loud_summarise(mtcars, average = mean(cyl)))$func)
  expect_equal("mtcars, average = mean(cyl), ",
               parse_function(loud_summarise(mtcars, average = mean(cyl)))$args)
})
