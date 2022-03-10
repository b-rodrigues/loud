library(purrr)

quiet_mean <- quietly(mean)

quiet_mean(c(1, 2, "3"))

quiet_mean(c(1, 2))

quiet_log <- quietly(log)

quiet_log(c(1, 2))
quiet_log(c(1, 2, "3"))

pure_log <- quietly(safely(log))
pure_log(c(1, 2, 3))
pure_log(c(1, 2, "3"))

purely <- function(.f){

  pure_f <- quietly(safely(.f))

}


purely(log)(c(1, 2, "3"))

purely(log)(c(1, 2, 3))

purely(mean)(c(1, 2, "3"))
purely(mean)(c(1, 2, 3))
purely(mean)(c(1, 2, NA))

purely(sqrt)(c(-1, 2, 3))

library(rlang)

ha <- catch_cnd(log("3"))

str(ha)

ha$call

hb <- catch_cnd(mean(c("3", 1)))

hb$call
hb$message

hc <- catch_cnd(log(3))


catchcnd <- function(expr) {
  tryCatch(
    expr,
    condition = function(cnd) cnd
  )

}

tryCatch(log("10"), condition = function(cnd)(("hu")))


conditionCall(hb)

## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
https://rdrr.io/github/INWT/mctools/man/WithCallingHandlers.html
