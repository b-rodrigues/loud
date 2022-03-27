as_loud <- function(a){

  structure(a, class = "loud")

}


make_log_df <- function(success,
                        fstring,
                        args,
                        res_pure,
                        start = Sys.time(),
                        end = Sys.time(),
                        .g = (\(x) NA)){

  outcome <- ifelse(success == 1,
                    "✔ Success",
                    "✖ Caution - ERROR")

  tibble::tibble(
            "outcome" = outcome,
            "function" = fstring,
            "arguments" = args,
            "message" = paste0(res_pure$log, collapse = " "),
            "start_time" = start,
            "end_time" = end,
            "run_time" = end - start,
            "g" = list(.g(res_pure$value))
          )

}

read_log <- function(x){

  log_df <- x$log_df

  make_func_call <- function(log_df, i){

    paste0(paste0(log_df[i, c("function", "arguments")],
                  collapse = "("),
           ")")

  }

  is_success <- function(log_df, i){

    ifelse(grepl("Success", log_df$outcome[i]),
           "successfully",
           "unsuccessfully")

  }


  make_sentence <- function(log_df, i){

    paste(make_func_call(log_df, i), "ran", is_success(log_df, i), "at", log_df$start_time[i])

  }

  total_runtime <- function(log_df){

    total_time <- log_df$run_time

    unit <- attr(total_time, "units")

    paste(as.numeric(sum(log_df$run_time)), unit)

  }


  sentences <- vector(length = nrow(log_df))

  for(i in 1:nrow(log_df)){

  sentences[i] <-  make_sentence(log_df, i)

  }

  c("Complete log:", sentences, paste("Total running time:", total_runtime(log_df)))

}

#' @export
print.loud <- function(x, ...){

  cat("Value\n")
  cat("---------------\n")
  print(x$value, ...)
  cat("\n")
  cat("---------------\n")
  cat("This is an object of type `loud`.\n")
  cat("Retrieve the value of this object with pick(x, \"value\").\n")
  cat("To read the log of this object, call read_log().\n")
  cat("\n")

}

#' Capture all errors, warnings and messages
#' @param .f A function to decorate
#' @return A function which returns a list. The first element of the list, $value, is the result of
#' the original function .f applied to its inputs. The second element, $log is NULL in case everything
#' goes well. In case of error/warning/message, $value is NA and $log holds the message.
#' purely() is used by loudly() to allow the latter to handle errors.
#' @importFrom rlang try_fetch eval_tidy cnd_message
#' @examples
#' purely(log)(10)
#' purely(log)(-10)
#' @export
purely <- function(.f){

  function(..., .log_df = "Log start..."){

    res <- rlang::try_fetch(
                    rlang::eval_tidy(.f(...)),
                    error = function(err) err,
                    warning = function(warn) warn,
                    message = function(message) message,
                  )

    final_result <- list(
      value = NULL,
      log_df = NULL
    )

    final_result$value <- if(any(c("error", "warning", "message") %in% class(res))){
                             NA
                           } else {
                             res
                           }

    final_result$log_df <- if(any(c("error", "warning", "message") %in% class(res))){
                          rlang::cnd_message(res)
                           } else {
                             NA
                           }

    final_result


  }
}

#' Add a simple logging message to any function
#' @param .f A function to decorate
#' @param .g Optional. A function to apply to the intermediary results for monitoring purposes.
#' @return A function which returns a list. The first element of the list, $value, is the result of
#' the original function .f applied to its inputs, and the second element is a data frame with
#' colmuns: outcome, function, arguments, message, start_time, end_time, run_time and g.
#' @importFrom rlang enexprs
#' @importFrom tibble tibble
#' @examples
#' loudly(sqrt)(10)
#' @export
loudly <- function(.f, .g = (\(x) NA)){

  fstring <- deparse1(substitute(.f))

  function(..., .log_df = data.frame()){

    args <- paste0(rlang::enexprs(...), collapse = ",")
    the_function_call <- paste0(fstring, "("  , args, ")")

    start <- Sys.time()
    pure_f <- purely(.f)
    res_pure <- (pure_f(...))
    end <- Sys.time()

    if(all(is.na(res_pure$value))){

      log_df <- make_log_df(
        success = 0,
        fstring = fstring,
        args = args,
        res_pure = res_pure,
        start = start,
        end = end,
        .g = .g
      )

    } else {

      log_df <- make_log_df(
        success = 1,
        fstring = fstring,
        args = args,
        res_pure = res_pure,
        start = start,
        end = end,
        .g = .g
      )

    }

    log_df <- rbind(.log_df,
                    log_df)

    list_result <- list(
      value = res_pure$value,
      log_df = log_df
    )

    as_loud(list_result)
  }
}

#' Evaluate a decorated function
#' @param .l A loud value (a list of two elements)
#' @param .f A loud function to apply to the returning value of .l
#' @param ... Further parameters to pass to .f
#' @return A list with elements .f(.l$value) and concatenated logs.
#' @examples
#' loud_sqrt <- loudly(sqrt)
#' loud_exp <- loudly(exp)
#' 3 |> loud_sqrt() |> bind_loudly(loud_exp)
#' @export
bind_loudly <- function(.l, .f, ...){

  .f(.l$value, ..., .log_df = .l$log_df)

}


#' Evaluate a non-loud function on a loud value
#' @param .l A loud value (a list of two elements)
#' @param .f A non-loud function
#' @param ... Further parameters to pass to .f
#' @return Returns the result of .f(.l$value)
#' @examples
#' loud_value(3) |> flat_loudly(sqrt)
#' @export
flat_loudly <- function(.l, .f, ...){

  .f(.l$value, ...) |>
    as_loud()

}

#' Create a loud value
#' @param .x Any object
#' @return Returns a loud value with the object as the $value
#' @importFrom tibble tibble
#' @examples
#' loud_value(3)
#' @export
loud_value <- function(.x){

  res_pure <- list("log" = NA,
                   "value" = NA)

  log_df <- make_log_df(
    success = 1,
    fstring = "as_loud",
    args = NA,
    res_pure = res_pure,
    start = Sys.time(),
    end = Sys.time())

  list(value = .x,
       log_df = log_df) |>
    as_loud()
}

#' Pipe a loud value to a decorated function
#' @param .l A value returned by loudly
#' @param .f A loud function to apply to the returning value of .l
#' @return A loud value.
#' @importFrom stringr str_extract
#' @examples
#' loud_sqrt <- loudly(sqrt)
#' loud_exp <- loudly(exp)
#' 3 |> loud_sqrt() %>=% loud_exp()
#' @export
`%>=%` <- function(.l, .f, ...) {

  parsed <- parse_function(deparse1(substitute(.f)))

  cmd <- make_command(parsed)
  eval(parse(text = cmd))

}

make_command <- function(parsed_function){

  paste0(".l$value |> ",
         parsed_function$func,
         "(",
         parsed_function$args,
         ".log_df = .l$log_df)")

}

parse_function <- function(.f_string){

  func <- gsub("\\(.*$", "", .f_string)
  args <- stringr::str_extract(.f_string, "\\(.*")
  args <- gsub("^\\(", "", args)
  args <- gsub("\\)$", "", args)
  args <- ifelse(args != "", paste0(args, ", "), "")

 # func <- .f_string[1]
 # args <- ifelse(is.na(.f_string[-1]),
 #                "",
 #                c(paste0(.f_string[-1], collapse = ", "), ", "))

  list("func" = func,
       "args" = args)

}



#' Retrieve an element from a loud value
#' @param .l A loud value
#' @param .e Element of interest to retrieve, one of "value" or "log"
#' @return The `value` or `log` element of the loud value .l
#' @examples
#' loud_sqrt <- loudly(sqrt)
#' loud_exp <- loudly(exp)
#' 3 |> loud_sqrt() %>=% loud_exp() |> pick("value")
#' @export
pick <- function(.l, .e){

  stopifnot('.e must be either "value", "log_df"' = .e %in% c("value", "log_df"))

  .l[[.e]]

}
