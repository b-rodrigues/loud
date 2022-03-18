as_loud <- function(a){

  structure(a, class = "loud")

}

#' @export
print.loud <- function(x, ...){

  cat("Result\n")
  cat("---------------\n")
  print(x$result, ...)
  cat("\n")
  cat("\n")
  cat("---------------\n")
  cat("Steps to achieve the above result:\n")
  print(x$log, ...)
  cat("\n")
  cat("\n")
  cat("---------------\n")
  cat("Running time:\n")
  print(x$run_time, ...)

}

#' Capture all errors, warnings and messages
#' @param .f A function to decorate
#' @return A function which returns a list. The first element of the list, $result, is the result of
#' the original function .f applied to its inputs. The second element, $log is NULL in case everything
#' goes well. In case of error/warning/message, $result is NA and $log holds the message.
#' purely() is used by loudly() to allow the latter to handle errors.
#' @importFrom rlang try_fetch eval_tidy cnd_message
#' @examples
#' purely(log)(10)
#' purely(log)(-10)
#' @export
purely <- function(.f){

  function(..., .log = "Log start..."){

    res <- rlang::try_fetch(
                    rlang::eval_tidy(.f(...)),
                    error = function(err) err,
                    warning = function(warn) warn,
                    message = function(message) message,
                  )

    final_result <- list(
      result = NULL,
      log = NULL
    )

    final_result$result <- if(any(c("error", "warning", "message") %in% class(res))){
                             NA
                           } else {
                             res
                           }

    final_result$log <- if(any(c("error", "warning", "message") %in% class(res))){
                          rlang::cnd_message(res)
                           } else {
                             NA
                           }

    final_result


  }
}

#' Add a simple logging message to any function
#' @param .f A function to decorate
#' @return A function which returns a list. The first element of the list, $result, is the result of
#' the original function .f applied to its inputs, and the second element is a log message, $log.
#' @importFrom rlang enexprs
#' @examples
#' loudly(sqrt)(10)
#' @export
loudly <- function(.f){

  fstring <- deparse1(substitute(.f))

  function(..., .log = "Log start...", .run_time = 0){

    args <- paste0(rlang::enexprs(...), collapse = ",")
    the_function_call <- paste0(fstring, "("  , args, ")")

    start <- Sys.time()
    pure_f <- purely(.f)
    res_pure <- (pure_f(...))
    end <- Sys.time()

    if(all(is.na(res_pure$result))){

      result <- NULL

      the_log <- c(.log,
                   paste0("✖ CAUTION - ERROR: ",
                          the_function_call,
                          " started at ",
                          start,
                          " and failed at ",
                          end,
                          " with following message: ",
                          res_pure$log))
    } else {

      result <- res_pure$result

      the_log <- c(.log,
                   paste0("✔ ",
                          the_function_call,
                          " started at ",
                          start,
                          " and ended at ",
                          end))

    }



    list_result <- list(
      result = result,
      log = the_log,
      run_time = .run_time + end - start
    )

    as_loud(list_result)
  }
}

#' Evaluate a decorated function
#' @param .l A loud value (a list of two elements)
#' @param .f A loud function to apply to the returning value of .l
#' @param ... Further parameters to pass to .f
#' @return A list with elements .f(.l$result) and concatenated logs.
#' @examples
#' loud_sqrt <- loudly(sqrt)
#' loud_exp <- loudly(exp)
#' 3 |> loud_sqrt() |> bind_loudly(loud_exp)
#' @export
bind_loudly <- function(.l, .f, ...){

  .f(.l$result, ..., .log = .l$log, .run_time = .l$run_time)

}


#' Evaluate a non-loud function on a loud value
#' @param .l A loud value (a list of two elements)
#' @param .f A non-loud function
#' @param ... Further parameters to pass to .f
#' @return Returns the result of .f(.l$result)
#' @examples
#' loud_value(3) |> flat_loudly(sqrt)
#' @export
flat_loudly <- function(.l, .f, ...){

  .f(.l$result, ...) |>
    as_loud()

}

#' Create a loud value
#' @param .x Any object
#' @return Returns a loud value with the object as the $result
#' @examples
#' loud_value(3)
#' @export
loud_value <- function(.x){

  list(result = .x,
       log = "Created loud value...",
       run_time = 0) |>
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

  paste0(".l$result |> ",
         parsed_function$func,
         "(",
         parsed_function$args,
         ".log = .l$log, .run_time = .l$run_time)")

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
#' @param .e Element of interest to retrieve, one of "result" or "log"
#' @return The `result` or `log` element of the loud value .l
#' @examples
#' loud_sqrt <- loudly(sqrt)
#' loud_exp <- loudly(exp)
#' 3 |> loud_sqrt() %>=% loud_exp() |> pick("result")
#' @export
pick <- function(.l, .e){

  stopifnot('.e must be either "result", "log" or "run_time"' = .e %in% c("result", "log", "run_time"))

  .l[[.e]]

}
