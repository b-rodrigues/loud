#' Add a simple logging message to any function
#' @param .f A function to decorate
#' @return A function which returns a list. The first element of the list, $result, is the result of
#' the original function .f applied to its inputs, and the second element is a log message, $log.
#' @importFrom rlang enexprs
#' @examples
#' loudly(sqrt)(10)
#' @export
loudly <- function(.f){
  
  fstring <- deparse(substitute(.f))

  function(..., .log = "Log start..."){

    args <- paste0(rlang::enexprs(...), collapse = ",")
    the_function_call <- paste0(fstring, "("  , args, ")")

    start <- Sys.time()
    result <- .f(...)
    end <- Sys.time()

    the_log <- c(.log,
                 paste0(the_function_call,
                        " started at ",
                        start,
                        " and ended at ",
                        end))
    
    list_result <- list(
      result = result,
      log = the_log
    )   

    list_result
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

  .f(.l$result, ..., .log = .l$log)

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

  #func <- gsub("\\(|\\)", "", deparse(substitute(.f)))
  func <- gsub("\\(.*$", "", deparse(substitute(.f)))
  args <- stringr::str_extract(deparse(substitute(.f)), "\\([^()]+\\)")
  args <- gsub("\\(|\\)", "", args)
  args <- ifelse(is.na(args), "", paste0(args, ", "))
  func <- gsub("\\(.*$", "", deparse(substitute(.f)))
  cmd <- paste0(".l$result |> ", func, "(", args, ".log = .l$log)")
  eval(parse(text = cmd)) 
    
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

  stopifnot('.e must be either "result" or "log"' = .e %in% c("result", "log"))

  .l[[.e]]

}
