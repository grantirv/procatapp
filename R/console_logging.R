# console logging function to aid debugging
  cl <- function(expr, id = NULL, message = NULL) {
    # time execution
      e1 <- parent.frame()
      start_time <- Sys.time()
      result <- try(eval(expr, e1), silent = TRUE)
      elapsed <- difftime(Sys.time(), start_time, units = "secs")
      elapsed <- sprintf("%7.3f", as.numeric(elapsed))

    # determine nesting level
      tb <- rlang::trace_back()
      calls <- unlist(lapply(tb$call, rlang::call_name))
      n_cl_calls <- sum(grepl("cl", calls, fixed = TRUE))
      nl <- n_cl_calls - 1

    # report timing
      indent <- paste(rep(" ", nl * 2L), collapse = "")
      msg <- paste0(indent, fix_width(id, 30 - 2 * nl), fix_width(message, 40))
      if (inherits(result, "try-error")) {
        error <- paste0(indent, result)
        cat(msg, "\n", crayon::yellow(error), "\n", sep = "")
      } else {
        cat(msg, elapsed, "\n", sep = " ")
      }

    return(result)
  }

  fix_width <- function(string, width) {
    string <- stringr::str_trunc(string, width)
    string <- stringr::str_pad(string, width, side = "right")
    return(string)
  }
