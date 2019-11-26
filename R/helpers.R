#' Number of unique values in a vector
#'
#' This function takes a vector and returns the number of unique values in that vector.
#'
#' This function is just shorthand for length(unique(x)), with a shorter name for reference in the vtable summ option.
#'
#' @param x A vector.
#' @examples
#' x <- c(1, 1, 2, 3, 4, 4, 4)
#' nuniq(x)
#'
#' @export
nuniq <- function(x) {
  return(length(unique(x)))
}

#' Proportion or number of missing values in a vector
#'
#' This function calculates the proportion of values in a vector that are NA.
#'
#' This function just shorthand for mean(is.na(x)), with a shorter name for reference in the vtable summ option.
#'
#' @param x A vector.
#' @examples
#' x <- c(1, 1, NA, 2, 3, NA)
#' propNA(x)
#' @export
propNA <- function(x) {
  mean(is.na(x))
}

#' Number of missing values in a vector
#'
#' This function calculates the number of values in a vector that are NA.
#'
#' This function just shorthand for sum(is.na(x)), with a shorter name for reference in the vtable summ option.
#'
#' @param x A vector.
#' @examples
#' x <- c(1, 1, NA, 2, 3, NA)
#' countNA(x)
#' @export
countNA <- function(x) {
  sum(is.na(x))
}

# Evaluate a series of functions
#
# Internal for summ, evaluates a function while allowing for the possibility that the class isn't right to evaluate that function
parsesumm <- function(x,summuse,summnames) {

  # Run each of the functions on the variable and get results
  results <- lapply(summuse, function(y) parsefcn(x,y))

  # If it's a number, round it
  results <- lapply(results, function(y) ifelse(is.numeric(y),
                                                round(y,3),y))

  # Get rid of functions that evaluated to NA (i.e. don't work)
  summnames <- summnames[!is.na(results)]
  results <- results[!is.na(results)]

  # Paste together
  results <- paste(summnames,results, sep = "")

  # And bring it all together with a break between each
  return(paste0(results, collapse = '<br/>'))
}

# Evaluate a function allowing it to not work
parsefcn <- function(x,y) {
  result <- suppressWarnings(try(eval(parse(text=y)),silent = TRUE))

  if (inherits(result,'try-error')) {
    result <- NA
  }

  return(result)
}
