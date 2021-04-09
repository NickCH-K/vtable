#' Group-Independence Test Function
#'
#' This function takes in two variables of equal length, the first of which is a categorical variable, and performs a test of independence between them. It returns a character string with the results of that test for putting in a table.
#'
#' In an attempt (and perhaps an encouragement) to use this function in weird ways, and because it's not really expected to be used directly, input is not sanitized. Have fun!
#'
#' @param x A categorical variable.
#' @param y A variable to test for independence with \code{x}. This can be a factor or numeric variable. If you want a numeric variable treated as categorical, convert to a factor first.
#' @param w A vector of weights to pass to the appropriate test.
#' @param factor.test Used when \code{y} is a factor, a function that takes \code{x} and \code{y} as its first arguments and returns a list with three arguments: (1) The name of the test for printing, (2) the test statistic, and (3) the p-value. Defaults to a Chi-squared test if there are no weights, or a design-based F statistic (Rao & Scott Aadjustment, see \code{survey::svychisq}) with weights, which requires that the \code{survey} package be installed. WARNING: the Chi-squared test's assumptions fail with small sample sizes. This function will be attempted for all non-numeric \code{y}.
#' @param numeric.test Used when \code{y} is numeric, a function that takes \code{x} and \code{y} as its first arguments and returns a list with three arguments: (1) The name of the test for printing, (2) the test statistic, and (3) the p-value. Defaults to a group differences F test.
#' @param star.cutoffs A numeric vector indicating the p-value cutoffs to use for reporting significance stars. Defaults to \code{c(.01,.05,.1)}. If you don't want stars, remove them from the \code{format} argument.
#' @param star.markers A character vector indicating the symbols to use to indicate significance cutoffs associated with \code{star.cuoffs}. Defaults to \code{c('***','**','*')}. If you don't want stars, remove them from the \code{format} argument.
#' @param digits Number of digits after the decimal to round the test statistic and p-value to.
#' @param fixed.digits \code{FALSE} will cut off trailing \code{0}s when rounding. \code{TRUE} retains them. Defaults to \code{FALSE}.
#' @param format The way in which the four elements returned by (or calculated after) the test - \code{{name}}, \code{{stat}}, \code{{pval}}, and \code{{stars}} - will be arranged in the string output. Note that the default \code{'{name}={stat}{stars}'} does not contain the p-value, and also does not contain superscript for the stars since it doesn't know what markup language you're aiming for. For LaTeX you may prefer \code{'{name}$={stat}^{{stars}}$'}, and for HTML \code{'{name}={stat}<sup>{stars}</sup>'}.
#' @param opts The options listed above, entered in named-list format.
#' @examples
#'
#' data(mtcars)
#' independence.test(mtcars$cyl,mtcars$mpg)
#'
#' @export
independence.test <- function(x,y,w=NA,
                              factor.test = NA,
                              numeric.test = NA,
                              star.cutoffs = c(.01,.05,.1),
                              star.markers = c('***','**','*'),
                              digits = 3,
                              fixed.digits = FALSE,
                              format = '{name}={stat}{stars}',
                              opts = list()) {
  #Bring in opts
  list2env(opts,envir=environment())
  #Are we using factor.test or numeric.test
  cla <- is.numeric(y)

  # Backwards consistency
  if (length(w) == 1) {
    if (is.na(w)) {
      w <- NULL
    }
  }

  #Fill in defaults
  if (identical(factor.test,NA)) {
    factor.test <- chisq.it
  }
  if (identical(numeric.test,NA)) {
    numeric.test <- groupf.it
  }

  if (cla) {
    result <- numeric.test(x,y,w)
  } else {
    result <- factor.test(x,y,w)
  }

  #Get stars
  #Order smallest to biggest
  star.markers <- star.markers[order(star.cutoffs)]
  star.cutoffs <- star.cutoffs[order(star.cutoffs)]
  #Find the first value that qualifies
  underneath <- result[[3]] < star.cutoffs
  stars <- star.markers[underneath][1]
  stars <- ifelse(is.na(stars),'',stars)

  #Rounding
  #First, check if we're going to get a 0, so we can set that separate
  is.zero <- result[[3]] < 10^(-digits)
  if (fixed.digits) {
    result[[2]] <- format(result[[2]],digits=digits,nsmall=digits)
    result[[3]] <- format(result[[3]],digits=digits,nsmall=digits)
  } else {
    result[[2]] <- round(result[[2]],digits=digits)
    result[[3]] <- round(result[[3]],digits=digits)
  }
  if (is.zero) {
    result[[3]] <- paste0('<',10^(-digits))
  }

  #And format the result
  printout <- format
  #Fill in our four things
  printout <- gsub('\\{name\\}',result[[1]],printout)
  printout <- gsub('\\{stat\\}',result[[2]],printout)
  printout <- gsub('\\{pval\\}',result[[3]],printout)
  printout <- gsub('\\{stars\\}',stars,printout)

  return(printout)
}

# Internal chi-square and group-F tests that return things in independence.test format
chisq.it <- function(x,y,w=NULL) {
  if (is.null(w)) {
    suppressWarnings(result <- stats::chisq.test(x,y))

    return(list(
      'X2',
      unname(result$statistic),
      result$p.value
    ))
  } else {
    # Create survey design
    d <- data.frame(x = x, y = y, w = w)
    errmess <- try(sdes <- survey::svydesign(~1, data = d, weights = ~w))

    if (grepl('Error in loadNamespace',errmess[1])) {
      stop('Using weights with group.test = TRUE and factor variables requires the survey package. install.packages("survey")')
    }

    ftest <- survey::svychisq(~x+y, sdes)
    return(list(
      'F',
      unname(ftest$statistic),
      unname(ftest$p.value)
    ))
  }
}
groupf.it <- function(x,y,w=NULL) {
  result <- stats::anova(stats::lm(y~factor(x),weights = w))

  return(list(
    'F',
    result$`F value`[1],
    result$`Pr(>F)`[1]
  ))
}
