#' Number of unique values in a vector
#'
#' This function takes a vector and returns the number of unique values in that vector.
#'
#' This function is just shorthand for \code{length(unique(x))}, with a shorter name for reference in the \code{vtable} or \code{sumtable} \code{summ} option.
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
#' This function just shorthand for \code{mean(is.na(x))}, with a shorter name for reference in the \code{vtable} or \code{sumtable} \code{summ} option.
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
#' This function just shorthand for \code{sum(is.na(x))}, with a shorter name for reference in the \code{vtable} or \code{sumtable} \code{summ} option.
#'
#' @param x A vector.
#' @examples
#' x <- c(1, 1, NA, 2, 3, NA)
#' countNA(x)
#' @export
countNA <- function(x) {
  sum(is.na(x))
}

#' Number of nonmissing values in a vector
#'
#' This function calculates the number of values in a vector that are not NA.
#'
#' This function just shorthand for \code{sum(!is.na(x))}, with a shorter name for reference in the \code{vtable} or \code{sumtable} \code{summ} option.
#'
#' @param x A vector.
#' @examples
#' x <- c(1, 1, NA, 2, 3, NA)
#' notNA(x)
#' @export
notNA <- function(x) {
  sum(!is.na(x))
}

#' Returns a vector of 100 percentiles
#'
#' This function calculates 100 percentiles of a vector and returns all of them.
#'
#' This function just shorthand for \code{quantile(x,1:100/100)}, with a shorter name for reference in the \code{vtable} or \code{sumtable} \code{summ} option, and which works with \code{sumtable} \code{summ.names} styling.
#'
#' @param x A vector.
#' @examples
#' x <- 1:500
#' pctile(x)[50]
#' quantile(x,.5)
#' median(x)
#' @export
pctile <- function(x) {
  quantile(x,1:100/100)
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

# Evaluate a function allowing it to not work
# Special version for sumtable that does the NA-dropping internally
parsefcn_summ <- function(x,y) {
  if (!any(sapply(c('anyNA','propNA','sumNA'),function(z) grepl(z,y)))) {
    x <- x[!is.na(x)]
  }

  result <- suppressWarnings(try(eval(parse(text=y)),silent = TRUE))

  if (inherits(result,'try-error')) {
    result <- NA
  }

  return(result)
}

# Create a summary statistics table for a single variable
# Internal function for sumtable
summary.row <- function(data,var,st,
                        title,summ,cla,factor.percent,
                        factor.count,factor.numeric,digits,fixed.digits) {
  numcols <- length(summ)
  if (cla == 'header') {
    st[1,] <- c(
                paste0(title,'_MULTICOL_l_',numcols+1),
                rep('DELETECELL',numcols))
  } else if (cla == 'factor' & !factor.numeric) {
    #Get data
    va <- data[[var]]
    #Total number of obs
    nonmiss <- notNA(va)
    #Header row
    st[1,] <- c(title,nonmiss,
                #Take this out for now.
                #ifelse(factor.percent,'100%','1'),
                '',
                rep('',numcols-2))
    #And now the per-factor stuff
    mat <- as.data.frame(table(va))
    mat$va <- paste('...',mat$va)
    propcalc <- mat$Freq/nonmiss * (100^factor.percent)
    if (fixed.digits) {
      mat$Prop <- sapply(1:length(propcalc), function(x)
        format(propcalc[x],
               digits=max(digits[2]-2*factor.percent,0),
               nsmall=max(digits[2]-2*factor.percent,0)))
    } else {
      mat$Prop <- round(propcalc,
                        digits=max(digits[2]-2*factor.percent,0))
    }
    if (!factor.count) {
      mat$Freq <- ''
    }
    if (factor.percent) {
      mat$Prop <- paste0(mat$Prop,'%')
    }
    if (ncol(mat) < ncol(st)) {
      mat[,(ncol(mat)+1):(ncol(st))] <- ''
    }
    names(mat) <- names(st)
    st <- rbind(st,mat)
  } else if (cla == 'factor' & factor.numeric) {
    #Header row
    st[1,] <- c(title,rep('',numcols))
    #Get data
    va <- data[,var]
    #Create dummies to treat as numeric
    mat <- model.matrix(~-1+va)
    #And names
    facnames <- paste('...',levels(va))
    #Run each of the functions on the variable and get results
    results <- lapply(1:ncol(mat),
                      function(x) sapply(summ, function(y) parsefcn_summ(mat[,x],y)))
    #Round
    if (fixed.digits) {

    } else {
      results <- lapply(results, function(x)
        sapply(1:length(x), function(y) as.character(round(x[y],digits=digits[y]))))
    }
    #Add factor names
    results <- lapply(1:length(results), function(x) c(facnames[x],results[[x]]))
    #And construct
    results <- as.data.frame(do.call(rbind,results))
    names(results) <- names(st)
    st <- rbind(st,results)
  } else {
    #Get data
    va <- data[,var]
    #Run each of the functions on the variable and get results
    results <- sapply(summ, function(y) parsefcn_summ(va,y))
    #Round
    if (fixed.digits) {
      results <- sapply(1:length(results), function(y) format(results[y],digits=digits[y],nsmall = digits[y]))
    } else {
      results <- sapply(1:length(results), function(y) round(results[y],digits=digits[y]))
    }
    #And construct
    st[1,] <- c(title,results)
  }

  return(st)
}

# cbinds character data frames with potentially unequal rows
# takes a list of data.frames
cbind_unequal <- function(x) {
  #Get longest length
  rowseach <- sapply(x,nrow)
  mostrows <- max(rowseach)

  #Loop through and add blank rows
  for (i in 1:length(x)) {
    if (rowseach[i] < mostrows) {
      x[[i]][(rowseach[i]+1):mostrows,] <- ''
    }
  }

  return(do.call(cbind,x))
}
