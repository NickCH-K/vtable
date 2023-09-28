# Forces evaluation of all functions
# In order to produce referential transparency and ensure the sub-arguments
# don't get copied over
# see similar function in the scales package
force_all <- function(...) {
  list(...)
}

#' Function-returning wrapper for format
#'
#' This function takes a set of options for the \code{format()} function and returns a function that itself calls \code{format()} with those settings.
#'
#' The only differences are:
#'
#' 1. \code{scientific} is set to \code{FALSE} by default, and \code{trim} is set to \code{TRUE}
#' 2. Passing a \code{NA} value produces \code{''} instead of \code{'NA'}.
#' 3. In addition to standard \code{format()} options, it also accepts a \code{percent} option to apply percentage formatting, and \code{prefix} and \code{suffix} options to apply prefixes or suffixes to formatted numbers.
#' 4. Has an attribute \code{'big.mark'} storing the \code{'big.mark'} option chosen.
#'
#' This is in the spirit of the \code{label_} functions in the scales package, except that it uses \code{format()}'s focus on significant digits instead of fixed decimal places, which is good for numbers that range across multiple orders of magnitude, common in \code{sumtable()} and \code{vtable()}.
#'
#' @param percent Whether to apply percentage formatting. Set to \code{TRUE} if 1 = 100\%. Or, optionally, set to any other number that represents 100\%. So \code{percent = TRUE} or \code{percent = 1} will interpret \code{.9} as \code{90\%}, or \code{percent = 100} will format \code{90} as \code{90\%}.
#' @param prefix A prefix to apply to the formatted number. For example, \code{prefix = '$'} would format \code{4} as \code{$4}.
#' @param suffix A suffix to apply to the formatted number. If specified alongside \code{percent}, the suffix comes after the \%.
#' @param scale A scalar value to be multiplied by all numbers prior to formatting. \code{scale = 1/1000}, for example, would convert the units into thousands. This is applied before \code{digits}.
#' @param digits Number of significant digits.
#' @param nsmall The minimum number of digits to the right of the decimal point.
#' @param big.mark A character to mark thousands places, for example producing "1,000" instead of "1000".
#' @param trim Whether numbers should be trimmed to their own size, rather than being right-justified to a common width. Unlike the actual \code{format()}, this defaults to \code{TRUE}. Note that in most vtable applications, the formatting function is applied one value at a time, rather than to a vector, so \code{trim = FALSE} may not work as intended.
#' @param scientific Whether numbers should be encoded in scientific format. Unlike the actual \code{format()}, this defaults to \code{FALSE}.
#' @param ... Arguments to be passed to \code{format()}. See \code{help(format)}. All other parameters listed above except for \code{percent}, \code{prefix}, or \code{suffix} are also just part of  \code{format}, but may be of particular interest, or have been included to show how defaults have changed.
#' @examples
#' x <- c(1, 1000, .000235, 1298.255, NA)
#' my.formatting.func = formatfunc(digits = 3, prefix = '$')
#' my.formatting.func(x)
#'
#' @export formatfunc
formatfunc <- function(percent = FALSE, prefix = '', suffix = '',
                        scale = 1,
                        digits = NULL, nsmall = 0L, big.mark = '',
                        trim = TRUE, scientific = FALSE, ...) {
  if (!is.null(digits)) {
    if (is.na(digits)) {
      digits <- NULL
    }
  }
  scalefactor <- 1
  if (is.numeric(percent)) {
    scalefactor <- 1/percent
    percent <- TRUE
  }
  if (percent) {
    scalefactor <- scalefactor*100
    suffix <- paste0('%',suffix)
  }
  force_all(scalefactor, digits, nsmall, big.mark, trim, scientific, prefix, suffix, scale, ...)
  the_function <- function(x) {
    x_fmt <- format(abs(x*scalefactor*scale), digits = digits, nsmall = nsmall, big.mark = big.mark,
                    trim = trim, scientific = scientific, ...)
    if (prefix != '' & trim == FALSE) {
      # If we need a prefix but we have blank-space padding at the start, add the prefix after
      first_nonspace <- regexpr('\\S',x_fmt)
      x_fmt <- sapply(1:length(x_fmt),
                      function(i) paste0(
                        c(rep(' ', first_nonspace[i]-1),
                          ifelse(x*scalefactor*scale < 0, '-',''),
                          prefix,
                          substr(x_fmt[i],first_nonspace[i],nchar(x_fmt[i]))),
                        suffix, collapse = ''))
    } else {
      x_fmt <- paste0(ifelse(x*scalefactor*scale < 0, '-',''), prefix, x_fmt, suffix)
    }
    x_fmt[is.na(x)] <- ''
    return(x_fmt)
  }
  attr(the_function, 'big.mark') <- big.mark
  return(the_function)
}


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

#' Weighted standard deviation
#'
#' This is a basic weighted standard deviation function, mainly for internal use with \code{sumtable}. For a more fully-fledged weighted SD function, see \code{Hmisc::wtd.var}, although it uses a slightly differend degree-of-freedom correction.
#'
#' @param x A numeric vector.
#' @param w A vector of weights. Negative weights are not allowed.
#' @param na.rm Set to \code{TRUE} to remove indices with missing values in \code{x} or \code{w}.
#' @examples
#' x <- c(1, 1, 2, 3, 4, 4, 4)
#' w <- c(4, 1, 3, 7, 0, 2, 5)
#' weighted.sd(x, w)
#'
#' @export weighted.sd
weighted.sd <- function(x, w, na.rm = TRUE) {
  if (length(x) != length(w)) {
    stop('Weights and data must be the same length.')
  }
  if (min(w) < 0) {
    stop('Negative weights found.')
  }
  if (na.rm) {
    missings <- is.na(x) | is.na(w)
    x <- x[!missings]
    w <- w[!missings]
  }
  weightsum <- sum(w)
  if (weightsum == 0) {
    stop('Weights sum to 0 among nonmissing data.')
  }
  mean_x <- sum(w*x)/weightsum
  num_nonzero <- sum(w > 0)

  var_x <- sum(w*((x-mean_x)^2))/(weightsum*(num_nonzero-1)/num_nonzero)
  sd_x <- sqrt(var_x)

  return(sd_x)
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
#' If \code{big.mark} is specified, will return a formatted string instead of a number, where the formatting is based on \code{format(x, big.mark = big.mark, scientific = FALSE, ...)}.
#'
#' @param x A vector.
#' @param big.mark Argument to pass to \code{format()}, if a formatted string is desired.
#' @param scientific Argument to pass to \code{format()} if \code{big.mark} is specified. Defaults to \code{FALSE}, unlike in \code{format()}.
#' @param ... Other arguments to pass to \code{format()}. Ignored if \code{big.mark} is not specified.
#' @examples
#' x <- c(1, 1, NA, 2, 3, NA)
#' notNA(x)
#' notNA(1:10000, big.mark = ',')
#' @export
notNA <- function(x, big.mark = NULL, scientific = FALSE, ...) {
  if (is.null(big.mark)) {
    return(sum(!is.na(x)))
  } else {
    return(format(sum(!is.na(x)), big.mark = big.mark, scientific = scientific, ...))
  }
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
  stats::quantile(x,1:100/100)
}

#' Checks if information is lost by rounding
#'
#' This function takes a vector and checks if any information is lost by rounding to a certain number of digits.
#'
#' Returns \code{TRUE} if rounding to \code{digits} digits after the decimal can be done without losing information.
#'
#' @param x A vector.
#' @param digits How many digits to round to.
#' @examples
#' is.round(1:5)
#'
#' x <- c(1, 1.2, 1.23)
#' is.round(x)
#' is.round(x,digits=2)
#' @export
is.round <- function(x,digits=0) {
  !any(!(x == round(x,digits)))
}

# Evaluate a series of functions
#
# Internal for summ, evaluates a function while allowing for the possibility that the class isn't right to evaluate that function
parsesumm <- function(x,summuse,summnames) {

  # Run each of the functions on the variable and get results
  results <- lapply(summuse, function(y) parsefcn(x,y))

  # If it's a number, round it
  results <- lapply(results, function(y) if(is.numeric(y)) { round(y,3) } else { y })

  # Get rid of functions that evaluated to NA (i.e. don't work)
  summnames <- summnames[!is.na(results)]
  results <- results[!is.na(results)]

  # Paste together
  results <- paste(summnames,sapply(results, as.character), sep = "")

  # And bring it all together with a break between each
  return(paste0(results, collapse = '<br/>'))
}

# Evaluate a function allowing it to not work
parsefcn <- function(x,y, ...) {
  list2env(list(...),envir=environment())
  result <- suppressWarnings(try(eval(parse(text=y)),silent = TRUE))

  if (inherits(result,'try-error')) {
    result <- NA
  }

  return(result)
}

# Evaluate a function allowing it to not work
# Special version for sumtable that does the NA-dropping internally
parsefcn_summ <- function(x,y, ...) {
  list2env(list(...),envir=environment())
  if (!any(sapply(c('anyNA','propNA','countNA'),function(z) grepl(z,y)))) {
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
                        factor.count,factor.numeric,digits,fixed.digits, wts = NULL,
                        fmt = NULL, skip.format = NULL, factor.not.numeric.count = notNA) {

  numcols <- length(summ)
  if (cla == 'header') {
    st[1,] <- c(
                paste0(title,'_MULTICOL_l_all'),
                rep('DELETECELL',numcols))
  } else if (cla == 'factor' & !factor.numeric) {
    #Get data
    va <- data[[var]]
    #Total number of obs
    nonmiss <- factor.not.numeric.count(va)
    nonmissnum <- notNA(va)
    #Header row
    st[1,] <- c(title,nonmiss,
                #Take this out for now.
                #ifelse(factor.percent,'100%','1'),
                '',
                rep('',numcols-2))
    #And now the per-factor stuff
    mat <- as.data.frame(table(va))
    #aggregate can't handle all-NAs
    if (nonmissnum > 0) {
      matlabel <- stats::aggregate(y~x, data.frame(y = 1, x = va), FUN = factor.not.numeric.count, drop = FALSE)
      matlabel$y[is.na(matlabel$y)] <- 0
      propcalc <- mat$Freq/nonmissnum
      if (!is.null(wts) & grepl('wts',summ[2])) {
        propcalc <- sapply(mat$va, function(x) stats::weighted.mean(va == x, w = wts, na.rm = TRUE))
      }
    } else {
      matlabel <- stats::aggregate(y~x, data.frame(y = 1, x = factor(levels(va), levels = levels(va))), FUN = factor.not.numeric.count, drop = FALSE)
      matlabel$y <- 0
      propcalc <- rep(NA,length(mat$Freq))
    }
    propcalc <- propcalc*(100^factor.percent)
    mat$va <- paste('...',mat$va)
    mat$Freq <- matlabel$y
    if (fixed.digits) {
      mat$Prop <- sapply(1:length(propcalc), function(x)
        format(propcalc[x],
               digits=max(digits[2]-2*factor.percent,1),
               nsmall=max(digits[2]-2*factor.percent,0),
               scientific = FALSE))
      st[1,2] <- format(as.numeric(st[1,2]),
                                   max(digits = digits[1],1),
                                   nsmall = digits[1],
                        scientific = FALSE)
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
    if (nonmissnum == 0) {
      mat$Prop <- ''
    }
    names(mat) <- names(st)
    st <- rbind(st,mat)
  } else if (cla == 'factor' & factor.numeric) {
    #Header row
    st[1,] <- c(title,rep('',numcols))
    #Get data
    va <- data[,var]
    #Create dummies to treat as numeric
    mat <- stats::model.matrix(~-1+va)
    #And names
    facnames <- paste('...',levels(va))
    #Run each of the functions on the variable and get results
    results <- lapply(1:ncol(mat),
                      function(x) lapply(summ, function(y) parsefcn_summ(mat[,x],y, wts = wts[!is.na(va)])))
    #Format
    results <- lapply(1:length(results),
                      function(x) sapply(1:length(results[[x]]), function(y) ifelse(summ[y] %in% skip.format, results[[x]][[y]], fmt(results[[x]][[y]]))))
    #Round
    if (paste0(deparse(fmt), collapse = '') == 'function (x) x') {
      if (fixed.digits) {
        results <- lapply(results, function(x)
          sapply(1:length(x), function(y) ifelse(is.character(x[y]), x[y], format(x[y],digits=digits[y],nsmall = max(digits[y],1),scientific=FALSE))))
      } else {
        results <- lapply(results, function(x)
          sapply(1:length(x), function(y) ifelse(is.character(x[y]), x[y], as.character(round(x[y],digits=digits[y])))))
      }
    }
    #Add factor names
    results <- lapply(1:length(results), function(x) c(facnames[x],results[[x]]))
    #And construct
    results <- as.data.frame(do.call(rbind,results))
    names(results) <- names(st)
    st <- rbind(st,results)
  } else {
    #Get data
    va <- data[[var]]

    #Run each of the functions on the variable and get results
    results <- lapply(summ, function(y) parsefcn_summ(va,y, wts = wts[!is.na(va)]))

    #Format
    results <- lapply(1:length(results), function(y) ifelse(summ[y] %in% skip.format, results[[y]], fmt(results[[y]])))

    # If our formatting function was function(x) x, apply digits options
    if (paste0(deparse(fmt), collapse = '') == 'function (x) x') {
      if (fixed.digits) {
        results <- sapply(1:length(results), function(y) ifelse(is.character(results[[y]]), results[[y]], format(results[[y]],digits=max(digits[y],1),nsmall = digits[y],scientific = FALSE)))
      } else {
        results <- sapply(1:length(results), function(y) ifelse(is.character(results[[y]]), results[[y]], round(results[[y]],digits=max(digits[y],1))))
      }
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

#For a table that is going to be seen "raw", remove all the multicolumn stuff
clean_multicol <- function(df) {
  df[1,1] <- gsub('HEADERROW','',df[1,1])

  clean_content <- function(x) {
    x <- sapply(x, function(y) gsub('DELETECELL','',y))

    x <- sapply(x, function(y) ifelse(grepl('_MULTICOL_',y),
                                      substr(y,1,gregexpr('_MULTICOL_',y)[[1]]-1),
                                      y))
  }

  for (i in 1:ncol(df)) {
    df[[i]] <- clean_content(df[[i]])
  }
  names(df) <- clean_content(names(df))
  return(df)
}

#For a table that is going to be seen "raw", remove all the multicolumn stuff
#Except for the top row which can become a multi-column header. Good for groups!
clean_multicol_kable <- function(df,title,note=NA) {
  # If the first row is a header, chop it off and save for later
  hasheader <- FALSE
  if (grepl('HEADERROW',df[1,1])) {
    headerrow <- df[1,]
    df <- df[2:nrow(df),]
    hasheader <- TRUE
  }
  df[1,1] <- gsub('HEADERROW','',df[1,1])

  clean_content <- function(x) {
    x <- sapply(x, function(y) gsub('DELETECELL','',y))

    x <- sapply(x, function(y) ifelse(grepl('_MULTICOL_',y),
                                      substr(y,1,gregexpr('_MULTICOL_',y)[[1]]-1),
                                      y))
  }

  for (i in 1:ncol(df)) {
    df[[i]] <- clean_content(df[[i]])
  }
  names(df) <- clean_content(names(df))

  # For this one, directly return the kable
  if (knitr::is_html_output()) {
    fmt <- 'html'
  } else if (knitr::is_latex_output()) {
    fmt <- 'latex'
  } else {
    fmt <- NULL
  }

  # And format the header if there is one
  if (hasheader) {
    # Get rid of deleted cells
    if (is.null(fmt)) {
      headerrow[headerrow == 'DELETECELL'] = ''
    } else {
      headerrow <- headerrow[headerrow != 'DELETECELL']
    }
    # HEADERROW itself is blank
    headerrow <- gsub('HEADERROW','',headerrow)

    # No alignment control anyway
    headerrow <- gsub('_c_','_l_',headerrow)
    headerrow <- gsub('_r_','_l_',headerrow)
  }

  if (is.null(fmt)) {
    if (hasheader) {
      headerrow <- gsub('_MULTICOL.*$','',headerrow)
      names(headerrow) = names(df)
      df = rbind(headerrow, df)
    }
    kb <- knitr::kable(df, caption = title, row.names = FALSE)
  } else if (fmt == 'html') {
    # escape by hand bc of test column which should not be escaped
    cols_to_escape <- 1:ncol(df)
    cols_to_escape <- cols_to_escape[names(df) != 'Test']
    for (c in cols_to_escape) {
      df[[c]] <- gsub("&", "&amp;", df[[c]], fixed = TRUE)
      df[[c]] <- gsub("<", "&lt;", df[[c]], fixed = TRUE)
      df[[c]] <- gsub(">", "&gt;", df[[c]], fixed = TRUE)
    }
    kb <- knitr::kable(df, caption = title, row.names = FALSE, format = fmt, escape = FALSE)
  } else if (fmt == 'latex') {
    # escape by hand bc of test column which should not be escaped
    cols_to_escape <- 1:ncol(df)
    cols_to_escape <- cols_to_escape[names(df) != 'Test']
    for (c in cols_to_escape) {
      # do backslash separately so we don't escape the backslashes we write
      df[[c]] <- gsub("\\\\", "\\\\\\\\", df[[c]])
      df[[c]] <- gsub("([&%$#_\\{\\}~^])", "\\\\\\1", df[[c]])
    }
    kb <- knitr::kable(df, caption = title, row.names = FALSE, format = fmt, booktabs = TRUE, escape = FALSE)
  }

  # And now add the header
  if (hasheader & !is.null(fmt)) {
    headercol <- eval(parse(text=paste('c(',
                                       paste(
                                         sapply(headerrow, FUN = function(x)
                                           ifelse(grepl('_MULTICOL_l_',x),
                                                  paste0('"',strsplit(x,'_MULTICOL_l_')[[1]][1],'"=',strsplit(x,'_MULTICOL_l_')[[1]][2]),
                                                  paste0('"',x,'"'))),
                                         collapse = ','),
                                       ')')))

    kb <- kableExtra::add_header_above(kb,headercol)
  }

  if (!is.na(note)) {
    kb <- kableExtra::add_footnote(kb, note, notation = 'none')
  }

  return(kb)
}


