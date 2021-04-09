#' Summary Table Function
#'
#' This function will output a summary statistics variable table either to the console or as an HTML file that can be viewed continuously while working with data, or sent to file for use elsewhere. \code{st()} is the same thing but requires fewer key presses to type.
#'
#' There are many, many functions in R that will produce a summary statisics table for you. So why use \code{sumtable()}? \code{sumtable()} serves two main purposes:
#'
#' (1) In the same spirit as \code{vtable()}, it makes it easy to view the summary statistics \emph{as you work}, either in the Viewer pane or in a browser window.
#'
#' (2) \code{sumtable()} is designed to \emph{have nice defaults} and is not really intended for deep customization. It's got lots of options, sure, but they're only intended to go so far. So you can have a summary statistics table without much work.
#'
#' Keeping with point (2), \code{sumtable()} is designed for use by people who want the kind of table that \code{sumtable()} produces, which is itself heavily influenced by the kinds of summary statistics tables you often see in economics papers. In that regard it is most similar to \code{stargazer::stargazer()} except that it can handle tibbles, factor variables, grouping, and produce multicolumn tables, or \code{summarytools::dfSummary()} or \code{skimr::skim()} except that it is easier to export with nice formatting. If you want a lot of control over your summary statistics table, check out the packages gtsummary, arsenal, qwraps2, or Amisc, and about a million more.
#'
#' If you would like to include a \code{sumtable} in an RMarkdown document, it should just work! If you leave \code{out} blank, it will default to a nicely-formatted \code{knitr::kable()}, although this will drop some formatting elements like multi-column cells (or do \code{out="kable"} to get an unformatted \code{kable} that you can format yourself). If you prefer the \code{vtable} package formatting, then use \code{out="latex"} if outputting to LaTeX or \code{out="htmlreturn"} for HTML, both with \code{results="asis"} in the code chunk. Alternately, in HTML, you can use the \code{file} option to write to file and use a \code{<iframe>} to include it.
#'
#' @param data Data set; accepts any format with column names.
#' @param vars Character vector of column names to include, in the order you'd like them included. Defaults to all numeric, factor, and logical variables, plus any character variables with six or fewer unique values. You can include strings that aren't columns in the data (including blanks) - these will create rows that are blank except for the string (left-aligned), for spacers or subtitles.
#' @param out Determines where the completed table is sent. Set to \code{"browser"} to open HTML file in browser using \code{browseURL()}, \code{"viewer"} to open in RStudio viewer using \code{viewer()}, if available. Use \code{"htmlreturn"} to return the HTML code to R, \code{"latex"} to return LaTeX code to R (use \code{"latexdoc"} to get a full buildable document rather than a fragment), \code{"return"} to return the completed summary table to R in data frame form, or \code{"kable"} to return it in \code{knitr::kable()} form. Combine \code{out = "csv"} with \code{file} to write to CSV (dropping most formatting). Defaults to \code{"viewer"} if RStudio is running, \code{"browser"} if it isn't,  or a \code{"kable"} passed through \code{kableExtra::kable_styling()} defaults if it's an RMarkdown document being built with \code{knitr}.
#' @param file Saves the completed summary table file to file with this filepath. May be combined with any value of \code{out}, although note that \code{out = "return"} and \code{out = "kable"} will still save the standard sumtable HTML file as with \code{out = "viewer"} or \code{out = "browser"}.
#' @param summ Character vector of summary statistics to include for numeric and logical variables, in the form \code{'function(x)'}. Defaults to \code{c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)')} if there's one column, or \code{c('notNA(x)','mean(x)','sd(x)')} if there's more than one. If all variables in a column are factors it defaults to \code{c('sum(x)','mean(x)')} for the factor dummies. If the table has multiple variable-columns and you want different statistics in each, include a list of character vectors instead. This option is flexible, and allows any summary statistic function that takes in a column and returns a single number. For example, \code{summ=c('mean(x)','mean(log(x))')} will provide the mean of each variable as well as the mean of the log of each variable. Keep in mind the special vtable package helper functions designed specifically for this option \code{propNA}, \code{countNA}, and \code{notNA}, which report counts and proportions of NAs, or counts of not-NAs, in the vectors, \code{nuniq}, which reports the number of unique values, and \code{pctile}, which returns a vector of the 100 percentiles of the variable. NAs will be omitted from all calculations other than \code{propNA(x)} and \code{countNA(x)}.
#' @param summ.names Character vector of names for the summary statistics included. If \code{summ} is at default, defaults to \code{c('N','Mean','Std. Dev.','Min','Pctl. 25','Pctl. 75','Max')} (or the appropriate shortened version with multiple columns) unless all variables in the column are factors in which case it defaults to \code{c('N','Percent')}. If \code{summ} has been set but \code{summ.names} has not, defaults to \code{summ} with the \code{(x)}s removed and the first letter capitalized.  If the table has multiple variable-columns and you want different statistics in each, include a list of character vectors instead.
#' @param add.median Adds \code{"median(x)"} to the set of default summary statistics. Has no effect if \code{"summ"} is also specified.
#' @param group Character variable with the name of a column in the data set that statistics are to be calculated over. Value labels will be used if found for numeric variables. Changes the default \code{summ} to \code{c('mean(x)','sd(x)')}.
#' @param group.long By default, if \code{group} is specified, each group will get its own set of columns. Set \code{group.long = TRUE} to instead basically just make a regular \code{sumtable()} for each group and stack them on top of each other. Good for when you have lots of groups. You can also set it to \code{'l'}, \code{'c'}, or \code{'r'} to determine how the group names are aligned. Defaults to centered.
#' @param group.test Set to \code{TRUE} to perform tests of whether each variable in the table varies over values of \code{group}. Only works with \code{group.long = FALSE}. Performs a joint F-test (using \code{anova(lm))}) for numeric variables, and a Chi-square test of independence (\code{chisq.test}) for categorical variables. If you want to adjust things like which tests are used, significance star levels, etc., see the help file for \code{independence.test} and pass in a named list of options for that function.
#' @param group.weights \emph{THIS OPTION DOES NOT AUTOMATICALLY WEIGHT ALL CALCULATIONS.} This is mostly to be used with \code{group} and \code{group.long = FALSE}, and while it's more flexible than that, you've gotta read this to figure out how else to use it. That's why I gave it the weird name. Set this to a vector of weights, or a string representing a column name with weights. If \code{summ} is not customized, this will replace \code{'mean(x)'} and \code{'sd(x)'} with the equivalent weighted versions \code{'weighted.mean(x, w = wts)'} and \code{'weighted.sd(x, w = wts)'} It will also add weights to the default \code{group.test} tests. This will not add weights to any other calculations, or to any custom \code{group.test} weights (although you can always do that yourself by customizing \code{summ} and passing in weights with this argument-the weights can be referred to in your function as \code{wts}). This is generally intended for things like post-matching balance tables. If you specify a column name, that column will be removed from the rest of the table, so if you want it to be kept, specify this as a numeric vector instead. If you have a variable in your data called \code{'wts'} that will mess the use of this option up, I recommend changing that.
#' @param col.breaks Numeric vector indicating the variables (or number of elements of \code{vars}) after which to start a new column. So for example with a data set with six variables, \code{c(3,5)} would put the first three variables in the first column, the next two in the middle, and the last on the right. Cannot be combined with \code{group} unless \code{group.long = TRUE}.
#' @param digits Number of digits after the decimal place to report. Set to a single number for consistent digits, or a vector the same length as \code{summ} for different digits for each calculation, or a list of vectors that match up to a multi-column \code{summ}. Defaults to 0 for the first calculation and 2 afterwards.
#' @param fixed.digits \code{FALSE} will cut off trailing \code{0}s when rounding. \code{TRUE} retains them. Defaults to \code{FALSE}.
#' @param factor.percent Set to \code{TRUE} to show factor means as percentages instead of proportions, i.e. \code{50\%} with a column header of "Percent" rather than \code{.5} with a column header of "Mean". Defaults to \code{TRUE}.
#' @param factor.counts Set to \code{TRUE} to show a count of each factor level in the first column. Defaults to \code{TRUE}.
#' @param factor.numeric By default, factor variable dummies basically ignore the \code{summ} argument and show count (or nothing) in the first column and percent or proportion in the second. Set this to \code{TRUE} to instead treat the dummies like numeric binary variables with values 0 and 1.
#' @param logical.numeric By default, logical variables are treated as factors with \code{TRUE = "Yes"} and \code{FALSE = "No"}. Set this to \code{FALSE} to instead treat them as numeric variables rather than factors, with \code{TRUE = 1} and \code{FALSE = 0}.
#' @param logical.labels When turning logicals into factors, use these labels for \code{FALSE} and \code{TRUE}, respectively, rather than "No" and "Yes".
#' @param labels Variable labels. labels will accept four formats: (1) A vector of the same length as the number of variables in the data that will be included in the table (tricky to use if many are being dropped, also won't work for your \code{group} variable), in the same order as the variables in the data set, (2) A matrix or data frame with two columns and more than one row, where the first column contains variable names (in any order) and the second contains labels, (3) A matrix or data frame where the column names (in any order) contain variable names and the first row contains labels, or (4) TRUE to look in the data for variable labels set by the haven package, \code{set_label()} from sjlabelled, or \code{label()} from Hmisc.
#' @param title Character variable with the title of the table.
#' @param note Table note to go after the last row of the table. Will follow significance star note if \code{group.test = TRUE}.
#' @param note.align Set the alignment for the multi-column table note. Usually "l", but if you have a long note in LaTeX you might want to set it with "p{}"
#' @param anchor Character variable to be used to set an anchor link in HTML tables, or a label tag in LaTeX.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in an HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @param col.align For HTML output, a character vector indicating the HTML \code{text-align} attributes to be used in the table (for example \code{col.align = c('left','center','center')}. Defaults to variable-name columns left-aligned and all others right-aligned (with a little extra padding between columns with \code{col.breaks}). If you want to get tricky, you can add a \code{";"} afterwards and keep putting in whatever CSS attributes you want. They will be applied to the whole column.
#' @param align For LaTeX output, string indicating the alignment of each column. Use standard LaTeX syntax (i.e. \code{l|ccc}). Defaults to left in the first column and right-aligned afterwards, with \code{@{\\hskip .2in}} spacers if you have \code{col.breaks}. If \code{col.width} is specified, defaults to all \code{p{}} columns with widths set by \code{col.width}. If you want the columns aligned on a decimal point, see \href{https://tex.stackexchange.com/questions/2746/aligning-numbers-by-decimal-points-in-table-columns#2747}{this explainer}.
#' @param fit.page For LaTeX output, uses a resizebox to force the table to a certain width. Set to \code{NA} to omit.
#' @param simple.kable For \code{out = 'kable'}, if you want the \code{kable} printed to console rather than HTML or PDF, then the multi-column headers and table notes won't work. Set \code{simple.kable = TRUE} to skip both.
#' @param opts The same \code{sumtable} options as above, but in a named list format. Useful for applying the same set of options to multiple \code{sumtable}s.
#' @examples
#' # Examples are only run interactively because they open HTML pages in Viewer or a browser.
#' if (interactive()) {
#' data(iris)
#'
#' # Sumtable handles both numeric and factor variables
#' st(iris)
#'
#' # Output to LaTeX as well for easy integration
#' # with RMarkdown, or \input{} into your LaTeX docs
#' # (specify file too  to save the result)
#' st(iris, out = 'latex')
#'
#' # Summary statistics by group
#' iris$SL.above.median <- iris$Sepal.Length > median(iris$Sepal.Length)
#' st(iris, group = 'SL.above.median')
#'
#' # Add a group test, or report by-group in "long" format
#' st(iris, group = 'SL.above.median', group.test = TRUE)
#' st(iris, group = 'SL.above.median', group.long = TRUE)
#'
#' # Going all out! Adding variable labels with labels,
#' # spacers and variable "category" titles with vars,
#' # Changing the presentation of the factor variable,
#' # and putting the factor in its own column with col.breaks
#' var.labs <- data.frame(var = c('SL.above.median','Sepal.Length',
#'                                'Sepal.Width','Petal.Length',
#'                                'Petal.Width'),
#'                        labels = c('Above-median Sepal Length','Sepal Length',
#'                        'Sepal Width','Petal Length',
#'                        'Petal Width'))
#' st(iris,
#'     labels = var.labs,
#'     vars = c('Sepal Variables','SL.above.median','Sepal.Length','Sepal.Width',
#'     'Petal Variables','Petal.Length','Petal.Width',
#'     'Species'),
#'     factor.percent = FALSE,
#'     col.breaks = 7)
#'
#' }
#' @rdname sumtable
#' @export

sumtable <- function(data,vars=NA,out=NA,file=NA,
                     summ=NA,
                     summ.names=NA,
                     add.median = FALSE,
                     group=NA,group.long=FALSE,group.test=FALSE,group.weights=NA,
                     col.breaks=NA,
                     digits=NA,fixed.digits=FALSE,factor.percent=TRUE,
                     factor.counts=TRUE,factor.numeric=FALSE,
                     logical.numeric=FALSE,logical.labels=c("No","Yes"),labels=NA,title='Summary Statistics',
                     note = NA, anchor=NA,col.width=NA,col.align=NA,
                     align=NA, note.align = 'l', fit.page = '\\textwidth', simple.kable=FALSE,opts=list()) {
  #Bring in opts
  list2env(opts,envir=environment())
  #######CHECK INPUTS
  if (is.null(colnames(data))) {
    stop('Requires data with variable names or column names.')
  }
  if (!is.na(file) & !is.character(file)) {
    stop('Incorrect file name.')
  }
  if (!identical(vars,NA) & !is.character(vars)) {
    stop('vars must be a character vector.')
  }
  if (!identical(note,NA) & !is.character(note)) {
    stop('note must be a character vector.')
  }
  if (!identical(anchor,NA) & !is.character(anchor)) {
    stop('anchor must be a character variable.')
  }
  if (min(is.na(col.width)) == 0 & (!is.vector(col.width) | !is.numeric(col.width) | sum(is.na(col.width)) > 0)) {
    stop('col.width must be a numeric vector with no missing values.')
  }
  if (min(is.na(col.width)) == 0 & (max(col.width) > 100 | min(col.width) < 0)) {
    stop('Elements of col.width must be between 0 and 100.')
  }
  if (!is.logical(add.median)) {
    stop('add.median must be TRUE or FALSE.')
  }
  if (!is.list(summ)) {
    if (min(is.na(summ)) == 0 & (!is.vector(summ) | !is.character(summ) | sum(is.na(summ)) > 0)) {
      stop('summ must be a character vector with no missing values.')
    }
  }
  if (!is.list(summ.names)) {
    if (min(is.na(summ.names)) == 0 & (!is.vector(summ.names) | !is.character(summ.names) | sum(is.na(summ.names)) > 0)) {
      stop('summ.names must be a character vector with no missing values.')
    }
  }
  if (!is.na(group) & !is.character(group)) {
    stop('group must be a string referring to a grouping variable in the data.')

    if (!(group %in% colnames(data))) {
      stop('group must be a column name in the data.')
    }
  }
  if (!is.logical(group.test) & !is.list(group.test)) {
    stop('group.test must be TRUE, FALSE, or a named list of options to pass to independence.test\'s opts argument.')
  }
  if (!identical(group.test,FALSE) & is.na(group)) {
    warning('group.test will be ignored, since no group is set.')
  }
  if (!identical(group.test,FALSE) & group.long == TRUE) {
    warning('group.test is incompatible with group.long == TRUE and will be ignored.')
  }
  if (!is.logical(factor.numeric) | !is.logical(logical.numeric)) {
    stop("factor.numeric and logical.numeric must each be TRUE or FALSE")
  }
  if (is.logical(group.long)) {
    group.long.align <- 'c'
  } else if (is.character(group.long)) {
    if (group.long %in% c('l','r','c')) {
      group.long.align <- group.long
      group.long <- TRUE
    } else {
      stop('group.long must be TRUE, FALSE, or a character l, c, or r.')
    }
  } else {
    stop('group.long must be TRUE, FALSE, or a character l, c, or r.')
  }
  if (!is.logical(fixed.digits)) {
    stop('fixed.digits must be TRUE or FALSE.')
  }
  if (!is.numeric(col.breaks) & !identical(col.breaks,NA)) {
    stop('col.breaks must be numeric.')
  }
  if (!is.na(group) & !identical(col.breaks,NA) & group.long == FALSE) {
    stop('group cannot be combined with col.breaks unless group.long = TRUE.')
  }
  if (!is.numeric(digits) & !is.list(digits) & !identical(digits,NA)) {
    stop('digits must be numeric.')
  }
  if (!is.logical(factor.percent) | !is.logical(factor.counts)) {
    stop('factor.percent and factor.counts must each be TRUE or FALSE.')
  }
  if (!is.character(title)) {
    stop('title must be a character variable.')
  }
  if (identical(out, 'csv') & is.na(file)) {
    warning('out = "csv" will just return the vtable as a data.frame unless combined with file')
  }

  # Weights
  wts <- NULL
  if (length(group.weights) > 1) {
    wts <- group.weights
  }
  if (length(group.weights) == 1) {
    if (is.character(group.weights)) {
      wts <- data[[group.weights]]
      data[[group.weights]] <- NULL
    }
  }
  if (!identical(group.weights, NA) & is.null(wts)) {
    stop('group.weights must be a vector of length nrow(data), or the name of a column in data')
  }
  if (!is.numeric(wts) & !is.null(wts)) {
    stop('group.weights must be numeric.')
  }
  if ((length(wts) != nrow(data)) & !is.null(wts)) {
    stop('group.weights must be the same length as the number of rows in data')
  }
  if (!is.null(wts)) {
    if (min(wts, na.rm = TRUE) < 0) {
      stop('No negative weights allowed in group.weights')
    }
  }
  if (!is.null(wts)) {
    # Drop missing values
    havewts <- !is.na(wts)
    wts <- wts[havewts]
    data <- subset(data, havewts)
  }

  #One-column matrices run into some problems later on
  if (is.matrix(data) & dim(data)[2] == 1) {
    data <- as.data.frame(data)
  }

  #######CONVERT ALL LABELED NUMERIC VARIABLES AND LOW-NUNIQ CHARACTERS TO FACTORS
  #Get classes of each variable, only caring about numeric/character/factor/logical/other
  var.classes <- sapply(data, function(x) ifelse(
    is.factor(x), 'factor', ifelse(
      is.logical(x), 'logical', ifelse(
        is.character(x), 'character', ifelse(
          is.numeric(x), 'numeric', 'other')))))

  labwarning <- FALSE
  for (c in 1:ncol(data)) {
    #Factorize each character variable with six or fewer unique values
    if (var.classes[c] == 'character') {
      if (vtable::nuniq(data[[c]]) <= 6) {
        data[[c]] <- as.factor(data[[c]])
      }
    } else if (var.classes[c] == 'logical') {
      #Turn logicals to numerics if logical.numeric = FALSE
      if (logical.numeric) {
        data[[c]] <- as.numeric(data[[c]])
      } else {
        # Otherwise make them factors
        data[[c]] <- factor(data[[c]], levels = c(FALSE,TRUE), labels = logical.labels)
      }
    } else if (var.classes[c] == 'numeric') {
      # If a numeric variable has value labels, turn this into a factor
      if ('labelled' %in% class(data[[c]]) | ('haven_labelled' %in% class(data[[c]]) | !is.null(unlist(sjlabelled::get_labels(data[[c]]))))) {
        #DON'T include variables with unlabelled values
        unlabvals <-  length(sjlabelled::get_labels(data[[c]])) == length(sjlabelled::get_labels(data[[c]], non.labelled = TRUE))
        if (!unlabvals) {
          data[[c]] <- as.numeric(data[[c]])
          labwarning <- TRUE
        } else {
          #Turn into the appropriately-titled factor
          suppressWarnings(data[[c]] <- sjlabelled::as_label(data[,c,drop=FALSE]))
        }
      }
    }
  }
  if (labwarning) {
    warning('Some labelled variables have unlabeled values. Treating these as numeric variables and ignoring labels.')
  }

  #Re-get classes of each variable, only caring about numeric/character/factor/logical
  var.classes <- sapply(data, function(x) ifelse(
    is.factor(x), 'factor', ifelse(
      is.logical(x), 'logical', ifelse(
        is.character(x), 'character', ifelse(
          is.numeric(x), 'numeric', 'other')))))

  #Do we have factor and also a potentially-non-compliant summ?
  factor.warning <- FALSE
  if (any(var.classes == 'factor') & !identical(summ,NA) & !factor.numeric) {
    if (is.list(summ) & !identical(col.breaks,NA)) {
      ext.col.breaks <- c(1,col.breaks,ncol(data))
      for (i in 1:length(summ)) {
        if ((!(summ[[i]][1] %in% c('length(x)','notNA(x)')) |
            !(summ[[i]][2] %in% 'mean(x)')) &
            any(var.classes[ext.col.breaks[i]:ext.col.breaks[i+1]] == 'factor')) {
          factor.warning <- TRUE
        }
      }
    } else if (!is.list(summ)) {
      if (!(summ[1] %in% c('length(x)','notNA(x)')) |
          !(summ[2] %in% 'mean(x)')) {
        factor.warning <- TRUE
      }
    } else {
      if (!(summ[[1]][1] %in% c('length(x)','notNA(x)')) |
          !(summ[[1]][2] %in% 'mean(x)')) {
        factor.warning <- TRUE
      }
    }
  }
  if (factor.warning) {
    warning('Factor variables ignore custom summ options. Cols 1 and 2 are count and percentage.\nBeware combining factors with a custom summ unless factor.numeric = TRUE.')
  }

  #######DEFAULTS
  if (identical(vars,NA)) {
    #By default, include numeric and factor vars (includable logicals and characters already converted)
    colkeeps <- sapply(1:ncol(data), function(x) ifelse(
      is.factor(data[[x]]) | is.numeric(data[[x]]), x, 0))
    if (sum(colkeeps > 0) == 0) {
      stop('It doesn\'t look like you have any variables that belong in a sumtable. Check your data. Use vars to explicitly choose variables, or convert things to numeric or factor before sending to sumtable.')
    }
    vars <- names(data)[colkeeps[colkeeps > 0]]

    #But not whatever is being used for group!
    if (!is.na(group)) {
      vars <- vars[vars != group]
    }

    var.classes <- sapply(as.data.frame(data[,vars]), function(x) ifelse(
      is.factor(x), 'factor', 'numeric'))
  } else {
    #Note that if vars is explicitly defined it might contain non-variables
    var.classes <- sapply(vars, function(x) ifelse(
      !(x %in% names(data)), 'header', ifelse(
        is.factor(data[[x]]), 'factor', ifelse(
          is.logical(data[[x]]), 'logical', ifelse(
            is.character(data[[x]]), 'character', ifelse(
              is.numeric(data[[x]]), 'numeric', 'other'))))))
  }
  if (identical(col.breaks,NA)) {
    col.breaks <- length(vars)
  }
  if (utils::tail(col.breaks,1) < length(vars)) {
    col.breaks[length(col.breaks) + 1] <- length(vars)
  }
  #Get a list of the variables that each column covers
  col.windows <- c(0,col.breaks)
  col.vars <- lapply(1:length(col.breaks), function(x) (col.windows[x]+1):col.breaks[x])

  #Summary function defaults, and fill in summ.names as well
  #Are we filling summ.names at the same time?
  fill.sn <- identical(summ.names,NA)
  if (identical(summ,NA)) {
    summ <- list()
    if (fill.sn) {
      summ.names <- list()
    }

    for (i in 1:length(col.vars)) {
      if (all(var.classes[col.vars[[i]]] == 'factor')) {
        summ[[i]] <- c('sum(x)','mean(x)')

        if (fill.sn & factor.percent) {
          summ.names[[i]] <- c('N','Percent')
        } else {
          summ.names[[i]] <- c('N','Mean')
        }

        # If there are weights
        if (!is.null(wts)) {
          summ[[i]] <- c('sum(x)', 'weighted.mean(x, w = wts)')
          if (fill.sn) {
            summ.names[[i]][2] <- paste0(summ.names[[i]][2], ' (Weighted)')
          }
        }
      } else if ((is.na(group) | group.long == TRUE) & length(col.breaks) == 1) {
        summ[[i]] <- c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','pctile(x)[75]','max(x)')

        if (fill.sn) {
          summ.names[[i]] <- c('N','Mean','Std. Dev.','Min','Pctl. 25','Pctl. 75','Max')
        }
        # Add median if desired
        if (add.median) {
          summ[[i]] <- c('notNA(x)','mean(x)','sd(x)','min(x)','pctile(x)[25]','median(x)','pctile(x)[75]','max(x)')

          if (fill.sn) {
            summ.names[[i]] <- c('N','Mean','Std. Dev.','Min','Pctl. 25','Pctl. 50', 'Pctl. 75','Max')
          }
        }

        # If there are weights
        if (!is.null(wts)) {
          summ[[i]][summ[[i]] == 'mean(x)'] <- 'weighted.mean(x, w = wts)'
          summ[[i]][summ[[i]] == 'sd(x)'] <- 'weighted.sd(x, w = wts)'
          if (fill.sn) {
            summ.names[[i]][summ.names[[i]] == 'Mean'] <- 'Wt. Mean'
            summ.names[[i]][summ.names[[i]] == 'Std. Dev.'] <- 'Wt. SD'
          }
        }

      } else if ((is.na(group) | group.long == TRUE) & length(col.breaks) > 1) {
        summ[[i]] <- c('notNA(x)','mean(x)','sd(x)')

        if (fill.sn) {
          summ.names[[i]] <- c('N','Mean','Std. Dev.')
        }
        if (add.median) {
          summ[[i]] <- c('notNA(x)','mean(x)','sd(x)', 'median(x)')

          if (fill.sn) {
            summ.names[[i]] <- c('N','Mean','Std. Dev.', 'Median')
          }
        }

        # If there are weights
        if (!is.null(wts)) {
          summ[[i]][summ[[i]] == 'mean(x)'] <- 'weighted.mean(x, w = wts)'
          summ[[i]][summ[[i]] == 'sd(x)'] <- 'weighted.sd(x, w = wts)'
          if (fill.sn) {
            summ.names[[i]][summ.names[[i]] == 'Mean'] <- 'Wt. Mean'
            summ.names[[i]][summ.names[[i]] == 'Std. Dev.'] <- 'Wt. SD'
          }
        }
      } else {
        summ[[i]] <- c('notNA(x)','mean(x)','sd(x)')

        if (fill.sn) {
          summ.names[[i]] <- c('N','Mean','SD')
        }
        if (add.median) {
          summ[[i]] <- c('notNA(x)','mean(x)','sd(x)', 'median(x)')

          if (fill.sn) {
            summ.names[[i]] <- c('N','Mean','SD', 'Median')
          }
        }

        # If there are weights
        if (!is.null(wts)) {
          summ[[i]][summ[[i]] == 'mean(x)'] <- 'weighted.mean(x, w = wts)'
          summ[[i]][summ[[i]] == 'sd(x)'] <- 'weighted.sd(x, w = wts)'
          if (fill.sn) {
            summ.names[[i]][summ.names[[i]] == 'Mean'] <- 'Wt. Mean'
            summ.names[[i]][summ.names[[i]] == 'SD'] <- 'Wt. SD'
          }
        }
      }
    }
  } else if (!is.list(summ)) {
    #If summ was entered as a vector, turn it into a list
    #And copy it if there are multiple columns
    summ <- lapply(1:length(col.vars), function(x) summ)
  }

  #Figure if digits started as a list or vector. If it did,
  #ignore the auto-zero-digits for integers
  digits.was.list <- is.list(digits)
  if (is.vector(digits)) {
    if (length(digits) > 1) {
      digits.was.list <- TRUE
    }
  }
  #Now fill in values for digits
  if (identical(digits,NA)) {
    digits <- list()
    for (i in 1:length(col.breaks)) {
      digits[[i]] <- rep(3,length(summ[[i]]))
      digits[[i]][1] <- 0
    }
  } else if (is.numeric(digits)) {
    if (length(digits) == 1) {
      digopt <- digits
      digits <- list()
      for (i in 1:length(col.breaks)) {
        digits[[i]] <- rep(digopt,length(summ[[i]]))
      }
    } else {
      digits <- lapply(1:length(col.breaks), function(x) digits)
    }
  }
  #If we have fixed.digits and digits weren't
  #explicitly set by list,
  #set digits to 0 for integers
  if (fixed.digits & !digits.was.list) {
    for (i in 1:length(summ)) {
      for (j in 1:length(summ[[i]])) {
        # Attempt to calc each variable for this function
        calcs <- sapply(vars, function(x) parsefcn_summ(data[[x]],summ[[i]][j]))
        calcs <- calcs[!is.na(calcs)]
        if (is.round(calcs) | summ[[i]][j] == 'notNA(x)') {
          digits[[i]][j] <- 0
        }
      }
    }
  }

  #And fill in summ.names the rest of the way
  #If a vector was specified for summ.names, turn it into a list
  if (!fill.sn & !is.list(summ.names)) {
    summ.names <- lapply(1:length(col.vars), function(x) summ.names)
  }
  #If summ.names is still missing, create it from summ
  if (identical(summ.names,NA)) {
    summ.names <- list()

    for (i in 1:length(col.vars)) {
      functionsused <- summ[[i]]
      functionsused <- sub('\\(x\\)','',functionsused)
      firstletters <- toupper(substring(functionsused,1,1))
      summ.names[[i]] <- paste0(firstletters,substring(functionsused,2))
    }
  }

  #group.test defaults
  #send the options to .opts, and make group.test be logical
  if (identical(group.test,TRUE)) {
    if (out %in% c('latex','latexpage') |
        (isTRUE(getOption('knitr.in.progress')) & is.na(out) & isTRUE(knitr::is_latex_output()))) {
      group.test.opts <- list(format = '{name}$={stat}^{{stars}}$')
    } else if (out %in% c('return','kable','csv') |
               (isTRUE(getOption('knitr.in.progress')) & is.na(out) & isFALSE(knitr::is_latex_output()) & isFALSE(knitr::is_html_output()))) {
      group.test.opts <- list(format = '{name}={stat}{stars}')
    } else {
      group.test.opts <- list(format = '{name}={stat}<sup>{stars}</sup>')
    }
  } else if (is.list(group.test)) {
    group.test.opts <- group.test
    group.test <- TRUE
  }
  starnote <- NA_character_

  ####### APPLY LABELS OPTION
  vartitles <- vars
  grouptitle <- group
  ####### APPLICATION OF LABELS OPTION
  #Pull from label attribute if present
  if (identical(labels,TRUE)) {
    labs <- sapply(vars, function(x) attr(data[[x]],'label'))
    has.no.labs <- unlist(sapply(labs, is.null))

    vartitles[!has.no.labs] <- unlist(labs[!has.no.labs])

    if (!is.na(group)) {
      if (!is.null(attr(data[[group]],'label'))) {
        grouptitle <- attr(data[[group]],'label')
      }
    }
  } else if (!identical(labels,NA)) {
    if (is.vector(labels)) {
      #Make sure it's the right length
      if (length(labels) == length(vars)) {
        vartitles[!is.na(labels)] <- labels[!is.na(labels)]
      } else {
        stop('label vector must have as many elements as there are variables as will be in the sumtable. Use NA elements to fill in, or see help(sumtable) for other label formats that do not require every variable to have a label.')
      }

      #Check for multi-row two-column format
    } else if(dim(labels)[1] > 1 & dim(labels)[2] == 2) {
      # What we have now
      temp.df <- data.frame(vars = vars, stringsAsFactors = FALSE)

      #Put labels in mergeable format
      labs <- as.data.frame(labels, stringsAsFactors = FALSE)
      names(labs) <- c('vars','vartitles')
      #They gotta be strings
      labs$vars <- as.character(labs$vars)
      labs$vartitles <- as.character(labs$vartitles)

      #Hold original order
      temp.df$order <- 1:nrow(temp.df)
      #Bring in variable labels by name, allow NA labels with all.x=TRUE
      temp.df <- merge(temp.df,labs,sort=FALSE,all.x=TRUE)
      temp.df <- temp.df[order(temp.df$order),]

      # Fill in the NAs with the column titles
      temp.df$vartitles[is.na(temp.df$vartitles)] <- temp.df$vars[is.na(temp.df$vartitles)]

      vartitles <- temp.df$vartitles

      if (!is.na(group)) {
        if (sum(labels[[1]] == group) > 0) {
          grouptitle <- labels[labels[[1]] == group,2]
        }
      }

      #Check if it's in the one-row variable-name format
    } else if (dim(labels)[1]==1 & !is.null(colnames(labels))) {
      #Put into two-column format
      labs <- data.frame(vars=colnames(labels),vartitles=as.character(t(labels[1,])),stringsAsFactors = FALSE)

      # What we have now
      temp.df <- data.frame(vars = vars, stringsAsFactors = FALSE)
      #Hold original order
      temp.df$order <- 1:nrow(temp.df)
      #Bring in variable labels by name, allow NA labels with all.x=TRUE
      temp.df <- merge(temp.df,labs,sort=FALSE,all.x=TRUE)
      temp.df <- temp.df[order(temp.df$order),]

      # Fill in the NAs with the column titles
      temp.df$vartitles[is.na(temp.df$vartitles)] <- temp.df$vars[is.na(temp.df$vartitles)]

      vartitles <- temp.df$vartitles

      if (!is.na(group)) {
        if (!is.null(labels[[group]])) {
          grouptitle <- labels[[group]][1]
        }
      }
    } else{
      stop('Unrecognized label format. See help(vtable).')
    }
  }

  ####### FORM SUMMARY TABLES TO BUILD ON
  if (is.na(group)) {
    # Create one for each column
    st <- list()
    for (i in 1:length(col.breaks)) {
      #Initialize with no rows
      st[[i]] <- utils::read.csv(text = paste(c('Variable',summ.names[[i]]),
                                              collapse =','),
                                 check.names = FALSE)

      contents <- lapply(col.vars[[i]], function(x)
        summary.row(data,
                    vars[x],
                    st[[i]],
                    vartitles[x],
                    summ[[i]],
                    var.classes[x],
                    factor.percent,
                    factor.counts,
                    factor.numeric,
                    digits[[i]],
                    fixed.digits,
                    wts))
      contents <- do.call(rbind, contents)
      st[[i]] <- rbind(st[[i]],contents)
    }
    #Make sure everybody has the same number of rows and bind
    st <- cbind_unequal(st)
  } else if(!group.long) {
    # One for each group
    st <- list()

    # Groups to loop over
    grouplevels <- sort(unique(data[[group]]))

    for (i in 1:length(grouplevels)) {
      #Initialize with no rows
      st[[i]] <- utils::read.csv(text = paste(c('Variable',summ.names[[1]]),
                                              collapse =','),
                                 check.names = FALSE)
      st[[i]][1,] <- c(paste0('HEADERROW',grouptitle),
                       paste0(grouplevels[i],'_MULTICOL_c_',length(summ.names[[1]])),
                       rep('DELETECELL',length(summ.names[[1]])-1))

      contents <- lapply(1:length(vars), function(x)
              summary.row(data[data[[group]] == grouplevels[i],],
                    vars[x],
                    st[[i]],
                    vartitles[x],
                    summ[[1]],
                    var.classes[x],
                    factor.percent,
                    factor.counts,
                    factor.numeric,
                    digits[[1]],
                    fixed.digits,
                    wts[data[[group]] == grouplevels[i]]))

      #On the last one, if there's a test, add it
      if (group.test & i == length(grouplevels)) {
        #Redo header with a Test column
        st[[i]] <- utils::read.csv(text = paste(c('Variable',summ.names[[1]],
                                                  'Test'), collapse =','),
                                   check.names = FALSE)
        st[[i]][1,] <- c(paste0('HEADERROW',grouptitle),
                         paste0(grouplevels[i],'_MULTICOL_c_',length(summ.names[[1]])),
                         rep('DELETECELL',length(summ.names[[1]])-1),'')

        for (x in 1:length(vars)) {
          #Sometimes perhaps an error!
          test.result <- suppressWarnings(
            try(independence.test(data[[group]],
                                  data[[vars[x]]],
                                  w = wts,
                                  opts=group.test.opts),
                silent = TRUE))
          if (inherits(test.result,'try-error')) {
            test.result <- ''
          }
          #We'll be no.escaping later, so escape the < in tiny pvals now
          if (!(out %in% c('latex','latexpage'))) {
            test.result <- gsub('<0','\\&lt0',test.result)
          }
          contents[[x]]$Test <- c(test.result,
                                  rep('',nrow(contents[[x]])-1))
        }
      }

      contents <- do.call(rbind, contents)
      st[[i]] <- rbind(st[[i]],contents)
      if (i > 1) {
        st[[i]]$Variable <- NULL
      }
    }
    st <- cbind_unequal(st)

    #If we did a test, add a table note
    if (group.test) {
      #It's possible they have chosen a format without stars
      havenote <- TRUE
      if (!is.null(group.test.opts[['format']])) {
        havenote <- grepl('\\{stars\\}',group.test.opts[['format']])
      }
      if (havenote) {
        star.cutoffs <- c(.01,.05,.1)
        star.markers <- c('***','**','*')
        if (!is.null(group.test.opts[['star.cutoffs']])) {
          star.cutoffs <- group.test.opts[['star.cutoffs']]
        }
        if (!is.null(group.test.opts[['star.markers']])) {
          star.cutoffs <- group.test.opts[['star.markers']]
        }
        #Order biggest to smallest
        star.markers <- star.markers[order(-star.cutoffs)]
        star.cutoffs <- star.cutoffs[order(-star.cutoffs)]
        starnote <- paste0(paste0(star.markers,' p<',star.cutoffs),collapse = '; ')
        starnote <- paste0('Statistical significance markers: ',starnote)
      }
    }
  } else {
    # One for each group
    st <- list()

    # Groups to loop over
    grouplevels <- sort(unique(data[[group]]))

    st.all <- list()

    for (j in 1:length(grouplevels)) {
      for (i in 1:length(col.breaks)) {
        #Initialize with no rows
        st[[i]] <- utils::read.csv(text = paste(c('Variable',summ.names[[i]]),
                                                collapse =','),
                                   check.names = FALSE)

        contents <- lapply(col.vars[[i]], function(x)
          summary.row(data[data[[group]] == grouplevels[j],],
                      vars[x],
                      st[[i]],
                      vartitles[x],
                      summ[[i]],
                      var.classes[x],
                      factor.percent,
                      factor.counts,
                      factor.numeric,
                      digits[[i]],
                      fixed.digits,
                      wts[data[[group]] == grouplevels[j]]))
        summcontents <- do.call(rbind, contents)
        st[[i]] <- rbind(st[[i]],summcontents)
      }

      st.all[[j]] <- cbind_unequal(st)

      #Header rows
      header.rows <- st.all[[j]][1,]
      addrow = 0
      if (j > 1) {
        header.rows[1,] <- rep('',ncol(header.rows))
        addrow = 1
      }
      header.rows[nrow(header.rows)+addrow,] <- c(
        paste0(grouptitle,': ',grouplevels[j],'_MULTICOL_',group.long.align,'_',ncol(header.rows)),
        rep('DELETECELL',ncol(header.rows)-1))

      st.all[[j]] <- rbind(header.rows,st.all[[j]])
    }
    st <- do.call(rbind,st.all)
  }

  ####### APPLICATION OF COL.WIDTH AND ALIGN DEFAULTS
  if (identical(col.width,NA) & identical(align,NA)) {
    align <- rep('r',ncol(st))
    align[names(st) == 'Variable'] <- 'l'
    #Padding only for non-first Variables, for col.breaks
    align[names(st) == 'Variable'] <- '@{\\hskip .1in}l'
    if (names(st)[1] == 'Variable') {
      align[1] <- 'l'
    }
    if (group.test) {
      align[names(st) == 'Test'] <- 'l'
    }
    align <- paste0(align, collapse = '')
  } else {
    align <- paste0('p{',col.width/100,'\\textwidth}')
    if (sum(names(st) == 'Variable') > 1) {
      align[names(st) == 'Variable'][-1] <- paste0('@{\\hskip .2in}',align[names(st) == 'Variable'][-1])
    }
    align <- paste0(align,collapse='')
  }
  if (identical(col.width,NA)) {
    col.width <- rep(1,ncol(st))

    #Any variable name columns are expanded
    col.width[names(st) == 'Variable'] <- 2
    if (group.test) {
      col.width[names(st) == 'Test'] <- 1.5
    }

    #Add it up
    totalwidth <- sum(col.width)

    #If total amount is less than two name-spaces, let table take up 60% of screen
    #From 2-3 name-spaces, 80%
    #More than 3 is full-screen
    tablescale <- 60 + 20*(totalwidth>=2) + 20*(totalwidth>=3)

    #And rescale column widths
    col.width <- (col.width/totalwidth)*tablescale
  }

  #col.align defaults
  if (identical(col.align, NA)) {
    col.align <- rep('right',ncol(st))
    #Padding only for non-first Variables, for col.breaks
    col.align[names(st) == 'Variable'] <- 'left; padding-left:10px'
    if (names(st)[1] == 'Variable') {
      col.align[1] <- 'left'
    }
    if (group.test) {
      col.align[names(st) == 'Test'] <- 'left'
    }
  }
  if (!is.na(group)) {
    #Center the column names unless it's a "variable" column
    names(st)[names(st) != 'Variable'] <- paste0(names(st)[names(st) != 'Variable'],'_MULTICOL_c_1')
  }

  # Finalize note
  if (!is.na(note) & !is.na(starnote)) {
    note <- paste0(starnote,'. ',note)
  } else if (!is.na(starnote)) {
    note <- starnote
  }

  ####### LATEX OUTPUT
  if (!identical(out, NA) & out %in% c('latex','latexpage')) {

    #Table only
    if (out == 'latex') {
      return(cat(dftoLaTeX(st, file = file,
                       align = align, anchor = anchor,
                       title = title,
                       note = note,
                       note.align = note.align,
                       fit.page = fit.page,
                       no.escape = ifelse(group.test,ncol(st),NA))))
    }

    #Now for the full page
    out.latex <- '\\documentclass{article}\n\\begin{document}\n\n%% sumtable \\{vtable\\}\n\n'

    #And bring in the table itself
    out.latex <- paste(out.latex,dftoLaTeX(st, align = align,
                                           anchor = anchor, title = title, note = note,
                                           note.align = note.align,
                                           fit.page = fit.page,
                                           no.escape = ifelse(group.test,ncol(st),NA)),'\n\n\\end{document}',sep='')

    ####### APPLICATION OF FILE OPTION
    if (!is.na(file)) {
      #If they forgot a file extension, fill it in
      if (!grepl("\\.tex",file)) {
        file <- paste(file,'.tex',sep='')
      }

      filepath <- file.path(file)
      #Create temporary tex file
      writeLines(out.latex,filepath)
    }

    return(cat(out.latex))
  }


  ####### CONSTRUCTION OF HTML
  #Head of file
  out.html <- paste('
                    <html style=\"font-family:Helvetica,Arial,Sans\">
                    <head><title>Summary Statistics</title>',
                    '<style type = \"text/css\">
                    p {
                    font-size:smaller;
                    }
                    table {
                    border: 0px;
                    border-collapse:collapse;
                    font-size:smaller;
                    table-layout:fixed;
                    margin-left:0%;
                    margin-right:auto;
                    }
                    .headtab {
                    width: 100%;
                    margin-left:auto;
                    margin-right:auto;
                    }
                    th {
                    background-color: #FFFFFF;
                    font-weight:bold;
                    text-align:left;
                    }
                    table tr:nth-child(odd) td {
                    background-color: #FFFFFF;
                    padding:4px;
                    word-wrap: break-word;
                    word-break:break-all;
                    }
                    table tr:nth-child(even) td {
                    background-color: #D3D3D3;
                    padding:4px;
                    word-wrap: break-word;
                    word-break:break-all;
                    }</style></head><body>',sep='')

  #Dataset name and description
  out.html <- paste(out.html,
                    '<table class=\"headtab\">',
                    '<tr><td style=\"text-align:left\">sumtable {vtable}</td>',
                    '<td style=\"text-align:right\">Summary Statistics</td></tr></table>',
                    '<h1>',title,'</h1>')

  #And bring in the table itself
  out.html <- paste(out.html,dftoHTML(st,out='htmlreturn',col.width=col.width,
                                      col.align=col.align,anchor=anchor, note = note, note.align = note.align,
                                      no.escape = ifelse(group.test,ncol(st),NA)),'</body></html>',sep='')


  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    if (identical(out,'csv')) {
      #If they forgot a file extension, fill it in
      if (!grepl("\\.csv",file)) {
        file <- paste(file,'.csv',sep='')
      }

      filepath <- file.path(file)
      #Create temporary html file
      utils::write.csv(clean_multicol(st),filepath, row.names = FALSE)
    } else {
      #If they forgot a file extension, fill it in
      if (!grepl("\\.htm",file)) {
        file <- paste(file,'.html',sep='')
      }

      filepath <- file.path(file)
      #Create temporary html file
      writeLines(out.html,filepath)
    }
  }

  #For more easily working with if statements
  if (is.na(out)) {
    out = ''
  }

  ####### APPLICATION OF OUT OPTION
  #If the plan is to produce a viewable HTML, create it
  if (out == 'viewer' | out == 'browser' | out == '') {
    #Get temporary dirpath
    tempDir <- tempfile()
    #Create temporary directory
    dir.create(tempDir)
    #Get temporary filepath
    htmlpath <- file.path(tempDir,'sumtable.html')
    #Create temporary html file
    writeLines(out.html,htmlpath)
  }

  #Either print the variable table to the help window
  #or return a variable table to the screen, as desired
  if (out == 'kable' | (isTRUE(getOption('knitr.in.progress')) & out == '')) {
    #kable can't handle the blank rows group.long makes
    st <- st[!apply(st,MARGIN=1,FUN=function(x) !any(!(x==rep('',ncol(st))))),]
    #I don't know how this would happen but just in case
    st <- st[!apply(st,MARGIN=1,FUN=function(x) propNA(x) == 1),]
    if (!simple.kable) {
      st <- clean_multicol_kable(st,title,note)
      if (isTRUE(getOption('knitr.in.progress')) & out == '') {
        st <- kableExtra::kable_styling(st)
      }
      return(st)
    } else {
      st <- knitr::kable(clean_multicol(st), caption = title)
      return(st)
    }
  } else if (Sys.getenv('RSTUDIO')=='1' & (out == 'viewer' | out == '')) {
    rstudioapi::viewer(htmlpath)
  } else if (Sys.getenv('RSTUDIO')=='' & out == 'viewer') {
    stop('out = viewer is not a valid option if RStudio is not running.')
  } else if ((Sys.getenv('RSTUDIO')=='' & out == '') | (out == 'browser')) {
    utils::browseURL(htmlpath)
  } else if (out == 'return' | out == 'csv') {
    return(clean_multicol(st))
  }  else if (out == 'htmlreturn') {
    return(cat(out.html))
  }
}


#' @rdname sumtable
#' @export
st <- sumtable

