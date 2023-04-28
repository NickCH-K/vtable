#' Variable Table Function
#'
#' This function will output a descriptive variable table either to the console or as an HTML file that can be viewed continuously while working with data. \code{vt()} is the same thing but requires fewer key presses to type.
#'
#' Outputting the variable table as a help file will make it easy to search through variable names or labels, or to refer to information about the variables easily.
#'
#' This function is in a similar spirit to \code{promptData()}, but focuses on variable documentation rather than dataset documentation.
#'
#' If you would like to include a \code{vtable} in an RMarkdown document, it should just work! If you leave \code{out} blank, it will default to a nicely-formatted \code{knitr::kable()}, although this will drop some formatting elements like multi-column cells (or do \code{out="kable"} to get an unformatted \code{kable} that you can format yourself). If you prefer the \code{vtable} package formatting, then use \code{out="latex"} if outputting to LaTeX or \code{out="htmlreturn"} for HTML, both with \code{results="asis"} in the code chunk. Alternately, in HTML, you can use the \code{file} option to write to file and use a \code{<iframe>} to include it.
#'
#' @param data Data set; accepts any format with column names. If variable labels are set with the haven package, \code{set_label()} from sjlabelled, or \code{label()} from Hmisc, \code{vtable} will extract them automatically.
#' @param out Determines where the completed table is sent. Set to \code{"browser"} to open HTML file in browser using \code{browseURL()}, \code{"viewer"} to open in RStudio viewer using \code{viewer()}, if available. Use \code{"htmlreturn"} to return the HTML code to R. Use \code{"return"} to return the completed variable table to R in data frame form or \code{"kable"} to return it as a \code{knitr::kable()}. Additional options include \code{"csv"} to write to CSV in conjunction with \code{file} (although this will drop most additional formatting), \code{"latex"} for a LaTeX table or \code{"latexpage"} for a full buildable LaTeX page. Defaults to \code{"viewer"} if RStudio is running, \code{"browser"} if it isn't, or a \code{"kable"} passed through \code{kableExtra::kable_styling()} defaults if it's an RMarkdown document being built with \code{knitr}.
#' @param file Saves the completed variable table file to HTML or .tex with this filepath. May be combined with any value of \code{out}, although note that \code{out = "return"} and \code{out = "kable"} will still save the standard vtable HTML file as with \code{out = "viewer"} or \code{out = "browser"}.
#' @param labels Variable labels. labels will accept three formats: (1) A vector of the same length as the number of variables in the data, in the same order as the variables in the data set, (2) A matrix or data frame with two columns and more than one row, where the first column contains variable names (in any order) and the second contains labels, or (3) A matrix or data frame where the column names (in any order) contain variable names and the first row contains labels. Setting the labels parameter will override any variable labels already in the data. Set to \code{"omit"} if the data set has embedded labels but you don't want any labels in the table.
#' @param class Set to \code{TRUE} to include variable classes in the variable table. Defaults to \code{TRUE}.
#' @param values Set to \code{TRUE} to include the range of values of each variable: min and max for numeric variables, list of factors for factor or ordered variables, and 'TRUE FALSE' for logicals. values will detect and use value labels set by the sjlabelled or haven packages, as long as every value is labelled. Defaults to \code{TRUE}.
#' @param missing Set to \code{TRUE} to include the number of NAs in the variable. Defaults to \code{FALSE}.
#' @param index Set to \code{TRUE} to include the index number of the column with the variable name. Defaults to \code{FALSE}.
#' @param factor.limit Sets maximum number of factors that will be included if \code{values = TRUE}. Set to 0 for no limit. Defaults to 5.
#' @param char.values Set to \code{TRUE} to include values of character variables as though they were factors, if \code{values = TRUE}. Or, set to a character vector of variable names to list values of only those character variables. Defaults to \code{FALSE}. Has no effect if \code{values = FALSE}.
#' @param data.title Character variable with the title of the dataset.
#' @param desc Character variable offering a brief description of the dataset itself. This will by default include information on the number of observations and the number of columns. To remove this, set \code{desc='omit'}, or include any description and then include \code{'omit'} as the last four characters.
#' @param note Table note to go after the last row of the table.
#' @param anchor Character variable to be used to set an anchor link in HTML tables, or a label tag in LaTeX.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @param col.align For HTML output, a character vector indicating the HTML \code{text-align} attributes to be used in the table (for example \code{col.align = c('left','center','center')}. Defaults to all left-aligned. If you want to get tricky, you can add a \code{";"} afterwards and keep putting in whatever CSS attributes you want. They will be applied to the whole column.
#' @param align For LaTeX output, string indicating the alignment of each column. Use standard LaTeX syntax (i.e. \code{l|ccc}). Defaults to all \code{p{}} columns with widths set using the same defaults as with \code{col.width}.  Be sure to escape special characters, in particular backslashes (i.e. \code{p{.25\\\\textwidth}} instead of \code{p{.25\\textwidth}}).
#' @param note.align Set the alignment for the multi-column table note. Usually "l", but if you have a long note in LaTeX you might want to set it with "p{}"
#' @param fit.page For LaTeX output, uses a resizebox to force the table to a certain width. Set to \code{NA} to omit. Often \code{'\\textwidth'}.
#' @param summ Character vector of summary statistics to include for numeric and logical variables, in the form \code{'function(x)'}. This option is flexible, and allows any summary statistic function that takes in a column and returns a single number. For example, \code{summ=c('mean(x)','mean(log(x))')} will provide the mean of each variable as well as the mean of the log of each variable. Keep in mind the special vtable package helper functions designed specifically for this option \code{propNA}, \code{countNA}, and \code{notNA}, which report counts and proportions of NAs, or counts of not-NAs, in the vectors, \code{nuniq}, which reports the number of unique values, and \code{pctile}, which returns a vector of the 100 percentiles of the variable. NAs will be omitted from all calculations other than \code{propNA(x)} and \code{countNA(x)}.
#' @param lush Set to \code{TRUE} to select a set of options with more information: sets \code{char.values} and \code{missing} to \code{TRUE}, and sets summ to \code{c('mean(x)', 'sd(x)', 'nuniq(x)')}. \code{summ} can be overwritten by setting \code{summ} to something else.
#' @param opts The same \code{vtable} options as above, but in a named list format. Useful for applying the same set of options to multiple \code{vtable}s.
#' @examples
#' \dontshow{
#' #These tests use the out='htmlreturn' option
#' #so that the same process of generating HTML is followed
#' #but a browser window is not opened during testing.
#' #This process is identical to regular operation except that
#' #HTML is written to the R output rather than a browser.
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),
#'     var4=as.factor(c('A','B','C','C')),var5=c(TRUE,TRUE,FALSE,FALSE))
#'
#' #Demonstrating different options:
#' vtable(df,labels=c('Number 1','Number 2','Some Letters',
#'     'Some Labels','You Good?'),
#'     out='htmlreturn')
#' vtable(subset(df,select=c(1,2,5)),
#'     labels=c('Number 1','Number 2','You Good?'),class=FALSE,values=FALSE,
#'     out='htmlreturn')
#' vtable(subset(df,select=c('var1','var4')),
#'     labels=c('Number 1','Some Labels'),
#'     factor.limit=1,col.width=c(10,10,40,35),
#'     out='htmlreturn')
#'
#' #Different methods of applying variable labels:
#' labelsmethod2 <- data.frame(var1='Number 1',var2='Number 2',
#'     var3='Some Letters',var4='Some Labels',var5='You Good?')
#' vtable(df,labels=labelsmethod2,out='htmlreturn')
#' labelsmethod3 <- data.frame(a =c("var1","var2","var3","var4","var5"),
#'     b=c('Number 1','Number 2','Some Letters','Some Labels','You Good?'))
#' vtable(df,labels=labelsmethod3,out='htmlreturn')
#'
#' #Using value labels and pre-labeled data:
#' library(sjlabelled)
#' df <- set_label(df,c('Number 1','Number 2','Some Letters',
#'     'Some Labels','You Good?'))
#' df$var1 <- set_labels(df$var1,labels=c('A little','Some more',
#' 'Even more','A lot'))
#' vtable(df,out='htmlreturn')
#'
#' #efc is data with embedded variable and value labels from the sjlabelled package
#' library(sjlabelled)
#' data(efc)
#' vtable(efc,out='htmlreturn')
#'
#' #Adding summary statistics for variable mean and proportion of data that is missing.
#' vtable(efc,summ=c('mean(x)','propNA(x)'),out='htmlreturn')
#'
#' }
#' if(interactive()){
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),
#'     var4=as.factor(c('A','B','C','C')),var5=c(TRUE,TRUE,FALSE,FALSE))
#'
#' #Demonstrating different options:
#' vtable(df,labels=c('Number 1','Number 2','Some Letters',
#'     'Some Labels','You Good?'))
#' vtable(subset(df,select=c(1,2,5)),
#'     labels=c('Number 1','Number 2','You Good?'),class=FALSE,values=FALSE)
#' vtable(subset(df,select=c('var1','var4')),
#'     labels=c('Number 1','Some Labels'),
#'     factor.limit=1,col.width=c(10,10,40,35))
#'
#' #Different methods of applying variable labels:
#' labelsmethod2 <- data.frame(var1='Number 1',var2='Number 2',
#'     var3='Some Letters',var4='Some Labels',var5='You Good?')
#' vtable(df,labels=labelsmethod2)
#' labelsmethod3 <- data.frame(a =c("var1","var2","var3","var4","var5"),
#'     b=c('Number 1','Number 2','Some Letters','Some Labels','You Good?'))
#' vtable(df,labels=labelsmethod3)
#'
#' #Using value labels and pre-labeled data:
#' library(sjlabelled)
#' df <- set_label(df,c('Number 1','Number 2','Some Letters',
#'     'Some Labels','You Good?'))
#' df$var1 <- set_labels(df$var1,labels=c('A little','Some more',
#' 'Even more','A lot'))
#' vtable(df)
#'
#' #efc is data with embedded variable and value labels from the sjlabelled package
#' library(sjlabelled)
#' data(efc)
#' vtable(efc)
#'
#' #Displaying the values of a character vector
#' data(USJudgeRatings)
#' USJudgeRatings$Judge <- row.names(USJudgeRatings)
#' vtable(USJudgeRatings,char.values=c('Judge'))
#'
#' #Adding summary statistics for variable mean and proportion of data that is missing.
#' vtable(efc,summ=c('mean(x)','propNA(x)'))
#'
#' }
#' @rdname vtable
#' @export
vtable <- function(data,out=NA,file=NA,labels=NA,class=TRUE,values=TRUE,missing=FALSE,
                   index=FALSE,factor.limit=5,char.values=FALSE,
                   data.title=NA,desc=NA,note = NA,note.align = 'l', anchor=NA,col.width=NA,col.align=NA,
                   align=NA,fit.page = NA, summ=NA,lush=FALSE,opts=list()) {
  #Bring in opts
  list2env(opts,envir=environment())
  #######CHECK INPUTS
  if (is.null(colnames(data))) {
    stop('Requires data with variable names or column names.')
  }
  if (!is.na(file) & !is.character(file)) {
    stop('Incorrect file name.')
  }
  if (!is.logical(class)) {
    stop('The class option must be TRUE or FALSE.')
  }
  if (!is.logical(values)) {
    stop('The values option must be TRUE or FALSE.')
  }
  if (!is.logical(missing)) {
    stop('The missing option must be TRUE or FALSE.')
  }
  if (!is.logical(index)) {
    stop('The index option must be TRUE or FALSE.')
  }
  if (!is.numeric(factor.limit) | factor.limit%%1 != 0) {
    stop('factor.limit must be an integer. Set to 0 for unlimited factors.')
  }
  if (!(is.logical(char.values) | is.character(char.values))) {
    stop('char.values must be FALSE, TRUE, or a character vector.')
  }
  if (!is.na(data.title) & !is.character(data.title)) {
    stop('data.title must be a character variable.')
  }
  if (!is.na(desc) & !is.character(desc)) {
    stop('desc must be a character variable.')
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
  if (min(is.na(summ)) == 0 & (!is.vector(summ) | !is.character(summ) | sum(is.na(summ)) > 0)) {
    stop('summ must be a character vector with no missing values.')
  }
  if (!is.logical(lush)) {
    stop('lush must be logical.')
  }
  if (!identical(out,NA) & !(out %in% c('viewer', 'browser','return','htmlreturn','kable','latex','latexpage', 'csv'))) {
    stop('out must be viewer, browser, return, htmlreturn, kable, latex, or latexpage')
  }
  if (identical(out, 'csv') & is.na(file)) {
    warning('out = "csv" will just return the vtable as a data.frame unless combined with file')
  }

  #One-column matrices run into some problems later on
  if (is.matrix(data) & dim(data)[2] == 1) {
    data <- as.data.frame(data)
  }

  ####### APPLICATION OF LUSH DEFAULTS
  if (lush) {
    char.values <- TRUE
    missing <- TRUE
    if (is.na(summ)) {
      summ <- c('mean(x)', 'sd(x)', 'nuniq(x)')
    }
  }

  ####### APPLICATION OF DATA.TITLE OPTION
  #If data.title is missing, fill in with name of the data frame read in
  if (is.na(data.title)) {
    data.title <- deparse(substitute(data))
  }

  ####### FORM VARIABLE TABLE TO BUILD ON
  #If index = TRUE, start with that. Otherwise, start with Name
  if (index==TRUE) {
    vt <- data.frame(Index=1:ncol(data),
                     Name=colnames(data))
  } else {
    #Start table with variable names or column names (earlier error check ensures one exists)
    vt <- data.frame(Name = colnames(data))
  }

  ####### APPLICATION OF CLASS OPTION

  #If user asks for variable classes, add them to the variable table
  if (class == TRUE) {
    #If multiple classes listed, take the first
    vt$Class <- sapply(data,function(x) class(x)[1])
  }

  ####### APPLICATION OF LABELS OPTION
  #Pull from label attribute if present
  #label attribute works for labels from Hmisc, sjlabelled, haven
  labs <- sapply(data, function(x) attr(x,'label'))
  #If there were any, add them
  if (!is.null(unlist(labs))) {
    vt$Label <- labs
  }
  #If labels are directly included, override the embedded labels
  #Use ifelse construction so that labels[1] is not evaluated for two-column
  #label styles (which throws an error)
  if (min(is.na(labels))==0 & ifelse(length(labels) == 1,labels[1] != "omit",TRUE)) {
    #Override embedded labels
    vt$Label <- NULL

    #Figure out the format of the data and fill in labs appropriately
    #First, check if it's just a vector of labels
    if (is.vector(labels)) {
      #Make sure it's the right length
      if (length(labels) == dim(vt)[1]) {
        vt$Label <- labels
      } else {
        stop('label vector must have as many elements as there are variables in the data. Use NA elements to fill in, or see help(vtable) for other label formats that do not require every variable to have a label.')
      }

      #Check if it's in the two-column format
    } else if(dim(labels)[1] > 1 & dim(labels)[2] == 2) {
      #Put labels in mergeable format
      labels <- as.data.frame(labels)
      names(labels) <- c('Name','Label')

      #Hold original order
      vt$order <- 1:nrow(vt)
      #Bring in variable labels by name, allow NA labels with all.x=TRUE
      vt <- merge(vt,labels,sort=FALSE,all.x=TRUE)
      vt <- vt[order(vt$order),]
      vt$order <- NULL
      #Keep row names in order
      rownames(vt) <- 1:nrow(vt)

      #Check if it's in the one-row variable-name format
    } else if (dim(labels)[1]==1 & !is.null(colnames(labels))) {
      #Put into two-column format
      labs <- data.frame(Name=colnames(labels),Label=as.character(t(labels[1,])))

      #Hold original order
      vt$order <- 1:nrow(vt)
      #Bring in variable labels by name, allow NA labels with all.x=TRUE
      vt <- merge(vt,labs,sort=FALSE,all.x=TRUE)
      vt <- vt[order(vt$order),]
      vt$order <- NULL
      rownames(vt) <- 1:nrow(vt)
    } else{
      stop('Unrecognized label format. See help(vtable).')
    }

  } else if (min(is.na(labels))==0 & ifelse(length(labels) == 1,labels[1] == "omit",FALSE)) {
    vt$Label <- NULL
  }


  ####### We need only one class
  #If there are multiples and one is factor, treat as factor
  if (sum(sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))) > 0) {
    data[,sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))] <-
      as.data.frame(sapply(data[,sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))],function(x) factor(x,ordered=FALSE)), stringsAsFactors = TRUE)
  }
  #Similarly, only take one class if it's numeric, UNLESS it's haven_labelled.
  if (sum(sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)) & !('haven_labelled' %in% class(x)))) > 0) {
    data[,sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)) & !('haven_labelled' %in% class(x)))] <-
      as.data.frame(sapply(data[,sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)) & !('haven_labelled' %in% class(x)))],function(x) as.numeric(haven::zap_labels(x))))
  }

  ####### APPLICATION OF VALUES OPTION
  #If user wants values, show the possible values the variable can take
  if (values == TRUE) {

    ####### APPLICATION OF CHAR.VALUES OPTION
    if (inherits(char.values,'logical')) {
      if (char.values == TRUE) {
        #See which are characters
        charvariables <- as.logical(unlist(sapply(data,function(x) max(class(x) == "character"))))
        #and convert
        data[,charvariables] <- as.data.frame(sapply(data[,charvariables],function(x) as.factor(x)), stringsAsFactors = TRUE)
        #clean
        rm(charvariables)
      }
    } else if (inherits(char.values,'character')) {
      #See which variables are in the list
      charvariables <- names(data) %in% char.values
      #and convert
      data[,charvariables] <- as.data.frame(sapply(data[,charvariables],function(x) as.factor(x)), stringsAsFactors = TRUE)
      #clean
      rm(charvariables)
    }

    #Create variable to hold ranges
    vt$Values <- ''

    #Are there any labelled values?
    #allow both for the labelled class and non-factor variables with value labels
    if (sum(unlist(sapply(data,function(x) class(x) == "labelled")))+
        sum(sapply(data,function(x) !is.factor(x) &
                   !is.null(unlist(sjlabelled::get_labels(x)))))>0) {

        #Since we've already extracted class, if necessary,
        #we can just turn these into factor variables with an included
        #numerical coding for clarity
        #Identify which variables have labels
        havelabels <- as.logical(unlist(sapply(data,function(x) max(class(x) == "labelled"))))
        #Include variables not of the class labelled or factor but which have labels
        havelabels[sapply(data,function(x) !is.factor(x) & !is.null(unlist(sjlabelled::get_labels(x,attr.only=TRUE))))] <- TRUE

        #DON'T include variables with unlabelled values
        unlabvals <-  sapply(data[,havelabels,drop = FALSE], function(x)
          length(sjlabelled::get_labels(x)) == length(sjlabelled::get_labels(x, non.labelled = TRUE)))
        if (sum(!unlabvals) > 0) {
          havelabels[havelabels] <- unlabvals
          warning('Some labelled variables have unlabeled values. Treating these as numeric variables and ignoring labels.')
        }

        if (sum(havelabels) > 0) {
          vallabs <- sjlabelled::get_labels(data,values='as.name')
          #Add numerical coding
          vallabscode <- lapply(vallabs, function(x) paste(names(x),': ',x,sep=''))
          #Make sure the labels are named chr vectors
          for (v in names(vallabscode)) {
            names(vallabscode[[v]]) <- names(vallabs[[v]])
          }


          #Set new coded labels among the variables with value labels
          suppressMessages(suppressWarnings(data[,havelabels] <- sjlabelled::set_labels(data[,havelabels,drop=FALSE],labels=vallabscode[havelabels])))
          #And turn into the appropriately-titled factor
          suppressWarnings(data[,havelabels] <- sjlabelled::as_label(data[,havelabels,drop=FALSE]))

        }
    }


    #If there are any factors:
    if (sum(sapply(data,is.factor)) > 0) {
      #Fill in description of factor levels for factor
      factorlevels <- lapply(subset(data,select=sapply(data,is.factor)),levels)

      #If there's not a limit on the number of factors requested
      toomany <- rep(FALSE,length(factorlevels))

      ####### APPLICATION OF FACTOR.LIMIT OPTION
      #if there's a limit on the number of factors requested
      if (factor.limit > 0) {
        #Find out which variables have too many factors
        numcut <- sapply(factorlevels,function(x) length(x) - factor.limit)
        toomany <- numcut > 0

        #Cut back to the limit
        factorlevels <- lapply(factorlevels,function(x) x[1:min(factor.limit,length(x))])
      }

      #Make sure each factor surrounded by '
      factorlevels <- paste('\'',
                            lapply(factorlevels,paste,collapse='\' \''),
                            '\'',sep='')

      #If some were cut, indicate that
      factorlevels <- ifelse(toomany,paste(factorlevels,'and', numcut, 'more'),factorlevels)

      #And fill in for output table
      vt[sapply(data,is.factor),]$Values <- factorlevels
    }

    #If there are any dates:
    if (sum(sapply(data,function(x) max(class(x) %in% c('Date','POSIXct','POSIXt','POSIXlt')) & min(is.na(x)) == 0)) > 0) {
      #Get minimums, be sure to skip any variables that are always NA
      min <- lapply(subset(data,select=sapply(data,function(x) max(class(x) %in% c('Date','POSIXct','POSIXt','POSIXlt')) & min(is.na(x)) == 0)),function(x) min(x,na.rm=TRUE))
      min <- sapply(min, as.character)

      #Get maximums
      max <- lapply(subset(data,select=sapply(data,function(x) max(class(x) %in% c('Date','POSIXct','POSIXt','POSIXlt')) & min(is.na(x)) == 0)),function(x) max(x,na.rm=TRUE))
      max <- sapply(max, as.character)

      #Range description
      range <- paste('Time:',min,'to',max)

      #Fill in for output table
      vt[sapply(data,function(x) max(class(x) %in% c('Date','POSIXct','POSIXt','POSIXlt')) & min(is.na(x)) == 0),]$Values <- range
    }


    #If there are any numeric variables:
    if (sum(sapply(data,function(x) is.numeric(x) & min(is.na(x)) == 0)) > 0) {
      #Get minimums, be sure to skip any variables that are always NA
      min <- lapply(subset(data,select=sapply(data,function(x) is.numeric(x) & min(is.na(x)) == 0)),function(x) round(min(x,na.rm=TRUE),3))

      #Get maximums
      max <- lapply(subset(data,select=sapply(data,function(x) is.numeric(x) & min(is.na(x)) == 0)),function(x) round(max(x,na.rm=TRUE),3))

      #Range description
      range <- paste('Num:',min,'to',max)

      #Fill in for output table
      vt[sapply(data,function(x) is.numeric(x) & min(is.na(x)) == 0),]$Values <- range
    }

    #Binary variables
    if (sum(sapply(data,is.logical))>0) {
      #Fill in for output table
      vt[sapply(data,is.logical),]$Values <- 'TRUE FALSE'
    }
  }
  ####### APPLICATION OF MISSING OPTION
  #If user asks for number of missing values in the column, add them to the variable table
  if (missing==TRUE) {
      vt$Missing <- sapply(data, countNA)
  }
  ####### APPLICATION OF SUMM OPTION
  #Check if anything included for summ
  if (min(is.na(summ)) == 0) {
    #Create blank to fill in
    vt$Summary = ''

    #First, do the propNA and countNA functions
    #Do separately so it can be applied to factors and characters
    #and also because all other functions are run only on nonmissings
    #So if we have NA counts and also factors to do them to
    if ('propNA(x)' %in% summ) {
      vt$Summary <-
        #Start the summary variable off by pasting together the propNA name
        paste('propNA: ',
              #with a summary calculation performed on each character or factor column of the data
              sapply(data,
                     #and in particular that function is mean(is.na(x))
                     function(x) round(propNA(x),3)),sep='')
      #If propNA isn't the only thing, use a line break to separate this from the next
      if (length(summ) > 1) {
        vt$Summary <-
          paste(vt$Summary,'<br/>',sep='')
      }
    }
    #Now do the exact same thing for countNA as was done for propNA
    if ('countNA(x)' %in% summ) {
      vt$Summary <-
        #Start the summary variable off by pasting together the countNA name
        paste(vt$Summary,'countNA: ',
              #with a summary calculation performed on each character or factor column of the data
              sapply(data, countNA),sep='')

      #If there's still more to come, add a line break
      if (length(summ[!summ %in% c('propNA(x)','countNA(x)')])>0) {
        vt$Summary <-
          paste(vt$Summary,'<br/>',sep='')
      }
    }

    #If there are propNA or countNA functions, drop them since we just used them
    summ <- summ[!summ %in% c('propNA(x)','countNA(x)')]

    #Create copy of summ for actual use
    summuse <- summ
    #And a copy for naming the summary stats
    summnames <- summ

    #Change names for presentation. If the name is simple
    #(i.e. ends in just (x)), cut that out. Otherwise, leave intact for clarity
    #replace the names of all names that end in '(x)'
    summnames[substring(summnames,nchar(summnames)-2)=='(x)'] <-
      #with a substring of those names that starts at 1 and ends before (x)
      substring(summnames[substring(summnames,nchar(summnames)-2)=='(x)'],
                1,nchar(summnames[substring(summnames,nchar(summnames)-2)=='(x)'])-3)
    #and tack on a ': ' that will go between the name and the number
    summnames <- paste(summnames,': ',sep='')

    #Now do all the stats.
    #Comments are numbered for the purpose of reading them in order
    vt$Summary <-
      #8. And finally paste it together with what we already have
      paste(vt$Summary,
            #3. Go through each of those variables one by one to calculate summary stats
            sapply(
              #2 Turn it into a list and restrict each of the columns to nonmissing
              #(can't just use complete.cases - you want each variable to have all its nonmissings)
              #(can't use na.rm since some functions don't take it)
              #(can't do this at the level of x on the innermost sapply since it may be difficult to locate the x that connotates data if the function has the letter x in it)
              lapply(as.list(
                #1. Take all the variables and just keep nonmissings
                data),function(x) x[!is.na(x)]),
              #4. within each of those variables, paste together a bunch of summary stats
              # Send to parsesumm so as to handle different cases
              # If it's a date, and summnames was set by lush = TRUE, use median and nuniq
              function(x) if (lush == TRUE & max(class(x) %in% c('Date','POSIXct','POSIXt','POSIXlt')) == 1) {
                parsesumm(x, c('median(x)','nuniq(x)'), c('median: ', 'nuniq: '))
              } else {
                parsesumm(x,summuse,summnames)
              }),sep='')
  }

  ####### APPLICATION OF COL.WIDTH OPTION
  #column percentages
  #Check which columns we have to account for
  haslabels <- 'Label' %in% colnames(vt)
  hasclass <- 'Class' %in% colnames(vt)
  hasvalues <- 'Values' %in% colnames(vt)
  hassumm <- 'Summary' %in% colnames(vt)
  #If col.width not manually set, use defaults
  if (sum(!is.na(col.width)) == 0) {
    #initialize vector
    col.width <- rep(0,length(colnames(vt)))

    #Default ratios:
    #Name:class:label:values:summ
    #1:.5:1.75:1.25:.75
    col.width[colnames(vt)=='Index'] <- .25
    col.width[colnames(vt)=='Name'] <- 1
    col.width[colnames(vt)=='Class'] <- .5
    col.width[colnames(vt)=='Label'] <- 1.75
    col.width[colnames(vt)=='Values'] <- 1.25
    col.width[colnames(vt)=='Missing'] <- .5
    col.width[colnames(vt)=='Summary'] <- .9

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
    col.align <- rep('left',ncol(vt))
  }

  #Do we have a summary?
  if ('Summary' %in% names(vt)) {
    # Don't escape any line breaking stuff in there
    no.escape <- which(names(vt) == 'Summary')
  } else {
    no.escape <- NA
  }

  ####### LATEX OUTPUT
  if (!identical(out, NA) & out %in% c('latex','latexpage')) {
    #Some <br/>s are hardcoded in there
    if ('Summary' %in% names(vt)) {
      vt$Summary <- gsub('<br/>',' \\\\\\\\ ',vt$Summary)
      vt$Summary <- paste0('\\begin{tabular}[c]{@{}c@{}}',vt$Summary,'\\end{tabular}')
    }
    if (is.na(align)) {
      col.width <- col.width/100
      align <- paste0(paste0('p{',col.width,'\\textwidth}'),collapse='')
    }

    #Table only
    if (out == 'latex') {
      return(cat(dftoLaTeX(vt, file = file,
                       align = align, note = note, note.align = note.align,
                       anchor = anchor, title = 'Variable Table', fit.page = fit.page,
                       no.escape = no.escape)))
    }

    #Now for the full page
    out.latex <- '\\documentclass{article}\n\\begin{document}\n\nvtable \\{vtable\\}\n\n'
    out.latex <- paste(out.latex,
                       '\\textbf{\\LARGE ', data.title,'}\n\n')

    #Applying description
    #By default, this is number of obs and number of columns, plus whatever is in desc.
    #"omit" will leave that out.
    description <- paste('This data contains ',dim(data)[1],' rows and ',dim(data)[2],' columns.',sep='')
    #four possibilities: desc is NA (print description),
    #desc is just omit (print nothing, do nothing)
    #desc is other stuff followed by omit (just print the other stuff)
    #desc is other stuff not followed by omit (print desc and the other stuff)
    #First, check for blank desc
    if (is.na(desc)) {
      out.latex <- paste(out.latex,description,'\n\n',sep='')

      #Evaluate these only if desc is nonmissing
    } else if(desc == "omit") {
      #Do nothing here

      #Next, stuff followed by omit
    } else if(substring(desc,nchar(desc)-3)=="omit" & desc != "omit") {
      #Don't actually print the omit!
      out.latex <- paste(out.latex,substring(desc,1,nchar(desc)-4),'\n\n',sep='')

      #Finally, stuff not followed by omit
    } else {
      out.latex <- paste(out.latex,desc,' ',description,'\n\n',sep='')
    }

    #And bring in the table itself
    out.latex <- paste(out.latex,dftoLaTeX(vt, align = align,
                                           anchor = anchor, note = note, note.align = note.align,
                                           title = 'Variable Table', fit.page = fit.page,
                                           no.escape = no.escape),'\n\n\\end{document}',sep='')

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
                    <head><title>',data.title,'</title>',
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
                    '<tr><td style=\"text-align:left\">vtable {vtable}</td>',
                    '<td style=\"text-align:right\">Variable Documentation</td></tr></table>',
                    '<h1>',data.title,'</h1>')

  #Applying description
  #By default, this is number of obs and number of columns, plus whatever is in desc.
  #"omit" will leave that out.
  description <- paste('This data contains ',dim(data)[1],' rows and ',dim(data)[2],' columns.',sep='')
  #four possibilities: desc is NA (print description),
  #desc is just omit (print nothing, do nothing)
  #desc is other stuff followed by omit (just print the other stuff)
  #desc is other stuff not followed by omit (print desc and the other stuff)
  #First, check for blank desc
  if (is.na(desc)) {
    out.html <- paste(out.html,'<p>',description,'</p>',sep='')

    #Evaluate these only if desc is nonmissing
  } else if(desc == "omit") {
    #Do nothing here

    #Next, stuff followed by omit
  } else if(substring(desc,nchar(desc)-3)=="omit" & desc != "omit") {
    #Don't actually print the omit!
    out.html <- paste(out.html,'<p>',substring(desc,1,nchar(desc)-4),'</p>',sep='')

    #Finally, stuff not followed by omit
  } else {
    out.html <- paste(out.html,'<p>',desc,' ',description,'</p>',sep='')
  }

  out.html <- paste(out.html,'<h3>Variable Table</h3>',sep='')

  #And bring in the table itself
  out.html <- paste(out.html,dftoHTML(vt,out='htmlreturn',
                                      col.width=col.width,
                                      col.align=col.align,
                                      note = note, note.align = note.align, anchor=anchor,
                                      no.escape = no.escape),'</body></html>',sep='')


  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    if (identical(out,'csv')) {
      #If they forgot a file extension, fill it in
      if (!grepl("\\.csv",file)) {
        file <- paste(file,'.csv',sep='')
      }

      for (i in 1:ncol(vt)) {
        vt[[i]] <- gsub('<br/>','; ',vt[[i]])
      }
      filepath <- file.path(file)
      utils::write.csv(vt, file = filepath, row.names = FALSE)
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
    htmlpath <- file.path(tempDir,'vtable.html')
    #Create temporary html file
    writeLines(out.html,htmlpath)
  }

  #Either print the variable table to the help window
  #or return a variable table to the screen, as desired
  #Default to kable if in knitr
  if (out == 'kable' | (isTRUE(getOption('knitr.in.progress')) & out == '')) {
    for (i in 1:ncol(vt)) {
      # Don't end on a break
      vt[[i]] <- gsub('<br/>$','',vt[[i]])
      # But replace all internal ones with commas
      vt[[i]] <- gsub('<br/>',', ',vt[[i]])
    }
    #kable can't handle blank rows. These should not occur in vtable but just in case
    vt <- vt[!apply(vt,MARGIN=1,FUN=function(x) !any(!(x==rep('',ncol(vt))))),]
    #I don't know how this would happen but just in case
    vt <- vt[!apply(vt,MARGIN=1,FUN=function(x) propNA(x) == 1),]
    if (knitr::is_latex_output()) {
      kb <- knitr::kable(vt, caption = data.title, row.names = FALSE, booktabs = TRUE, format = 'latex')
      if (!is.na(note)) {
        kb <- kableExtra::add_footnote(kb, note, notation = 'none')
      }
    } else if(knitr::is_html_output()) {
      kb <- knitr::kable(vt, caption = data.title, row.names = FALSE, format = 'html')
      if (!is.na(note)) {
        kb <- kableExtra::add_footnote(kb, note, notation = 'none')
      }
    } else {
      kb <- knitr::kable(vt, caption = data.title, row.names = FALSE)
    }

    # If it's just a default RMarkdown kable, style it for HTML because the default is ew
    if (isTRUE(getOption('knitr.in.progress')) & out == '') {
      if (isTRUE(knitr::is_html_output())) {
        kb <- kableExtra::kable_styling(kb)
      }
    }
    return(kb)
  } else if (Sys.getenv('RSTUDIO')=='1' & (out == 'viewer' | out == '')) {
    rstudioapi::viewer(htmlpath)
  } else if (Sys.getenv('RSTUDIO')=='' & out == 'viewer') {
    stop('out = "viewer" is not a valid option if RStudio is not running.')
  } else if ((Sys.getenv('RSTUDIO')=='' & out == '') | (out == 'browser')) {
    utils::browseURL(htmlpath)
  } else if (out == 'return' | out == 'csv') {
    for (i in 1:ncol(vt)) {
      vt[[i]] <- gsub('<br/>','; ',vt[[i]])
    }
    return(vt)
  } else if (out == 'htmlreturn') {
    return(cat(out.html))
  }
}

#' @rdname vtable
#' @import kableExtra
#' @export
vt <- vtable
