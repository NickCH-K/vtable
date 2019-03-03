#' Variable Table Function
#'
#' This function will output a descriptive variable table either to the console or as an HTML file that can be viewed continuously while working with data.
#'
#' Outputting the variable table as a help file will make it easy to search through variable names or labels, or to refer to information about the variables easily.
#'
#' This function is in a similar spirit to promptData(), but focuses on variable documentation rather than dataset documentation.
#'
#' @param data Data set; accepts any format with column names. If variable labels are set with the haven package, set_label() from sjlabelled, or label() from Hmisc, vtable will extract them automatically.
#' @param out Determines where the completed table is sent. Set to "browser" to open HTML file in browser using browseURL(), "viewer" to open in RStudio viewer using viewer(), if available. Use "htmlreturn" to return the HTML code to R, or "return" to return the completed variable table to R in data frame form. Defaults to "viewer" if RStudio is running and "browser" if it isn't.
#' @param file Saves the completed variable table file to HTML with this filepath. May be combined with any value of out.
#' @param labels Variable labels. labels will accept three formats: (1) A vector of the same length as the number of variables in the data, in the same order as the variables in the data set, (2) A matrix or data frame with two columns and more than one row, where the first column contains variable names (in any order) and the second contains labels, or (3) A matrix or data frame where the column names (in any order) contain variable names and the first row contains labels. Setting the labels parameter will override any variable labels already in the data. Set to "omit" if the data set has embedded labels but you don't want any labels in the table.
#' @param class Set to TRUE to include variable classes in the variable table. Defaults to TRUE.
#' @param values Set to TRUE to include the range of values of each variable: min and max for numeric variables, list of factors for factor or ordered variables, and 'TRUE FALSE' for logicals. values will detect and use value labels set by the sjlabelled or haven packages. Defaults to TRUE.
#' @param missing Set to TRUE to include whether the variable contains any NAs. Defaults to FALSE.
#' @param index Set to TRUE to include the index number of the column with the variable name. Defaults to FALSE.
#' @param factor.limit Sets maximum number of factors that will be included if values = TRUE. Set to 0 for no limit. Defaults to 5.
#' @param data.title Character variable with the title of the dataset.
#' @param desc Character variable offering a brief description of the dataset itself. This will by default include information on the number of observations and the number of columns. To remove this, set desc='omit', or include any description and then include 'omit' as the last four characters.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @param summ Character vector of summary statistics to include for numeric and logical variables, in the form 'function(x)'. This option is flexible, and allows any summary statistic function that takes in a column and returns a single number. For example, summ=c('mean(x)','mean(log(x))') will provide the mean of each variable as well as the mean of the log of each variable. This also allows the special functions `propNA(x)` and `countNA(x)`,  which provide the proportion and total number of missing values in the variable, respectively, which will always be displayed first and which are applied to factor and character variables as well as numeric and logical. NAs will be omitted from all calculations other than propNA(x) and countNA(x).
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
#' \dontrun{
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
#' #Adding summary statistics for variable mean and proportion of data that is missing.
#' vtable(efc,summ=c('mean(x)','propNA(x)'))
#'
#' }
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @export
vtable <- function(data,out=NA,file=NA,labels=NA,class=TRUE,values=TRUE,missing=FALSE,
                   index=FALSE,factor.limit=5,data.title=NA,desc=NA,col.width=NA,summ=NA) {

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
  if (!is.na(data.title) & !is.character(data.title)) {
    stop('data.title must be a character variable.')
  }
  if (!is.na(desc) & !is.character(desc)) {
    stop('desc must be a character variable.')
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

  #One-column matrices run into some problems later on
  if (is.matrix(data) & dim(data)[2] == 1) {
    data <- as.data.frame(data)
  }

  ####### APPLICATION OF DATA.TITLE OPTION
  #If data.title is missing, fill in with name of the data frame read in
  if (is.na(data.title)) {
    data.title <- deparse(substitute(data))
  }

  ####### FORM VARIABLE TABLE TO BUILD ON
  #If index = TRUE, start with that. Otherwise, start wtih Name
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
  #Now that we've taken the class names, we need only one class
  #If there are multiples and one is factor, treat as factor
  if (sum(sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))) > 0) {
    data[,sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))] <-
      factor(data[,sapply(data,function(x) (length(class(x)) >1) & (is.factor(x)))],ordered=FALSE)
  }
  #If there are multiples and one is numeric do the same
  if (sum(sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)))) > 0) {
    data[,sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)))] <-
      as.numeric(data[,sapply(data,function(x) (length(class(x)) >1) & (is.numeric(x)))])
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

  ####### APPLICATION OF VALUES OPTION
  #If user wants values, show the possible values the variable can take
  if (values == TRUE) {
    #Create variable to hold ranges
    vt$Values <- ''

    #Are there any labelled values?
    #allow both for the labelled class and non-factor variables with value labels
    if (sum(sapply(data,function(x) class(x) == "labelled"))+
        sum(sapply(data,function(x) !is.factor(x) &
                   !is.null(unlist(sjlabelled::get_labels(x)))))>0) {

        #Since we've already extracted class, if necessary,
        #we can just turn these into factor variables with an included
        #numerical coding for clarity
        #Identify which variables have labels
        havelabels <- sapply(data,function(x) class(x) == "labelled")
        #Include variables not of the class labelled or factor but which have labels
        havelabels[sapply(data,function(x) !is.factor(x) & !is.null(unlist(sjlabelled::get_labels(x,attr.only=TRUE))))] <- TRUE

        vallabs <- sjlabelled::get_labels(data,values=TRUE)
        #Add numerical coding
        vallabscode <- lapply(vallabs, function(x) paste(names(x),': ',x,sep=''))
        #Make sure the labels are named chr vectors
        names(vallabscode) <- lapply(vallabs,function(x) names(x))

        #Set new coded labels among the variables with value labels
        #Run through try because this will produce an error if
        #values are not exactly labeled (some values unlabeled, or some labels unused)
        #If there is an error, use drop and fill labels to correct;
        #check if necessary first because these commands are slow.
        labcheck <- try(data[,havelabels] <- sjlabelled::set_labels(data[,havelabels],labels=vallabscode[havelabels]),silent=TRUE)
        if (class(labcheck)[1] == "try-error") {
          warning("Labelled variables detected that have some values unlabelled, or some labels without matching values in data.\n  This will take a moment to correct. To avoid this delay in the future, ensure labels match values exactly, or run drop_labels() and/or fill_labels() on your data.",immediate.=TRUE)

          #Make sure labels match
          data[,havelabels] <- sjlabelled::drop_labels(data[,havelabels])
          data[,havelabels] <- sjlabelled::fill_labels(data[,havelabels])

          vallabs <- sjlabelled::get_labels(data,values=TRUE)
          #Add numerical coding
          vallabscode <- lapply(vallabs, function(x) paste(names(x),': ',x,sep=''))
          #Make sure the labels are named chr vectors
          names(vallabscode) <- lapply(vallabs,function(x) names(x))

          data[,havelabels] <- sjlabelled::set_labels(data[,havelabels],labels=vallabscode[havelabels])

        }

        data[,havelabels] <- sjlabelled::as_label(data[,havelabels])

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
        toomany <- sapply(factorlevels,function(x) length(x) > factor.limit)

        #Cut back to the limit
        factorlevels <- lapply(factorlevels,function(x) x[1:min(factor.limit,length(x))])
      }

      #Make sure each factor surrounded by '
      factorlevels <- paste('\'',
                            lapply(factorlevels,paste,collapse='\' \''),
                            '\'',sep='')

      #If some were cut, indicate that
      factorlevels <- ifelse(toomany,paste(factorlevels,'and more'),factorlevels)

      #And fill in for output table
      vt[sapply(data,is.factor),]$Values <- factorlevels
    }


    #If there are any numeric variables:
    if (sum(sapply(data,is.numeric)) > 0) {
      #Get minimums
      min <- lapply(subset(data,select=sapply(data,is.numeric)),function(x) round(min(x,na.rm=TRUE),3))

      #Get maximums
      max <- lapply(subset(data,select=sapply(data,is.numeric)),function(x) round(max(x,na.rm=TRUE),3))

      #Range description
      range <- paste('Num:',min,'to',max)

      #Fill in for output table
      vt[sapply(data,is.numeric),]$Values <- range
    }

    #Binary variables
    if (sum(sapply(data,is.logical))>0) {
      #Fill in for output table
      vt[sapply(data,is.logical),]$Values <- 'TRUE FALSE'
    }
  }
  ####### APPLICATION OF MISSING OPTION
  #If user asks for whether column includes missing values, add them to the variable table
  if (missing==TRUE) {
      vt$Missing <- sapply(data,function(x) anyNA(x))
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
                     function(x) round(mean(is.na(x)),3)),sep='')
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
              sapply(data,
                     #and in particular that function is mean(is.na(x))
                     function(x) sum(is.na(x))),sep='')

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

    #Now do all the stats for numeric and logicals. This one's a beast!
    #Comments are numbered for the purpose of reading them in order
    vt[!(sapply(data,class) %in% c("character","factor")),]$Summary <-
      #8. And finally paste it together with what we already have
      paste(vt[!(sapply(data,class) %in% c("character","factor")),]$Summary,
      #3. Go through each of those variables one by one to calculate summary stats
      sapply(
      #2 Turn it into a list and restrict each of the columns to nonmissing
        #(can't just use complete.cases - you want each variable to have all its nonmissings)
        #(can't use na.rm since some functions might not take it)
        #(can't do this at the level of x on the innermost sapply since it may be difficult to locate the x that connotates data if the function has the letter x in it)
      lapply(as.list(
      #1. Take all the variables that aren't characters or factors
      data[,!(sapply(data,class) %in% c("character","factor")),drop=FALSE]),function(x) x[!is.na(x)]),
      #4. within each of those variables, paste together a bunch of summary stats
      function(x) paste0(
        #5. Each of those summary stats should be preceded by the name of the summary stat
        paste(summnames,
        #6. then, the summary stat itself, for which we need to run through everything in summuse
        sapply(summuse,function(y) round(eval(parse(text=y)),3)),sep=''),
        #7. Bring together with a line break between each summary stat except the last
        collapse='<br/>')),sep='')

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


  #And bring in the table itself
  out.html <- paste(out.html,dftoHTML(vt,out='htmlreturn',col.width=col.width),'</body></html>',sep='')


  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    #If they forgot a file extension, fill it in
    if (!grepl("\\.",file)) {
      file <- paste(file,'.html',sep='')
    }

    filepath <- file.path(file)
    #Create temporary html file
    writeLines(out.html,filepath)
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
  if (Sys.getenv('RSTUDIO')=='1' & (out == 'viewer' | out == '')) {
    rstudioapi::viewer(htmlpath)
  } else if (Sys.getenv('RSTUDIO')=='' & out == 'viewer') {
    stop('out = viewer is not a valid option if RStudio is not running.')
  } else if ((Sys.getenv('RSTUDIO')=='' & out == '') | (out == 'browser')) {
    utils::browseURL(htmlpath)
  } else if (out == 'return') {
    return(vt)
  } else if (out == 'htmlreturn') {
    return(out.html)
  } else {
    stop('Unrecognized value of out. Set to \"viewer\", \"browser\", \"return\", \"htmlreturn\", or leave blank.')
  }
}


#' Data Frame to HTML Function
#'
#' This function takes a data frame or matrix with column names and outputs an HTML table version of that data frame.
#'
#' This function is designed to feed HTML versions of variable tables to vtable().
#'
#' @param data Data set; accepts any format with column names.
#' @param out Determines where the completed table is sent. Set to "browser" to open HTML file in browser using browseURL(), "viewer" to open in RStudio viewer using viewer(), if available, or "htmlreturn" to return the HTML code. Defaults to Defaults to "viewer" if RStudio is running and "browser" if it isn't.
#' @param file Saves the completed variable table file to HTML with this filepath. May be combined with any value of out.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @param row.names Flag determining whether or not the row names should be included in the table. Defaults to FALSE.
#' @examples
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),
#'     var4=as.factor(c('A','B','C','C')),var5=c(TRUE,TRUE,FALSE,FALSE))
#' dftoHTML(df,out="browser")
#'

#' @export
dftoHTML <- function(data,out=NA,file=NA,col.width=NA,row.names=FALSE) {
  if (is.null(colnames(data))) {
    stop('Requires data with variable names or column names.')
  }
  if (!is.na(file) & !is.character(file)) {
    stop('Incorrect file name.')
  }
  if (min(is.na(col.width)) == 0 & (!is.vector(col.width) | !is.numeric(col.width) | sum(is.na(col.width)) > 0)) {
    stop('col.width must be a numeric vector with no missing values.')
  }
  if (min(is.na(col.width)) == 0 & (max(col.width) > 100 | min(col.width) < 0)) {
    stop('Elements of col.width must be between 0 and 100.')
  }
  if (!is.logical(row.names)) {
    stop('The row.names option must be TRUE or FALSE.')
  }

  #If row.names = TRUE, the row names must be included as their own column
  if (row.names==TRUE) {
    data <- cbind(row.names(data),data)
    names(data)[1] <- "row.names"
  }

  #Get the column headers
  heads <- colnames(data)

  #Set default column widths
  if (sum(!is.na(col.width))==0) {
    col.width = rep(100/dim(data)[2],dim(data)[2])
  }

  #Turn column widths to rounded characters
  col.width <- as.character(round(as.numeric(col.width,2)))

  #Put together column widths with the column titles. First, create <th style = "width:pct%"> tags
  pctsh <- sapply(col.width,function(x) paste('<th style=\"width:',x,'%\">',sep=""))
  #Then, combine those tags with the column headers
  heads <- apply(cbind(pctsh,heads),1,paste0,collapse='')

  #Begin table html with a row of table headers, finishing off those '<th>Name' strings with '</td>'
  table.html <- paste('<table><tr>',
                    paste0(sapply(heads,function(x) paste(x,'</th>',sep='')),collapse=''),
                    '</tr>',sep='')

  #Convert rows of data to html
  #Put together column percentages with the td's, getting <td style = "width:pct%"> tags
  pctsd <- sapply(col.width,function(x) paste('<td style=\"width:',x,'%\">',sep=""))

  #Alright! This command.
  #The outer apply goes through each row of the data frame and sends it to the inner apply
  #The inner apply combines the <td> opener tags with the data in that cell using cbind(),
  #then collapses it all together so you get a single string <td>DATA.
  #Then t() transposes things back to how we need them
  #Split it based on the number of columns because if there's only one column,
  #the double-apply will squish it too far into just one row!
  if (dim(data)[2]==1) {
    vtablerows <- paste(pctsd[1],data[,1],sep="")
  } else{
    vtablerows <- t(apply(data,1,function(y) apply(cbind(pctsd,y),1,paste0,collapse='')))
  }
  #Then, this one starts with the inner sapply combining <td>DATA with </td> so we have <td>DATA</td>
  #Then, the outer apply takes each of those <td>DATA</td>s and slaps them all together
  #into a single string
  #Again, separate by number of columns to avoid over-smushing
  if (dim(data)[2]==1) {
    vtablerows <- paste(vtablerows,"</td>",sep='')
  } else{
    vtablerows <- t(apply(vtablerows,1,function(y)
      paste0(sapply(y,function(x) paste(x,'</td>',sep='')),collapse='')))
  }
  #Then finally, we take those <td>DATA</td><td>DATA</td> strings, wrap them
  #in <tr> and </tr>, and finally stick them all together to make the bulk of our table
  table.html <- paste(table.html,
                    paste0(sapply(vtablerows,function(x)
                      paste('<tr>',x,'</tr>',sep='')),collapse=''))
  #And close out the table
  table.html <- paste(table.html,'</table>',sep='')

  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    #If they forgot a file extension, fill it in
    if (!grepl("\\.",file)) {
      file <- paste(file,'.html',sep='')
    }

    filepath <- file.path(file)
    #Create temporary html file
    writeLines(table.html,filepath)
  }

  #For better evaluating if statements
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
    htmlpath <- file.path(tempDir,'dftoHTML.html')
    #Create temporary html file
    writeLines(table.html,htmlpath)
  }

  #Either print the variable table to the help window
  #or return a variable table to the screen, as desired

  if (Sys.getenv('RSTUDIO')=='1' & (out == 'viewer' | out == '')) {
    rstudioapi::viewer(htmlpath)
  } else if (Sys.getenv('RSTUDIO')=='' & out == 'viewer') {
    stop('out = viewer is not a valid option if RStudio is not running.')
  } else if ((Sys.getenv('RSTUDIO')=='' & out == '') | (out == 'browser')) {
    utils::browseURL(htmlpath)
  } else if (out == 'htmlreturn') {
    return(table.html)
  } else {
    stop('Unrecognized value of out. Set to \"viewer\", \"browser\", \"htmlreturn\", or leave blank.')
  }

}
