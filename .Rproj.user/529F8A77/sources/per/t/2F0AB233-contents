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
#' @param values Set to TRUE to include the range of values of each variable: min and max for numeric variables, list of factors for factor variables, and 'TRUE FALSE' for logicals. values will detect and use value labels set by the sjlabelled or haven packages. Defaults to FALSE.
#' @param factor.limit Sets maximum number of factors that will be included if values = TRUE. Set to 0 for no limit. Defaults to 5.
#' @param data.title Character variable with the title of the dataset.
#' @param desc Character variable offering a brief description of the dataset itself.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @examples
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),var4=as.factor(c('A','B','C','C')),var5=c(T,T,F,F))
#' #Demonstrating different options:
#' vtable(df,labels=c('Number 1','Number 2','Some Letters','Some Labels','You Good?'))
#' vtable(subset(df,select=c(1,2,5)),labels=c('Number 1','Number 2','You Good?'),class=FALSE)
#' vtable(subset(df,select=c('var1','var4')),labels=c('Number 1','Some Labels'),values=TRUE,factor.limit=1,col.width=c(10,10,40,35))
#' #Different methods of applying variable labels:
#' labelsmethod2 <- data.frame(var1='Number 1',var2='Number 2',var3='Some Letters',var4='Some Labels',var5='You Good?')
#' labelsmethod3 <- data.frame(a =c("var1","var2","var3","var4","var5"),b=c('Number 1','Number 2','Some Letters','Some Labels','You Good?'))
#' vtable(df,labels=labelsmethod2)
#' vtable(df,labels=labelsmethod3)
#' #Using value labels and pre-labeled data:
#' library(sjlabelled)
#' df <- set_label(df,c('Number 1','Number 2','Some Letters','Some Labels','You Good?'))
#' df$var1 <- set_labels(df$var1,labels=c('A little','Some more','Even more','A lot'))
#' vtable(df)
#' #efc is data with variable and value lables from the sjlabelled package
#' data(efc)
#' vtable(efc)
#'
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
vtable <- function(data,out=NA,file=NA,labels=NA,class=TRUE,values=FALSE,
                   factor.limit=5,data.title=NA,desc=NA,col.width=NA) {

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

  ####### APPLICATION OF DATA.TITLE OPTION
  #If data.title is missing, fill in with name of the data frame read in
  if (is.na(data.title)) {
    data.title <- deparse(substitute(data))
  }

  ####### FORM VARIABLE TABLE TO BUILD ON
  #Start table with variable names or column names (earlier error check ensures one exists)
  vt <- data.frame(Name = colnames(data))


  ####### APPLICATION OF CLASS OPTION
  #If user asks for variable classes, add them to the variable table
  if (class == TRUE) {
    vt$Class <- sapply(data,class)
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
  if (min(is.na(labels))==0 & (length(labels) == 1 & labels[1] == "omit") == FALSE){
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

  } else if (min(is.na(labels))==0 & (length(labels) == 1 & labels[1] == "omit") == TRUE) {
    vt$Label <- NULL
  }

  ####### APPLICATION OF VALUES OPTION
  #If user wants values, show the possible values the variable can take
  if (values == TRUE) {
    #Create variable to hold ranges
    vt$Values <- ''

    #Determine whether we need to be looking for labeled values with sjlabelled or haven
    if ('sjlabelled' %in% .packages() | 'haven' %in% .packages()) {
      #Are there any labelled values?
      if (!is.null(unlist(get_labels(data)))) {
        #Since we've already extracted class, if necessary,
        #we can just turn these into factor variables with an included
        #numerical coding for clarity

        #Pull out labels
        vallabs <- get_labels(data,values=TRUE)
        #Add numerical coding
        vallabscode <- lapply(vallabs, function(x) paste(names(x),': ',x,sep=''))
        #Make sure the labels are named chr vectors
        names(vallabscode) <- lapply(vallabs,function(x) names(x))

        #Identify which variables had labels
        havelabels <- sapply(vallabs,function(x) !is.null(x))
        #Don't count the factors, we don't want to relabel them
        havelabels[sapply(data,is.factor)] <- FALSE
        #Set new coded labels among the variables with value labels
        data[,havelabels] <- set_labels(data[,havelabels],labels=vallabscode[havelabels])

        #And turn 'em into factors
        data[,havelabels] <- as_label(data[,havelabels])
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
      min <- lapply(subset(data,select=sapply(data,is.numeric)),min,na.rm=TRUE)

      #Get maximums
      max <- lapply(subset(data,select=sapply(data,is.numeric)),max,na.rm=TRUE)

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

  #If there's a description
  if (!is.na(desc)) {
    out.html <- paste(out.html,'<p>',desc,'</p>',sep='')
  }

  out.html <- paste(out.html,'<h3>Variable Table</h3>',sep='')

  ####### APPLICATION OF COL.WIDTH OPTION
  #column percentages
  #Check which columns we have to account for
  haslabels <- 'Label' %in% colnames(vt)
  hasclass <- 'Class' %in% colnames(vt)
  hasvalues <- 'Values' %in% colnames(vt)
  #If col.width not manually set, use defaults
  if (sum(!is.na(col.width)) == 0) {
    #Default values:
    #All four columns: Name 25%, class 10%, label 40%, Values 25%
    #Name only: Name 50%
    #Name and class: Name 35%, class 20%
    #Name and label: Name 35%, Label 65%
    #Name and values: Name 35%, Label 65%
    #Name, class, label: Name 30%, Class 10%, Label 55%
    #Name, class, Values: Name 30%, Class 10%, Values 55%
    #Name, label, values: Name 25%, label 45%, Values 30%
    #By default, names gets 20% with labels, 30% without.
    if (haslabels == 1 & hasclass == 1 & hasvalues == 1) {
      col.width <- c(25,10,40,25)
    } else if (haslabels == 0 & hasclass == 1 & hasvalues == 1) {
      col.width <- c(30,10,55)
    } else if (haslabels == 1 & hasclass == 0 & hasvalues == 1) {
      col.width <- c(25,45,30)
    } else if (haslabels == 1 & hasclass == 1 & hasvalues == 0) {
      col.width <- c(30,10,55)
    } else if (haslabels == 0 & hasclass == 0 & hasvalues == 1) {
      col.width <- c(35,65)
    } else if (haslabels == 0 & hasclass == 1 & hasvalues == 0) {
      col.width <- c(35,20)
    } else if (haslabels == 1 & hasclass == 0 & hasvalues == 0) {
      col.width <- c(35,65)
    } else if (haslabels == 0 & hasclass == 0 & hasvalues == 0) {
      col.width <- c(50)
    }
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

  ####### APPLICATION OF OUT OPTION
  #If the plan is to produce a viewable HTML, create it
  if (out == 'viewer' | out == 'browser' | is.na(out)) {
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
  if (out == 'viewer' | (Sys.getenv('RSTUDIO')==1 & is.na(out))) {
    rstudioapi::viewer(htmlpath)
  } else if (out == 'browser' | (Sys.getenv('RSTUDIO')==0 & is.na(out))) {
    browseURL(htmlpath)
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
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),var4=as.factor(c('A','B','C','C')),var5=c(T,T,F,F))
#' dftoHTML(data,out="browser")
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

  ####### APPLICATION OF OUT OPTION
  #If the plan is to produce a viewable HTML, create it
  if (out == 'viewer' | out == 'browser' | is.na(out)) {
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
  if (out == 'viewer' | (Sys.getenv('RSTUDIO')==1 & is.na(out))) {
    rstudioapi::viewer(htmlpath)
  } else if (out == 'browser' | (Sys.getenv('RSTUDIO')==0 & is.na(out))) {
    browseURL(htmlpath)
  } else if (out == 'htmlreturn') {
    return(table.html)
  } else {
    stop('Unrecognized value of out. Set to \"viewer\", \"browser\", \"htmlreturn\", or leave blank.')
  }

}
