#' Label Table Function
#'
#' This function output a descriptive table listing, for each value of a given variable, either the label of that value, or all values of another variable associated with that value. The table is output either to the console or as an HTML file that can be viewed continuously while working with data.
#'
#' Outputting the label table as a help file will make it easy to search through value labels, or to see the correspondence between the values of one variable and the values of another.
#'
#' Labels that are not in the data will also be reported in the table.
#'
#' @param var A vector. Label table will show, for each of the values of this variable, its label (if labels can be found with sjlabelled::get_labels()), or the values in the ... variables.
#' @param ... As described above. If specified, will show the values of these variables, instead of the labels of var, even if labels can be found.
#' @param out Determines where the completed table is sent. Set to "browser" to open HTML file in browser using browseURL(), "viewer" to open in RStudio viewer using viewer(), if available. Use "htmlreturn" to return the HTML code to R, or "return" to return the completed variable table to R in data frame form. Defaults to "viewer" if RStudio is running and "browser" if it isn't.
#' @param file Saves the completed variable table file to HTML with this filepath. May be combined with any value of out.
#' @param desc Description of variable (or labeling system) to be included with the table.
#' @examples
#' \dontshow{
#' #These tests use the out='htmlreturn' option
#' #so that the same process of generating HTML is followed
#' #but a browser window is not opened during testing.
#' #This process is identical to regular operation except that
#' #HTML is written to the R output rather than a browser.
#'
#' #Input a single labelled variable to see a table relating values to labels.
#' #Values not present in the data will be included in the table but moved to the end.
#' library(sjlabelled)
#' data(efc)
#' labeltable(efc$e15relat,out='htmlreturn')
#'
#' #Include multiple variables to see, for each value of the first variable,
#' #each value of the others present in the data.
#' data(efc)
#' labeltable(efc$e15relat,efc$e16sex,efc$e42dep,out='htmlreturn')
#'
#' #Commonly, the multi-variable version might be used to recover the original
#' #values of encoded variables
#' data(USJudgeRatings)
#' USJudgeRatings$Judge <- row.names(USJudgeRatings)
#' USJudgeRatings$JudgeID <- as.numeric(as.factor(USJudgeRatings$Judge))
#' labeltable(USJudgeRatings$JudgeID,USJudgeRatings$Judge,out='htmlreturn')
#' }
#' if(interactive()){
#' #Input a single labelled variable to see a table relating values to labels.
#' #Values not present in the data will be included in the table but moved to the end.
#' library(sjlabelled)
#' data(efc)
#' labeltable(efc$e15relat)
#'
#' #Include multiple variables to see, for each value of the first variable,
#' #each value of the others present in the data.
#' data(efc)
#' labeltable(efc$e15relat,efc$e16sex,efc$e42dep)
#'
#' #Commonly, the multi-variable version might be used to recover the original
#' #values of encoded variables
#' data(USJudgeRatings)
#' USJudgeRatings$Judge <- row.names(USJudgeRatings)
#' USJudgeRatings$JudgeID <- as.numeric(as.factor(USJudgeRatings$Judge))
#' labeltable(USJudgeRatings$JudgeID,USJudgeRatings$Judge)
#' }
#' @export

labeltable <- function(var,...,out=NA,file=NA,desc=NA) {
  #Just in case, noting that if ...s are labeled,
  #but a package that supports the class isn't loaded it messes things up
  comp.vars <- data.frame(lapply(list(...),function(x) sjlabelled::unlabel(x)))
  names(comp.vars) <- sapply(as.list(substitute(list(...)))[-1L], function(x)
    utils::tail(as.character(x),n=1))

  if (ncol(comp.vars)==0 & is.null(unlist(sjlabelled::get_labels(var)))) {
    stop('Either var must have labels, or variables must be specified in ...')
  }
  if (!is.na(desc) & !is.character(desc)) {
    stop('desc must be a character.')
  }

  #Get actual name of variable
  var.name <- deparse(substitute(var))
  var.name <- utils::tail(strsplit(var.name,'\\$')[[1]],1)

  #labels version
  if (ncol(comp.vars)==0) {
    #Put in a data frame for working with
    lt <- data.frame(var)
    #Only need one of each value
    lt <- subset(lt,!duplicated(lt))

    #Why aren't labels preserved with subset anyway??
    lt$var <- sjlabelled::set_labels(lt$var,labels=attr(var,'labels'))
    #Create the column with labels
    lt$labs <- sjlabelled::as_label(lt$var)
    lt <- lt[order(lt$var),]
    lt$var <- as.character(lt$var)

    #And a row with the unused labels
    #extract all labels
    labs <- attr(var,'labels')
    #find which ones aren't present
    labs <- labs[!(names(labs) %in% lt$labs)]
    if (length(labs) > 0) {
      #Get into the same format as above
      lt2 <- data.frame(labs)
      names(lt2)[1] <- 'var'
      lt2$var <- paste(lt2$var,' [NOT IN DATA]',sep='')
      lt2$labs <- row.names(lt2)

      lt <- rbind(lt,lt2)
    }

    names(lt) <- c(var.name,'Label')
  }
  #comp.var version
  else {
    #Put in a data frame for working with
    prelt <- data.frame(var,comp.vars)
    #Only need one of each value
    prelt <- subset(prelt,!duplicated(prelt))

    lt <- data.frame(
      var=unique(prelt$var),
      lapply(names(prelt)[-1],function(y) sapply(unique(prelt$var),function(x)
        paste0(unique(subset(prelt,prelt$var==x)[[y]]),collapse=', '))))

    lt <- lt[order(lt$var),]
    names(lt) <- c(var.name,names(prelt)[-1])
  }

  ####### CONSTRUCTION OF HTML
  #Head of file
  out.html <- paste('
                    <html style=\"font-family:Helvetica,Arial,Sans\">
                    <head><title>',var.name,' Label Table</title>',
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
                    '<tr><td style=\"text-align:left\">labeltable {vtable}</td>',
                    '<td style=\"text-align:right\">Variable Documentation: Label Table</td></tr></table>',
                    '<h1>',var.name,'</h1>')

  #Applying description
  if (!is.na(desc)) {
    out.html <- paste(out.html,'<p>',desc,'</p>',sep='')
  }

  out.html <- paste(out.html,'<h3>Label Table</h3>',sep='')

  #And bring in the table itself
  out.html <- paste(out.html,dftoHTML(lt,out='htmlreturn'),'</body></html>',sep='')

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
    htmlpath <- file.path(tempDir,'labeltable.html')
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
    return(lt)
  } else if (out == 'htmlreturn') {
    return(out.html)
  } else {
    stop('Unrecognized value of out. Set to \"viewer\", \"browser\", \"return\", \"htmlreturn\", or leave blank.')
  }

}
