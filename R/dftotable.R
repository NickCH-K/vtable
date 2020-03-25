#' Data Frame to HTML Function
#'
#' This function takes a data frame or matrix with column names and outputs an HTML table version of that data frame.
#'
#' This function is designed to feed HTML versions of variable tables to \code{vtable()}, \code{sumtable()}, and \code{labeltable()}.
#'
#' Multi-column cells are supported. Set the cell's contents to \code{"content_MULTICOL_c_5"} where "content" is the content of the cell, "c" is the cell's alignment (l, c, r), and 5 is the number of columns to span. Then fill in the cells that need to be deleted to make room with "DELETECELL".
#'
#' If the first column and row begins with the text "HEADERROW", then the first row will be put above the column names.
#'
#' @param data Data set; accepts any format with column names.
#' @param out Determines where the completed table is sent. Set to \code{"browser"} to open HTML file in browser using \code{browseURL()}, \code{"viewer"} to open in RStudio viewer using \code{viewer()}, if available, or \code{"htmlreturn"} to return the HTML code. Defaults to Defaults to \code{"viewer"} if RStudio is running and \code{"browser"} if it isn't.
#' @param anchor Character variable to be used to set an \code{<a name>} tag for the table.
#' @param file Saves the completed variable table file to HTML with this filepath. May be combined with any value of \code{out}.
#' @param note Table note to go after the last row of the table.
#' @param col.width Vector of page-width percentages, on 0-100 scale, overriding default column widths in HTML table. Must have a number of elements equal to the number of columns in the resulting table.
#' @param col.align Vector of 'left', 'right', 'center', etc. to be used with the HTML table text-align attribute in each column. If you want to get tricky, you can add a \code{";"} afterwards and keep putting in whatever CSS attributes you want. They will be applied to the whole column.
#' @param row.names Flag determining whether or not the row names should be included in the table. Defaults to \code{FALSE}.
#' @param no.escape Vector of column indices for which special characters should not be escaped (perhaps they include markup text of their own).
#' @examples
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),
#'     var4=as.factor(c('A','B','C','C')),var5=c(TRUE,TRUE,FALSE,FALSE))
#' dftoHTML(df,out="browser")
#'

#' @export
dftoHTML <- function(data,out=NA,file=NA,note=NA,anchor=NA,col.width=NA,col.align=NA,row.names=FALSE,no.escape = NA) {
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
    names(data)[1] <- "Row Names"
  }

  #This assumes we work with characters
  for (i in 1:ncol(data)) {
    data[[i]] <- as.character(data[[i]])
  }
  #Put in the note
  if (!is.na(note)) {
    data[nrow(data)+1,] <- c(paste0(note,'_MULTICOL_l_all'),
                             rep('DELETECELL',ncol(data)-1))
  }

  #Set default column widths
  if (identical(col.width, NA)) {
    col.width <- rep(100/dim(data)[2],dim(data)[2])
  }
  #Set default column align
  if (identical(col.align, NA)) {
    col.align <- rep('left',dim(data)[2])
  }

  #Turn column widths to rounded characters
  col.width <- as.character(round(as.numeric(col.width,2)))
  #Combine column widths and aligns to form a style argument
  style <- paste0('width:',col.width,'%; text-align:',col.align)

  #Escape characters
  for (i in (1:ncol(data))[!(1:ncol(data) %in% no.escape)]) {
    data[[i]] <- as.character(data[[i]])
    data[[i]] <- gsub('\\&','\\&amp',data[[i]])
    data[[i]] <- gsub('<','\\&lt',data[[i]])
    data[[i]] <- gsub('>','\\&gt',data[[i]])
  }

  cellprocess <- function(x,celltype,style,maxall) {
    if (grepl('_MULTICOL_',x)) {
      #Split into the text and arguments
      spl <- strsplit(x,'_MULTICOL_')
      mcargs <- strsplit(spl[[1]][2],'_')

      #If it's "all", make it all the following DELETECELLs
      if (mcargs[[1]][2] == 'all') {
        mcargs[[1]][2] <- as.character(maxall)
      }

      align <- ifelse(mcargs[[1]][1] == 'l','left',
                      ifelse(mcargs[[1]][1] == 'r','right',
                             ifelse(mcargs[[1]][1] == 'c','center','oops')))
      if (align == 'oops') {
        stop("Unsupported multi-column alignment used. Use l, r, or c.")
      }

      #And construct the multicol
      x <- paste0('<',celltype,
             ' colspan = \"',mcargs[[1]][2],'\"',
             ' style = \"text-align: ',align,'\">',
             spl[[1]][1],'</',celltype,'>')
    } else {
      x <- paste0('<',celltype,
                  ' style = \"',style,'\">',
                  x,'</',celltype,'>')
    }
    return(x)
  }

  # Do this separately so as to allow for multicolumns
  rowprocess <- function(x,celltype) {
    x <- unname(x)
    x <- as.character(x)
    x[is.na(x)] <- ''
    rowstyle <- style[x != 'DELETECELL']
    # How many DELETECELLs follow each cell? Necessary for MULTICOL_X_all
    # Only bother if we have DELETECELLs
    if (any(x == 'DELETECELL')) {
      rl <- rle(x)
      #Start with 1s and only override if you are right next to a DELETECELL
      maxall <- rep(1,length(x))
      #Add 1 because we want to include both DELETECELLs and the original multicol
      maxall[which(x != 'DELETECELL' & c(utils::tail(x,-1) == 'DELETECELL',FALSE))] <-
        rl$lengths[rl$values == 'DELETECELL'] + 1
      maxall <- maxall[x != 'DELETECELL']
    } else {
      maxall <- rep(0,length(x))
    }
    x <- x[x != 'DELETECELL']
    x <- sapply(1:length(x), function(y) cellprocess(x[y],celltype,rowstyle[y],maxall[y]))
    return(paste('<tr>',paste(x, collapse = ''),'</tr>\n',sep =''))
  }

  #Begin table.html
  table.html <- '<table>'
  #Add an anchor if there is one
  if (!is.na(anchor)) {
    table.html <- paste0(table.html,'<a name = \"',anchor,'\">')
  }


  #Get the column headers
  heads <- colnames(data)
  headrow <- rowprocess(heads,'th')

  #Header row
  #Check for a secondary header row
  if (substr(data[1,1],1,9) == 'HEADERROW') {
    data[1,1] <- substring(data[1,1],10)
    hrow <- rowprocess(data[1,],'th')
    data <- data[2:nrow(data),]
    headrow <- paste(hrow,headrow)
  }

  #Convert rows of data to LaTeX
  rows <- apply(data, 1, function(x) rowprocess(x,'td'))
  rows <- paste(rows, collapse = '')

  #Then finally, we take those <td>DATA</td><td>DATA</td> strings, wrap them
  #in <tr> and </tr>, and finally stick them all together to make the bulk of our table
  table.html <- paste0(table.html,
                       headrow,
                       rows,
                       '</table>')

  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    #If they forgot a file extension, fill it in
    if (!grepl("\\.htm",file)) {
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


#' Data Frame to LaTeX Function
#'
#' This function takes a data frame or matrix with column names and outputs a lightly-formatted LaTeX table version of that data frame.
#'
#' This function is designed to feed LaTeX versions of variable tables to \code{vtable()}, \code{sumtable()}, and \code{labeltable()}.
#'
#' Multi-column cells are supported. Wrap the cell's contents in a \code{multicolumn} tag as normal, and then fill in any cells that need to be deleted to make room for the multi-column cell with "DELETECELL". Or use the MULTICOL syntax of \code{dftoHTML}, that works too.
#'
#' If the first column and row begins with the text "HEADERROW", then the first row will be put above the column names.
#'
#' @param data Data set; accepts any format with column names.
#' @param file Saves the completed table to LaTeX with this filepath.
#' @param frag Set to TRUE to produce only the LaTeX table itself, or FALSE to produce a fully buildable LaTeX. Defaults to TRUE.
#' @param title Character variable with the title of the table.
#' @param note Table note to go after the last row of the table.
#' @param anchor Character variable to be used to set a label tag for the table.
#' @param align Character variable with standard LaTeX formatting for alignment, for example \code{'lccc'}. You can also use this to force column widths with \code{p} in standard LaTeX style. Defaults to the first column being left-aligned and all others centered. Be sure to escape special characters, in particular backslashes (i.e. \code{p{.25\\\\textwidth}} instead of \code{p{.25\\textwidth}}).
#' @param row.names Flag determining whether or not the row names should be included in the table. Defaults to \code{FALSE}.
#' @param no.escape Vector of column indices for which special characters should not be escaped (perhaps they include markup text of their own).
#' @examples
#' df <- data.frame(var1 = 1:4,var2=5:8,var3=c('A','B','C','D'),
#'     var4=as.factor(c('A','B','C','C')),var5=c(TRUE,TRUE,FALSE,FALSE))
#' dftoLaTeX(df, align = 'ccccc')
#'

#' @export
dftoLaTeX <- function(data,file=NA,frag=TRUE,title=NA,note=NA,anchor=NA,align=NA,row.names=FALSE,no.escape = NA) {
  if (is.null(colnames(data))) {
    stop('Requires data with variable names or column names.')
  }
  if (!is.na(file) & !is.character(file)) {
    stop('Incorrect file name.')
  }
  if (!is.na(align) & (!is.character(align) | length(align) > 1)) {
    stop('Align must be a single character variable.')
  }
  if (!is.logical(row.names)) {
    stop('The row.names option must be TRUE or FALSE.')
  }

  #If row.names = TRUE, the row names must be included as their own column
  if (row.names==TRUE) {
    data <- cbind(row.names(data),data)
    names(data)[1] <- "Row Names"
  }

  #Work with everything as strings
  for (i in 1:ncol(data)) {
    data[[i]] <- as.character(data[[i]])
  }

  #Defaults
  if (is.na(align)) {
    align <- paste(rep('l',ncol(data)),collapse = '')
  }

  multicoller <- function(x,maxall) {
    if (grepl('_MULTICOL_',x)) {
      #Split into the text and arguments
      spl <- strsplit(x,'_MULTICOL_')
      mcargs <- strsplit(spl[[1]][2],'_')

      #If it's "all", make it all the columns
      if (mcargs[[1]][2] == 'all') {
        mcargs[[1]][2] <- as.character(maxall)
      }

      #And construct the multicol
      x <- paste0('\\multicolumn{',mcargs[[1]][2],'}{',mcargs[[1]][1],'}{',spl[[1]][1],'}')
    }
    return(x)
  }

  # Process multicols
  multicol.row <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ''
    # How many DELETECELLs follow each cell? Necessary for MULTICOL_X_all
    # Only bother if we have DELETECELLs
    if (any(x == 'DELETECELL')) {
      rl <- rle(x)
      #Start with 1s and only override if you are right next to a DELETECELL
      maxall <- rep(1,length(x))
      #Add 1 because we want to include both DELETECELLs and the original multicol
      maxall[which(x != 'DELETECELL' & c(utils::tail(x,-1) == 'DELETECELL',FALSE))] <-
        rl$lengths[rl$values == 'DELETECELL'] + 1
    } else {
      maxall <- rep(0,length(x))
    }
    x <- sapply(1:length(x),
                        function(j) multicoller(x[j],maxall[j]))
    return(x)
  }

  for (i in 1:nrow(data)) {
    # Do data.frame(t()) for tibble 3.0.0
    data[i,] <- data.frame(t(multicol.row(as.character(data[i,]))))
  }

  #Escape characters (Do this after multicol since that has _)
  for (i in (1:ncol(data))[!(1:ncol(data) %in% no.escape)]) {
    for (char in c('\\&','\\%','\\$','\\#','\\_')) {
      data[[i]] <- gsub(char,paste0('\\',char),data[[i]])
    }
    data[[i]] <- gsub('\\~','\\\\textasciitilde',data[[i]])
    data[[i]] <- gsub('\\^','\\\\textasciicircum',data[[i]])
  }
  if (!is.na(note)) {
    for (char in c('\\&','\\%','\\$','\\#','\\_')) {
      note <- gsub(char,paste0('\\',char),note)
    }
    note <- gsub('\\~','\\\\textasciitilde',note)
    note <- gsub('\\^','\\\\textasciicircum',note)
  }

  #Begin table latex code by opening the table
  table.latex <- '\\begin{table}[!htbp] \\centering \n \\renewcommand*{\\arraystretch}{1.1} \n'

  #Add a caption if there is one
  if (!is.na(title)) {
    table.latex <- paste0(table.latex,'\n\\caption{',title,'}\n')
  }
  #Add an anchor if there is one
  if (!is.na(anchor)) {
    table.latex <- paste0(table.latex,'\n\\label{',anchor,'}\n')
  }
  #Start the tabular
  table.latex <- paste0(table.latex,'\n\\begin{tabular}{',align,'}\n\\hline\n\\hline\n')

  #Get the column headers
  heads <- colnames(data)
  #Process
  heads <- multicol.row(heads)

  # Allow for multicolumns
  heads <- heads[heads != 'DELETECELL']
  headrow <- paste(heads, collapse = ' & ')
  headrow <- paste(headrow,'\\\\ \n\\hline\n')

  # Do this separately so as to allow for multicolumns
  rowprocess <- function(x) {
    x <- unname(x)
    x <- as.character(x)
    x <- x[x != 'DELETECELL']
    return(paste(x, collapse = ' & '))
  }

  #Check for a header row
  if (substr(data[1,1],1,9) == 'HEADERROW') {
    data[1,1] <- substring(data[1,1],10)
    hrow <- paste(rowprocess(data[1,]),' \\\\ \n')
    data <- data[2:nrow(data),]
    headrow <- paste(hrow,headrow)
  }

  #Convert rows of data to LaTeX
  rows <- apply(data, 1, rowprocess)
  rows <- paste(rows, collapse = ' \\\\ \n')

  #Paste the opener, header row, and rows
  table.latex <- paste0(table.latex,headrow,rows)

  #And close the table
  table.latex <- paste0(table.latex,'\\\\ \n\\hline\n\\hline\n')
  if (!is.na(note)) {
    table.latex <- paste0(table.latex,
                          '\\multicolumn{',ncol(data),'}{l}{',note,'}\\\\ \n')
  }
  table.latex <- paste0(table.latex,'\\end{tabular}\n\\end{table}')

  #Make into a page if requested
  if (!frag) {
    table.latex <- paste0('\\documentclass{article}\n\\begin{document}\n\n',
                          table.latex,
                          '\n\n\\end{document}')
  }

  ####### APPLICATION OF FILE OPTION
  if (!is.na(file)) {
    #If they forgot a file extension, fill it in
    if (!grepl("\\.tex",file)) {
      file <- paste(file,'.tex',sep='')
    }

    filepath <- file.path(file)
    #Create temporary tex file
    writeLines(table.latex,filepath)
  }

  return(table.latex)
}
