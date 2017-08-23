# Things you might want to change

# options(papersize="a4")
# options(editor="notepad")
# options(pager="internal")

# set the default help type
# options(help_type="text")
  options(help_type="html")

# set a site library
# .Library.site <- file.path(chartr("\\", "/", R.home()), "site-library")

# set a CRAN mirror
# local({r <- getOption("repos")
#       r["CRAN"] <- "http://my.local.cran"
#       options(repos=r)})

# Give a fortune cookie, but only to interactive sessions
# (This would need the fortunes package to be installed.)
#  if (interactive()) 
#    fortunes::fortune()

  
#  install.packages("RJDBC") # If any error with rJava go to: https://www.r-statistics.com/tag/rjava/
#  install.packages("data.table")
#  install.packages("tidyr")
#  install.packages("openxlsx")
#  install.packages("XLConnect")
#  install.packages("readxl")
#  install.packages("ggplot2")
#  install.packages("stringr")
#  install.packages("plyr")
#  install.packages("dplyr")
#  install.packages("pastecs")
#  install.packages("countrycode")
#  install.packages("scatterplot3d")
#  install.packages("rgl")
#  install.packages("car")
#  install.packages("shape")
#  install.packages("graphics")
#  install.packages("Surrogate")
#  install.packages("fields")
#  install.packages("WDI")
#  install.packages("qdap")
#  install.packages("plotrix")
#  install.packages("microbenchmark")
#  install.packages("ineq")
#  install.packages("gdxrrw") # Problem > GDXRRW: Interfacing GAMS and R >> Not necessary now
#  install.packages("gridExtra")
#  install.packages("ggrepel")
#  install.packages("colorRamps")
#  install.packages("devtools")  # This library needed to do multiple returns from functions  
  
  
options(java.parameters = "-Xmx16g") 
library(RJDBC) # If any error with rJava go to: https://www.r-statistics.com/tag/rjava/
library(data.table)
library(tidyr)
library(openxlsx)
library(XLConnect)
library(readxl)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(pastecs)
library(countrycode)
library(scatterplot3d)
library(rgl)
library(car)
library(shape)
library(graphics)
library(Surrogate)
library(fields)
library(WDI)
library(qdap)
library(plotrix)
library(data.table)
library(microbenchmark)
library(ineq)
library(gdxrrw) # Problem > GDXRRW: Interfacing GAMS and R >> Not necessary now
library(gridExtra)
library("ggrepel")
library(colorRamps)
library(devtools)  # This library needed to do multiple returns from functions

source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

# Run RAS and construct final matrix in original dimension
library(mipfp)

#setwd("H:/MyDocuments/IO work/DLE_scripts")
#setwd("C:/Users/tudeschi/SharePoint/DLE - Documents/WS2 - Documents/Analysis/IO/Code/")
setwd("C:/Users/tudeschi/OneDrive - IIASA/Projects/DLE_scripts_Git/")

xlcFreeMemory()
# source("Generic function to access database.R") # Better run the Script directly




.First <- function(){
	.ls.objects <- function (pos = 1, pattern, order.by,
	                        decreasing=FALSE, head=FALSE, n=5) {
	    napply <- function(names, fn) sapply(names, function(x)
	                                         fn(get(x, pos = pos)))
	    names <- ls(pos = pos, pattern = pattern)
	    obj.class <- napply(names, function(x) as.character(class(x))[1])
	    obj.mode <- napply(names, mode)
	    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
	    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    	obj.size <- napply(names, object.size)
	    obj.dim <- t(napply(names, function(x)
	                        as.numeric(dim(x))[1:2]))
	    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
	    obj.dim[vec, 1] <- napply(names, length)[vec]
	    out <- data.frame(obj.type, obj.size, obj.dim)
	    names(out) <- c("Type", "Size", "Rows", "Columns")
	    if (!missing(order.by))
	        out <- out[order(out[[order.by]], decreasing=decreasing), ]
	    if (head)
	        out <- head(out, n)
	    out
	}
	# shorthand
	lsos <- function(..., n=10) {
	    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
	}
	
	view <- function(data, autofilter=TRUE) {
    # data: data frame
    # autofilter: whether to apply a filter to make sorting and filtering easier
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')
    require(XLConnect)
    temp_file <- paste0(tempfile(), '.xlsx')
    wb <- XLConnect::loadWorkbook(temp_file, create = TRUE)
    XLConnect::createSheet(wb, name = "temp")
    XLConnect::writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
    XLConnect::saveWorkbook(wb, )
    system(paste(open_command, temp_file))
	}
}
