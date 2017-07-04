# Create database connection to HH_DATA via RJDBC (Java)
require(RJDBC)
options(java.parameters = "-Xmx96g")  # Arbitrarily set to 96GB; doesn't seem to matter if too high
drv = JDBC("oracle.jdbc.driver.OracleDriver","C:/Users/tudeschi/Documents/DLE_scripts/ojdbc6.jar", identifier.quote="\"") 
db_conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")

#------------

selectDBdata = function(..., tables, lower=TRUE, conn=NULL, fun=NULL) {
  
  # If database connection is not specified, use 'db_conn' object in global environment
  require(dplyr)
  if (is.null(conn)) conn=get('db_conn', envir=globalenv())
  
  # Coerce arguments to upper case for compatability with Oracle DB
  vars = toupper(as.character(substitute(list(...)))[-1])
  tables = toupper(tables)

  # Identify partial matching table names if none specified explicitly
  # Return error message if feasible table(s) cannot be identified
  tbs = dbFetch(dbSendQuery(conn,"select TABLE_NAME from user_tables"))[[1]]
  if (length(tables)==1) {
    if (!tables %in% tbs) tables = sort(grep(paste0("*",tables,"$"), tbs, value=T))
  }
  if (length(tables)==0 | any(!tables %in% tbs)) stop("Cannot find tables that match given argument(s)")
  
  # Extract column names associated with each table
  col_names = dbFetch(dbSendQuery(conn, paste(
    "SELECT table_name, column_name
    FROM USER_TAB_COLUMNS
    ",paste0("WHERE table_name IN ('", paste(tables,collapse="','"),"')"))))

  # Check if 'variable' argument is valid; if not, return error indicating erroneous value(s)
  miss = setdiff(vars, col_names$COLUMN_NAME)
  if (length(miss)>0) stop("Variable(s) not present in the table(s): ", paste(miss,collapse=","))
  
  # Fetch data, pulling only the set of suitable column names
  # Bind (append) individual tables via 'bind_rows'
  out = bind_rows(lapply(tables, function(x) {
    if (length(vars)==0) {
      y = dbReadTable(conn, x)
    } else {
      v = intersect(vars, filter(col_names, TABLE_NAME==x)$COLUMN_NAME)
      y = dbFetch(dbSendQuery(conn, paste("SELECT",paste(v, collapse=","),"FROM",x)))
    }
    if (lower) names(y) = tolower(names(y))
    if (!is.null(fun)) y = fun(y)
    return(y)
  }))
  
  return(out)
  
}

#------------

# TESTING 1
#test = selectDBdata(ID, REGION, CONSUMPTION, tables=c('ZAF1_HH','ZAF2_HH'))
#test = selectDBdata(tables=c('ZAF1_HH','ZAF2_HH'))

#------------

# TESTING 2
# require(tidyr)
# 
# food_codes = data_frame(
#   code = c('11111251', '02111100', '01112101', '01112102', '01145101'),
#   food = c('Beverage', 'Beverage', 'Bread', 'Bread', 'Cheese')
# )
# 
# myFun = function(x) {
#   x %>% 
#     inner_join(food_codes) %>% 
#     group_by(survey, id, food) %>% 
#     summarize_each(funs(sum), val_tot) %>% 
#     spread(food, val_tot, fill=0)
# }
# 
# test = selectDBdata(survey, id, code, val_tot, tables=c('ZAF1_FOOD','ZAF2_FOOD'), fun=myFun)

#------------
# END
#------------

# Original email message from Kevin Ummel to ENE group explaining how to use selectDBdata()

# From:	UMMEL Kevin
# Sent:	Friday, April 15, 2016 09:32
# To:	RAO Narasimha; PACHAURI Shonali; PARKINSON Simon; MIN Jihoon; GIDDEN 
# Matthew; ZIMM Caroline
# Subject:	RE: How to access household data in Oracle DB
# 
# Two simple/useful improvements occurred to me overnight:
# 
# -- You can now specify NO specific variables, and the function will return ALL available variables/columns 
# by default:
# 
# > test = selectDBdata(tables=c('ZAF1_HH','ZAF2_HH'))
# > dim(test)
# [1] 46472    36
# 
# -- You can now pass a custom pre-processing function (argument ?fun?) which will be applied to the data 
# frame of each individual table. I envisions some potentially useful applications. In the following example, 
# I construct a ?food_codes? data frame that links specific food codes in the ZAF surveys to generic food 
# categories. I then write a simple custom function (myFun) that merges this information, aggregates 
# expenditures, and ?spreads? results by household and food category. Easier seen than explained:
# 
# > require(tidyr)
# Loading required package: tidyr
# > food_codes = data_frame(
# +   code = c('11111251', '02111100', '01112101', '01112102', '01145101'),
# +   food = c('Beverage', 'Beverage', 'Bread', 'Bread', 'Cheese')
# + )
# > myFun = function(x) {
# +   x %>% 
# +     inner_join(food_codes) %>% 
# +     group_by(survey, id, food) %>% 
# +     summarize_each(funs(sum), val_tot) %>% 
# +     spread(food, val_tot, fill=0)
# + }
# > test = selectDBdata(survey, id, code, val_tot, tables=c('ZAF1_FOOD','ZAF2_F
# OOD'), fun=myFun)
# Joining by: "code"
# Joining by: "code"
# > test
# Source: local data frame [41,723 x 5]
# 
#    survey                 id Beverage  Bread Cheese
#     (chr)              (chr)    (dbl)  (dbl)  (dbl)
# 1    ZAF1 101000170000000801     0.00  41.15   0.00
# 2    ZAF1 101000170000002901    29.89   0.00   0.00
# 3    ZAF1 101000170000005001     0.00  84.44   0.00
# 4    ZAF1 101000220000008701     0.00  34.36 104.24
# 5    ZAF1 101000220000012201     0.00 322.05   0.00
# 
# The primary advantage here is that ?myFun? performs the desired operations on each individual table 
# before combining/returning results. If you are querying very large tables, this can reduce local memory 
# requirements by allowing R to perform aggregation operations on individual tables one at a time. This 
# setup is also amendable to multi-core processing, should folks require that kind of power down the 
# road.
# 
# Cheers,
# Kevin
# 
# 
# From: UMMEL Kevin  
# Sent: Thursday, April 14, 2016 16:21 
# To: RAO Narasimha <nrao@iiasa.ac.at>; PACHAURI Shonali <pachauri@iiasa.ac.at>; PARKINSON Simon 
# <parkinso@iiasa.ac.at>; MIN Jihoon <min@iiasa.ac.at>; GIDDEN Matthew <gidden@iiasa.ac.at>; ZIMM 
# Caroline <zimmc@iiasa.ac.at> 
# Subject: How to access household data in Oracle DB
# 
# I?ve received multiple requests for a convenient R function to ?grab? data from the Oracle household 
# database; I expect this will also be a demand of new hires and YSSP?s in future. It?s not immediately clear 
# to me what such function(s) necessarily should/need to do ? or how much speed/performance matter in 
# practice ? but I?ve taken a crack at a simple function for getting a subset of data into local memory to 
# work with in R.  If there is additional functionality that you know will be required beyond what is 
# below, please let me know.
# 
# You can load the new function ? named ?selectDBdata? ? by calling (for now) the following from within 
# R:
# 
# > source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to acces
# s database.R")
# 
# By default, this will set up a connection to the HH_DATA database (schema). It requires that both the 
# RJDBC and dplyr packages be installed. The selectDBdata() function allows you to specify the variables 
# (column names) you require and the tables from which to extract data. Variable names can be given as 
# convenient unquoted (bare) names, similar to dplyr?s select() function. The table names must be quoted. 
# For example:
# 
# > test = selectDBdata(ID, REGION, CONSUMPTION, tables=c('ZAF1_HH','ZAF2_HH'))
# 
# This extracts the specified variables from the ZAF1_HH and ZAF2_HH tables and returns appended result 
# as a ?tibble? (i.e. data frame):
# 
# > class(test)
# [1] "tbl_df"     "tbl"        "data.frame"
# > dim(test)
# [1] 46472     3
# > test
# Source: local data frame [46,472 x 3]
# 
#                    id       region consumption
#                 (chr)        (chr)       (dbl)
# 1  104000290000027901 Western Cape     6953.39
# 2  104000290000031201 Western Cape    10695.43
# 3  104000630000001201 Western Cape    42425.59
# ..                ...          ...         ...
# 
# Note column names are coerced to lower case by default (turn off using argument lower=FALSE). The 
# function is not case-sensitive (since column names in Oracle DB are not case-sensitive), and it will return 
# an informative error if you tell it to do something impossible:
# 
# > selectDBdata(Id, regiON, consumption, tables=c('zaf1_HH','ZAF2_hh'))
# Source: local data frame [46,472 x 3]
# 
#                    id       region consumption
#                 (chr)        (chr)       (dbl)
# 1  104000290000027901 Western Cape     6953.39
# 2  104000290000031201 Western Cape    10695.43
# 3  104000630000001201 Western Cape    42425.59
# ..                ...          ...         ...
# 
# > selectDBdata(ID, REGION, CONSUMPTION, cupcakes, tables=c('ZAF1_HH','ZAF2_HH
# '))
# Error in selectDBdata(ID, REGION, CONSUMPTION, cupcakes, tables = c("ZAF1_HH"
# ,  : 
#   Following variable(s) not present in any of the tables: CUPCAKES
# 
# If you wish to obtain variables from all of the ?_HH? tables in the database (multiple surveys), you can 
# specify the ?tables? argument generically and the function will return results for all tables that contain 
# the specified ending:
# 
# > test = selectDBdata(SURVEY, ID, REGION, CONSUMPTION, tables='HH')
# > dim(test)
# [1] 826043      4
# > table(test$survey)
# 
#   BRA1   BRA2   GHA1   GHA2   IDN1   IND1   IND2   IND3   IND4   IND5   ZAF
# 1   ZAF2 
#  55970  48470   8687  16772 282387 101662 124644  57273  42152  41554  2532
# 8  21144 
# > test
# Source: local data frame [826,043 x 4]
# 
#    survey       id   region consumption
#     (chr)    (chr)    (chr)       (dbl)
# 1    BRA1 11004302 Rond?nia    2381.320
# 2    BRA1 11004303 Rond?nia   23703.515
# 3    BRA1 11004304 Rond?nia   25513.754
# ..    ...      ...      ...         ...
# 
# And similar for other table types (e.g. ASSET, FOOD, etc.):
# 
# > test = selectDBdata(survey, id, item, pos, tables='ASSET')
# > dim(test)
# [1] 3981238       4
# > test
# Source: local data frame [3,981,238 x 4]
# 
#    survey       id                      item   pos
#     (chr)    (chr)                     (chr) (dbl)
# 1    BRA1 11002704              Stereo/Hi-Fi     1
# 2    BRA1 11002704                     Stove     1
# 3    BRA1 11002704         Television, color     1
# ..    ...      ...                       ...   ...
# 
# We can integrate easily with dplyr manipulation of the results. Below, I calculate mean expenditure per 
# household, by urban/rural, across all available surveys and add survey identifier (metadata) information 
# to the results from another table (ignoring GHA1 since I have yet to fix the currency issue):
# 
# > test = selectDBdata(survey, weight, urban, expenditure, tables='HH') %>% 
# +   group_by(survey, urban) %>% 
# +   summarize(expend_hh = round(weighted.mean(expenditure, weight, na.rm=T))) 
# %>% 
# +   filter(survey!="GHA1") %>% 
# +   left_join(selectDBdata(survey, survey_name, tables="SURVEY_DATA"))
# Joining by: "survey"
# > test
# Source: local data frame [22 x 4]
# Groups: survey [?]
# 
#    survey urban expend_hh          survey_name
#     (chr) (dbl)     (dbl)                (chr)
# 1    BRA1     0      9373    BRA POF 2008-2009
# 2    BRA1     1     17596    BRA POF 2008-2009
# 3    BRA2     0     10916    BRA POF 2002-2003
# 4    BRA2     1     16334    BRA POF 2002-2003
# 5    GHA2     0      6267  GHA GLSS6 2012-2013
# 6    GHA2     1     14804  GHA GLSS6 2012-2013
# 7    IDN1     0      3585   IDN NSES July 2008
# 8    IDN1     1      6427   IDN NSES July 2008
# 9    IND1     0      3966 IND NSS 68 2011-2012
# 10   IND1     1      7638 IND NSS 68 2011-2012
# ..    ...   ...       ...                  ...
# 
# Note that the function is written for convenience rather than speed. It is designed to simply get data to 
# your machine, where you can manipulate, merge, etc. via dplyr and tidyr. If you are making calls on the 
# database that stress the RAM limits of your machine or are unacceptably slow, then come see me. 
# Ideally, none of this would be necessary and dplyr code could be written in R, translated to SQL 
# silently/automatically, executed on the server itself, and the processed results returned to local 
# machine. This functionality actually already exists in dplyr for many open-source database backends 
# (https://gist.github.com/piccolbo/3d8ac40291f4eaee644b), but not Oracle (grrr). There is no 
# fundamental reason this capability could not be extended to an Oracle backend, but efforts to date have 
# stalled (https://github.com/tomasgreif/dplyrOracle), and I am not inclined to take on the project myself.
# 
# Cheers, 
# Kevin