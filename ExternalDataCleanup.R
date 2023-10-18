
# Introduction ------------------------------------------------------------

# Clean up of external datasets for use with H-D dealer network project
# Mars & Co Consulting
# Created October 2023


# Libraries ---------------------------------------------------------------

library(Hmisc)
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

# Helper functions
copyfromexcel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

copytoexcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


# File Setup -------------------------------------------------------------

chr.wd = getwd()
# uncomment and change this to manually set working dir:
# chr.wd = "C:/Users/wgeyer/OneDrive - Mars & Co Consulting LLC/Documents/HDrepo"
setwd(chr.wd)

if (!dir.exists("./ExternalDataRaw")) {
  dir.create("./ExternalDataRaw")
}
if (!dir.exists("./ExternalDataOutput")) {
  dir.create("./ExternalDataOutput")
}

# Data Pull ---------------------------------------------------------------

dt.s1901 = data.table(read.csv("./ExternalDataRaw/ACSST5Y2021.S1901-Data.csv"))
dt.s1901_colmap = data.table(V1=colnames(dt.s1901),V2=unname(t(dt.s1901[1]),force = T)) %>% 
  setnames(old=c("V1","V2.V1"),new=c("colm","descr"))
dt.s1901 = dt.s1901[!1]
dt.s1901_colmap[str_sub(colm,-1,-1)=="E"] %>% View
chr.colm_keep = c(
  "Geographic Area Name",
  "Estimate!!Households!!Total",
  "Estimate!!Households!!Median income (dollars)",
  "Estimate!!Households!!Mean income (dollars)"
)
chr.new_names = c(
  "zcta",
  "n_households",
  "hh_median_income",
  "hh_mean_income"
)
dt.hhs = dt.s1901[,dt.s1901_colmap[descr %in% chr.colm_keep,colm],with=F]
setnames(dt.hhs,colnames(dt.hhs),chr.new_names)
str(dt.hhs)
dt.hhs[,n_households := as.numeric(n_households)]
dt.hhs[,hh_median_income := as.numeric(hh_median_income)]
dt.hhs[,hh_mean_income := as.numeric(hh_mean_income)]
View(dt.hhs)
