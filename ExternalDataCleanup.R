
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


