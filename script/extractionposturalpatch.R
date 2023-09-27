library(readxl)
library(readr)
library(data.table)
library(purrr)
library(dplyr)

posturalpatch <-
  list.files(
    path = "Data/PATCH",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )

readcsvfun <- function(fun) {read.csv(fun ,
                                      header = TRUE ,
                                      sep = "," ,
                                      dec = "," ,
                                      quote = "\"\"" ,
                                      skip = 8
)}

dfposturalpatch <- lapply(posturalpatch, readcsvfun)
names(dfposturalpatch) <- tools::file_path_sans_ext(basename(posturalpatch))

#u <- which(sapply(dfposturalpatch, function(x) "pgof" %in% x))
#test <- lapply(dfposturalpatch , filtereddf)
#test <- dfposturalpatch[!grep("pgof", names(dfposturalpatch))]
#filtereddf <- function(filt) {!grep("pgof", names(filt))}

removezero <- function(zero){zero[, colSums(zero != 0) > 0]}
dfposturalpatchfilt <- lapply(dfposturalpatch , removezero)

removeZ <- function(z){select(z, -contains("Z"))}
dfposturalpatchfilt <- lapply(dfposturalpatchfilt , removeZ)

dfposturalpatchfilt <-lapply(dfposturalpatchfilt, removerow)
dfposturalpatchfilt25hz <- lapply(dfposturalpatchfilt , filter25hz)

ren <- c("temps" , "copx" , "copy")
dfposturalpatchfilt25hz <- lapply(dfposturalpatchfilt25hz , setNames , ren)
