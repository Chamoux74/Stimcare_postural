library(readxl)
library(readr)
library(data.table)
library(purrr)
library(dplyr)


posturalplacebo <-
  list.files(
    path = "C:/Users/maxch/Git/POSTURAL/Data/PLACEBO",
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

dfposturalplacebo <- lapply(posturalplacebo, readcsvfun)
names(dfposturalplacebo) <- tools::file_path_sans_ext(basename(posturalplacebo))

removezero <- function(zero){zero[, colSums(zero != 0) > 0]}
dfposturalplacebofilt <- lapply(dfposturalplacebo , removezero)

removeZ <- function(z){select(z, -contains("Z"))}
dfposturalplacebofilt <- lapply(dfposturalplacebofilt , removeZ)

removerow <- function(r) {r[c(9001 : 29001), ]}
dfposturalplacebofilt <- lapply(dfposturalplacebofilt , removerow)

filter25hz <- function(hz) {hz[seq(1, nrow(hz) , by = 26),]}
dfposturalplacebofilt25hz <- lapply(dfposturalplacebofilt , filter25hz)

dfposturalplacebofilt25hz <- lapply(dfposturalplacebofilt25hz , setNames , ren)
