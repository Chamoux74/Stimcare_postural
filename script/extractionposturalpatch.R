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

ren <- c("temps" , "copx" , "copy")
dfposturalpatchfilt <- lapply(dfposturalpatchfilt , setNames , ren)

# Sample rate (in Hz)
fs <- 1000

# Cutoff frequency for the low-pass filter (in Hz)
cutoff_frequency <- 10
#butterworth_filter <- butter(4, cutoff_frequency / (fs / 2), type = "low")
# Create a Butterworth low-pass filter
dfposturalBFpat <-
  lapply(dfposturalpatchfilt, function (b) {
    butterworth_filter <-
      butter(4, (cutoff_frequency / (fs /2)), type = "low")
    filtered_data <-
      as.data.frame(lapply(b, function (colm)
        filtfilt(butterworth_filter, colm)))
    return(filtered_data)
  })


#reduce from 1000hz to 100Hz

dfpostural100Hzpat <- lapply(dfposturalBFpat, function(df) {
  df_decimated <-
    as.data.frame(lapply(df, function(col)
      decimate(col, 10)))
  return(df_decimated)
})

#centered value around mean equal to 0

dfpostural100HzBFpat <-
  lapply(dfpostural100HzBFpat, function(bobi) {
    bob(bobi, c("copx", "copy"))
  })
