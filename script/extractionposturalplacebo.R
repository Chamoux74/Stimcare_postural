library(readxl)
library(readr)
library(data.table)
library(purrr)
library(dplyr)
library(gsignal)
library(seewave)

posturalplacebo <-
  list.files(
    path = "Data/PLACEBO",
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

ren <- c("temps" , "copx" , "copy")
dfposturalplacebofilt <- lapply(dfposturalplacebofilt , setNames , ren)

# Sample rate (in Hz)
fs <- 1000

# Cutoff frequency for the low-pass filter (in Hz)
cutoff_frequency <- 10
#butterworth_filter <- butter(4, cutoff_frequency / (fs / 2), type = "low")
# Create a Butterworth low-pass filter
dfposturalBFpb <-
  lapply(dfposturalplacebofilt, function (b) {
    butterworth_filter <-
      butter(4, (cutoff_frequency / (fs /2)), type = "low")
    filtered_data <-
      as.data.frame(lapply(b, function (colm)
        filtfilt(butterworth_filter, colm)))
    return(filtered_data)
  })

#reduce from 1000hz to 100Hz

dfpostural100HzBFpb <- lapply(dfposturalBFpb, function(df) {
  df_decimated <-
    as.data.frame(lapply(df, function(col)
      decimate(col, 10)))
  return(df_decimated)
})

# centered value for limit error due of foot position
bob <- function(dataframe, column_names) {
  for (col_name in column_names) {
    if (!col_name %in% colnames(dataframe)) {
      stop(paste("La colonne", col_name, "n'existe pas dans le dataframe."))
    }

    # Calcul de la moyenne de la colonne spécifiée
    col_mean <- mean(dataframe[[col_name]], na.rm = TRUE)

    # Soustraction de la moyenne à chaque valeur de la colonne
    dataframe[[col_name]] <- dataframe[[col_name]] - col_mean
  }

  return(dataframe)
}

dfpostural100HzBFpb <-
  lapply(dfpostural100HzBFpb, function(bobi) {
    bob(bobi, c("copx", "copy"))
  })
