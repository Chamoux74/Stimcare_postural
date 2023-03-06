library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

#plot trace COP

ren <- c("temps" , "copx" , "copy")
dfposturalpatchfilt25hz <- lapply(dfposturalpatchfilt25hz , setNames , ren)

diffx <- function(d){diff(d$copx)}
diffcopx <- lapply(dfposturalpatchfilt25hz , diffx)
diffcopx <- lapply(diffcopx , as.data.frame)

diffy <- function (dy){diff(dy$copy)}
diffcopy <- lapply(dfposturalpatchfilt25hz , diffy)
diffcopy <- lapply(diffcopy , as.data.frame)

diffxy <- mapply(cbind , diffcopx , diffcopy , SIMPLIFY = FALSE)

nam <- c("diffcopx" , "diffcopy")
diffxy <- lapply(diffxy , setNames , nam)

cov_mat <- cov(dfposturalpatchfilt25hz$`Alban LE Gall-2022.09.28-13.33.43PREA2POO`$copx ,
               dfposturalpatchfilt25hz$`Alban LE Gall-2022.09.28-13.33.43PREA2POO`$copy)
eig <- eigen(cov_mat)
vec <- eig$vectors
val <- as.numeric(eig$values)
result <- pi*prod(2.4478*sqrt(svd(val)))

is.numeric(val)

plottest <-
  ggplot(data = diffxy$`Alban LE Gall-2022.09.28-13.33.43PREA2POO`,
         aes(x = diffcopx, y = diffcopy, group = 1)) +
  geom_path(color = "blue" , )

plottest
