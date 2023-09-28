# calcule
e <- function(x) {base::sum(sqrt(diff(x$copy)^2 + diff(x$copx)^2))}

dflenght_pb <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, e)))
dflenght_p <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, e)))

dflenght <- rbind(dflenght_p, dflenght_pb)
colnames(dflenght) <- "lenght"

dflenght <- cbind(dfpostfin, dflenght)

dflenght <- filter(dflenght,instant_mesure != "POST48")

dflenght <- filter(dflenght, sujet != "GS")
dflenght <- filter(dflenght, sujet != "MD")
dflenght <- filter(dflenght, sujet != "AL")

dflenghtF <- filter(dflenght, test == "2POF")
dflenghtO <- filter(dflenght, test == "2POO")

dflenghtF <- select(dflenghtF,lenght)
dflenghtO <- select(dflenghtO,lenght)

dfanalysisF <- cbind(dfanalysisF, dflenghtF)
dfanalysisO <- cbind(dfanalysisO, dflenghtO)


# graphical visualization

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

dftest <- data_summary(dfanalysisF, varname = "lenght", groupnames = c("condition", "instant_mesure"))

ggplot(dftest,
       aes(
         x = instant_mesure,
         y = lenght,
         group = condition,
         color = condition
       )) +
  geom_errorbar(aes(ymin = lenght - sd, ymax = lenght + sd),
                width = .1,
                position = position_dodge(0.08)) +
  geom_line(aes(linetype = condition), size = 1, position = position_dodge(0.08)) +
  geom_point(aes(shape = condition), position = position_dodge(0.08), size = 2) +
  scale_color_manual(values = c('#999999', '#E69F00')) + theme_classic() +
  labs(title = "Mean lenght by condition", x = "Timing", y = "Length CoP")


