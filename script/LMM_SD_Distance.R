# calcule
d <- function(x) {sd(sqrt((x$copy^2) +(x$copx^2)))}

dfdistsd_pb <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, d)))
dfdistsd_p <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, d)))

dfdistsd <- rbind(dfdistsd_p, dfdistsd_pb)
colnames(dfdistsd) <- "distsd"

dfdistsd <- cbind(dfpostfin, dfdistsd)

dfdistsd <- filter(dfdistsd,instant_mesure != "POST48")

dfdistsd <- filter(dfdistsd, sujet != "GS")
dfdistsd <- filter(dfdistsd, sujet != "MD")
dfdistsd <- filter(dfdistsd, sujet != "AL")

dfdistsdF <- filter(dfdistsd, test == "2POF")
dfdistsdO <- filter(dfdistsd, test == "2POO")

dfdistsdF <- select(dfdistsdF,distsd)
dfdistsdO <- select(dfdistsdO,distsd)

dfanalysisF <- cbind(dfanalysisF, dfdistsdF)
dfanalysisO <- cbind(dfanalysisO, dfdistsdO)

# graphical visualization

ggplot(aes(x=instant_mesure, y = distsd), data = dfanalysisO) +
  geom_boxplot(aes(fill= condition)) +
  stat_summary(
    geom = "point",
    fun.y = mean ,
    aes(group = condition) ,
    shape = 20 ,
    size = 3 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    strip.background = element_rect(color = "black" , fill = "#373737")
    ,
    strip.text = element_text(
      color = "white" ,
      face = "bold" ,
      size = 8
    ) ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  )

#mixed model

M8 <-
  lme4::lmer(
    distsd ~ condition * instant_mesure + (1 |
                                               sujet),
    data = DFclean,
    REML = T
  )

tab_model(M8)


##lmer model

#no differences
