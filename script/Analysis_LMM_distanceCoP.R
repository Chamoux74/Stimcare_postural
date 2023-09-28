# calcule

c <- function(x) {mean(sqrt((x$copy^2) +(x$copx^2)))}

dfdistance_pb <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, c)))
dfdistance_p <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, c)))

dfdistance <- rbind(dfdistance_p, dfdistance_pb)
colnames(dfdistance) <- "distance"

dfdistance <- cbind(dfpostfin, dfdistance)

dfdistance <- filter(dfdistance,instant_mesure != "POST48")

dfdistance <- filter(dfdistance, sujet != "GS")
dfdistance <- filter(dfdistance, sujet != "MD")
dfdistance <- filter(dfdistance, sujet != "AL")

dfdistanceF <- filter(dfdistance, test == "2POF")
dfdistanceO <- filter(dfdistance, test == "2POO")

dfdistanceF <- select(dfdistanceF,distance)
dfdistanceO <- select(dfdistanceO,distance)

dfanalysisF <- cbind(dfanalysisF, dfdistanceF)
dfanalysisO <- cbind(dfanalysisO, dfdistanceO)

# graphical visualization

ggplot(aes(x=instant_mesure, y = distance), data = dfanalysisF) +
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

M7 <-
  lme4::lmer(
    distance ~ condition * instant_mesure + (1 |
                                             sujet),
    data = dfanalysisF,
    REML = T
  )

# after testing no effect in any model
