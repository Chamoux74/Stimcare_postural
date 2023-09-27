#analyse range

rx <- function (r) {
  max(r$copx) - min(r$copx)
}
ry <- function (r) {
  max(r$copy) - min(r$copy)
}

rangecopxPB <-
  as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , rx)))
rangecopxP <-
  as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , rx)))
rangecopyPB <-
  as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , ry)))
rangecopyP <-
  as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , ry)))

colnames(rangecopxPB) <- c("rangecopxpb")
colnames(rangecopxP) <- c("rangecopxpatch")
colnames(rangecopyPB) <- c("rangecopypb")
colnames(rangecopyP) <- c("rangecopypatch")

dfsumplacebo <- cbind(dfsumplacebo , rangecopxPB , rangecopyPB)

colnames(dfsumplacebo) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "test" ,
    "sujet" ,
    "condition",
    "rangecopx" ,
    "rangecopy")

dfsumpatch <- cbind(dfsumpatch , rangecopyP , rangecopxP)

colnames(dfsumpatch) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "test" ,
    "sujet" ,
    "condition",
    "rangecopx" ,
    "rangecopy")

dfpostfin <- rbind(dfsumpatch , dfsumplacebo)

dfanalysis <- filter(dfpostfin, instant_mesure != "POST48")
dfanalysis$instant_mesure <- factor(dfanalysis$instant_mesure, levels = c("PRE", "MID", "POST"))

dfanalysis <- filter(dfanalysis, sujet != "GS")
dfanalysis <- filter(dfanalysis, sujet != "MD")
dfanalysis <- filter(dfanalysis, sujet != "AL")

dfanalysisO <- filter(dfanalysis, test == "2POO")
dfanalysisF <- filter(dfanalysis, test == "2POF")

dfanalysisF$condition <- factor(dfanalysisF$condition, levels = c("placebo", "patch"))
dfanalysisO$condition <- factor(dfanalysisO$condition, levels = c("placebo", "patch"))

#plot wrap grid en fonction des test

dfanalysisF %>% group_by(condition,instant_mesure) %>% shapiro_test(rangecopy)
dfanalysisO %>% group_by(condition,instant_mesure) %>% shapiro_test(rangecopx)

plotrangecopxtest <- ggplot(aes(x = instant_mesure, y = rangecopy), data= dfanalysisO) +
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

plotrangecopxtest
