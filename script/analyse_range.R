#analyse range

rx <- function (r) {
  max(r$copx) - min(r$copx)
}
ry <- function (r) {
  max(r$copy) - min(r$copy)
}

rangecopxPB <-
  as.data.frame(do.call(rbind , lapply(dfpostural100HzBFpb , rx)))
rangecopxP <-
  as.data.frame(do.call(rbind , lapply(dfpostural100HzBFpat , rx)))
rangecopyPB <-
  as.data.frame(do.call(rbind , lapply(dfpostural100HzBFpb , ry)))
rangecopyP <-
  as.data.frame(do.call(rbind , lapply(dfpostural100HzBFpat, ry)))

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
