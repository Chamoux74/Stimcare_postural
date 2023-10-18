
c <- function(x) {mean(sqrt((x$copy^2)+(x$copx^2)))}

dfdistance_pb <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpb, c)))
dfdistance_p <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpat, c)))

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

#mixed model

M7 <-
  lmer(
    distance ~ condition * instant_mesure + (1 |
                                             sujet),
    data = dfanalysisF,
    REML = T
  )

 summary(M7)

 tab_model(M7)
# after testing no effect in any model

 emm_options(lmer.df = "satterthwaite")
 emmeans_out <- emmeans(M7, ~instant_mesure*condition, weights = "show.levels")
 emmeans_out
 plot(emmeans_out)
 pair1 <- pairs(emmeans_out, adjust ="holm")
 summary(pair1)
 pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
 summary(pair2)
 pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
 summary(pair3)
