# calcule
d <- function(x) {sd(sqrt((x$copy^2) +(x$copx^2)))}

dfdistsd_pb <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpb, d)))
dfdistsd_p <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpat, d)))

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


#mixed model

M8 <-
  lmer(
    distsd ~ condition * instant_mesure + (1 |
                                               sujet),
    data = dfanalysisF,
    REML = T
  )

tab_model(M8)

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M8, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)
