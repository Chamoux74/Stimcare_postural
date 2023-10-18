# calcule
e <- function(x) {base::sum(sqrt(diff(x$copy)^2 + diff(x$copx)^2))}

dflenght_pb <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpb, e)))
dflenght_p <-
  as.data.frame(do.call(rbind, lapply(dfpostural100HzBFpat, e)))

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
  labs(title = "Mean lenght CE by condition", x = "Timing", y = "Length CoP")

#mixed model

M9 <-
  lmer(
    lenght ~ condition * instant_mesure + (1 |
                                             sujet),
    data = dfanalysisF,
    REML = T
  )

tab_model(M9)

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M9, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)
