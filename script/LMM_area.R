library(lme4)
library(lmerTest)
library(ggplot2)
library(outliers)
library(kableExtra)
library(pander)
library(tigerstats)
library(performance)
library(MuMIn)
library(phia)
library(emmeans)
library(sjPlot)
library(xtable)
library(robustlmm)

#modelemixed

M4 <-
  rlmer(
    area95 ~ condition * instant_mesure + (1 |
                                                sujet),
    data = dfanalysisF,
    REML = T
  )

summary(M4)
tab_model(M4)

anova(M4)


emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M4, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)
