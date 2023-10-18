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
library(nlme)
library(xtable)
library(robustlmm)


#analyse uniquement sur test 2P

dfanalysis <- filter(dfpostfin, test == "2POO")
test <- filter(dfpostfin, test == "2POF")
dfanalysis <- rbind(test, dfanalysis)
dfanalysis <- filter(dfanalysis, instant_mesure != "POST48")
dfanalysis$instant_mesure <- factor(dfanalysis$instant_mesure, levels = c("PRE", "MID", "POST"))

#description data

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# on crée le graphique en indiquant le jeu de données et les variables d'intérêt
p<- ggplot(data=dfanalysis, aes(x= instant_mesure, y= sumcopx, fill=condition) ) +
  geom_violin()+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="right")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)+
  stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9)) +
  facet_wrap(~test)
p

#Create first model

dfanalysisO <- filter(dfanalysis, test == "2POO")
dfanalysisF <- filter(dfanalysis, test == "2POF")

dfanalysisF <- filter(dfanalysisF, sujet != "GS")
dfanalysisF <- filter(dfanalysisF, sujet != "MD")
dfanalysisF <- filter(dfanalysisF, sujet != "AL")

dfanalysisO <- filter(dfanalysisO, sujet != "GS")
dfanalysisO <- filter(dfanalysisO, sujet != "MD")
dfanalysisO <- filter(dfanalysisO, sujet != "AL")


M1 <-
  lmer(
    sumcopx ~ condition * instant_mesure + (1 | sujet),
    data = dfanalysisF,
    REML = T
  )

tab_model(M1)


#pas d'effet quelques soit le model sauf OF sumcopy et sumcopx PRE vs MID avec tendance
#uniquement pour placebo

anova(modele_mixte1, modele_mixte1_1)

#outlier and application condition

##lmer model

residus <- residuals(M2, type="pearson",scaled=TRUE)
dfanalysisO$residus<-residus
outliers::grubbs.test(dfanalysisO$residus, type = 10, opposite = FALSE, two.sided = FALSE)

##clean
DFclean <- dfanalysisO

data.frame()->valeur.influentes
while(outliers::grubbs.test(DFclean$residus, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  {
  max<-which.max(abs(DFclean$residus)) #cherche la valeur maximale qu'on stocke dans l'objet max                            # récupère les observations considérées comme influentes et les stocke
  valeur.influentes<-rbind(valeur.influentes,DFclean[max, ])
  DFclean<-DFclean[ -max, ] # supprime la valeur maximale de rat.clean
}

## normality test for residuals

n1<-shapiro.test(dfanalysisO$residus)
n2<-shapiro.test(DFclean$residus)
r<-data.frame(W=c(n1$statistic, n2$statistic),
              p=c(n1$p.value, n2$p.value))
dimnames(r)[[1]]<-c("jeu de données complet", "jeu de données nettoyées lmer")
kable(pandoc.table(r, style='simple',split.tables=150))

#plot normality

ggplot(DFclean, aes(x=residus)) +
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-ax,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#ggqqplot

ggplot(DFclean, aes(sample=residus))+stat_qq()

#normality random parameters model lmer
aleatoires <- lmer(sumcopx~1+(1|condition:sujet), data= DFclean)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(M2)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(modele_mixte1_2)

#testing prediction quality of model

AICc(modele_mixte1_2)

#plot interest of random effect
ranef(modele_mixte1)
dotplot(ranef(modele_mixte1, condVar=T))

#ICC for random factor Sujet


icc(modele_mixte1_2)

#test effet fixe

anova(modele_mixte1_2)

#determination of variance in model trough F test value

anova(modele_mixte1, type= 3)

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M1, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)

#variance homogeneity

modele_mixte1_2_vara <- nlme::lme(
  sumcopx ~ condition * instant_mesure,
  data = dfanalysisPF,
  random = ~ 1 | sujet,
  method = "REML",
  weights = varIdent(form = ~1|instant_mesure))

VarCorr(modele_mixte1_2_vara)
summary(modele_mixte1_2_vara)$modelStruct$varStruct

#if not homogenous anova between two model

anova(modele_mixte1, modele_mixte1_2_vara)
# plot to investigated crossed fixed and random effect in repeated measures
interaction.plot(DFIMVCmax_sanspost48$condition, DFIMVCmax_sanspost48$sujet, DFIMVCmax_sanspost48$IMVC, las=1,
                 trace.label="identifiant du sujet", xlab="Traitement", ylab="IMVCmax")

interaction.plot(DFIMVCmax_sanspost48$instant_mesure, DFIMVCmax_sanspost48$sujet, DFIMVCmax_sanspost48$IMVC, las=1,
                 trace.label="identifiant du sujet", xlab="instant", ylab="IMVC")

# plot individual variation

p<-ggplot(DFIMVCmax_sanspost48, aes(x=instant_mesure, y=IMVC, colour=sujet, group=sujet))+
  geom_line() +
  facet_wrap(~condition)
p
