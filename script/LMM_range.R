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

#description data

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# on crée le graphique en indiquant le jeu de données et les variables d'intérêt
p<- ggplot(data=dfanalysisO, aes(x= instant_mesure, y= rangecopx, fill=condition) ) +
  geom_violin()+
  scale_fill_brewer(palette="PRGn")+
  theme(legend.position="right")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)+
  stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
p

#modelemixed who working

M7 <-
  rlmer(
    rangecopx ~ condition * instant_mesure + (1 |
                                              sujet),
    data = dfanalysisO,
    REML = T
  )

M8 <-
  lmer(
    rangecopy ~ condition * instant_mesure + (1 |
                                                sujet),
    data = dfanalysisO,
    REML = T
  )

M9 <- rlmer(rangecopx ~ condition * instant_mesure + (1 |
                                                                  sujet),
                        data = dfanalysisF,
                        REML = T)

M10 <- lmer(rangecopy ~ condition * instant_mesure + (1 |
                                                        sujet),
            data = dfanalysisF,
            REML = T)


summary(M9)
tab_model(M10)

#outlier and application condition

##lmer model

residus <- residuals(M10, type="pearson",scaled=TRUE)
dfanalysisF$residus<-residus
outliers::grubbs.test(dfanalysisF$residus, type = 10, opposite = FALSE, two.sided = FALSE)

##clean
DFclean <- dfanalysisF

data.frame()->valeur.influentes
while(outliers::grubbs.test(DFclean$residus, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  {
  max<-which.max(abs(DFclean$residus)) #cherche la valeur maximale qu'on stocke dans l'objet max                            # récupère les observations considérées comme influentes et les stocke
  valeur.influentes<-rbind(valeur.influentes,DFclean[max, ])
  DFclean<-DFclean[ -max, ] # supprime la valeur maximale de rat.clean
}

## normality test for residuals

n1<-shapiro.test(dfanalysisF$residus)
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
aleatoires <- lmer(rangecopy~1+(1|sujet), data= DFclean)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(M10)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(M3)

#plot interest of random effect
ranef(M3)
dotplot(ranef(M3, condVar=T))

#ICC for random factor Sujet

icc(M3)

#determination of variance in model trough F test value

anova(M1, type= 3)

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M10, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm")
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)
