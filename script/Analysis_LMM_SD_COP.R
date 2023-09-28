library(stats)
library(base)

# calcule

b <- function(x) {sapply(x, sd)}

dfsdcop_pb <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, b)))
dfsdcop_p <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, b)))

colnames(dfsdcop_pb) <- c("time", "sdcopx", "sdcopy")
colnames(dfsdcop_p)<- c("time", "sdcopx", "sdcopy")

dfsd <- rbind(dfsdcop_p, dfsdcop_pb)
dfsd <- select(dfsd, sdcopx, sdcopy)

dfsd <- cbind(dfpostfin, dfsd)

dfsd <- filter(dfsd,instant_mesure != "POST48")

dfsd <- filter(dfsd, sujet != "GS")
dfsd <- filter(dfsd, sujet != "MD")
dfsd <- filter(dfsd, sujet != "AL")

dfsdF <- filter(dfsd, test == "2POF")
dfsdO <- filter(dfsd, test == "2POO")

dfsdF <- select(dfsdF,sdcopx, sdcopy)
dfsdO <- select(dfsdO,sdcopx, sdcopy)

dfanalysisF <- cbind(dfanalysisF, dfsdF)
dfanalysisO <- cbind(dfanalysisO, dfsdO)

# graphical visualization

ggplot(aes(x=instant_mesure, y = sdcopx), data = dfanalysisO) +
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

M5 <-
  lme4::lmer(
    sdcopx ~ condition * instant_mesure + (1 |
                                             sujet),
    data = dfanalysisO,
    REML = T
  )

M6 <-
  lme4::lmer(
    sdcopy ~ condition * instant_mesure + (1 |
                                             sujet),
    data = dfanalysisO,
    REML = T
  )

tab_model(M6)

##lmer model

residus <- residuals(M6, type="pearson",scaled=TRUE)
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
aleatoires <- lmer(sdcopy~1+(1|sujet), data= DFclean)

pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

r_int<- ranef(M6)$sujet$"(Intercept)"
qqnorm(r_int)
shapiro.test(r_int)

#independance between random var and residual

splom(pr01)

#Testting interest of random effect lmer

ranova(M6)

#plot interest of random effect
ranef(M3)
dotplot(ranef(M3, condVar=T))

#ICC for random factor Sujet

icc(M3)

#determination of variance in model trough F test value

anova(M1, type= 3)

#second method to calculate interraction contrast

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M6, ~instant_mesure*condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("instant_mesure"), adjust = "holm", )
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("condition"), adjust = "holm")
summary(pair3)
