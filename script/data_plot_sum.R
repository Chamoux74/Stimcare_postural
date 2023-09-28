library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library(tidyr)
library(dplyr)
library(stringr)

#fonction somme de valeur abs

a <- function(x) {colSums(abs(x))}

dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO` <-
  dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO`[, -c(4, 5)]

dfsumplacebo <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, a)))
dfsumpatch <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, a)))

#extract information of rownames

instantp <- str_match(rownames(dfsumpatch), "PRE|POST48|MID|POST")
instantpb <- str_match(rownames(dfsumplacebo), "PRE|POST48|MID|POST")
testp <- str_match(rownames(dfsumpatch), "2POO|2POF|PGOO|PDOO")
testpb <- str_match(rownames(dfsumplacebo), "2POO|2POF|PGOO|PDOO")

sujetp <- str_match(rownames(dfsumpatch), "^(\\w)\\w*\\s+(\\w)\\w*") %>% as.data.frame()
sujetpb <- str_match(rownames(dfsumplacebo), "^(\\w)\\w*\\s+(\\w)\\w*") %>% as.data.frame()
sujetp <- paste0(sujetp$V2, sujetp$V3)
sujetpb <- paste0(sujetpb$V2, sujetpb$V3)

patch <- c("patch")
placebo <- c("placebo")

dfsumpatch <- cbind(dfsumpatch , instantp, testp, sujetp, patch)
dfsumplacebo <- cbind(dfsumplacebo , instantpb, testpb, sujetpb, placebo)


colnames(dfsumpatch) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "test" ,
    "sujet" ,
    "condition")
colnames(dfsumplacebo) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "test" ,
    "sujet" ,
    "condition")

dfpostfin <- rbind(dfsumpatch , dfsumplacebo)

dfpostfin$condition <- factor(dfpostfin$condition, levels = c("placebo", "patch"))

#normality test

dfpostfin %>% group_by(condition, instant_mesure, test) %>%  shapiro_test(sumcopx)
dfpostfin %>% group_by(condition, instant_mesure, test) %>%  shapiro_test(sumcopy)

#homogeneity of variance

levene_test(sumcopx ~ condition*instant_mesure, data = dfpostfin)
levene_test(sumcopy ~ condition*instant_mesure, data = dfpostfin)

#identify outliers

dfpostfin %>% group_by(condition, instant_mesure, test) %>%  identify_outliers(sumcopx)
dfpostfin %>% group_by(condition, instant_mesure, test) %>%  identify_outliers(sumcopy)

# plot facewrap sans les pvalue, les pvalues sont dans les dataframe :
#dfres...

plotsumcopxwrap <- ggboxplot( data = dfpostfin ,
  x = "instant_mesure",
  y = "sumcopx",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c("PRE" ,
            "MID" ,
            "POST" ,
            "POST48"), width = 0.5 ,
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "sumcopx",
  xlab = "instant_mesure" ,
  title = "sumcopy_patch_placebo-test"
) +
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
  ) +
  stat_friedman_test(aes(wid = sujet)) +
  facet_wrap(vars(test))

plotsumcopxwrap

#plot indiv wrap

plotindivsum <- ggplot(dfpostfin, aes(x = instant_mesure , y = sumcopy)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.text.x = element_text(angle = 45 , vjust = 0.7) ,
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
  ) +
  geom_line(
    aes(
      x = instant_mesure ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.4 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 0.5 ,
    width = 0.2) +
  geom_point(
    aes(x = instant_mesure , group = sujet),
    shape = 21,
    colour = "black",
    size = 1.1,
    position = "identity"
  ) +
  geom_boxplot(
    aes(x = instant_mesure , y = sumcopy) ,
    width = .3,
    fill = "white" , alpha = 0.3
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 0.5 ,
    position = "identity",
    color = "#ff0000"
  ) +
  scale_color_manual(
    values = c(
      "purple" ,
      "#0416f5" ,
      "#b00000" ,
      "#19a3e8" ,
      "#fd4c4c" ,
      "#E7B800" ,
      "#5ef11a" ,
      "#c58ede" ,
      "#3e020b" ,
      "#febd02" ,
      "#16161e" ,
      "#24844b" ,
      "#f604fd" ,
      "#439bab" ,
      "#6e711d" ,
      "#156901" ,
      "#62fdff"
    )) +
  labs(color = "sujet") +
  labs(title = "sumcopy_individual_variation-test") +
  facet_grid(condition ~ test , scales = "free_y")

plotindivsum
