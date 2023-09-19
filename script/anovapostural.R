library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

a <- function(x) {colSums(abs(x))}

dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO` <-
  dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO`[, -c(4, 5)]

dfposturalpatchfilt25hz <-
  dfposturalpatchfilt25hz[!grepl("Alban" , names(dfposturalpatchfilt25hz))]
dfposturalpatchfilt25hz <-
  dfposturalpatchfilt25hz[!grepl("Sacha" , names(dfposturalpatchfilt25hz))]

dfposturalplacebofilt25hz <- dfposturalplacebofilt25hz[!grepl("Alban" , names(dfposturalplacebofilt25hz))]
dfposturalplacebofilt25hz <-
  dfposturalplacebofilt25hz[!grepl("Sacha" , names(dfposturalplacebofilt25hz))]

dfsumplacebo <-
  as.data.frame(do.call(rbind, lapply(dfposturalplacebofilt25hz, a)))
dfsumpatch <-
  as.data.frame(do.call(rbind, lapply(dfposturalpatchfilt25hz, a)))


#dfsumpatch <- dfsumpatch[, -c(4, 5)]

bloc <-
  c(
    "PRE" ,
    "PRE" ,
    "PRE" ,
    "PRE" ,
    "MID" ,
    "MID" ,
    "MID" ,
    "MID" ,
    "POST" ,
    "POST" ,
    "POST" ,
    "POST" ,
    "POST48" ,
    "POST48" ,
    "POST48" ,
    "POST48"
  )
bloc1 <- c(rep(x = bloc , times = 17))

dfsumpatch <- cbind(dfsumpatch , bloc1)
dfsumplacebo <- cbind(dfsumplacebo , bloc1)

bloc2 <-
  c(
    #rep(x = "AL" , times = 16)  ,
    rep(x = "AB" , times = 16) ,
    rep(x = "BM" , times = 16) ,
    rep(x = "BR" , times = 16) ,
    rep(x = "BA" , times = 16) ,
    rep(x = "GA" , times = 16) ,
    rep(x = "GM" , times = 16) ,
    rep(x = "GS" , times = 16) ,
    rep(x = "JS" , times = 16) ,
    rep(x = "MA" , times = 16) ,
    rep(x = "MF" , times = 16) ,
    rep(x = "MD" , times = 16) ,
    rep(x = "PN" , times = 16) ,
    rep(x = "RF" , times = 16) ,
    rep(x = "RO" , times = 16) ,
    rep(x = "SL" , times = 16) ,
    rep(x = "TM" , times = 16) ,
    rep(x = "VP" , times = 16)
  )

dfsumpatch <- cbind(dfsumpatch , bloc2)
dfsumplacebo <- cbind(dfsumplacebo , bloc2)

patch <- c("patch")
placebo <- c("placebo")

dfsumpatch <- cbind(dfsumpatch , patch)
dfsumplacebo <- cbind(dfsumplacebo , placebo)

strpatch <- str_extract(string = row.names(dfsumpatch), pattern = "(.{4}$)")
strplacebo <- str_extract(string = row.names(dfsumplacebo), pattern = "(.{4}$)")

dfsumpatch <- cbind(dfsumpatch , strpatch)
dfsumplacebo <- cbind(dfsumplacebo , strplacebo)

colnames(dfsumpatch) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")
colnames(dfsumplacebo) <-
  c("time" ,
    "sumcopx" ,
    "sumcopy" ,
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")

dfpostfin <- rbind(dfsumpatch , dfsumplacebo)

deuxPOO <- as.data.frame(dfpostfin[dfpostfin$test == "2POO",])
deuxPOF <- as.data.frame(dfpostfin[dfpostfin$test == "2POF",])
PDOO <- as.data.frame(dfpostfin[dfpostfin$test == "PDOO",])
PGOO <- as.data.frame(dfpostfin[dfpostfin$test == "PGOO",])

bobo <-
  deuxPOF %>% aggregate(
    sumcopx ~ condition + instant_mesure ,
    FUN = function (x) {
      mean(x)
    }
  )

#identify outliers

mylist <- list(deuxPOO , deuxPOF , PDOO , PGOO)

outl <-
  function(o) {
    o %>% group_by(condition , instant_mesure) %>% identify_outliers(sumcopx)
  }
outliersumcopx <- lapply(mylist , outl)
names(outliersumcopx) <- c("deuxPOO" , "deuxPOF" , "PDOO" , "PGOO")

outly <-
  function(oy) {
    oy %>% group_by(condition , instant_mesure) %>% identify_outliers(sumcopy)
  }

outliersumcopy <- lapply(mylist , outly)
names(outliersumcopy) <- c("deuxPOO" , "deuxPOF" , "PDOO" , "PGOO")

#removeoutliers

df2 <-
  subset(deuxPOF,
         !sumcopx %in% identify_outliers(deuxPOF, "sumcopx")$sumcopx)

#shapirotest

shap <-
  function(s) {
    s %>% group_by(condition , instant_mesure) %>% shapiro_test(sumcopx)
  }
shapiro <- lapply(mylist, shap)
names(shapiro) <- c("deuxPOO" , "deuxPOF" , "PDOO" , "PGOO")

shapy <-
  function(sy) {
    sy %>% group_by(condition , instant_mesure) %>% shapiro_test(sumcopy)
  }
shapiroy <- lapply(mylist, shapy)
names(shapiroy) <- c("deuxPOO" , "deuxPOF" , "PDOO" , "PGOO")

#filtre sans le post 48

deuxPOFtest <- deuxPOF[!deuxPOF$instant_mesure == "POST48" , ]
deuxPOOtest <- deuxPOO[!deuxPOO$instant_mesure == "POST48" , ]
PDOOtest <- PDOO[!PDOO$instant_mesure == "POST48" , ]
PGOOtest <- PGOO[!PGOO$instant_mesure == "POST48" , ]

#filtre sans MD

deuxPOFtest <- deuxPOFtest %>% filter(!sujet == "MD")
deuxPOOtest <- deuxPOOtest %>% filter(!sujet == "MD")
PDOOtest <- PDOOtest %>%  filter(!sujet == "MD")
PGOOtest <- PGOOtest %>%  filter(!sujet == "MD")


#filtre sans le sujet BR

tdeuxPOF <- filter(deuxPOFtest , !sujet == "BR")

# plot facewrap sans les pvalue, les pvalues sont dans les dataframe :
#dfres...

plotsumcopxwrap <- ggboxplot( data = dfpostfin ,
  x = "instant_mesure",
  y = "sumcopy",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c("PRE" ,
            "MID" ,
            "POST" ,
            "POST48"), width = 0.5 ,
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "sumcopy",
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
  facet_wrap(vars(test) , scales = "free_y")

plotsumcopxwrap

#plot indiv wrap

dfpostfin$instant_mesure <-
  factor(dfpostfin$instant_mesure ,
         levels = c("PRE" , "MID" , "POST" , "POST48"))

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

#plot 2POF sumcopx

plot2POFx <- ggboxplot(
  deuxPOF ,
  x = "instant_mesure",
  y = "sumcopx",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "PRE" ,
    "MID" ,
    "POST" ,
    "POST48"
  ),
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "sumcopx",
  xlab = "instant_mesure" ,
  title = "pathswaycopx2POF_patch_placebo"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  )

plot2POFx

#plot variation indiv

deuxPOFtest$instant_mesure <-
  factor(deuxPOFtest$instant_mesure , levels = c("PRE", "MID", "POST"))
deuxPOF$instant_mesure <-
  factor(deuxPOF$instant_mesure , levels = c("PRE", "MID", "POST" , "POST48"))

plotest1 <- ggplot(deuxPOF, aes(x = instant_mesure , y = sumcopx)) +
  theme_bw() +
  theme(
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
    legend.text = element_text(size = 6)
  ) +
  geom_line(
    aes(
      x = instant_mesure ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.6 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2) +
  geom_point(
    aes(x = instant_mesure , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot(
    aes(x = instant_mesure , y = sumcopx) ,
    width = .5,
    fill = "white" , alpha = 0.3
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 1 ,
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
  labs(title = "pathswaycopx2POF_individual_variation_PRE/MID/POST/POST48") +
  facet_wrap(vars(condition))

plotest1


#test friedman sans le post48

mylistout <-
  list(
    deuxPOF = deuxPOF ,
    deuxPOO = deuxPOO ,
    PDOO = PDOO ,
    PGOO = PGOO
  )

mylist <-
  list(
    deuxPOFtest = deuxPOFtest ,
    deuxPOOtest = deuxPOOtest ,
    PDOOtest = PDOOtest,
    PGOOtest = PGOOtest ,
    tdeuxPOF = tdeuxPOF
  )

res.friedcopxpb <-
  function(f) {
    f %>% filter(condition == "placebo") %>%  friedman_test(sumcopx ~ instant_mesure |
                                                        sujet)
  }

res.friedcopxpatch <-
  function(f) {
    f %>% filter(condition == "patch") %>%  friedman_test(sumcopx ~ instant_mesure |
                                                              sujet)
  }

res.friedcopypb <-
  function (f) {
    f %>% filter(condition == "placebo") %>% friedman_test(sumcopy ~ instant_mesure |
                                                             sujet)
  }

res.friedcopypatch <-
  function (f) {
    f %>% filter(condition == "patch") %>% friedman_test(sumcopy ~ instant_mesure |
                                                             sujet)
  }

dfres.friedsumcopxpb <-
  lapply(mylist , res.friedcopxpb) %>% bind_rows(.id = "my_listdf")
dfres.friedsumcopxpatch <-
  lapply(mylist , res.friedcopxpatch) %>% bind_rows(.id = "my_listdf")

dfres.friedsumcopypb <-
  lapply(mylist , res.friedcopypb) %>% bind_rows(.id = "my_listdf")
dfres.friedsumcopypatch <-
  lapply(mylist , res.friedcopypatch) %>% bind_rows(.id = "my_listdf")

pwc <-
  deuxPOFtest %>% group_by(condition) %>%  wilcox_test(sumcopx ~ instant_mesure ,
                              paired = TRUE,
                              p.adjust.method = "bonferroni")
pwc

pwc2 <-
  deuxPOF %>% group_by(instant_mesure) %>% wilcox_test(sumcopx ~ condition ,
                                                           paired = TRUE,
                                                           p.adjust.method = "bonferroni")
pwc2

#plot 2POF avec pvalue

pwc <- pwc %>% add_xy_position(x = "instant_mesure")
pwc$xmin <- c(1 , 1 , 2 , 1.2 , 1 , 2)
pwc$xmax <- c(2 , 3 , 3 , 2.2 , 3, 3)

pwc2 <- pwc2 %>% add_xy_position(x = "condition")
pwc2$xmin <- c(2 , 2 , 4 , 2)
pwc2$xmax <- c(2 , 2 , 4 , 2)

plot2POFx +
  stat_pvalue_manual(
    pwc,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}"  , y.position = 14300 ,color = "#FC4E07"
  ) +
  stat_pvalue_manual(
    pwc2,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p}"  , y.position = 100
  )

#wilcoxon différence entre les conditions sur chaque instant de mesure


pwc2copx <- function (w) {
  w %>% group_by(instant_mesure) %>% wilcox_test(sumcopx ~ condition ,
                                                       paired = TRUE,
                                                       p.adjust.method = "bonferroni")}
pwc2copy <- function (w) {
  w %>% group_by(instant_mesure) %>% wilcox_test(sumcopy ~ condition ,
                                                 paired = TRUE,
                                                 p.adjust.method = "bonferroni")}

dfres.wilcoxsumcopx <- lapply(mylistout ,pwc2copx) %>% bind_rows(.id = "mylistwil")

dfres.wilcoxsumcopy <- lapply(mylistout ,pwc2copy) %>% bind_rows(.id = "mylistwil")

#wilcoxon groupé par conditon

pwc2copx <- function (w) {
  w %>% group_by(condition) %>% wilcox_test(sumcopx ~ instant_mesure ,
                                                 paired = TRUE,
                                                 p.adjust.method = "bonferroni")}
pwc2copy <- function (w) {
  w %>% group_by(condition) %>% wilcox_test(sumcopy ~ instant_mesure ,
                                                 paired = TRUE,
                                                 p.adjust.method = "bonferroni")}

dfres.wilcoxsumcopxC <- lapply(mylistout ,pwc2copx) %>% bind_rows(.id = "mylistwil")

dfres.wilcoxsumcopyC <- lapply(mylistout ,pwc2copy) %>% bind_rows(.id = "mylistwil")
