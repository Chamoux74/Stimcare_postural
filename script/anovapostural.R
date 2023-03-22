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
bloc1 <- c(rep(x = bloc , times = 16))

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
    rep(x = "MF" , times = 16) ,
    rep(x = "MD" , times = 16) ,
    rep(x = "PN" , times = 16) ,
    rep(x = "RF" , times = 16) ,
    rep(x = "RO" , times = 16) ,
    #rep(x = "SP" , times = 16) ,
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

colnames(dfsumpatch) <- c("time" , "sumcopx" , "sumcopy" , "instant_mesure" , "sujet" , "condition" , "test")
colnames(dfsumplacebo) <- c("time" , "sumcopx" , "sumcopy" , "instant_mesure" , "sujet" , "condition" , "test")

dfpostfin <- rbind(dfsumpatch , dfsumplacebo)

deuxPOO <- as.data.frame(dfpostfin[dfpostfin$test == "2POO",])
deuxPOF <- as.data.frame(dfpostfin[dfpostfin$test == "2POF",])
PDOO <- as.data.frame(dfpostfin[dfpostfin$test == "PDOO",])
PGOO <- as.data.frame(dfpostfin[dfpostfin$test == "PGOO",])

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

#plot sumcop sans le post 48

deuxPOFtest <- deuxPOF[!deuxPOF$instant_mesure == "POST48" , ]
deuxPOOtest <- deuxPOO[!deuxPOO$instant_mesure == "POST48" , ]
PDOOtest <- PDOO[!PDOO$instant_mesure == "POST48" , ]
PGOOtest <- PGOO[!PGOO$instant_mesure == "POST48" , ]

plot2POOx <- ggboxplot(
  deuxPOFtest ,
  x = "instant_mesure",
  y = "sumcopx",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "PRE" ,
    "MID" ,
    "POST" #,
    #"POST48"
  ),
  add = "jitter" ,
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
  theme_bw()

plot2POOx

#séparation de chaque test en placebo et patch pour analyse de friedman + plot indiv

test <- deuxPOFtest[deuxPOFtest$condition == "placebo" , ]
test1 <- deuxPOFtest[deuxPOFtest$condition == "patch", ]

test2 <- deuxPOOtest[deuxPOOtest$condition == "patch" , ]
test3 <- deuxPOOtest[deuxPOOtest$condition == "placebo" , ]

test4 <- PDOOtest[PDOOtest$condition == "patch" , ]
test5 <- PDOOtest[PDOOtest$condition == "placebo" , ]

test6 <- PGOOtest[PGOOtest$condition == "patch" , ]
test7 <- PGOOtest[PGOOtest$condition == "placebo" ,]

#plot variation indiv

test$instant_mesure <- factor(test$instant_mesure , levels = c("PRE", "MID", "POST"))
test1$instant_mesure <- factor(test1$instant_mesure , levels = c("PRE", "MID", "POST"))

plotest1 <- ggplot(test, aes(x = instant_mesure , y = sumcopx)) +
  theme_bw() +
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
                "#156901"
    )) +
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2
  ) +
  labs(title = "pathswaycopx2POF_placebo")

plotest

#combiner graphique placebo et patch

figure <- ggarrange(plotest , plotest1 , labels = c("A", "B"),
                    ncol = 2 , nrow = 1 , common.legend = TRUE , legend = "right")

figure

#test friedman sans le post48

test <- as.data.frame(test)

my_listdf <-
  list(
    test = test ,
    test1 = test1 ,
    test2 = test2 ,
    test3 = test3 ,
    test4 = test4 ,
    test5 = test5 ,
    test6 = test6 ,
    test7 = test7
  )

res.friedcopx <- function(f) {friedman_test(sumcopx ~ instant_mesure | sujet , data = f)}
res.friedcopy <- function (f) {friedman_test(sumcopy ~ instant_mesure | sujet , data = f)}

dfres.friedsumcopx <- lapply(my_listdf ,res.friedcopx) %>% bind_rows(.id = "my_listdf")
dfres.friedsumcopy <- lapply(my_listdf ,res.friedcopy) %>% bind_rows(.id = "my_listdf")


pwc <- wilcox_test(sumcopx ~ instant_mesure , paired = TRUE, p.adjust.method = "bonferroni" , data = test)
pwc

pwc2 <- deuxPOFtest %>% group_by(instant_mesure) %>% wilcox_test(sumcopx ~ condition ,
                                                        paired = TRUE,
                                                        p.adjust.method = "bonferroni")
pwc2

#plot 2POF avec pvalue

pwc <- pwc %>% add_xy_position(x = "instant_mesure")
pwc$xmin <- c(2 , 2 , 3)
pwc$xmax <- c(3 , 1 , 1)

pwc2 <- pwc2 %>% add_xy_position(x = "condition")
pwc2$xmin <- c(2 , 2 , 2)
pwc2$xmax <- c(2 , 2 , 2)

plot2POOx +
  stat_pvalue_manual(
    pwc,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}"  , y.position = 16000 ,color = "#FC4E07"
  ) +
  stat_pvalue_manual(
    pwc2,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p}"  , y.position = 100
  )

#wilcoxon différence entre les conditions sur chaque instant de mesure

mylistwil <-
  list(
    deuxPOFtest = deuxPOFtest ,
    deuxPOOtest = deuxPOOtest ,
    PDOOtest = PDOOtest,
    PGOOtest = PGOOtest
  )

pwc2copx <- function (w) {
  w %>% group_by(instant_mesure) %>% wilcox_test(sumcopx ~ condition ,
                                                       paired = TRUE,
                                                       p.adjust.method = "bonferroni")}
pwc2copy <- function (w) {
  w %>% group_by(instant_mesure) %>% wilcox_test(sumcopy ~ condition ,
                                                 paired = TRUE,
                                                 p.adjust.method = "bonferroni")}

dfres.wilcoxsumcopx <- lapply(mylistwil ,pwc2copx) %>% bind_rows(.id = "mylistwil")

dfres.wilcoxsumcopy <- lapply(mylistwil ,pwc2copy) %>% bind_rows(.id = "mylistwil")

