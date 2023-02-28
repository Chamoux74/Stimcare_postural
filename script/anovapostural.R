library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

a <- function(x) {colSums(abs(x))}

dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO` <-
  dfposturalpatchfilt25hz$`Bastien Marsan-2022.10.06-15.20.56MIDA2POO`[,-c(4,5)]

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
bloc1 <- c(rep(x = bloc , times = 18))

dfsumpatch <- cbind(dfsumpatch , bloc1)
dfsumplacebo <- cbind(dfsumplacebo , bloc1)

bloc2 <-
  c(
    rep(x = "AL" , times = 16)  ,
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
    rep(x = "SP" , times = 16) ,
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

plot2POOx <- ggboxplot(
  deuxPOOrange ,
  x = "instant_mesure",
  y = "rangecopx",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "PRE" ,
    "MID" ,
    "POST" ,
    "POST48"
  ),
  add = "jitter" ,
  ylab = "rangecopx",
  xlab = "instant_mesure" ,
  title = "rangecopx2POO_patch_placebo"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd , aes(group = condition) ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1 , position = position_dodge2(width = 0.75,
                                          preserve = "single")
  )

plot2POOx

testdf <- as.data.frame(apply(deuxPOO, 2, as.vector))

testnum <- deuxPOO %>% group_by(sujet)

testnum %>%
  spread(condition, instant_mesure) %>%
  select(-grouped_id)

duplicated(testnum)

dplyr::count(deuxPOO, time, sumcopx, sumcopy, instant_mesure, sujet, condition , test , sort = TRUE)

estnum <- deuxPOO %>% dplyr::group_by(sujet)

res.aov1 <- rstatix::anova_test(
  data = testnum , dv = sumcopx , wid = sujet ,
  within = c(condition, instant_mesure) , effect.size = "ges",
  detailed = TRUE,
)


get_anova_table(res.aov1 , correction = "auto")

name  <- c("temps" , "copx" , "copy")

dfposturalplacebofilt25hz <- lapply(dfposturalplacebofilt25hz , set_names , name)
dfposturalpatchfilt25hz <- lapply(dfposturalpatchfilt25hz , set_names , name)

rx <- function (r) {max(r$copx) - min(r$copx)}
ry <- function (r) {max(r$copy) - min(r$copy)}

rangecopxPB <- as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , rx)))
rangecopxP <- as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , rx)))
rangecopyPB <- as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , ry)))
rangecopyP <- as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , ry)))

colnames(rangecopxPB) <- c("rangecopxpb")
colnames(rangecopxP) <- c("rangecopxpatch")
colnames(rangecopyPB) <- c("rangecopypb")
colnames(rangecopyP) <- c("rangecopypatch")

dfrangecopxyPB <- cbind(dfsumplacebo , rangecopxPB , rangecopyPB)
dfrangecopxyPB <- dfrangecopxyPB[,-c(1, 2 , 3)]
colnames(dfrangecopxyPB) <-
  c("instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test" ,
    "rangecopx" ,
    "rangecopy")

dfrangecopxypatch <- cbind(dfsumpatch , rangecopyP , rangecopxP)
dfrangecopxypatch <- dfrangecopxypatch[,-c(1, 2 , 3)]
testrem <-
colnames(dfrangecopxypatch) <-
  c("instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test" ,
    "rangecopx" ,
    "rangecopy")

dfinrange <- rbind(dfrangecopxypatch , dfrangecopxyPB)

deuxPOOrange <- as.data.frame(dfinrange[dfinrange$test == "2POO",])
deuxPOFrange <- as.data.frame(dfinrange[dfinrange$test == "2POF",])
PDOOrange <- as.data.frame(dfinrange[dfinrange$test == "PDOO",])
PGOOrange <- as.data.frame(dfinrange[dfinrange$test == "PGOO",])

oldeuxPOO <- identify_outliers(as.data.frame(deuxPOOrange$rangecopx))
oldeuxPOO <- as.numeric(oldeuxPOO$`deuxPOOrange$rangecopx`)

#remove outilers

deuxPOOrange <-
  deuxPOOrange[-which(
    deuxPOOrange$rangecopx > quantile(deuxPOOrange$rangecopx)[4] + 1.5 * IQR(deuxPOOrange$rangecopx) |
      deuxPOOrange$rangecopx < quantile(deuxPOOrange$rangecopx)[2] - 1.5 * IQR(deuxPOOrange$rangecopx)
  ), ]
