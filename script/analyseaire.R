library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library(ggforce)
library(car)

#plot trace COP

diffx <- function(d) {
  diff(d$copx)
}
diffcopx <- lapply(dfposturalpatchfilt25hz , diffx)
diffcopx <- lapply(diffcopx , as.data.frame)

diffy <- function (dy) {
  diff(dy$copy)
}
diffcopy <- lapply(dfposturalpatchfilt25hz , diffy)
diffcopy <- lapply(diffcopy , as.data.frame)

diffxy <- mapply(cbind , diffcopx , diffcopy , SIMPLIFY = FALSE)

nam <- c("diffcopx" , "diffcopy")
diffxy <- lapply(diffxy , setNames , nam)


test <- as.data.frame(dfposturalpatchfilt25hz$`Alban LE Gall-2022.09.28-13.33.43PREA2POO`[, 2:3])

#fonction calcule d'aire publication

covpub <- function (bob) {covmat <- cov(bob[,2:3])}

test1 <- lapply(dfposturalpatchfilt25hz , covpub)
test2 <- lapply(dfposturalplacebofilt25hz , covpub)

#calcule eig pour toute la liste valeur

resultats_eigen <- lapply(test1, function(cov_mat) {
  eig <- eigen(cov_mat)
  vec <- eig$vectors
  val <- as.data.frame(eig$values)
  return(list(vec = vec, val = val))
})

resultats_eigenpb <- lapply(test2, function(cov_mat) {
  eig <- eigen(cov_mat)
  vec <- eig$vectors
  val <- as.data.frame(eig$values)
  return(list(vec = vec, val = val))
})

#calcule de l'aire

dfairepub <-
  as.data.frame(lapply(resultats_eigen , function(result) {
    (pi*prod((2.4478*(sqrt(result$val[1 ,]))) + (2.4478 * (sqrt(result$val[2 ,])))))
  }))

dfairepubpb <-
  as.data.frame(lapply(resultats_eigenpb , function(result) {
    (pi*prod((2.4478*(sqrt(result$val[1 ,]))) + (2.4478 * (sqrt(result$val[2 ,])))))
  }))

#mise en forme dataframe

dfairepub <- as.data.frame(t(dfairepub))
colnames(dfairepub)[1] <- "area95%"

dfairepubpb <- as.data.frame(t(dfairepubpb))
colnames(dfairepubpb)[1] <- "area95%"

dfairepatpb <- cbind(dfairepub , dfairepubpb)
colnames(dfairepatpb)[1:2] <- c("area95%patch" , "area95%placebo")

#export en csv

write.csv2(dfairepatpb, file="dataaire_literature.csv")

#theme qui met tout en blanc derrière et titre centré en gras
mytheme = list(
  theme_classic() +
    theme(
      panel.background = element_blank(),
      strip.background = element_rect(colour = NA, fill = NA),
      panel.border = element_rect(fill = NA, color = "black"),
      legend.title = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 9),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13)
    )
)

#test plot trajet du COP + aire

# plottest <-
#   ggplot(
#     data = test ,
#     aes(x = copx, y = copy) #, group = 1)
#   ) +
#   geom_path(color = "black" ,
#             linejoin = "round" ,
#             linewidth = 0.8) +
#   stat_ellipse(
#     geom = "polygon" ,
#     color = "#505050" ,
#     linetype = 5 ,
#     level = 0.95 ,
#     linewidth = 1.5 ,
#     alpha = 0.13
#   ) +
#   mytheme
#
# plottest

#calcule de l'aire à partir de la fonction stat_ellipse de R

# plot de toute la liste de dataframe

listplotpatch <- lapply(dfposturalpatchfilt25hz , function(plot) {
  ggplot(
    data = plot ,
    aes(x = copx, y = copy) #, group = 1)
  ) +
  geom_path(color = "black" ,
            linejoin = "round" ,
            linewidth = 0.8) +
  stat_ellipse(
    geom = "polygon" ,
    color = "#505050" ,
    linetype = 5 ,
    level = 0.95 ,
    linewidth = 1.5 ,
    alpha = 0.13
  ) +
  mytheme })

listplotplacebo <- lapply(dfposturalplacebofilt25hz , function(plot) {
  ggplot(
    data = plot ,
    aes(x = copx, y = copy) #, group = 1)
  ) +
    geom_path(color = "black" ,
              linejoin = "round" ,
              linewidth = 0.8) +
    stat_ellipse(
      geom = "polygon" ,
      color = "#505050" ,
      linetype = 5 ,
      level = 0.95 ,
      linewidth = 1.5 ,
      alpha = 0.13
    ) +
    mytheme })

#extraction des données de l'ellipse crée par stat_ellipse


extractplotpatch <-
  lapply(listplotpatch , function (dat) {
    pb <- ggplot_build(dat)
    el <-
      pb$data[[2]][c("x", "y")] #extraction du deuxième élément de la list de tout les éléments de chaque plot
    ctr <- MASS::cov.trob(el)$center # Center of ellipse
    dist2center <- sqrt(rowSums((t(t(
      el
    ) - ctr)) ^ 2)) # Calculate distance to center from each point on the ellipse
    return(as.data.frame(c(
      el = el ,
      ctr = ctr ,
      dist2center = dist2center
    ))) #retourne une liste de tout les éléments de la fonction
  })

extractplotplacebo <-
  lapply(listplotplacebo , function(dat) {
    pb <- ggplot_build(dat)
    el <- pb$data[[2]][c("x", "y")] #extraction du deuxième élément de la list de tout les éléments de chaque plot
    ctr <- MASS::cov.trob(el)$center # Center of ellipse
    dist2center <- sqrt(rowSums((t(t(el) - ctr)) ^ 2)) # Calculate distance to center from each point on the ellipse
    return(as.data.frame(c(
      el = el ,
      ctr = ctr ,
      dist2center = dist2center
    ))) #retourne une liste de tout les éléments de la fonction
  })

# Calculate area of ellipse from semi-major and semi-minor axes.
# These are, respectively, the largest and smallest values of dist2center.

dfaireellipsepatch <-
  as.data.frame(lapply(extractplotpatch , function(area) {
    pi * min(area[1, 5:56]) * max(area[1, 5:56])
  }))

dfaireellipseplacebo <-
  as.data.frame(lapply(extractplotplacebo , function(area) {
    pi * min(area[1, 5:56]) * max(area[1, 5:56])
  }))

#mise en forme

dfaireellipsepatch <- as.data.frame(t(dfaireellipsepatch))
dfaireellipseplacebo <- as.data.frame(t(dfaireellipseplacebo))

dftotaireellispe <- cbind(dfaireellipsepatch , dfaireellipseplacebo)

colnames(dftotaireellispe)[1:2] <- c("airepatch" , "aireplacebo")

#extraction csv

write.csv2(dftotaireellispe, file="dataaire_ellipse.csv")

#mise en forme data pour analyse aire_ellipse

dfaireellipsepatch <- cbind(dfaireellipsepatch , bloc1 , bloc2 , patch , strpatch)
dfaireellipseplacebo <- cbind(dfaireellipseplacebo , bloc1 , bloc2 , placebo , strplacebo)

colnames(dfaireellipsepatch) <-
  c("area",
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")
colnames(dfaireellipseplacebo) <-
  c("area",
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")

dfairetot <- rbind(dfaireellipsepatch , dfaireellipseplacebo)

# identify outliers

dfout <-
  dfairetot %>% group_by(condition , instant_mesure) %>% identify_outliers(area)

# test normalité

dfnorm <- dfairetot %>% group_by(condition , instant_mesure , test) %>% shapiro_test(area)

#qqplot

ggqqplot <- ggqqplot(dfairetot , "area", ggtheme = theme_bw()) +
    facet_grid(instant_mesure ~ condition + test , scales = "free_y") +
    ggtitle("qqplotarea")

ggqqplot

# filtre sans sujet PN

dfairetotfilt <- filter(dfairetot , !sujet == "PN")

#plot

plotareawrap <- ggboxplot( data = dfairetotfilt ,
                              x = "instant_mesure",
                              y = "area",
                              color = "condition",
                              palette = c("#00AFBB" , "#FC4E07"),
                              order = c("PRE" ,
                                        "MID" ,
                                        "POST" ,
                                        "POST48"), width = 0.5 ,
                              add = "jitter" , size = 0.6 , shape = "condition" ,
                              ylab = "aire",
                              xlab = "instant_mesure" ,
                              title = "area_patch_placebo-test"
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

plotareawrap

#plot variation indiv

dfairetotfilt$instant_mesure <-
  factor(dfairetotfilt$instant_mesure ,
         levels = c("PRE" , "MID" , "POST" , "POST48"))

plotindivarea <- ggplot(dfairetotfilt, aes(x = instant_mesure , y = area)) +
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
    aes(x = instant_mesure , y = area) ,
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
      "#156901"
    )) +
  labs(color = "sujet") +
  labs(title = "area_individual_variation-test") +
  facet_grid(condition ~ test , scales = "free_y")

plotindivarea

# analyse non paramétrique

reswilc <- dfairetotfilt %>% group_by(condition , test) %>%  wilcox_test(area ~ instant_mesure ,
                                                     paired = TRUE,
                                                     p.adjust.method = "bonferroni")

reswilcinstant <- dfairetotfilt %>% group_by(instant_mesure , test) %>%  wilcox_test(area ~ condition ,
                                                   paired = TRUE,
                                                   p.adjust.method = "bonferroni")

#mise en forme donnée calculé formule littérature

dfairepub <- cbind(dfairepub , bloc1 , bloc2 , patch , strpatch)
dfairepubpb <- cbind(dfairepubpb , bloc1 , bloc2 , placebo , strplacebo)

colnames(dfairepub) <-
  c("area",
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")
colnames(dfairepubpb) <-
  c("area",
    "instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test")

dfairepubtot <- rbind(dfairepub , dfairepubpb)

# identify outliers

dfout1 <-
  dfairepubtot %>% group_by(condition , instant_mesure) %>% identify_outliers(area)

# test normalité

dfnorm1 <- dfairepubtot %>% group_by(condition , instant_mesure , test) %>% shapiro_test(area)

#qqplot

ggqqplot1 <- ggqqplot(dfairepubtot , "area", ggtheme = theme_bw()) +
  facet_grid(instant_mesure ~ condition + test , scales = "free_y") +
  ggtitle("qqplotarea")

ggqqplot1

#analyse non paramétrique je pense que l'analyse paramétrique pourrait ce faire

reswilc1 <- dfairepubtot %>% group_by(condition , test) %>%  wilcox_test(area ~ instant_mesure ,
                                                                         paired = TRUE,
                                                                         p.adjust.method = "bonferroni")

reswilcinstant1 <- dfairepubtot %>% group_by(instant_mesure , test) %>%  wilcox_test(area ~ condition ,
                                                                                     paired = TRUE,
                                                                                     p.adjust.method = "bonferroni")
