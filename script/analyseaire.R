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
diffcopx <- lapply(dfpostural100HzBFpat , diffx)
diffcopx <- lapply(diffcopx , as.data.frame)

diffy <- function (dy) {
  diff(dy$copy)
}
diffcopy <- lapply(dfpostural100HzBFpat , diffy)
diffcopy <- lapply(diffcopy , as.data.frame)

diffxy <- mapply(cbind , diffcopx , diffcopy , SIMPLIFY = FALSE)

nam <- c("diffcopx" , "diffcopy")
diffxy <- lapply(diffxy , setNames , nam)

#fonction calcule d'aire publication

covpub <- function (bob) {covmat <- cov(bob[,2:3])}

test1 <- lapply(dfpostural100HzBFpat , covpub)
test2 <- lapply(dfpostural100HzBFpb , covpub)

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
colnames(dfairepub)[1] <- "area95"

dfairepubpb <- as.data.frame(t(dfairepubpb))
colnames(dfairepubpb)[1] <- "area95"

dfaire95 <- rbind(dfairepub, dfairepubpb)
dfaire95 <- cbind(dfpostfin, dfaire95)
dfaire95 <- filter(dfaire95 ,instant_mesure != "POST48")

dfaire95 <- filter(dfaire95, sujet != "GS")
dfaire95 <- filter(dfaire95, sujet != "MD")
dfaire95 <- filter(dfaire95, sujet != "AL")

dfaire95F <- filter(dfaire95, test == "2POF")
dfaire950 <- filter(dfaire95, test == "2POO")

dfaire95F <- select(dfaire95F, area95)
dfaire950 <- select(dfaire950, area95)

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

# plot de toute la liste de dataframe

listplotpatch <- lapply(dfpostural100HzBFpat , function(plot) {
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

listplotplacebo <- lapply(dfpostural100HzBFpb , function(plot) {
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

colnames(dfaireellipsepatch) <- "aire"
colnames(dfaireellipseplacebo) <- "aire"

dfaire <- rbind(dfaireellipsepatch, dfaireellipseplacebo)
dfaire <- cbind(dfpostfin, dfaire)
dfaire <- filter(dfaire,instant_mesure != "POST48")

dfaire <- filter(dfaire, sujet != "GS")
dfaire <- filter(dfaire, sujet != "MD")
dfaire <- filter(dfaire, sujet != "AL")

dfaireF <- filter(dfaire, test == "2POF")
dfaire0 <- filter(dfaire, test == "2POO")

dfaireF <- select(dfaireF, aire)
dfaire0 <- select(dfaire0, aire)

#filter to add on dfanalysis

dfanalysisF <- cbind(dfanalysisF, dfaire95F, dfaireF)
dfanalysisO <- cbind(dfanalysisO, dfaire0, dfaire950)
