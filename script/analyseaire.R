library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library(ggforce)
library(car)

#plot trace COP

diffx <- function(d){diff(d$copx)}
diffcopx <- lapply(dfposturalpatchfilt25hz , diffx)
diffcopx <- lapply(diffcopx , as.data.frame)

diffy <- function (dy){diff(dy$copy)}
diffcopy <- lapply(dfposturalpatchfilt25hz , diffy)
diffcopy <- lapply(diffcopy , as.data.frame)

diffxy <- mapply(cbind , diffcopx , diffcopy , SIMPLIFY = FALSE)

nam <- c("diffcopx" , "diffcopy")
diffxy <- lapply(diffxy , setNames , nam)

cov_mat <- cov(test)

eig <- eigen(cov_mat)
vec <- eig$vectors
val <- as.data.frame(eig$values)
result <- pi*prod((2.4478*(sqrt(val$`eig$values`[1]))) + (2.4478*(sqrt(val$`eig$values`[2]))))

test <- as.data.frame(dfposturalpatchfilt25hz$`Alban LE Gall-2022.09.28-13.33.43PREA2POO`[,2:3])

#theme qui met tout en blanc derrière et titre centré en gras
mytheme = list(
  theme_classic()+
    theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_blank(),legend.position="bottom", strip.text = element_text(face="bold", size=9),
          axis.text=element_text(face="bold"),axis.title = element_text(face="bold"),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)

plottest <-
  ggplot(data = dfposturalpatchfilt25hz$`Pierre Emmanuelle Naullet-2022.10.27-08.58.07POST48B2POO`,
         aes(x = copx, y = copy, group = 1)) +
  geom_path(color = "#d6e2e2" ,
            linejoin = "round" ,
            linewidth = 1) +
  stat_ellipse(
    geom = "polygon" ,
    color = "#080e0e" ,
    linetype = 5 ,
    level = 0.95 ,
    linewidth = 1.5 ,
    alpha = 0.13
  ) +
  mytheme

plottest

pb = ggplot_build(plottest)
el = pb$data[[2]][c("x","y")]

# Center of ellipse
ctr = MASS::cov.trob(el)$center  # Per @Roland's comment

# Calculate distance to center from each point on the ellipse
dist2center <- sqrt(rowSums((t(t(el)-ctr))^2))

# Calculate area of ellipse from semi-major and semi-minor axes.
# These are, respectively, the largest and smallest values of dist2center.
area <- pi*min(dist2center)*max(dist2center)

cov.wt(test[, c(1, 2)])$cov
