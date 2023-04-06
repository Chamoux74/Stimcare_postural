library(goeveg)

#analyse cv range

dfcvrangecopx <-
  aggregate(
    rangecopx ~ test + condition + instant_mesure,
    data = dfinrange,
    FUN = function(x) {
      sd(x) / mean(x) * 100
    }
  )

dfcvrangecopy <-  aggregate(
  rangecopy ~ test + condition + instant_mesure,
  data = dfinrange,
  FUN = function(x) {
    sd(x) / mean(x) * 100
  }
)

#analyse CV sumccopx

dfcvsumcopx <- aggregate(
  sumcopx ~ test + condition + instant_mesure,
  data = dfpostfin,
  FUN = function(x) {
    sd(x) / mean(x) * 100
  }
)

dfcvsumcopy <- aggregate(
  sumcopy ~ test + condition + instant_mesure,
  data = dfpostfin,
  FUN = function(x) {
    sd(x) / mean(x) * 100
  }
)

#analyse CV aire

dfairecv <- aggregate(
  area ~ test + condition + instant_mesure,
  data = dfairepubtot,
  FUN = function(x) {
    sd(x) / mean(x) * 100
  }
)

test <- aggregate(
  area ~ test + condition + instant_mesure,
  data = dfairepubtot,
  FUN = function(x) {var(x)
  }
)


#plot cv

plotcvareawrap <- ggbarplot( data = dfcvrangecopx ,
                           x = "instant_mesure",
                           y = "rangecopx",
                           color = "condition",
                           palette = c("#00AFBB" , "#FC4E07"),
                           order = c("PRE" ,
                                     "MID" ,
                                     "POST" ,
                                     "POST48"), position = position_dodge(), width = 0.5 ,
                           ylab = "CV",
                           xlab = "instant_mesure" ,
                           title = "CVarea_patch_placebo-test"
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

plotcvareawrap
