#analyse range

rx <- function (r) {
  max(r$copx) - min(r$copx)
}
ry <- function (r) {
  max(r$copy) - min(r$copy)
}

rangecopxPB <-
  as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , rx)))
rangecopxP <-
  as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , rx)))
rangecopyPB <-
  as.data.frame(do.call(rbind , lapply(dfposturalplacebofilt25hz , ry)))
rangecopyP <-
  as.data.frame(do.call(rbind , lapply(dfposturalpatchfilt25hz , ry)))

colnames(rangecopxPB) <- c("rangecopxpb")
colnames(rangecopxP) <- c("rangecopxpatch")
colnames(rangecopyPB) <- c("rangecopypb")
colnames(rangecopyP) <- c("rangecopypatch")

dfrangecopxyPB <- cbind(dfsumplacebo , rangecopxPB , rangecopyPB)
dfrangecopxyPB <- dfrangecopxyPB[, -c(1, 2 , 3)]
colnames(dfrangecopxyPB) <-
  c("instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test" ,
    "rangecopx" ,
    "rangecopy")

dfrangecopxypatch <- cbind(dfsumpatch , rangecopyP , rangecopxP)
dfrangecopxypatch <- dfrangecopxypatch[, -c(1, 2 , 3)]
testrem <-
  colnames(dfrangecopxypatch) <-
  c("instant_mesure" ,
    "sujet" ,
    "condition" ,
    "test" ,
    "rangecopx" ,
    "rangecopy")

dfinrange <- rbind(dfrangecopxypatch , dfrangecopxyPB)

#séparation de chaque datafram en fonction des tests

deuxPOO_range <- as.data.frame(dfinrange[dfinrange$test == "2POO", ])
deuxPOF_range <- as.data.frame(dfinrange[dfinrange$test == "2POF", ])
PDOO_range <- as.data.frame(dfinrange[dfinrange$test == "PDOO", ])
PGOO_range <- as.data.frame(dfinrange[dfinrange$test == "PGOO", ])

rangelist <-
  list(deuxPOO_range , deuxPOF_range , PDOO_range , PGOO_range)


outlrange <-
  function(or) {
    or %>% group_by(condition , instant_mesure) %>% identify_outliers(rangecopx)
  }
outlierrangecopx <- lapply(rangelist , outlrange)
names(outlierrangecopx) <-
  c("deuxPOOrange" , "deuxPOFrange" , "PDOOrange" , "PGOOrange")

outlrangey <-
  function(ory) {
    ory %>% group_by(condition , instant_mesure) %>% identify_outliers(rangecopy)
  }

outlierrangecopy <- lapply(rangelist , outlrangey)
names(outliersumcopy) <-
  c("deuxPOOrange" , "deuxPOFrange" , "PDOOrange" , "PGOOrange")

#shapiro

shapr <-
  function(sr) {
    sr %>% group_by(condition , instant_mesure) %>% shapiro_test(rangecopx)
  }
shapirorange <- lapply(rangelist, shapr)
names(shapirorange) <-
  c("deuxPOOrange" , "deuxPOFrange" , "PDOOrange" , "PGOOrange")

shaprangey <-
  function(sry) {
    sry %>% group_by(condition , instant_mesure) %>% shapiro_test(rangecopy)
  }
shapiroyrange <- lapply(rangelist, shaprangey)
names(shapiroyrange) <-
  c("deuxPOOrange" , "deuxPOFrange" , "PDOOrange" , "PGOOrange")

#qqplot

my_listrange <-
  list(
    deuxPOFrange = deuxPOFrange ,
    deuxPOOrange = deuxPOOrange ,
    PDOOrange = PDOOrange ,
    PGOOrange = PGOOrange
  )

ggqqplotcopxlist <- lapply(seq_along(my_listrange), function(i) {
  ggqqplot(my_listrange[[i]], "rangecopx", ggtheme = theme_bw()) +
    facet_grid(instant_mesure ~ condition , labeller = "label_both") +
    ggtitle(names(my_listrange)[i])
})

ggqqplotcopylist  <- lapply(seq_along(my_listrange), function(h) {
  ggqqplot(my_listrange[[h]], "rangecopy", ggtheme = theme_bw()) +
    facet_grid(instant_mesure ~ condition , labeller = "label_both") +
    ggtitle(names(my_listrange)[h])
})

ggqqplotcopxlist[[1]]
ggqqplotcopylist[[3]]

# analyse de friedman

res.friedrangecopx <-
  function(f) {
    f %>% group_by(condition) %>%  friedman_test(rangecopx ~ instant_mesure |
                                                   sujet)
  }
res.friedrangecopy <-
  function (f) {
    f %>% group_by(condition) %>%  friedman_test(rangecopy ~ instant_mesure |
                                                   sujet)
  }

dfres.friedrangecopx <-
  lapply(my_listrange , res.friedrangecopx) %>% bind_rows(.id = "my_listrange")
dfres.friedrangecopy <-
  lapply(my_listrange , res.friedrangecopy) %>% bind_rows(.id = "my_listrange")

#analyse wilcoxon par instant de mesure


pwcrangecopx <-
  function (wi) {
    wi %>% group_by(instant_mesure) %>% wilcox_test(rangecopx ~ condition ,
                                                    paired = TRUE,
                                                    p.adjust.method = "bonferroni")
  }
pwcrangecopy <-
  function (wl) {
    wl %>% group_by(instant_mesure) %>% wilcox_test(
      rangecopy ~ condition ,
      paired = TRUE,
      p.adjust.method = "bonferroni" %>% add_xy_position(x = "condition")
    )
  }

dfres.wilcoxrangecopx <-
  lapply(my_listrange , pwcrangecopx) %>% bind_rows(.id = "my_listrange")

dfres.wilcoxrangecopy <-
  lapply(my_listrange , pwcrangecopy) %>% bind_rows(.id = "my_listrange")

dfres.wilcoxrangecopx <-
  dfres.wilcoxrangecopx %>% add_xy_position(x = "instant_mesure")

dfres.wilcoxrangecopy <-
  dfres.wilcoxrangecopy %>% add_xy_position(x = "instant_mesure")

test <- rep(c("2POF" , "2POO" , "PDOO" , "PGOO"), each = 4 )

dfres.wilcoxrangecopx <- cbind(test , dfres.wilcoxrangecopx)
dfres.wilcoxrangecopy <- cbind(test , dfres.wilcoxrangecopy)

#plot wrap grid en fonction des test

dfrangecopx <-
  select(dfinrange , rangecopx , sujet , condition , instant_mesure , test)
axe <- rep("rangecopx" , time = 512)
dfrangecopx <- cbind(dfrangecopx , axe)

dfinrange$rangecopx <- NULL
axe <- rep("rangecopy" , time = 512)
dfinrange <- cbind(dfinrange , axe)
colnames(dfinrange)[5] <- "rangecopx"
dfinrange <- rbind(dfinrange , dfrangecopx)

plotrangecopxtest <- ggboxplot(
  x = "instant_mesure",
  y = "rangecopx",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c("PRE" ,
            "MID" ,
            "POST" ,
            "POST48"), width = 0.5 ,
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "rangecopx",
  xlab = "instant_mesure" ,
  title = "rangecopx_patch_placebo-test"
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
  facet_wrap(vars(test) , scales = "free_y") +
  stat_pvalue_manual(
    dfres.wilcoxrangecopx ,
    hide.ns = TRUE ,
    label = "p= {p}" ,
    label.size = 2.5 ,
    y.position = c(43 , 40 , 35 , 41 , 27 , 28 , 32 , 40, 44, 40 , 41, 48)
  )

plotrangecopxtest

#plot indiv wrap grid

dfinrange$instant_mesure <-
  factor(dfinrange$instant_mesure ,
         levels = c("PRE" , "MID" , "POST" , "POST48"))
dfinrange$test <-
  factor(dfinrange$test , levels = c("2POF", "2POO" , "PDOO" , "PGOO"))
dfinrange$condition <-
  factor(dfinrange$condition , levels = c("patch" , "placebo"))


plotrangeindivrange <- ggplot(dfinrange, aes(x = instant_mesure , y = rangecopy)) +
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
    aes(x = instant_mesure , y = rangecopy) ,
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
  labs(title = "rangecopy_individual_variation-test") +
  facet_grid(condition ~ test , scales = "free_y")

plotrangeindivrange

# plot range function

plotrangex <- mapply(
  function(bob, my_results, my_listname) {
    ggboxplot(
      my_listrange[[bob]] ,
      x = "instant_mesure",
      y = "rangecopx",
      color = "condition",
      palette = c("#00AFBB" , "#FC4E07"),
      order = c("PRE" ,
                "MID" ,
                "POST" ,
                "POST48"),
      add = "jitter" ,
      ylab = "rangecopx",
      xlab = "instant_mesure" ,
    ) +
      stat_summary(
        geom = "point",
        fun.y = mean ,
        aes(group = condition) ,
        shape = 20 ,
        size = 4 ,
        position = position_dodge2(width = 0.75,
                                   preserve = "single")
      ) +
      theme_bw() +
      ggtitle(names(my_listrange)[bob]) +
      labs(caption = paste(my_results[[my_listname[[1]]]][[8]]))

    paste(my_results[[my_listname]][["p"]]) %>% print()
    paste(my_listname[[1]]) %>% print()
    paste(my_results[1]) %>% print()
    print(my_listname)
  },
  bob = seq_along(my_listrange),
  my_results = dfres.wilcoxrangecopx,
  my_listname = names(dfres.wilcoxrangecopx)
)

my_results = dfres.wilcoxrangecopx
caption = paste(my_results[[my_listname[[1]]]][[8]])

# plot indiv

#plot tout les graphs indiv en les groupant par test mais avec un ggarrange
#pour avoir placebo et patch sur la même figure

fact <-
  function(f) {
    f[[1]] <-
      factor(f[[1]] , levels = c("PRE", "MID", "POST" , "POST48"))
  }

my_listrange <- lapply(my_listrange , fact)


plotindivlist <- lapply(seq_along(my_listrange) , function(ind) {
  ggplot(my_listrange[[ind]], aes(x = instant_mesure , y = rangecopx)) +
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
    labs(color = "sujet") +
    stat_summary(
      geom = "errorbar" ,
      fun.data = mean_sd ,
      colour = "black" ,
      size = 1 ,
      width = 0.2
    ) +
    geom_boxplot(
      aes(x = instant_mesure , y = rangecopx) ,
      width = .5,
      fill = "white" ,
      alpha = 0.3
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
      )
    )  +
    ggtitle(names(my_listrange)[ind]) +
    facet_wrap(vars(condition))
})

plotest




