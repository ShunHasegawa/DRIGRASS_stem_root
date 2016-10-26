
## check the up-to-date processed df
ls(pattern = "prcssd_root_LSA")


# inspect data ----------------------------------------------------------------------


## each diameter class
ggplot(prcssd_root_LSA3, aes(x = dmclass, y = SA , col = treatment))+
  labs(y = expression(Root~SA~(mm^2~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)
### remove ouliers in plantago and hypochaeris
prcssd_root_LSA4 <- prcssd_root_LSA3 %>% 
  mutate(SA = replace(SA, which(spp %in% c("Hypochaeris", "Plantago") & SA > .3), 
                      NA))
ggplot(prcssd_root_LSA4, aes(x = dmclass, y = SA , col = treatment))+
  labs(y = expression(Root~SA~(mm^2~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)


## cumulative sum
ggplot(prcssd_root_LSA4, aes(x = dmclass, y = SA.cum, col = treatment))+
  labs(y = expression(Root~SA~(mm^2~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)
### remove outliers in Hypochaeris and Plantago
prcssd_root_LSA5 <- prcssd_root_LSA4 %>% 
  mutate(SA.cum = replace(SA.cum, 
                          which(spp %in% c("Hypochaeris", "Plantago") & SA.cum > .5),
                          NA))
ggplot(prcssd_root_LSA5, aes(x = dmclass, y = SA.cum, col = treatment))+
  labs(y = expression(Root~SA~(mm^2~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)




# check transformation --------------------------------------------------


pdf("Output/Figs/boxwhisker_root_SA.pdf", width = 6, height = 5, onefile = TRUE)
d_ply(prcssd_root_LSA5, .(spp, dmclass), function(x){
  figtitle <- paste0(unique(x$spp), unique(x$dmclass))
  print(figtitle)
  tryCatch({
    create_trans_boxplot(SA.cum ~ treatment, data = x)
    title(main = figtitle, outer = TRUE, line = -1)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})
dev.off()
### log/sqrt seem fine for most of measurements


# analysis for each spp  ----------------------------------------------------------------


rootSA_m_l <- dlply(prcssd_root_LSA5, .(spp, dmclass), function(x){
  
  # get plot mean
  d <- x %>% 
    group_by(treatment, plot) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), SA.cum)
  
  print(paste(unique(x$spp), unique(x$dmclass)))
  tryCatch({
    m <- lm(log(SA.cum) ~ treatment, data = d)
    pval <- anova(m)$`Pr(>F)`[1]
    list(model = m, pval = pval)
  }, 
  error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})

plot_rootSA_pval <- ldply(rootSA_m_l, function(x) x$pval) %>% 
  filter(!is.na(V1)) %>% 
  ggplot(., aes(x = dmclass, y = V1)) +
  labs(y = expression(italic(P))) +
  geom_point()+
  geom_hline(yintercept = .05, col = "red") +
  facet_wrap(~ spp)
plot_rootSA_pval
### only first 3 classes of Axonopus was significnat

ax_rootSA_m1 <- rootSA_m_l[["Axonopus.1"]]$model
anova(ax_rootSA_m1)
plot_diag(ax_rootSA_m1)

plot(lsmeans::lsmeans(ax_rootSA_m1, specs = "treatment"), comparisons = TRUE)




# analysis to compare spp ----------------------------------------------------


create_trans_boxplot(SA.cum ~ treatment * spp, 
                     data = filter(prcssd_root_LSA5, dmclass == 19))


spcomp_rootSA_m_l <- dlply(prcssd_root_LSA5, .(dmclass), function(x){
  m <- lmer(log(SA.cum) ~ spp + treatment + (1 | plot / subplot), data = x)
  pval <- Anova(m, test.statistic = "F")$`Pr(>F)`
  return(list(model = m, pval = pval))
})

## result summary 
ldply(spcomp_rootSA_m_l, function(x){
  data.frame(P_spp = x$pval[1], P_treatment = x$pval[2])
})


## model diagnosis
plot(spcomp_rootSA_m_l[[1]]$model)
plot(spcomp_rootSA_m_l[[19]]$model)
qqnorm(resid(spcomp_rootSA_m_l[[1]]$model))
qqline(resid(spcomp_rootSA_m_l[[1]]$model))
qqnorm(resid(spcomp_rootSA_m_l[[19]]$model))
qqline(resid(spcomp_rootSA_m_l[[19]]$model))


lsmeans::lsmeans(spcomp_rootL_m_l[[19]]$model, pairwise ~ spp)
filter(prcssd_root_LSA5, dmclass == 19) %>% 
  ggplot(., aes(x = spp, y = log(SA.cum))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

