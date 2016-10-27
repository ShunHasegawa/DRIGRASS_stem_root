
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
  
  print(paste(unique(x$spp), unique(x$dmclass)))
  
  ## get plot mean
  d <- x %>% 
    group_by(treatment, plot, spp, dmclass) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), SA.cum)
  
  get_all_anova_tbl(.data = d, formula = log(SA.cum) ~ treatment)
  
})

smmry_rootSA_byRain_tbl <- ldply(rootSA_m_l, function(x) x$summary_tbl) %>% 
  mutate(trait = "surface area", unit = "mm2 mg-1")
filter(smmry_rootSA_byRain_tbl, P < 0.1)
### only first 3 classes of Axonopus was significnat

ax_rootSA_m1 <- rootSA_m_l[["Axonopus.1"]]$model
anova(ax_rootSA_m1)
plot_diag(ax_rootSA_m1)




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


## summary table by spp

## diameter class doesn't matter, so just use 19
smmry_rootSA_tbl <- get_all_anova_tbl_bySpp(model = spcomp_rootSA_m_l[[19]]$model,
                                            .data = filter(prcssd_root_LSA5, dmclass == 19),
                                            variable = "SA.cum") %>% 
  mutate(trait = "surface area", unit = "mm2 mg-1") %>%
  select(trait, unit, F, P, everything())                   
smmry_rootSA_tbl


