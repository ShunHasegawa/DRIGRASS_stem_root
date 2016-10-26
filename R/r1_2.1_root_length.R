
## check the up-to-date processed df
ls(pattern = "prcssd_root_LSA")


# inspect data ------------------------------------------------------------


## each diameter class
ggplot(prcssd_root_LSA1, aes(x = dmclass, y = L , col = treatment))+
  labs(y = expression(Root~length~(mm~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)


## remove obvious outliers from Plantago and Hypochaeris
prcssd_root_LSA2 <- prcssd_root_LSA1 %>% 
  mutate(L = replace(L, which(spp %in% c("Plantago", "Hypochaeris") & L > 10), NA))
ggplot(prcssd_root_LSA2, aes(x = dmclass, y = L , col = treatment))+
  labs(y = expression(Root~length~(mm~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)


## cumulative
ggplot(prcssd_root_LSA2, aes(x = dmclass, y = L.cum, col = treatment))+
  labs(y = expression(Root~length~(mm~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)
### remove outliers as suggested above as well
prcssd_root_LSA3 <- prcssd_root_LSA2 %>% 
  mutate(L.cum = replace(L.cum, which(spp %in% c("Plantago", "Hypochaeris") & 
                                        L.cum > 20), NA))
ggplot(prcssd_root_LSA3, aes(x = dmclass, y = L.cum, col = treatment))+
  labs(y = expression(Root~length~(mm~mg^'-1'))) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = FALSE)




# check transformation ----------------------------------------------------


pdf("Output/Figs/boxwhisker_root_length.pdf", width = 6, height = 5, onefile = TRUE)
d_ply(prcssd_root_LSA3, .(spp, dmclass), function(x){
  figtitle <- paste0(unique(x$spp), unique(x$dmclass))
  print(figtitle)
  tryCatch({
    create_trans_boxplot(L.cum ~ treatment, data = x)
    title(main = figtitle, outer = TRUE, line = -1)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})
dev.off()
### sqrt or log seems fine for most of measurements




# analysis for each spp ----------------------------------------------------------------


rootL_m_l <- dlply(prcssd_root_LSA3, .(spp, dmclass), function(x){
  
  # get plot mean
  d <- x %>% 
    group_by(treatment, plot) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), L.cum)
  
  print(paste(unique(x$spp), unique(x$dmclass)))
  tryCatch({
    m <- lm(sqrt(L.cum) ~ treatment, data = d)
    pval <- anova(m)$`Pr(>F)`
    list(model = m, pval = pval)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})


plot_rootL_pval <- ldply(rootL_m_l, function(x) x$pval) %>% 
  filter(!is.na(V1)) %>% 
  ggplot(., aes(x = dmclass, y = V1)) +
  labs(y = expression(italic(P))) +
  geom_point()+
  geom_hline(yintercept = .05, col = "red") +
  facet_wrap(~ spp)
plot_rootL_pval
### only Axnopus showed a significant treatment effect; diameter class don't
### appear to matter too much


ax_rootL_m1 <- rootL_m_l[["Axonopus.19"]]$model
anova(ax_rootL_m1)
plot_diag(ax_rootL_m1)
plot(lsmeans::lsmeans(ax_rootL_m1, specs = "treatment"), comparisons = TRUE)




# analysis to compare spp ----------------------------------------------------


## The above anlsysis showed that diameter classes don't matter too much. So
## just use the cumulative sum at DC 19.
create_trans_boxplot(L.cum ~ treatment * spp, 
                     data = filter(prcssd_root_LSA3, dmclass == 19))


spcomp_rootL_m_l <- dlply(prcssd_root_LSA3, .(dmclass), function(x){
  m <- lmer(log(L.cum) ~ spp + treatment + (1 | plot / subplot), data = x)
  pval <- Anova(m, test.statistic = "F")$`Pr(>F)`
  return(list(model = m, pval = pval))
})

## result summary 
ldply(spcomp_rootL_m_l, function(x){
  data.frame(P_spp = x$pval[1], P_treatment = x$pval[2])
})


## model diagnosis
plot(spcomp_rootL_m_l[[1]]$model)
plot(spcomp_rootL_m_l[[19]]$model)
qqnorm(resid(spcomp_rootL_m_l[[1]]$model))
qqline(resid(spcomp_rootL_m_l[[1]]$model))
qqnorm(resid(spcomp_rootL_m_l[[19]]$model))
qqline(resid(spcomp_rootL_m_l[[19]]$model))


lsmeans::lsmeans(spcomp_rootL_m_l[[19]]$model, pairwise ~ spp)
filter(prcssd_root_LSA3, dmclass == 19) %>% 
  ggplot(., aes(x = spp, y = log(L.cum))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

