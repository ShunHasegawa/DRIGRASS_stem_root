
## check the up-to-date processed df
ls(pattern = "prcssd_root_trait_byDC")


# inspect data ------------------------------------------------------------


ggplot(raw_root_trait_byDC, aes(x = treatment, y = Forks, fill = treatment)) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_boxplot()



# check transformation --------------------------------------------------


pdf("Output/Figs/boxwhisker_root_fork.pdf", width = 6, height = 5, onefile = TRUE)
d_ply(raw_root_trait_byDC, .(spp), function(x){
  
  try({
    create_trans_boxplot(Forks ~ treatment, data = x)
    title(main = unique(x$spp), outer = TRUE, line = -1)
  }, 
  silent = TRUE)
}
)
dev.off()
### log seems fine



# analysis --------------------------------------------------------------

rootFork_m_l <- dlply(raw_root_trait_byDC, .(spp), function(x){
  
  print(unique(x$spp))
  ## get plot mean
  d <- x %>% 
    mutate(dmclass = 19) %>%                         # dmclas is added so that get_all_anova_tble function can be used
    group_by(treatment, plot, spp, dmclass) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), Forks)
  
  get_all_anova_tbl(d, log(Forks) ~ treatment)
  
})


smmry_rootForks_byRain_tbl <- ldply(rootFork_m_l, function(x) x$summary_tbl) %>% 
  mutate(trait = "forks", unit = "mg-1")
filter(smmry_rootForks_byRain_tbl, P < 0.1)
### only Axnopus showed a significant treatment effect; Senecio was marginally
### significant at .06

ax_rootFork_m1 <- rootFork_m_l[["Axonopus"]]$model
anova(ax_rootFork_m1)
plot_diag(ax_rootFork_m1)

snc_rootFork_m1 <- rootFork_m_l[["Senecio"]]$model
anova(snc_rootFork_m1)
plot_diag(snc_rootFork_m1)




# analysis to compare between spp -----------------------------------------

create_trans_boxplot(Forks ~ spp, data = raw_root_trait_byDC)
spcomp_rootFork_m1 <- lmer(log(Forks) ~ spp + treatment + (1|plot/subplot), data = raw_root_trait_byDC)
Anova(spcomp_rootFork_m1, test.statistic = "F")
smmry_rootFork_tbl <- get_all_anova_tbl_bySpp(model    = spcomp_rootFork_m1,
                                              .data    = raw_root_trait_byDC,
                                              variable = "Forks") %>% 
  mutate(trait = "forks", unit = "mg-1") %>%
  select(trait, unit, F, P, everything()) 
smmry_rootFork_tbl
