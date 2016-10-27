
## check the up-to-date processed df
ls(pattern = "prcssd_root_trait_byDC")

# Stem/root ratios --------------------------------------------------------------------

ggplot(prcssd_root_trait_byDC1, aes(x = treatment, y = sr_ratio, fill = treatment)) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_boxplot()


# . check transformation --------------------------------------------------


pdf("Output/Figs/boxwhisker_stem.rootR.pdf", width = 6, height = 5, onefile = TRUE)
d_ply(prcssd_root_trait_byDC1, .(spp), function(x){
  figtitle <- paste0(unique(x$spp), unique(x$dmclass))
  print(figtitle)
  tryCatch({
    create_trans_boxplot(sr_ratio ~ treatment, data = x)
    title(main = figtitle, outer = TRUE, line = -1)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})
dev.off()
### log seems fine



# . analysis --------------------------------------------------------------

stm.rtR_m_l <- dlply(prcssd_root_trait_byDC1, .(spp), function(x){
  
  print(unique(x$spp))
  
  ## get ring mean
  d <- x %>% 
    mutate(dmclass = 19) %>%                         # dmclas is added so that get_all_anova_tble function can be used
    group_by(treatment, plot, spp, dmclass) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), sr_ratio)
  
  ## run anova and get associated summary table
  get_all_anova_tbl(d, log(sr_ratio) ~ treatment)
  
})

smmry_stm.rtRs_byRain_tbl <- ldply(stm.rtR_m_l, function(x) x$summary_tbl) %>% 
  mutate(trait = "shoot_root_ratio", unit = "")
filter(smmry_stm.rtRs_byRain_tbl, P < 0.1)
### Senecio showed a significnat treatment effect


sn_stm.rtR_m1 <- stm.rtR_m_l[["Senecio"]]$model
anova(sn_stm.rtR_m1)
plot_diag(sn_stm.rtR_m1)


# analysis to compare between spp -----------------------------------------

create_trans_boxplot(sr_ratio ~ spp, data = prcssd_root_trait_byDC1)
spcomp_stm.rtR_m1 <- lmer(log(sr_ratio) ~ spp + treatment + (1|plot/subplot), 
                          data = prcssd_root_trait_byDC1)
Anova(spcomp_stm.rtR_m1, test.statistic = "F")
smmry_stm.rtR_tbl <- get_all_anova_tbl_bySpp(model    = spcomp_stm.rtR_m1,
                                             .data    = prcssd_root_trait_byDC1,
                                             variable = "sr_ratio") %>% 
  mutate(trait = "shoot_root_ratio", unit = "mg-1") %>%
  select(trait, unit, F, P, everything()) 
smmry_stm.rtR_tbl
