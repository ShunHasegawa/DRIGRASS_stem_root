
## check the up-to-date processed df
ls(pattern = "prcssd_root_trait_byDC")

# Tips --------------------------------------------------------------------


ggplot(raw_root_trait_byDC, aes(x = treatment, y = Tips, fill = treatment)) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_boxplot()
### remove an obvious outlier in Hypochaeris and plantago
prcssd_root_trait_byDC1 <- raw_root_trait_byDC %>% 
  mutate(Tips = replace(Tips, which(spp %in% c("Hypochaeris", "Plantago") & Tips > 1000), NA))
ggplot(prcssd_root_trait_byDC1, aes(x = treatment, y = Tips, fill = treatment)) +
  facet_wrap(~ spp, scale = "free_y") +
  geom_boxplot()


# . check transformation --------------------------------------------------


pdf("Output/Figs/boxwhisker_root_tip.pdf", width = 6, height = 5, onefile = TRUE)
d_ply(prcssd_root_trait_byDC1, .(spp), function(x){
  figtitle <- paste0(unique(x$spp), unique(x$dmclass))
  print(figtitle)
  tryCatch({
    create_trans_boxplot(Tips ~ treatment, data = x)
    title(main = figtitle, outer = TRUE, line = -1)
  }, error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})
dev.off()
### log seems fine



# . analysis --------------------------------------------------------------

rootTip_m_l <- dlply(prcssd_root_trait_byDC1, .(spp), function(x){
  
  print(unique(x$spp))
  
  ## get ring mean
  d <- x %>% 
    mutate(dmclass = 19) %>%                         # dmclas is added so that get_all_anova_tble function can be used
    group_by(treatment, plot, spp, dmclass) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), Tips)
  
  ## run anova and get associated summary table
  get_all_anova_tbl(d, log(Tips) ~ treatment)
  
})

smmry_rootTips_byRain_tbl <- ldply(rootTip_m_l, function(x) x$summary_tbl) %>% 
  mutate(trait = "tips", unit = "mg-1")
filter(smmry_rootTips_byRain_tbl, P < 0.1)
### only Axnopus showed a significant treatment effect, with marginally
### significant Hypochaeris

ax_rootTip_m1 <- rootTip_m_l[["Axonopus"]]$model
anova(ax_rootTip_m1)
plot_diag(ax_rootTip_m1)


# analysis to compare between spp -----------------------------------------

create_trans_boxplot(Tips ~ spp, data = prcssd_root_trait_byDC1)
spcomp_rootTip_m1 <- lmer(log(Tips) ~ spp + treatment + (1|plot/subplot), 
                           data = prcssd_root_trait_byDC1)
Anova(spcomp_rootTip_m1, test.statistic = "F")
smmry_rootTip_tbl <- get_all_anova_tbl_bySpp(model    = spcomp_rootTip_m1,
                                              .data    = prcssd_root_trait_byDC1,
                                              variable = "Tips") %>% 
  mutate(trait = "tips", unit = "mg-1") %>%
  select(trait, unit, F, P, everything()) 
smmry_rootTip_tbl
