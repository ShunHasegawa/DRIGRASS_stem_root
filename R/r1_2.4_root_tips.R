
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
  
  ## get ring mean
  d <- x %>% 
    group_by(treatment, plot) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), Tips)
  
  tryCatch({
    m <- lm(log(Tips) ~ treatment, data = d)
    pval <- anova(m)$`Pr(>F)`[1]
    list(model = m, pval = pval)
  }, 
  error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})

ldply(rootTip_m_l, function(x) x$pval) %>% 
  filter(!is.na(V1))
### only Axnopus showed a significant treatment effect, with marginally
### significant Hypochaeris

ax_rootTip_m1 <- rootTip_m_l[["Axonopus"]]$model
anova(ax_rootTip_m1)
plot_diag(ax_rootTip_m1)
plot(lsmeans::lsmeans(ax_rootTip_m1, specs = "treatment"), comparisons = TRUE)
