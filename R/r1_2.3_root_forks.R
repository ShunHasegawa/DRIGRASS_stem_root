
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
  
  ## get plot mean
  d <- x %>% 
    group_by(treatment, plot) %>% 
    summarise_each(funs(mean(., na.rm = TRUE)), Forks)
  
  tryCatch({
    m <- lm(log(Forks) ~ treatment, data = d)
    pval <- anova(m)$`Pr(>F)`[1]
    list(model = m, pval = pval)
  }, 
  error = function(e){
    cat("ERROR :",conditionMessage(e), "\n")
  })
})

ldply(rootFork_m_l, function(x) x$pval) %>% 
  filter(!is.na(V1))
### only Axnopus showed a significant treatment effect; Senecio was marginally
### significant at .06

ax_rootFork_m1 <- rootFork_m_l[["Axonopus"]]$model
anova(ax_rootFork_m1)
plot_diag(ax_rootFork_m1)
plot(lsmeans::lsmeans(ax_rootFork_m1, specs = "treatment"), comparisons = TRUE)

snc_rootFork_m1 <- rootFork_m_l[["Senecio"]]$model
anova(snc_rootFork_m1)
plot_diag(snc_rootFork_m1)
plot(lsmeans::lsmeans(snc_rootFork_m1, specs = "treatment"), comparisons = TRUE)

