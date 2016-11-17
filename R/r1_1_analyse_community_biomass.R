summary(comm_biom)


# prepare df --------------------------------------------------------------
comm_biom_ed  <- comm_biom %>% 
  filter(treatment != "No.shelter") %>%                    # remove No-shelter
  select(-side) %>%                                        # remove unnecessary column
  group_by(treatment, herb, plot) %>%                      # summarise by plot
  summarise_each(funs(mean), -subplot)                     # get plot mean
combio_rxh    <- filter(comm_biom_ed, treatment %in% c("Reduced frequency", "Ambient", "Reduced"))  # test rainfall x herb
combio_contrh <- filter(comm_biom_ed, herb == "Control")   # test rainfall; subset control-herb





# total biomass -----------------------------------------------------------


# . rain x herb -------------------------------------------------------------

create_trans_boxplot(total ~ treatment * herb, data = combio_rxh)
ttlb_rxh_m1 <- lm(log(total) ~ treatment * herb, data = combio_rxh)
Anova(ttlb_rxh_m1)
plot_diag(ttlb_rxh_m1)





# . rain --------------------------------------------------------------------

## the above analysis showed no herb effect, so use the complete data set

create_trans_boxplot(total ~ treatment, data = comm_biom_ed)
ttlb_r_m1 <- lm(total^(1/3) ~ treatment, data = comm_biom_ed)
Anova(ttlb_r_m1)
plot_diag(ttlb_r_m1)




# above-ground biomass ----------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(ab_mass ~ treatment * herb, data = comm_biom_ed)
abb_rxh_m1 <- lm(ab_mass ~ treatment * herb, data = combio_rxh)
Anova(abb_rxh_m1)
plot_diag(abb_rxh_m1)




# . rain ------------------------------------------------------------------

## the above analysis showed no herb effect, so use the complete data set

create_trans_boxplot(ab_mass ~ treatment, data = comm_biom_ed)
abb_r_m1 <- lm(ab_mass ~ treatment, data = comm_biom_ed)
Anova(abb_r_m1)
plot_diag(abb_r_m1)




# below-ground biomass ----------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(bl_mass ~ treatment * herb, data = comm_biom_ed)
blb_rxh_m1 <- lm(bl_mass^(1/3) ~ treatment * herb, data = combio_rxh)
Anova(blb_rxh_m1)
plot_diag(blb_rxh_m1)




# . rain ------------------------------------------------------------------

## the above analysis showed no herb effect, so use the complete data set

create_trans_boxplot(bl_mass ~ treatment, data = comm_biom_ed)
blb_r_m1 <- lm(bl_mass^(1/3) ~ treatment, data = comm_biom_ed)
Anova(blb_r_m1)
plot_diag(blb_r_m1)




# above/below ratios ------------------------------------------------------


# . rain x herb -----------------------------------------------------------

create_trans_boxplot(ab_ratio ~ treatment * herb, data = combio_rxh)
abr_rxh_m1 <- lm(log(ab_ratio) ~ treatment * herb, data = combio_rxh)
Anova(abr_rxh_m1, test.statistic = "F")
plot_diag(abr_rxh_m1)
### no herb effect




# . rain ------------------------------------------------------------------

## the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(ab_ratio ~ treatment, data = comm_biom_ed)
abr_r_m1 <- lm(log(ab_ratio) ~ treatment, data = comm_biom_ed)
Anova(abr_r_m1, test.statistic = "F")
plot_diag(abr_r_m1)




# bl_10_20_ratio ----------------------------------------------------------


# . rain x herb -----------------------------------------------------------
create_trans_boxplot(bl_10_20_ratio ~ treatment * herb, data = combio_rxh)
bl12r_rxh_m1 <- lm(log(bl_10_20_ratio) ~ treatment * herb, data = combio_rxh)
Anova(bl12r_rxh_m1)
plot_diag(bl12r_rxh_m1)




# . rain ------------------------------------------------------------------

## the above analysis showed no herb effect, so use the complete dataset
create_trans_boxplot(bl_10_20_ratio ~ treatment, data = comm_biom_ed)
bl12r_r_m1 <- lm(log(bl_10_20_ratio) ~ treatment, data = comm_biom_ed)
Anova(bl12r_r_m1)
plot_diag(bl12r_r_m1)




# figure ------------------------------------------------------------------


## summary df
names(comm_biom_ed)
smmry_comm_biom_ttl   <- comm_biom_ed %>% 
  mutate(ab_mass = ab_mass / 1000,                                   # unit mg- > g
         bl_mass_0_10  = -bl_mass_0_10 / 1000,                       # plot downwards; unit mg- > g
         bl_mass_10_20 = -bl_mass_10_20 / 1000) %>%                  # plot downwards; unit mg- > g
  gather(variable, value, ab_mass, bl_mass_0_10, bl_mass_10_20) %>%  # reshape to a long format
  group_by(treatment, variable) %>%                                  # summarise by group
  summarise_each(funs(M = mean, SE = se, N = get_n), value) %>%      # get mean, SE and n
  ungroup() %>% 
  droplevels()

ttl_ab  <-  filter(smmry_comm_biom_ttl, variable == "ab_mass")                          # df for positive values (abvoe)
ttl_bl1 <-  filter(smmry_comm_biom_ttl, variable == "bl_mass_0_10")                     # df for negative vluaes (below)
ttl_bl2 <-  filter(smmry_comm_biom_ttl, variable == "bl_mass_10_20") %>%                # df for 10-20cm, mean is correct, but SE has to be adjusted as the barplot is stacked
  left_join(transmute(ttl_bl1, treatment, bl1 = M), by = "treatment") %>%               # bind with df for 0-10cm
  transmute(treatment, variable, M, N, SEmin = M + bl1 - SE, SEmax = M + bl1 + SE) %>%  # SE has to be drawn from where the box for 10-20cm was stacked under the 0-10cm
  bind_rows(ttl_bl1) %>% 
  mutate(variable = factor(variable, levels = c("bl_mass_0_10", "bl_mass_10_20"))) %>% 
  arrange(variable)                                                                     # order matters when making stacked barplots. Bars are stacked as a variable appears in rows

fig_comm_biom <- ggplot(smmry_comm_biom_ttl, 
                        aes(x = treatment, y = M, fill = variable, col = treatment)) +
  labs(x = NULL, y = expression(Biomass~(g~plot^'-1'))) +         # axis labels
  
  geom_bar(data = ttl_ab , stat = "identity", size = .7) +        # bar plot for above-ground
  geom_bar(data = ttl_bl2, stat = "identity", size = .7) +        # bar plot for below-ground
  geom_errorbar(data = ttl_ab,                                    # error bar for above-ground
                aes(ymin = M - SE, ymax = M + SE), 
                width = .3, col = "black", linetype = "solid") +
  geom_errorbar(data = ttl_bl1,                                   # error bar for below-ground (0-10cm)
                aes(ymin = M - SE, ymax = M + SE),
                width = .3, col = "black", linetype = "solid", 
                position = position_dodge(.5)) +
  geom_errorbar(data = ttl_bl2,                                   # error bar for below-ground (10-20cm)
                aes(ymin = SEmin, ymax = SEmax), 
                width = .3, col = "black", linetype = "solid", 
                position = position_dodge(-.5)) +
  
  
  scale_fill_grey(start = .4, labels = c("Above-ground",          # define colors and legends to fill bar plots for each depths
                                         "Below-ground(0-10cm)",
                                         "Below-ground(10-20cm)")) +
  scale_y_continuous(limits = c(-15, 20),                         # yaxis tick labels
                     breaks = seq(-15, 20, 5), 
                     labels = abs(seq(-15, 20, 5))) +
  scale_x_discrete(labels = gsub(" ", "\n",levels(smmry_comm_biom_ttl$treatment))) + # x axis tick label; replace " " with "\n" so that a new line is added
  scale_color_manual(values = rain_cols, guide = FALSE) +
  science_theme +
  theme(legend.position = c(.25, .95),
        legend.key.width = unit(.2, "inches"))
  
fig_comm_biom
ggsavePP("Output/Figs/fig_comm_biom", fig_comm_biom, width = 5, height = 4)




# summary table -----------------------------------------------------------

# raw 
summary(comm_biom)
comm_biom <- select(comm_biom, 
                    treatment, herb, plot, subplot, side, everything())


# summary by rain x herb
summary(combio_rxh)

names(combio_rxh)
smmry_com_biom_tble_byRxH <- combio_rxh %>%
  group_by(treatment, herb) %>% 
  summarise_each(funs(M = mean, SE = se, N = get_n), -plot) %>% 
  select(treatment, herb, starts_with("total"), starts_with("ab_mass"), 
         starts_with("bl_mass_"), starts_with("bl_mass_0"),
         starts_with("bl_mass_10"), starts_with("ab_ratio"))


# summary by rain fall
summary(comm_biom_ed)

smmry_com_biom_tble_byR <- comm_biom_ed %>% 
  group_by(treatment) %>% 
  summarise_each(funs(M = mean, SE = se, N = get_n), -herb, -plot) %>% 
  select(treatment, starts_with("total"), starts_with("ab_mass"), 
         starts_with("bl_mass_"), starts_with("bl_mass_0"),
         starts_with("bl_mass_10"), starts_with("ab_ratio"))


# save 
all_comm_biom_tbl_l <- list('community_biom_raw'                 = comm_biom,
                            'community_biom_smmry_byRainxHerb' = smmry_com_biom_tble_byRxH,
                            'community_biom_smmry_byRain'      = smmry_com_biom_tble_byR)

# save as csv
l_ply(names(all_comm_biom_tbl_l), function(x){
  fname <- paste0("Output/Tables/summary_csv/", x, ".csv")
  write.csv(all_comm_biom_tbl_l[[x]], fname, row.names = FALSE)
})


# save as excel
get_excel_tble_comm_biom <- function(){
  writeWorksheetToFile(file  = "Output/Tables/summary_community_biom.xlsx",
                       data  = all_comm_biom_tbl_l[1],
                       sheet = names(all_comm_biom_tbl_l)[1])
}




# summary stat table ------------------------------------------------------


# . Rain x Herb -----------------------------------------------------------


### store models in a list
comm_biom_rxh_m_l <- list(ttl_biom = ttlb_rxh_m1,   # total biomass
                          ab_biom  = abb_rxh_m1,    # above biomass
                          bl_biom  = blb_rxh_m1,    # below biomass
                          ab_rt    = abr_rxh_m1,    # above:below ratios
                          bl12_rt  = bl12r_rxh_m1)  # below0-10:below10-20 ratios

### bind anova results into a df                          
rxh_tbl <- ldply(comm_biom_rxh_m_l, function(x){
  tidy(Anova(x, test.statistic = "F")) %>% 
    filter(grepl("herb", term))
})




# . Rain --------------------------------------------------------------------


comm_biom_r_m_l <- list(ttl_biom = ttlb_r_m1,
                       ab_biom  = abb_r_m1,
                       bl_biom  = blb_r_m1,
                       ab_rt    = abr_r_m1,
                       bl12_rt  = bl12r_r_m1)
r_tbl <- ldply(comm_biom_r_m_l, function(x){
  tidy(Anova(x, test.statistic = "F")) %>% 
    filter(term == "treatment")
})





# . merge -----------------------------------------------------------------


summary_stt_tbl <- bind_rows(rxh_tbl, r_tbl) %>% 
  select(.id, term, p.value) %>% 
  mutate(p.value = ifelse(p.value <= .05, 
                          as.character(get_star(p.value, dagger = FALSE)), "ns"),                                               # change pvalues to start marks
         term = recode_factor(term, treatment = "Rain", herb = "Herbivore",   # relabel and reorder factors
                              `treatment:herb` = "RxH")) %>% 
  spread(term, p.value) %>% 
  arrange(gsub(".*_", "", .id))
summary_stt_tbl  
write.csv(summary_stt_tbl, file = "Output/Tables/summary_stat_biomass_allocation.csv")  
  
