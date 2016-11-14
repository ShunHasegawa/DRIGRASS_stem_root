summary(comm_biom)


# prepare df --------------------------------------------------------------
comm_biom_ed  <- comm_biom %>% 
  filter(treatment != "No.shelter") %>%                    # remove No-shelter
  select(-side) %>%                                        # remove unnecessary column
  group_by(treatment, herb, plot) %>%                      # summarise by plot
  summarise_each(funs(mean), -subplot)                     # get plot mean
combio_rxh    <- filter(comm_biom_ed, treatment %in% c("Reduced.frequency", "Ambient", "Reduced"))  # test rainfall x herb
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
  ungroup()

ttl_ab  <-  filter(smmry_comm_biom_ttl, variable == "ab_mass")                          # df for positive values (abvoe)
ttl_bl1 <-  filter(smmry_comm_biom_ttl, variable == "bl_mass_0_10")                     # df for negative vluaes (below)
ttl_bl2 <-  filter(smmry_comm_biom_ttl, variable == "bl_mass_10_20") %>%                # df for 10-20cm, mean is correct, but SE has to be adjusted as the barplot is stacked
  left_join(transmute(ttl_bl1, treatment, bl1 = M), by = "treatment") %>%               # bind with df for 0-10cm
  transmute(treatment, variable, M, N, SEmin = M + bl1 - SE, SEmax = M + bl1 + SE) %>%  # SE has to be drawn from where the box for 10-20cm was stacked under the 0-10cm
  bind_rows(ttl_bl1) %>% 
  mutate(variable = factor(variable, levels = c("bl_mass_0_10", "bl_mass_10_20"))) %>% 
  arrange(variable)                                                                     # order matters when making stacked barplots. Bars are stacked as a variable appears in rows

fig_comm_biom <- ggplot(smmry_comm_biom_ttl, aes(x = treatment, y = M, fill = variable, col = treatment)) +
  labs(x = NULL, y = expression(Biomass~(g~plot^'-1'))) +
  geom_bar(data = ttl_ab , stat = "identity") +
  geom_bar(data = ttl_bl2, stat = "identity") +
  geom_errorbar(data = ttl_ab, 
                aes(ymin = M - SE, ymax = M + SE), 
                width = .3, col = "black", linetype = "solid") +
  geom_errorbar(data = ttl_bl1, 
                aes(ymin = M - SE, ymax = M + SE),
                width = .3, col = "black", linetype = "solid", 
                position = position_dodge(.5)) +
  geom_errorbar(data = ttl_bl2, 
                aes(ymin = SEmin, ymax = SEmax), 
                width = .3, col = "black", linetype = "solid", 
                position = position_dodge(-.5)) +
  scale_fill_grey(start = .4) +
  science_theme +
  theme(legend.position = "right",
        legend.key.width = unit(.2, "inches"))
fig_comm_biom
ggsavePP("Output/Figs/fig_comm_biom", fig_comm_biom, width = 5, height = 3)




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
