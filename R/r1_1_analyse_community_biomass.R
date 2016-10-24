summary(comm_biom)


# prepare df --------------------------------------------------------------
comm_biom_ed  <- comm_biom %>% 
  filter(treatment != "No.shelter") %>%                    # remove No-shelter
  select(-side) %>%                                        # remove unnecessary column
  group_by(treatment, herb, plot) %>%                      # summarise by plot
  summarise_each(funs(mean), -subplot)                     # get plot mean
combio_rxh    <- filter(comm_biom_ed, treatment %in% c("Pulsed.drought", "Ambient", "Drought"))  # test rainfall x herb
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
abr_r_m1 <- lm(log(ab_ratio) ~ treatment * herb, data = comm_biom_ed)
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

