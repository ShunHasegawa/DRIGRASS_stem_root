# check newly generated procced dfs
ls(pattern = "prcssd_root_LSA|prcssd_root_trait_byDC")


# 1) by DC class; raw data ------------------------------------------------
summary(raw_root_LSA)


# 2) by DC class; processed ------------------------------------------------
summary(prcssd_root_LSA5)         


# 3) by plants; row data ------------------------------------------------ 

## This is data for each plant. Root length and surface area are aggregated to 
## get total. Note that diameter class 20 is not included as the volues in this 
## class occasionally odd.

raw_root_LSA_byPlant <- raw_root_LSA %>% 
  filter(dmclass == 19) %>%                                                        # cumulative sum of L and SA (L.cum, SA.cum)
  right_join(raw_root_trait_byDC, 
             by = c("treatment", "herb", "plot", "subplot", "spp")) %>% 
  ungroup() %>% 
  select(treatment, plot, subplot, spp, above_single, below_single, sr_ratio, 
         L.cum, SA.cum, Forks, Tips) %>% 
  rename(total_L = L.cum, total_SA = SA.cum) %>% 
  arrange(spp, plot, subplot)


# 4) by plant; processed ------------------------------------------------

prcssd_root_LSA_byPlant <- prcssd_root_LSA5 %>% 
  filter(dmclass == 19) %>% # cumulative sum of L and SA (L.cum, SA.cum)
  left_join(prcssd_root_trait_byDC1, by = c("treatment", "herb", "plot", "subplot", 
                                            "spp")) %>% 
  ungroup() %>% 
  select(treatment, plot, subplot, spp, above_single, below_single, sr_ratio, 
         L.cum, SA.cum, Forks, Tips) %>% 
  rename(total_L = L.cum, total_SA = SA.cum) %>% 
  arrange(spp, plot, subplot)


# 5) by plot; summary ------------------------------------------------

## The above processed data for each plant is summarised by plots

smmry_root_trait_byPlot <- prcssd_root_LSA_byPlant %>% 
  group_by(treatment, plot, spp) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),
                 sr_ratio, total_L, total_SA, Forks, Tips) %>% 
  select(treatment, plot, sr_ratio, spp,starts_with("total_L"), 
         starts_with("total_SA"), starts_with("Forks"), starts_with("Tips")) %>% 
  arrange(spp, plot)


# 6) by treatment; summary ------------------------------------------------

## The above plat sumamry data is summarised by treatment

smmry_root_trait_byRain <- smmry_root_trait_byPlot %>% 
  group_by(treatment, spp) %>% 
  summarise_each(funs(M  = mean(., na.rm = TRUE),
                      SE = se(., na.rm = TRUE),
                      N  = get_n),
                 sr_ratio, total_L, total_SA, Forks, Tips) %>% 
  select(treatment, spp, starts_with("total_L"), starts_with("total_SA"), 
         starts_with("Forks"), starts_with("Tips")) %>% 
  arrange(spp, treatment)


# 7) by species; summary ------------------------------------------------

## The above processed data for each plant is summarised by species. For sp 
## comparisons, subplot or plot don't matter, so treat all subplots are  treated
## as as an independent

smmry_root_trait_bySpp <- prcssd_root_LSA_byPlant %>% 
  group_by(spp) %>%
  summarise_each(funs(M  = mean(., na.rm = TRUE),
                      SE = se(., na.rm = TRUE),
                      N  = get_n),
                 sr_ratio, total_L, total_SA, Forks, Tips) %>% 
  select(spp, starts_with("total_L"), starts_with("total_SA"), 
         starts_with("Forks"), starts_with("Tips")) %>% 
  arrange(spp)


# 8) by treatment; stats summary ------------------------------------------------

## summary of anova test by Rainfall treatment for each species

smmry_anova_byRain_tbl <- rbind.fill(smmry_rootL_byRain_tbl,         # summary table for root length
                                     smmry_rootSA_byRain_tbl,        # summary table for root surface area
                                     smmry_rootForks_byRain_tbl,     # summary table for root forks
                                     smmry_rootTips_byRain_tbl,      # summary table for root tips
                                     smmry_stm.rtRs_byRain_tbl       # summary table for stem:root ratios
                                     ) %>%  
  filter(dmclass == 19) %>% 
  select(trait, unit, everything(), -dmclass)


# 9) by spp; stats summary ------------------------------------------------

## summary of anova test by species

smmry_anova_bySpp_tbl <- rbind.fill(smmry_rootFork_tbl, 
                                    smmry_rootL_tbl, 
                                    smmry_rootSA_tbl, 
                                    smmry_rootTip_tbl,
                                    smmry_stm.rtR_tbl)



# save --------------------------------------------------------------------

all_tbl_l <- list('root_trait_raw_byDC'           = raw_root_LSA,
                  'root_trait_processed_byDC'     = prcssd_root_LSA5,
                  'root_trait_raw_byPlant'        = raw_root_LSA_byPlant,
                  'root_trait_processed_byPlant'  = prcssd_root_LSA_byPlant,
                  'root_trait_smmry_byPlot'       = smmry_root_trait_byPlot,
                  'root_trait_smmry_byRain'       = smmry_root_trait_byRain,
                  'root_trait_smmry_bySpp'        = smmry_root_trait_bySpp,
                  'root_trait_anova_smmry_byRain' = smmry_anova_byRain_tbl,
                  'root_trait_anova_smmry_bySpp'  = smmry_anova_bySpp_tbl)


# save as csv
l_ply(names(all_tbl_l), function(x){
  fname <- paste0("Output/Tables/summary_csv/", x, ".csv")
  write.csv(all_tbl_l[[x]], fname, row.names = FALSE)
})


# save as excel
get_excel_tble_root_trait <- function(){
  sheet_name <- gsub("root_trait_", "", names(all_tbl_l))
  writeWorksheetToFile(file  = "Output/Tables/summary_root_traits.xlsx",
                       data  = all_tbl_l,
                       sheet = sheet_name)
}
