
names(root_trait)

# prepare df --------------------------------------------------------------


raw_root_trait_byDC <- root_trait %>% 
  select(treatment, herb, plot, subplot, spp, ends_with("single"), AvgDiam, Tips, 
         Forks, starts_with("L"), starts_with("SA"), -LenPerVol) %>% 
  gather(variable, value, -treatment, -herb,-plot, -subplot, -spp, 
         -ends_with("single")) %>% 
  mutate(value = value / below_single) %>%                                  # standerdise by root mass
  spread(variable, value) %>% 
  mutate(sr_ratio = above_single / below_single)  # shoot : root ratios


## length and surface area for different diameter classes
raw_root_LSA <- raw_root_trait_byDC %>% 
  select(treatment, herb, plot, subplot, spp, starts_with("L"),                 # get required columns
         starts_with("SA")) %>%  
  gather(variable, value, -plot, -subplot, -treatment, -herb, -spp) %>%         # rehape to long format
  mutate(measure = tstrsplit(variable, split =  "_")[[1]],                      # get measurement type (L or SA)
         dmclass = as.numeric(tstrsplit(variable, split =  "_")[[2]])) %>%      # get diameter class (1-20)
  select(-variable) %>%                                                         # remove unnecessary column
  spread(measure, value) %>%                                                    # reshape back to wider format with L and SA as columns
  group_by(plot, subplot, treatment, herb, spp) %>%                             # get cummulative sum for each group
  arrange(dmclass) %>%                                                          # make sure the rows ordered by diameter class before getting cumulative sum
  mutate(L.cum  = cumsum(L),                                                    # get cumulative sum for length
         SA.cum = cumsum(SA)) %>%                                               # get cumulative sum for SA
  arrange(plot, subplot, spp, dmclass) %>% 
  droplevels(.)


## remove diameter class 20 as it's not very reliable
prcssd_root_LSA1 <- filter(raw_root_LSA, dmclass != 20)




# analysis ----------------------------------------------------------------

source("R/r1_2.1_root_length.R")       # root length
source("R/r1_2.2_root_SA.R")           # root surface area
source("R/r1_2.3_root_forks.R")        # root forks
source("R/r1_2.4_root_tips.R")         # root tips
source("R/r1_2.5_stem_root_ratio.R")   # shoot:root ratios

