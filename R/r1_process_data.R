
## treatment df
treat_d <- read.csv("Data/treatment_code.csv")


# traits by spp -----------------------------------------------------------


# . biomass ---------------------------------------------------------------


spp_biom <- read.csv("Data/spp_above_below_biomass_2015sep.csv") %>%         # read data
  group_by(plot, subplot, spp) %>%                                           # group by species 
  summarise_each(funs(sum(., na.rm = TRUE)), above_community, above_single,  # only anagallis was sorted by live/dead, so just combine dead and live to get total biomass
                 below_single) %>% 
  mutate(sr_ratio = above_single / below_single)                             # shoot/root ratios


# . root traits -------------------------------------------------------------


raw_rt_trait <- read.csv("Data/root_traits_sep2015.csv", stringsAsFactors = FALSE)
head(raw_rt_trait)


## Diameter classes
diameter_class <- strsplit(unique(raw_rt_trait$ClassBoundaries), " ")[[1]]
Nclass         <- unique(raw_rt_trait$NumberOfClasses)
raw_rt_trait[raw_rt_trait$NumberOfClasses == 40, ]  
## why do those measurements have 40 diamter classes instead of 20 as 
## others.....? # ignore for the time being and assume all measurements have
## 20 classes

diameter_classification <- data.frame(DiameterClass = 1:20, 
                                      lower_d       = diameter_class,
                                      upper_d       = c(diameter_class[-1], 100))


## edit column names as they're too long using diameter classes
names(raw_rt_trait)
newnames <- as.vector(t(outer(c("L", "SA", "PA", "V", "T"), 1:20, paste, sep = "_")))   # create new names 
data.frame(o = names(raw_rt_trait)[grep("^X|^[.]", names(raw_rt_trait))], n = newnames) # check if the order is correct
names(raw_rt_trait)[grep("^X|^[.]", names(raw_rt_trait))] <- newnames                   # change names


## rename and modify raw data
root_trait_d <- raw_rt_trait %>% 
  rename(plot       = Plot,
         subplot    = Orientation..left.right.,
         spp        = Species,
         depth      = Depth..cm.,
         total_L    = Length.cm.,                             # total length
         total_SA   = SurfArea.cm2.,                          # total surface area
         total_PA   = ProjArea.cm2.,                          # total projection area
         AvgDiam    = AvgDiam.mm.,                            # average diamter
         LenPerVol  = LenPerVol.cm.m3.,
         RootVolume = RootVolume.cm3.) %>%                    # root volume 
  select(-ClassBoundaries) %>% 
  mutate(subplot = factor(ifelse(subplot == "r", "R", "L")))  # capitalise labels in subplots
root_trait_d$spp[root_trait_d$spp == "Brome"] <- "Bromus"     # correct name


## merge with sp biomass and treatment
root_trait <- select(spp_biom, -above_community, -sr_ratio) %>% 
  right_join(root_trait_d, by = c("plot", "subplot", "spp")) %>% 
  left_join(treat_d, by = "plot") %>% 
  ungroup()




# community biomass -------------------------------------------------------


## below-ground community biomass
bl_biom <- read.csv("Data/below_biomass_perPlot_sep2015.csv",
                    na.strings = c(NA, "", "N/A")) %>% 
  select(-non_attached_root_0_10cm, -non_attached_root_10_20cm) %>%         # only scaled values are needed
  filter(!(is.na(attached_root) & is.na(scaled_non_attached_root_0_10cm) &  # remove rows with all NA
             is.na(scaled_non_attached_root_10_20cm))) %>% 
  transmute(plot, subplot, 
            root_0_10 = rowSums(.[, c("attached_root", "scaled_non_attached_root_0_10cm")], # attached root is collected at 0-10 cm so combine
                               na.rm = TRUE),  
            root_10_20 = scaled_non_attached_root_10_20cm)


## total community biomass
comm_biom <- spp_biom %>% 
  group_by(plot, subplot) %>%                                                 # summarise for each plot and subplot
  summarise_each(funs(sum), above_community, above_single, below_single) %>%  # aggregate all spp
  left_join(bl_biom, by = c("plot", "subplot")) %>%                           # merge with belowgound community biomass
  ungroup() %>% 
  replace(is.na(.), 0) %>%
  transmute(plot, subplot,
            ab_mass        = above_community + above_single,  # above biomss
            bl_mass_0_10   = below_single+ root_0_10,         # below biomass at 0-10 cm
            bl_mass_10_20  = root_10_20,                      # below biomass at 10-20 cm
            bl_mass        = bl_mass_0_10 + bl_mass_10_20,    # total below biomass
            bl_10_20_ratio = bl_mass_0_10 / bl_mass_10_20,    # below biomass 0-10:10-20 ratios
            ab_ratio       = ab_mass / bl_mass,               # above/below ratio
            total          = ab_mass + bl_mass) %>%           # total biomass
  left_join(treat_d, by = "plot")

