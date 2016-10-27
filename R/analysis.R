rm(list = ls(all = TRUE))
source("R/packages.R")
source("R/functions.R")

## process data
source("R/r1_process_data.R")
summary(comm_biom)                # community above/belowground biomass
summary(root_trait)               # root traits (length, surface area etc.) with associated shoot and root mass
summary(diameter_classification)  # diameter classification
summary(spp_biom)                 # shoot and root biomass by spp


## analysis
source("R/r1_1_analyse_community_biomass.R")       # analysis on community biomass
source("R/r1_2_analyse_root_traits.R")             # analysis on root traits
source("R/r1_2_1_create_smmry_tbl_root_traits.R")  # create summary table (.csv) based on the result of anslysis for root tratis
## run the lines below if one wants to save the sumamry table as an excel. This may take 
## some time. If Java error occurred, restart R session and try again
# get_excel_tble_comm_biom()
# get_excel_tble_root_trait()


# save all objects
save.image("Output/Data/all_obj.RData")
