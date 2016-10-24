raw_rt_trait <- read.csv("Data/root_traits_sep2015.csv", stringsAsFactors = FALSE)
head(raw_rt_trait)
  
# Clean up data-----------------------------------------------------------------
  
  
  # Diameter classes
  diameter_class <- strsplit(unique(raw_rt_trait$ClassBoundaries), " ")[[1]]
  Nclass <- unique(raw_rt_trait$NumberOfClasses)
  raw_rt_trait[raw_rt_trait$NumberOfClasses == 40, ]  
    # why do those measurements have 40 diamter classes instead of 20 as others.....?
    # ignore for the time being and assume all measurements have 20 classes
  Nclass <- 20
  
  
  diameter_classification <- data.frame(DiameterClass = 1:Nclass, 
                                        lower_d       = diameter_class,
                                        upper_d       = c(diameter_class[-1], 100))
  
  newnames <- as.vector(t(outer(c("L", "SA", "PA", "V", "T"), 1:Nclass, paste, sep = "_")))
  data.frame(o = names(raw_rt_trait)[grep("^X|^[.]", names(raw_rt_trait))], n = newnames)
  names(raw_rt_trait)[grep("^X|^[.]", names(raw_rt_trait))] <- newnames
  
  gsub("[.]cm.", "",names(raw_rt_trait))
  names(raw_rt_trait_ed)
  raw_rt_trait_ed <- raw_rt_trait %>% 
    rename(plot       = Plot,
           subplot    = Orientation..left.right.,
           spp        = Species,
           depth      = Depth..cm.,
           total_L    = Length.cm.,
           total_SA   = SurfArea.cm2.,
           total_PA   = ProjArea.cm2.,
           AvgDiam    = AvgDiam.mm.,
           LenPerVol  = LenPerVol.cm.m3.,
           RootVolume = RootVolume.cm3.) %>% 
    select(-ClassBoundaries) %>% 
    mutate(subplot = factor(ifelse(subplot == "r", "R", "L")))
  
 
# # Organise data-----------------------------------------------------------------
#   raw_rt_trait_mlt <- melt(raw_rt_trait, id = "sample_id")
#   # Diameter classes
#   dcdd <- ldply(strsplit(as.character(raw_rt_trait_mlt$variable), "_"))
#   colnames(dcdd) <- c("measurement", "DiameterClass")
#   raw_rt_trait_mlt      <- cbind(raw_rt_trait_mlt, dcdd)
#   raw_rt_trait_mlt      <- merge(raw_rt_trait_mlt, diameter_classification, 
#                           by    = "DiameterClass", 
#                           all.x = TRUE)
#   
#   # Combine with table for treatment applications
#   treatdd <- read.csv("Data/sample_id.csv")
#   
#   root_str_dd <- merge(raw_rt_trait_mlt, treatdd, 
#                        by    = "sample_id", 
#                        all.x = TRUE)
#   root_str_dd$DiameterClass <- factor(root_str_dd$DiameterClass,
#                                       levels = 1:20)
#   
#   # remove roots > 3mm diamter (diameter class > 16)
#   root_str_dd <- subsetD(root_str_dd, 
#                          !DiameterClass %in% c(16:20))
#   
#   # reshape data  frame
#   root_dd     <- dcast(sample_id + DiameterClass + lower_d + upper_d + species + co2 + water + block ~ measurement, 
#                        value.var = "value", 
#                        data      = root_str_dd) 
#   root_dd <- within(root_dd, {
#     lower_d <- as.numeric(as.character(lower_d))
#     upper_d <- as.numeric(as.character(upper_d))
#   })
# 
#   # save
#   save(root_dd, file = "Output/Data/root_dd.RData")
#   write.csv(root_dd, 
#             file      = "Output/Table/processed_WinRhizo_result.csv", 
#             row.names = FALSE)
#   
#   # aggregate for each diameter class from the smallest
#   root_CumSum_dd <- ldply(1:15, function(x) {
#     # subset data for all diameter classes smaller than x
#     d <- subset(root_dd, as.numeric(as.character(DiameterClass)) <= x)
#     
#     # add up all diameter classes win a subset for each sample_id
#     dd <- ddply(d, .(sample_id), summarise, 
#                 L  = sum(L), 
#                 SA = sum(SA))
#     dd$DiameterClass <- paste0("DC", x)
#     return(dd)
#     })
# 
#   root_CumSum_dd_mlt <- melt(root_CumSum_dd, id.vars = c("sample_id", "DiameterClass"))
#   root_CumSum_dd_cst <- dcast(root_CumSum_dd_mlt, 
#                               sample_id ~ variable + DiameterClass)
#   head(root_CumSum_dd_cst)
#   
#   save(root_CumSum_dd_cst, file = "Output/Data/root_CumSum_dd_cst.RData")
#   
#   # create metadata
#   md <- ddply(unique(root_dd[, c("DiameterClass", "lower_d", "upper_d")]), 
#              .(DiameterClass),
#              function(x){
#                variable    <- c(paste0("L_DC", x[1]), paste0("SA_DC", x[1]))
#                description <- c(paste("Total length of roots (<=", x[3], "mm)", "(mm)"), 
#                                 paste("Total surface area of roots <=", x[3], "mm", "(mm-2)"))
#                return(data.frame(variable, description))
#                })
#   md$filename <- "root_CumSum_dd_cst.RData"
#   md <- md[order(as.character(md$variable)), -1]
#   write.csv(md, file = "Output/root_MD.csv", row.names = FALSE)
#   
# # Figure-----------------------------------------------------------------------  
#   p <- ggplot(subset(root_dd, L != 0),
#               aes(x   = factor(lower_d + 0.1), 
#                   y   = log10(L), 
#                   col = co2))
#   p2 <- p + 
#     geom_boxplot(size = 0.3) + 
#     labs(x = "Diamter (mm)", 
#          y = expression(log[10](Root~length(mm)))) +
#     theme(axis.text.x = element_text(size = 5)) +
#     facet_grid(species ~ water)
#   
#   ggsavePP(p2, 
#          filename = "Output/Figs/root_length_for_each_diameter_classes",
#          width    = 6, 
#          height   = 6)
