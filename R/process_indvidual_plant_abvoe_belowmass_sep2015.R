library(car)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

d <- read.csv("Data/indvidual_plant_abvoe_belowmass_sep2015.csv",
              skip = 2, header = FALSE)
head(d)
cols <- read.csv("Data/indvidual_plant_abvoe_belowmass_sep2015.csv", header = FALSE,
                 nrow = 2)

col2 <- as.character(apply(cols, 2, function(x) paste(x[1], x[2], sep = "_")))
col2[1:2] <- c("plot", "subplot")
names(d) <- col2
d_ed <- d %>% 
  gather(variable, value, -plot, -subplot) %>% 
  mutate(plot = as.numeric(as.character(plot)),
         spp     = tstrsplit(variable, "_")[[1]],
         measure = tstrsplit(variable, "_")[[2]],
         type    = ifelse(grepl("alive", measure), "live",
                          ifelse(grepl("dead", measure), "dead", "total")),
         part    = ifelse(grepl("Community", measure), "above_community",
                          ifelse(grepl("[aA]bove", measure), "above_single",
                                 "below_single"))) %>% 
  select(plot, subplot, spp, type, part, value) %>% 
  spread(part, value) %>% 
  arrange(spp, plot, subplot)
write.csv(d_ed, "Data/spp_above_below_biomass_2015sep.csv", row.names = FALSE)

d_ed[is.na(d_ed)] <- 0
total_d <- d_ed %>% 
  mutate(above_total = above_community + above_single) %>% 
  group_by(plot) %>% 
  summarise(above_torl = sum(above_total)) %>% 
  ungroup() %>% 
  arrange(as.numeric(plot))
total_d

summary(total_d)

