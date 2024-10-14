rm(list=ls())
library(tidyverse)

###### Load dataset for analyses ######
gr_tr <- readRDS("data/traits_groups_clean.rds")

####### Explore relationships ########

# Taxonomic bias

colSums(is.na(gr_tr))/nrow(gr_tr)

ord <- gr_tr %>%
  group_by(Order1) %>%
  summarize(na_clutch = sum(is.na(clutch_size)),
            n_tot = n()) %>%
  mutate(prop = na_clutch/n_tot)

ggplot(ord, aes(x=reorder(Order1, -n_tot), y = prop))+
  geom_bar(stat = "identity") +
  geom_text(aes(y = 1.05, label = n_tot))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



# Group differences

ggplot(gr_tr)+
  geom_point(aes(x=biogeo_status, y = clutch_size), position = "jitter", alpha = .5, size=2, color = "cyan4")+
  geom_boxplot(aes(x=biogeo_status, y = clutch_size), fill = NA, outlier.shape = NA)+
  theme_bw()
ggplot(gr_tr)+
  geom_point(aes(x=biogeo_status, y = log(clutch_size)), position = "jitter", alpha = .5, size=2, color = "cyan4")+
  geom_boxplot(aes(x=biogeo_status, y = log(clutch_size)), fill = NA, outlier.shape = NA)+
  theme_bw()

ggplot(gr_tr)+
  geom_point(aes(x=biogeo_status, y = log(Mass)), position = "jitter", alpha = .5, size=2, color = "firebrick")+
  geom_boxplot(aes(x=biogeo_status, y = log(Mass)), fill = NA, outlier.shape = NA)+
  theme_bw()


ggplot(gr_tr)+
  geom_point(aes(x=log(Mass), y=log(clutch_size)), size = 2)+
  theme_bw()

ggplot(gr_tr)+
  geom_point(aes(x=log(Mass), y=log(clutch_size), color = biogeo_status), size = 2)+
  theme_bw()

ggplot(gr_tr)+
  geom_histogram(aes(x=log(Mass), fill = biogeo_status))+
  theme_bw()

ggplot(gr_tr)+
  geom_histogram(aes(x=log(clutch_size), fill = biogeo_status))+
  theme_bw()

