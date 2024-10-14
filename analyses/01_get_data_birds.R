
# AVONET
# https://figshare.com/s/b990722d72a26b5bfead

# Open the file
# check the metadata
# check the interesting sheets

# avonet
# avonet <- openxlsx::read.xlsx("data/AVONET Supplementary dataset 1.xlsx", 2)
# saveRDS(avonet, "data/avonet_light.rds")


# amniote
# amniote <- read.csv2("data/Amniote_Database_Aug_2015.csv", sep = ",")
# saveRDS(amniote, "data/amniote_light.rds")


####### Load datasets #######

# Trait datasets

avonet <- readRDS("data/avonet_light.rds")
amniote <- readRDS("data/amniote_light.rds")

summary(avonet)
summary(avonet)

# keep columns of interest
# one taxonomic column to match databases
# one trait (body mass from AVONET, clutch size from Amniote)


# Species groups
# From Marino & Bellard 2023
# https://github.com/claramarino/IAS-T_and_alien_birds/blob/main/Data
status <- readRDS("data/Biogeo_status_birds.rds")

####### Clean datasets ########

# Nice package to handle large datasets
# install.packages(tidyverse)
library(tidyverse)

avo <- avonet %>%
  dplyr::select(Species1, Mass, Order1)

amno <- amniote %>%
  # create a taxonomic column to match with avonet
  dplyr::mutate(Species1=paste(genus, species, sep=" ")) %>%
  # filter only birds
  dplyr::filter(class=="Aves") %>%
  # change "-999" with NA
  dplyr::mutate(clutch_size = if_else(litter_or_clutch_size_n=="-999", NA, litter_or_clutch_size_n)) %>%
  # select columns of interest
  dplyr::select(Species1, clutch_size, order) %>%
  dplyr::mutate(clutch_size = as.numeric(clutch_size))

length(unique(amno$order))

####### Bind datasets ########

# Traits

traits <- left_join(avo, amno)
summary(traits)

sum(is.na(traits$clutch_size))
sum(is.na(traits$order))
nrow(avo) - nrow(amno)

# ~1100 espèces n'ont pas matché...


# Comment accorder les taxonomies ?

# install.packages("rredlist")
# install.packages("taxize")
taxize::synonyms()
rredlist::rl_synonyms() # besoin d'une clé API

# 1. Identifier les espèces qui n'ont pas matché
no_match <- setdiff(amno$Species1, avo$Species1)

# 2. Chercher si elle sont des synonymes
syno = data.frame()
for (i in 10:20){
  syno_i <- taxize::synonyms(no_match[i], db = "itis", rows =  2)[[1]]
  if(class(syno_i) == "data.frame"){
    if(nrow(syno_i)>0){
      syno_i$amno_name = no_match[i]
      syno = bind_rows(syno, syno_i)
    }}
  print(i)
}

# 3. Faire correspondre les synonymes avec la nouvelle base

avo %>% filter(Species1 %in% syno$acc_name)
amno %>% filter(Species1 %in% syno$acc_name)
amno %>% filter(Species1 %in% syno$amno_name)
# on peut gagner des infos pour 4 espèces sur les 10 pour lesquelles on a cherché des synonymes
# mais attention, certaines espèces ont des traits différents 
# mêmes si elles sont considérées comme synonymes... il faut donc trouver des
# manières de ne pas avoir de doublons ni d'incohérences dans les données
# souvent il est recommandé de sélectionner une taxonomie à utiliser



# traits and group

gr_tr <- left_join(status, traits %>% rename(binomial = Species1))

colSums(is.na(gr_tr))/nrow(gr_tr)
# A ce stade, il faudrait 
# - s'assurer d'avoir tous les bons matchs taxonomiques
# - décider de la gestion des NA (suppression des espèces, imputation ?)


# save intermediate dataset for further analyses
saveRDS(gr_tr, "data/traits_groups_clean.rds")

