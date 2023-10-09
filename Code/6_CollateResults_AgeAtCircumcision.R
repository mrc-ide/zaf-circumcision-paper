#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/zaf-circumcision-rates')

# Loading source code 
source('Code/0_Source.R')

# Number of samples to use 
N <- 100

################################################
### Preparing location/shapefile information ###
################################################
# Loading shapefiles 
area_hierarchy <- read.csv("~/Dropbox/Github/zaf-subnational-hiv/data/zaf_area_hierarchy.csv")
area_boundaries <- read_sf("~/Dropbox/Github/zaf-subnational-hiv/data/zaf_area_boundaries.geojson")

# Adding a unique identifier within Admin code and merging to boundaries
area_hierarchy <- area_hierarchy %>%
  group_by(area_level) %>%
  mutate(space = seq(dplyr::n())) %>%
  ungroup()

# Adding a unique identifier within Admin code and merging to boundaries
area_boundaries <- area_hierarchy %>%
  group_by(area_level) %>%
  mutate(space = seq(dplyr::n())) %>%
  left_join(x =  area_boundaries,
            by = 'area_id') %>%
  ungroup()

#########################################
### Loading rates from survival model ###
#########################################
# Model with total rate
tmp1 <- read_csv('Output/Predictions/Results_DistrictAgeTime_ByType.csv'); tmp1$model <- 'No program data'
tmp1_1 <- tmp1; tmp1_2 <- tmp1; tmp1_3 <- tmp1; tmp1_4 <- tmp1; tmp1_5 <- tmp1; tmp1_6 <- tmp1; 
load('Output/Models/TMBObjects_DistrictAgeTime_ByType.RData')

# Extracting samples for the incidence rate
tmp1_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                            tmp1_1$type <- 'Average age at circumcision (MMC-nT)'
tmp1_2[, paste('samp_', 1:N, sep = '')] <- 0;                                                   tmp1_2$type <- 'Average age at circumcision (MMC-T)'
tmp1_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                            tmp1_3$type <- 'Average age at circumcision (TMC)'
tmp1_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                            tmp1_4$type <- 'Average age at circumcision (MMC)'
tmp1_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                            tmp1_5$type <- 'Average age at circumcision (TMIC)'
tmp1_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_tmc[,1:N]; tmp1_6$type <- 'Average age at circumcision (MC)'

# Appending things togehter 
tmp1 <- rbind(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6)

# Only keeping relevant columns 
tmp1 <- tmp1[,c('area_id', "year", "age", "population", "type", "model", paste('samp_', 1:N, sep = ''))]

# Removing unecessary datasets 
rm(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6)

# Model with total rate
tmp2 <- read_csv('Output/Predictions/Results_DistrictAgeTime_ByType_withProgram_withBorrowing.csv'); tmp2$model <- 'With program data'
tmp2_1 <- tmp2; tmp2_2 <- tmp2; tmp2_3 <- tmp2; tmp2_4 <- tmp2; tmp2_5 <- tmp2; tmp2_6 <- tmp2; 
load('Output/Models/TMBObjects_DistrictAgeTime_ByType_withProgram_withBorrowing.RData')

# Extracting samples for the incidence rate
tmp2_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                             tmp2_1$type <- 'Average age at circumcision (MMC-nTs)'
tmp2_2[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmct[,1:N];                            tmp2_2$type <- 'Average age at circumcision (MMC-T)'
tmp2_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                             tmp2_3$type <- 'Average age at circumcision (TMC)'
tmp2_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_4$type <- 'Average age at circumcision (MMC)'
tmp2_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_5$type <- 'Average age at circumcision (TMIC)'
tmp2_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_tmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_6$type <- 'Average age at circumcision (MC)'

# Appending things togehter 
tmp2 <- rbind(tmp2_1, tmp2_2, tmp2_3, tmp2_4, tmp2_5, tmp2_6)

# Only keeping relevant columns 
tmp2 <- tmp2[,c('area_id', "year", "age", "population", "type", "model", paste('samp_', 1:N, sep = ''))]

# Removing unecessary datasets 
rm(tmp2_1, tmp2_2, tmp2_3, tmp2_4, tmp2_5, tmp2_6)

# Appending things together 
results <- rbind(tmp1, tmp2)

# REmovign unecessary datasets 
rm(tmp1, tmp2, fit)

########################
### District results ###
########################
# Appending together 
results1 <- results %>% 
  # Adding parent area id 
  left_join(area_hierarchy[,c('area_id', 'area_name')],
            by = 'area_id')

# Multiplying by population to population weight
results1[, paste('samp_',1:N,sep='')] <- results1[, paste('samp_',1:N,sep='')] * results1$population

########################
### Province results ###
########################
# Setting labels for province
results2 <- results %>%
  # Adding parent area id
  left_join(area_hierarchy[,c('area_id', 'parent_area_id')],
            by = 'area_id') %>%
  # Overiding district with province
  dplyr::select(-c(area_id)) %>%
  dplyr::rename(area_id = parent_area_id) %>%
  left_join(area_hierarchy[,c('area_id', 'area_name')],
            by = 'area_id')

# Multiplying by population to population weight
results2[,paste('samp_',1:N,sep='')] <- results2[,paste('samp_',1:N,sep='')] * results2$population

# Getting summarising samples 
results2 <- aggregate(data = results2,
                      . ~ area_id + area_name + year + age + model + type,
                      FUN = sum)

########################
### National results ###
########################
# Setting labels for national
results3 <- results %>% 
  mutate(area_id = 'ZAF') %>% 
  # Adding parent area id 
  left_join(area_hierarchy[,c('area_id', 'area_name')],
            by = 'area_id') 

# Multiplying by population to population weight
results3[,paste('samp_',1:N,sep='')] <- results3[,paste('samp_',1:N,sep='')] * results3$population

# Getting summarising samples 
results3 <- aggregate(data = results3,
                      . ~ area_id + area_name + year + age + model + type,
                      FUN = sum)

####################################
### Preparing results for output ###
####################################
# Appending together 
results <- rbind(results1, results2, results3)

# Removing unecessary datasets 
rm(results1, results2, results3)

# Summmarising to get the average age at circumcision
results <- results %>%
  # Grouping 
  group_by(area_id, area_name, year, type, model) %>%
  # Summarising
  summarise(across(samp_1:samp_100, function(x) weighted.mean(x = age, w = x, na.rm=TRUE))) 

# Getting medians and CIs
results$mean <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){mean(x, na.rm = TRUE)})
results$sd <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){sd(x, na.rm = TRUE)})
results$median <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.5), na.rm = TRUE)})
results$lower  <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.025), na.rm = TRUE)})
results$upper  <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.975), na.rm = TRUE)})

# Removing uncessary columns 
results[,paste('samp_',1:N,sep='')] <- NULL

# Merging regional information on the dataset 
results <- results %>%
  # Adding region information 
  left_join(area_hierarchy[,c('area_id', 'area_name', 'area_level', 'parent_area_id')],
            by = c('area_id', 'area_name')) %>%
  # Adding region information 
  left_join(area_hierarchy %>%
              dplyr::select(c(parent_area_id = area_id, parent_area_name = area_name)),
            by = 'parent_area_id')

####################
### Saving files ###
####################
# Saving files 
write_csv(results, file = 'Output/Analysis/Results_AverageAgeAtCircumcision.csv')

# Clearing Workspace
rm(list = ls())












