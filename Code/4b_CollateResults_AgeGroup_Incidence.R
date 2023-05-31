#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/Github/zaf-circumcision-paper')

# Loading source code 
source('Code/0_Source.R')

# Number of samples to use 
N <- 100

################################################
### Preparing location/shapefile information ###
################################################
# Loading shapefiles and area hierarchy 
area_hierarchy <- read.csv("Data/zaf_area_hierarchy.csv")
area_boundaries <- read_sf("Data/zaf_area_boundaries.geojson")

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
tmp1 <- read_csv('Output/Predictions/Results_SurveyOnlyModel.csv'); tmp1$model <- 'No program data'
tmp1_1 <- tmp1; tmp1_2 <- tmp1; tmp1_3 <- tmp1; tmp1_4 <- tmp1; tmp1_5 <- tmp1; tmp1_6 <- tmp1; 
tmp1_7 <- tmp1; tmp1_8 <- tmp1; tmp1_9 <- tmp1; tmp1_10 <- tmp1; tmp1_11 <- tmp1; tmp1_12 <- tmp1;
load('Output/Models/TMBObjects_SurveyOnlyModel.RData')

# Extracting samples for the incidence rate
tmp1_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                            tmp1_1$type <- 'MMC-nT incidence'
tmp1_2[, paste('samp_', 1:N, sep = '')] <- 0;                                                   tmp1_2$type <- 'MMC-T incidence'
tmp1_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                            tmp1_3$type <- 'TMC incidence'
tmp1_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                            tmp1_4$type <- 'MMC incidence'
tmp1_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                            tmp1_5$type <- 'TMIC incidence'
tmp1_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_tmc[,1:N]; tmp1_6$type <- 'MC incidence'

# Samples for the number of MCs performed
tmp1_7 <- tmp1_1;  tmp1_7$type <- 'MMC-nTs performed'
tmp1_8 <- tmp1_2;  tmp1_8$type <- 'MMC-Ts performed'
tmp1_9 <- tmp1_3;  tmp1_9$type <- 'TMCs performed'
tmp1_10 <- tmp1_4; tmp1_10$type <- 'MMCs performed'
tmp1_11 <- tmp1_5; tmp1_11$type <- 'TMICs performed'
tmp1_12 <- tmp1_6; tmp1_12$type <- 'MCs performed'

# Appending things togehter 
tmp1 <- rbind(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6, 
	          tmp1_7, tmp1_8, tmp1_9, tmp1_10, tmp1_11, tmp1_12)

# Only keeping relevant columns 
tmp1 <- tmp1[,c('area_id', "year", "age", "population", "type", "model", paste('samp_', 1:N, sep = ''))]

# Removing unecessary datasets 
rm(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6, 
   tmp1_7, tmp1_8, tmp1_9, tmp1_10, tmp1_11, tmp1_12)

# Model with total rate
tmp2 <- read_csv('Output/Predictions/Results_FullModel.csv'); tmp2$model <- 'With program data'
tmp2_1 <- tmp2; tmp2_2 <- tmp2; tmp2_3 <- tmp2; tmp2_4 <- tmp2; tmp2_5 <- tmp2; tmp2_6 <- tmp2; 
tmp2_7 <- tmp2; tmp2_8 <- tmp2; tmp2_9 <- tmp2; tmp2_10 <- tmp2; tmp2_11 <- tmp2; tmp2_12 <- tmp2;
load('Output/Models/TMBObjects_FullModel.RData')

# Extracting samples for the incidence rate
tmp2_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N];                             tmp2_1$type <- 'MMC-nT incidence'
tmp2_2[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmct[,1:N];                            tmp2_2$type <- 'MMC-T incidence'
tmp2_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N];                             tmp2_3$type <- 'TMC incidence'
tmp2_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_4$type <- 'MMC incidence'
tmp2_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_tmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_5$type <- 'TMIC incidence'
tmp2_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$inc_mmc[,1:N] + fit$sample$inc_tmc[,1:N] + fit$sample$inc_mmct[,1:N]; tmp2_6$type <- 'MC incidence'

# Samples for the number of MCs performed
tmp2_7  <- tmp2_1; tmp2_7$type <- 'MMC-nTs performed'
tmp2_8  <- tmp2_2; tmp2_8$type <- 'MMC-Ts performed'
tmp2_9  <- tmp2_3; tmp2_9$type <- 'TMCs performed'
tmp2_10 <- tmp2_4; tmp2_10$type <- 'MMCs performed'
tmp2_11 <- tmp2_5; tmp2_11$type <- 'TMICs performed'
tmp2_12 <- tmp2_6; tmp2_12$type <- 'MCs performed'

# Appending things togehter 
tmp2 <- rbind(tmp2_1, tmp2_2, tmp2_3, tmp2_4, tmp2_5,
              tmp2_6, tmp2_7, tmp2_8, tmp2_9, tmp2_10,
              tmp2_11, tmp2_12)

# Only keeping relevant columns 
tmp2 <- tmp2[,c('area_id', "year", "age", "population", "type", "model", paste('samp_', 1:N, sep = ''))]

# Removing unecessary datasets 
rm(tmp2_1, tmp2_2, tmp2_3, tmp2_4, tmp2_5, tmp2_6, 
   tmp2_7, tmp2_8, tmp2_9, tmp2_10, tmp2_11, tmp2_12)

# Appending things together 
results <- rbind(tmp1, tmp2)

# REmovign unecessary datasets 
rm(tmp1, tmp2, fit)

###########################
### Aggregating results ###
###########################
# Appending together 
results1 <- results %>% 
  # Adding parent area id 
  left_join(area_hierarchy[,c('area_id', 'area_name')],
            by = 'area_id')

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

# Setting labels for national
results3 <- results %>% 
  mutate(area_id = 'ZAF') %>% 
  # Adding parent area id 
  left_join(area_hierarchy[,c('area_id', 'area_name')],
            by = 'area_id')

# Multiplying by population to population weight
results1[,paste('samp_',1:N,sep='')] <- results1[,paste('samp_',1:N,sep='')] * results1$population
results2[,paste('samp_',1:N,sep='')] <- results2[,paste('samp_',1:N,sep='')] * results2$population
results3[,paste('samp_',1:N,sep='')] <- results3[,paste('samp_',1:N,sep='')] * results3$population

# Empty file for output results
results <- NULL

# Looping for each age group
for (i in c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29',
            '30-34', '35-39', '40-44', '45-49', '50-54', '54-59',
            '0+',    '10+',   '15+',   '15-24', '10-24', 
			'15-29', '10-29', '15-39', '10-39', '15-49', '10-49')){
  # If upper limit use this split
  if (grepl('-', i) == TRUE) {
    age1 <- as.numeric(strsplit(i, '-')[[1]][1])
    age2 <- as.numeric(strsplit(i, '-')[[1]][2])
  }
  # If no upper limit use this split
  if (grepl('\\+', i) == TRUE) {
    age1 <-  as.numeric(strsplit(i, '\\+')[[1]][1])
    age2 <- Inf
  }
  # Getting summarising samples
  tmp1 <- aggregate(data = subset(results1, age >= age1 & age <= age2)[,!(names(results1) %in% c('age'))],
                    . ~ area_id + area_name + year + model + type,
                    FUN = sum)
  tmp2 <- aggregate(data = subset(results2, age >= age1 & age <= age2)[,!(names(results2) %in% c('age'))],
                    . ~ area_id + area_name + year + model + type,
                    FUN = sum)
  tmp3 <- aggregate(data = subset(results3, age >= age1 & age <= age2)[,!(names(results3) %in% c('age'))],
                    . ~ area_id + area_name + year + model + type,
                    FUN = sum)
  # Adding age group
  tmp1$age_group <- i
  tmp2$age_group <- i
  tmp3$age_group <- i
  # Appending together
  results <- rbind(results, tmp1, tmp2, tmp3)
  # Removing unecessary datasets
  rm(tmp1, tmp2, tmp3)
  # Printing index
  print(i)
}

# REmoving unecessary columns
rm(results1, results2, results3)

# Multiplying by population to population weight
results[-which(results$type %in% c('MCs performed', 'MMC-Ts performed', 'MMC-nTs performed', 'TMCs performed',
                                   'TMICs performed', 'MMCs performed', 'MMCs performed', 'TMICs performed')),
        paste('samp_',1:N,sep='')] <-
  results[-which(results$type %in% c('MCs performed', 'MMC-Ts performed', 'MMC-nTs performed', 'TMCs performed',
                                     'TMICs performed', 'MMCs performed', 'MMCs performed', 'TMICs performed')),
          paste('samp_',1:N,sep='')] / 
  results[-which(results$type %in% c('MCs performed', 'MMC-Ts performed', 'MMC-nTs performed', 'TMCs performed',
                                     'TMICs performed', 'MMCs performed', 'MMCs performed', 'TMICs performed')),'population']

# Getting medians and CIs
results$mean <- apply(results[,paste('samp_',1:N,sep='')], 1, mean)
results$sd <- apply(results[,paste('samp_',1:N,sep='')], 1, sd)
results$median <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.5))})
results$lower  <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.025))})
results$upper  <- apply(results[,paste('samp_',1:N,sep='')], 1, function(x){quantile(x, probs = c(0.975))})

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

# Ordering area names 
tmp <- subset(area_hierarchy, area_level <= 2) %>%
  arrange(parent_area_id, area_name)
results$area_name <- factor(results$area_name, levels = tmp$area_name)

####################
### Saving files ###
####################
# Saving files 
write_csv(results, file = 'Output/Analysis/Results_AgeGroup_Incidence.csv')

# Clearing Workspace
rm(list = ls())




