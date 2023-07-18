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
tmp1_1 <- tmp1; tmp1_2 <- tmp1; tmp1_3 <- tmp1; tmp1_4 <- tmp1; tmp1_5 <- tmp1; tmp1_6 <- tmp1; tmp1_7 <- tmp1;
load('Output/Models/TMBObjects_SurveyOnlyModel.RData')

# Extracting samples for the prevalence
tmp1_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N];                                tmp1_1$type <- 'MMC-nT coverage'
tmp1_2[, paste('samp_', 1:N, sep = '')] <- 0;                                                           tmp1_2$type <- 'MMC-T coverage'
tmp1_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_tmc[,1:N];                                tmp1_3$type <- 'TMC coverage'
tmp1_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N];                                tmp1_4$type <- 'MMC coverage'
tmp1_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_tmc[,1:N];                                tmp1_5$type <- 'TMIC coverage'
tmp1_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N] + fit$sample$cum_inc_tmc[,1:N]; tmp1_6$type <- 'MC coverage'

# Appending things togehter 
tmp1 <- rbind(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6)

# Only keeping relevant columns 
tmp1 <- tmp1[,c('area_id', "year", "age", "population", "type", "model", paste('samp_', 1:N, sep = ''))]

# Removing unecessary datasets 
rm(tmp1_1, tmp1_2, tmp1_3, tmp1_4, tmp1_5, tmp1_6)

# Model with total rate
tmp2 <- read_csv('Output/Predictions/Results_FullModel.csv'); tmp2$model <- 'With program data'
tmp2_1 <- tmp2; tmp2_2 <- tmp2; tmp2_3 <- tmp2; tmp2_4 <- tmp2; tmp2_5 <- tmp2; tmp2_6 <- tmp2; 
# tmp2_7 <- tmp2; tmp2_8 <- tmp2; tmp2_9 <- tmp2; tmp2_10 <- tmp2;
load('Output/Models/TMBObjects_FullModel.RData')

# Extracting samples for the prevalence
tmp2_1[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N];                                  tmp2_1$type <- 'MMC-nT coverage'
tmp2_2[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmct[,1:N];                                 tmp2_2$type <- 'MMC-T coverage'
tmp2_3[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_tmc[,1:N];                                  tmp2_3$type <- 'TMC coverage'
tmp2_4[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N] + fit$sample$cum_inc_mmct[,1:N];  tmp2_4$type <- 'MMC coverage'
tmp2_5[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_tmc[,1:N] + fit$sample$cum_inc_mmct[,1:N];  tmp2_5$type <- 'TMIC coverage'
tmp2_6[, paste('samp_', 1:N, sep = '')] <- fit$sample$cum_inc_mmc[,1:N] + fit$sample$cum_inc_tmc[,1:N] + fit$sample$cum_inc_mmct[,1:N]; tmp2_6$type <- 'MC coverage'

# Appending things togehter 
tmp2 <- rbind(tmp2_1, tmp2_2, tmp2_3, tmp2_4, tmp2_5, tmp2_6)

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
            '0+',    '10+',   '15+',   '15-24', '10-24', '15-29', 
            '10-29', '15-39', '10-39', '15-49', '10-49', '25-49')){
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
results[,paste('samp_',1:N,sep='')] <- results[,paste('samp_',1:N,sep='')] / results[,'population']

####################################
### Getting change in prevalence ###
####################################
# Getting prevalence estimates 
tmp1 <- subset(results, type == 'MMC-nT coverage')
tmp2 <- subset(results, type == 'MMC-T coverage')
tmp3 <- subset(results, type == 'TMC coverage')
tmp4 <- subset(results, type == 'MMC coverage')
tmp5 <- subset(results, type == 'TMIC coverage')
tmp6 <- subset(results, type == 'MC coverage')

# Samples for the change in prevalence from 2010
for (i in c(2006:2007, 2009:2020, 2008)){
  tmp1[which(tmp1$year == i), paste('samp_', 1:N, sep = '')] <- tmp1[which(tmp1$year == i), paste('samp_', 1:N, sep = '')] -
    tmp1[which(tmp1$year == 2008), paste('samp_', 1:N, sep = '')]
  tmp2[which(tmp2$year == i), paste('samp_', 1:N, sep = '')] <- tmp2[which(tmp2$year == i), paste('samp_', 1:N, sep = '')] -
    tmp2[which(tmp2$year == 2008), paste('samp_', 1:N, sep = '')]
  tmp3[which(tmp3$year == i), paste('samp_', 1:N, sep = '')] <- tmp3[which(tmp3$year == i), paste('samp_', 1:N, sep = '')] -
    tmp3[which(tmp3$year == 2008), paste('samp_', 1:N, sep = '')]
  tmp4[which(tmp4$year == i), paste('samp_', 1:N, sep = '')] <- tmp4[which(tmp4$year == i), paste('samp_', 1:N, sep = '')] -
    tmp4[which(tmp4$year == 2008), paste('samp_', 1:N, sep = '')]
  tmp5[which(tmp5$year == i), paste('samp_', 1:N, sep = '')] <- tmp5[which(tmp5$year == i), paste('samp_', 1:N, sep = '')] -
    tmp5[which(tmp5$year == 2008), paste('samp_', 1:N, sep = '')]
  tmp6[which(tmp6$year == i), paste('samp_', 1:N, sep = '')] <- tmp6[which(tmp6$year == i), paste('samp_', 1:N, sep = '')] -
    tmp6[which(tmp6$year == 2008), paste('samp_', 1:N, sep = '')]
}

# Relabelling the results 
tmp1$type <- 'Change in MMC-nT coverage from 2008'
tmp2$type <- 'Change in MMC-T coverage from 2008'
tmp3$type <- 'Change in TMC coverage from 2008'
tmp4$type <- 'Change in MMC coverage from 2008'
tmp5$type <- 'Change in TMIC coverage from 2008'
tmp6$type <- 'Change in MC coverage from 2008'

# Appending together 
results <- rbind(results, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

# Removing unecessary datasets
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6)

############################################
### Getting number of people circumcised ###
############################################
# Getting number of circumcised men
tmp1 <- subset(results, type == 'MMC-nT coverage')
tmp2 <- subset(results, type == 'MMC-T coverage')
tmp3 <- subset(results, type == 'TMC coverage')
tmp4 <- subset(results, type == 'MMC coverage')
tmp5 <- subset(results, type == 'TMIC coverage')
tmp6 <- subset(results, type == 'MC coverage')
tmp7 <- subset(results, type == 'MC coverage')

# Getting circumcised population by type
tmp1[, paste('samp_', 1:N, sep = '')] <- tmp1$population * tmp1[, paste('samp_', 1:N, sep = '')]
tmp2[, paste('samp_', 1:N, sep = '')] <- tmp2$population * tmp2[, paste('samp_', 1:N, sep = '')]
tmp3[, paste('samp_', 1:N, sep = '')] <- tmp3$population * tmp3[, paste('samp_', 1:N, sep = '')]
tmp4[, paste('samp_', 1:N, sep = '')] <- tmp4$population * tmp4[, paste('samp_', 1:N, sep = '')]
tmp5[, paste('samp_', 1:N, sep = '')] <- tmp5$population * tmp5[, paste('samp_', 1:N, sep = '')]
tmp6[, paste('samp_', 1:N, sep = '')] <- tmp6$population * tmp6[, paste('samp_', 1:N, sep = '')]
tmp7[, paste('samp_', 1:N, sep = '')] <- tmp7$population * (1 - tmp7[, paste('samp_', 1:N, sep = '')])

# Relabelling the results 
tmp1$type <- 'Number circumcised (MMC-nT)'
tmp2$type <- 'Number circumcised (MMC-T)'
tmp3$type <- 'Number circumcised (TMC)'
tmp4$type <- 'Number circumcised (MMC)'
tmp5$type <- 'Number circumcised (TMIC)'
tmp6$type <- 'Number circumcised'
tmp7$type <- 'Unmet need'

# Appending together 
results <- rbind(results, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7)

# Removing unecessary datasets
rm(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7)

##########################################
### Summarising and outputting dataset ###
##########################################
# Opening cluster
cl <- makePSOCKcluster(8)
registerDoParallel(cl)
nCluster = detectCores()

# Splitting 
results$Proc <- cut(1:nrow(results), 
                    breaks = quantile(1:nrow(results), 
                                      probs = seq(0,1, length.out = (nCluster + 1))), 
                    labels = 1:nCluster, 
                    include.lowest = TRUE)

# Parallel loop for each 
results <- foreach(i=1:nCluster, .combine=rbind) %dopar% {
  # Only keeping allocated chunk 
  tmp <- subset(results, Proc == i)
  # Summarising predictions
  tmp$mean   <- apply(tmp[,grep('samp_', names(tmp))], 1, function(x) {mean(x, na.rm=TRUE)})
  tmp$median <- apply(tmp[,grep('samp_', names(tmp))], 1, function(x) {median(x, na.rm=TRUE)})
  tmp$lower  <- apply(tmp[,grep('samp_', names(tmp))], 1, function(x) {quantile(x, prob = 0.025, na.rm=TRUE)})
  tmp$upper  <- apply(tmp[,grep('samp_', names(tmp))], 1, function(x) {quantile(x, prob = 0.975, na.rm=TRUE)})
  tmp$sd     <- apply(tmp[,grep('samp_', names(tmp))], 1, function(x) {sd(x, na.rm=TRUE)})
  # Returning object 
  return(tmp)
}

# Closing cluster
stopCluster(cl)

# Removing uncessary columns 
results[,paste('samp_',1:N,sep='')] <- NULL
results[,'Proc'] <- NULL

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
write_csv(results, file = 'Output/Analysis/Results_AgeGroup_Prevalence.csv')

# Clearing Workspace
rm(list = ls())




