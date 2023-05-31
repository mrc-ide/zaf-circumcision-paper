#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/Github/zaf-circumcision-paper')

# Loading source code 
source('Code/0_Source.R')

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

##########################################
### Preparing survey data for analysis ###
##########################################
# Reading in Survey data
survey_circumcision <- read_csv("Data/zaf_survey_circumcision.csv", guess_max = 1E5)
survey_individuals <- read_csv("Data/zaf_survey_individuals.csv", guess_max = 1E5)
survey_clusters <- read_csv("Data/zaf_survey_clusters.csv", guess_max = 1E5)

# Merging indidual weights to 
survey_circumcision <- survey_circumcision %>%
  # Merging on individual information to  the circumcision dataset 
  left_join(survey_individuals %>%
              dplyr::select(c(survey_id, cluster_id, individual_id, sex, age, pop_group, language, indweight))) %>%
  # Merging on cluster information to the circumcision dataset 
  left_join(unique(survey_clusters %>%
                     dplyr::select(c(survey_id, cluster_id, area_id = geoloc_area_id)))) %>%
  # Remove thos with missing circumcison status 
  filter(!is.na(circ) & !is.na(age) & !is.na(indweight) & !(circ == 1 & is.na(circ_where) & is.na(circ_who))) %>%
  # Extra variables needed for analysis 
  mutate(
    # Survey year 
    year = as.numeric(substr(survey_id, 4, 7)),
    # Year of Birth (estimated as no DOB filly yet)
    yob = year - age,
    # If circumcision age is greater than the age of the individual set reset circumcision age
    circ_age = ifelse(circ_age > age, NA, circ_age)) 

# Removing unecessary datasest
rm(survey_clusters, survey_individuals)

# Two types of censoring, (1) No circumcisions over 59 
# and (2) No circumcisions in 2017 as low N's
survey_circumcision <- survey_circumcision %>%
  # Censoring indivduals from analysis at 60
  mutate(
    # No circumcision after 59
    circ = ifelse(circ == 1 & !is.na(circ_age) & circ_age > 59, 0, circ),
    # Resetting age at circumcision 
    circ_age = ifelse(circ_age > 59, NA, circ_age),
    # Resetting age for everyone else 
    age = ifelse(age > 59, 59, age),
    # Year of circumcision (or censoring) (again estimated using the age as no date of circumcision)
    yoc = ifelse(!is.na(circ_age), yob + circ_age, yob + age)) %>%
  # Censoring at 2016 
  filter(yob < 2017) %>%
  # Final variables for modelling
  mutate(
    # Censoring circumcison status for those circumcised in 2017, 
    # Assuming the interval censored people were circumcised before 2017
    circ = if_else(yoc == 2017 & circ == 1 & !is.na(circ_age), 0, circ),
    # Censoring year of circumcision (or censor year in 2016) at 2016 
    yoc = if_else(yoc == 2017, 2016, yoc))

# Getting the area level id to province
for (i in 1:3){
  survey_circumcision <- survey_circumcision %>%
    left_join(area_boundaries[,c("area_id","area_level","parent_area_id","space")],
              by = "area_id") %>%
    mutate(area_id = if_else(area_level == 2, as.character(area_id), as.character(parent_area_id))) %>%
    dplyr::select(-c(geometry, parent_area_id, space, area_level))
}

# Adding all survival analysis variables 
survey_circumcision <- survey_circumcision %>%
  # Merging on the region index 
  left_join(area_hierarchy[,c("area_id","area_name",'space')],
            by = "area_id") %>%
  # Variables needed for survival analysis 
  mutate(
    # Event type 
    event = ifelse(circ == 1 & !is.na(circ_age), 1, 
                   ifelse((circ == 1 & is.na(circ_age)), 2, 0)),
    # Time interval for the individual
    time1 = yob - 2006 + 1,
    time2 = yoc - 2006 + 1,
    # Circumcision age 
    circ_age = yoc - yob,
    age = circ_age + 1,
    # Type of circumcision
    type = case_when(circ_who == 'Healthcare worker' & circ_where == 'Medical' ~ 'MMC',
                     circ_who == 'Healthcare worker' & circ_where == 'Traditional' ~ 'MMC',
                     circ_who == 'Traditional practitioner' & circ_where == 'Medical' ~ 'MMC',
                     circ_who == 'Traditional practitioner' & circ_where == 'Traditional' ~ 'TMC',
                     is.na(circ_who) & circ_where == 'Medical' ~ 'MMC',
                     is.na(circ_who) & circ_where == 'Traditional' ~ 'TMC',
                     circ_who == 'Healthcare worker' & is.na(circ_where) ~ 'MMC',
                     circ_who == 'Traditional practitioner' & is.na(circ_where) ~ 'TMC',
                     is.na(circ_who) & is.na(circ_where) ~ 'Missing')) 

# Preparing survey weights for the model 
survey_circumcision <- survey_circumcision %>%
  # Standardising survey weights 
  group_by(survey_id, area_id) %>%
  mutate(indweight_st = indweight / mean(indweight, na.rm=TRUE)) %>%
  ungroup() %>%
  # Applying Kish coefficient to the survey weights 
  left_join(ddply(survey_circumcision,
                  .(survey_id),
                  summarize,
                  N = length(survey_id),
                  Neff = (sum(indweight) ^2)/sum(indweight * indweight),
                  ratio = N/Neff)) %>%
  mutate(indweight_st = indweight_st / ratio)

################################################
### Shell dataset to estimate empirical rate ###
################################################
# Skeleton dataset 
out <- data.frame(expand.grid(year = seq(2006, 2021, by =  1),
                              circ_age = (min(survey_circumcision$circ_age):max(survey_circumcision$circ_age)),
                              area_id = sort(unique(survey_circumcision$area_id)))) %>%
  # Getting time and age variable 
  mutate(time = year - min(year) + 1,
         age  = circ_age + 1,
         age_tmp1 = if_else(age <= 10, age, 10),
         age_tmp2 = if_else(age >= 11, age, 11),
         age_flag = if_else(age <= 10, 1, 0)) %>%
  # Merging on the region index 
  left_join(area_hierarchy %>% 
              dplyr::select(c(area_id, area_name, space)),
            by = "area_id")  %>%
  # Sorting dataset 
  arrange(space, age, time)

# Obtaining the number of person years
out$N <- (survey_circumcision$indweight_st * create.integration.matrix.agetime(dat = survey_circumcision,
                                                                               time1 = 'time1',
                                                                               time2 = 'time2',
                                                                               strat = 'space',
                                                                               age = 'age',
                                                                               Ntime = length(unique(out$time))))%>%
  colSums()

# Obtaining the number of Medical anf Traditional circumcisions
out$obs_mmc <- create.hazard.matrix.agetime(dat = survey_circumcision, 
                                            subset = "event == 1 & type == 'MMC'",
                                            time1  = 'time1',
                                            time2  = 'time2',
                                            strat = 'space',
                                            age    = 'age',
                                            circ   = 'indweight_st',
                                            Ntime = length(unique(out$time))) %>% 
  colSums()

out$obs_tmc <- create.hazard.matrix.agetime(dat = survey_circumcision, 
                                             subset = "event == 1 & type == 'TMC'",
                                             time1  = 'time1',
                                             time2  = 'time2',
                                             strat = 'space',
                                             age    = 'age',
                                             circ   = 'indweight_st',
                                             Ntime = length(unique(out$time))) %>% 
  colSums()

# Obtaining the number of censored (did not get circumcised)
out$cens <- create.hazard.matrix.agetime(dat = survey_circumcision, 
                                         subset = "event == 0",
                                         time1 = 'time1',
                                         time2 = 'time2',
                                         strat = 'space',
                                         age = 'age',
                                         circ = 'indweight_st',
                                         Ntime = length(unique(out$time)))%>%
  colSums()

# Obtaining the number of left-censored (circumcised at unknown age)
out$icens <- create.hazard.matrix.agetime(dat = survey_circumcision, 
                                          subset = "event == 2",
                                          time1 = 'time1',
                                          time2 = 'time2',
                                          strat = 'space',
                                          age = 'age',
                                          circ = 'indweight_st',
                                          Ntime = length(unique(out$time)))%>%
  colSums()

##############################################
### Preparing population data for analysis ###
##############################################
# Read in population dataset
pop <- read_csv("Data/zaf_population_district_singleage.csv")

# Subsetting for popoulation of interest
pop <- pop %>%
  # Only keeping men
  filter(sex == 'male') %>%
  # Getting year 
  mutate(year = as.numeric(substr(calendar_quarter, 3, 6))) %>%
  dplyr::select(c(area_id, year, circ_age = age, population)) 

# Merging on population
out <- out %>%
  left_join(pop,
            by = c('area_id', 'circ_age', 'year'))

#######################
### Design matrices ###
#######################
k_dt <- 5 ## knot spacing

# Design matrix for the fixed effects (Traditional)
X_fixed_tmc <- sparse.model.matrix(N ~ 1, data = out)

# Design matrix for the spatial random effects (Traditional)
X_space_tmc <- sparse.model.matrix(N ~ -1 + as.factor(space), data = out)

# Design matrix for the age random effects (Traditional)
k_age <- k_dt * (floor(min(out$age) / k_dt) - 3L):(ceiling(max(out$age) / k_dt) + 3L)
X_age_tmc <- splines::splineDesign(k_age, out$age, outer.ok=TRUE)
X_age_tmc <- as(X_age_tmc, "sparseMatrix")

# Design matrix for the age-space random effects (Medical, adolescent/adult)
X_agespace_tmc <- mgcv::tensor.prod.model.matrix(list(X_space_tmc, X_age_tmc))

# Design matrix for the fixed effects (Medical, youth)
X_fixed_mmc_y <- sparse.model.matrix(N ~ -1 + age_flag, data = out)

# Design matrix for the spatial random effects (Medical, youth)
X_space_mmc_y <- model.matrix(N ~ -1 + as.factor(space), data = out)
X_space_mmc_y <- out$age_flag * X_space_mmc_y
X_space_mmc_y <- as(X_space_mmc_y, "sparseMatrix")

# Design matrix for the age random effects (Medical, youth)
k_age <- k_dt * (floor(min(out$age_tmp1) / k_dt) - 3L):(ceiling(max(out$age_tmp1) / k_dt) + 3L)
X_age_mmc_y <- splines::splineDesign(k_age, out$age_tmp1, outer.ok=TRUE)
X_age_mmc_y <- out$age_flag * X_age_mmc_y
X_age_mmc_y <- as(X_age_mmc_y, "sparseMatrix")

# Design matrix for the age-space random effects (Medical, adolescent/adult)
X_agespace_mmc_y <- mgcv::tensor.prod.model.matrix(list(X_space_mmc_y, X_age_mmc_y))

# Design matrix for the fixed effects (Medical, adolescent/adult)
X_fixed_mmc_a <- sparse.model.matrix(N ~ -1 + I(1 - age_flag), data = out)

# Design matrix for the stratification random effects (Medical, adolescent/adult)
X_space_mmc_a <- model.matrix(N ~ -1 + as.factor(space), data = out)
X_space_mmc_a <- (1 - out$age_flag) * X_space_mmc_a
X_space_mmc_a <- as(X_space_mmc_a, "sparseMatrix")

# Design matrix for the age random effects (Medical, adolescent/adult)
k_age <- k_dt * (floor(min(out$age_tmp2) / k_dt) - 3L):(ceiling(max(out$age_tmp2) / k_dt) + 3L)
X_age_mmc_a <- splines::splineDesign(k_age, out$age_tmp2, outer.ok=TRUE)
X_age_mmc_a <- (1 - out$age_flag) * X_age_mmc_a
X_age_mmc_a <- as(X_age_mmc_a, "sparseMatrix")

# Design matrix for the temporal random effects(Medical, adolescent/adult)
X_time_mmc_a <- model.matrix(N ~ -1 + as.factor(time), data = out)
X_time_mmc_a <- (1 - out$age_flag) * X_time_mmc_a
X_time_mmc_a <- as(X_time_mmc_a, "sparseMatrix")

# Design matrix for the age-space random effects (Medical, adolescent/adult)
X_agespace_mmc_a <- mgcv::tensor.prod.model.matrix(list(X_space_mmc_a, X_age_mmc_a))

# Design matrix for the age-time random effects (Medical, adolescent/adult)
X_agetime_mmc_a <- mgcv::tensor.prod.model.matrix(list(X_time_mmc_a, X_age_mmc_a))

# Design matrix for the age-time random effects (Medical, adolescent/adult)
X_spacetime_mmc_a <- model.matrix(N ~ -1 + factor(group_indices(.data = out, space, time)), data = out)
X_spacetime_mmc_a <- (1 - out$age_flag) * X_spacetime_mmc_a
X_spacetime_mmc_a <- as(X_spacetime_mmc_a, "sparseMatrix")

#########################
### Survival matrices ###
#########################
# Variables needed for matrices 
out$time1 <- out$time - out$circ_age
out$time2 <- out$time

# Matrix for selecting instantaneous hazard rate for medical circumcision rate 
A_mmc <- create.hazard.matrix.agetime(dat = out, 
                                      time1 = 'time1',
                                      time2 = 'time2',
                                      age = 'age',
                                      strat = 'space',
                                      circ = 'obs_mmc',
                                      Ntime = length(unique(out$time)))

# Matrix for selecting instantaneous hazard rate for traditional circumcision  
A_tmc <- create.hazard.matrix.agetime(dat = out, 
                                       time1 = 'time1',
                                       time2 = 'time2',
                                       age = 'age',
                                       strat = 'space',
                                       circ = 'obs_tmc',
                                       Ntime = length(unique(out$time)))

# Matrix for selecting instantaneous hazard rate for no circumcision 
B <- create.hazard.matrix.agetime(dat = out, 
                                  time1 = 'time1',
                                  time2 = 'time2',
                                  age = 'age',
                                  strat = 'space',
                                  circ = 'cens',
                                  Ntime = length(unique(out$time)))

# Matrix for selecting instantaneous hazard rate for left-censored
C <- create.hazard.matrix.agetime(dat = out, 
                                  time1 = 'time1',
                                  time2 = 'time2',
                                  age = 'age',
                                  strat = 'space',
                                  circ = 'icens',
                                  Ntime = length(unique(out$time)))

# Removing unecessary columns 
out$time1 <- NULL
out$time2 <- NULL

##########################
### Integration matrix ###
##########################
# Prepping dataset 
tmp <- out
tmp$time1 <- tmp$time - tmp$circ_age
tmp$time2 <- tmp$time
tmp$age <- tmp$circ_age + 1

# Integration matrix for cumulative hazard
IntMat1 <- create.integration.matrix.agetime(dat = tmp, 
                                             time1 = 'time1',
                                             time2 = 'time2',
                                             strat = 'space',
                                             age = 'age',
                                             Ntime = length(unique(out$time)))

# Integration matrix for lagged cumulative hazard
IntMat2 <- create.integration.matrix.agetime.lag(dat = tmp, 
                                                 time1 = 'time1',
                                                 time2 = 'time2',
                                                 strat = 'space',
                                                 age = 'age',
                                                 Ntime = length(unique(out$time)))

# Removing unecessary datasets
rm(tmp)

##########################
### Precision matrices ###
##########################
# Precision matrix for the spatial random effects 
Q_space <- create.icar.prec.matrix(sf_obj = subset(area_boundaries, area_level == 2), row.names = 'space')

###########################################
### Preparing program data for analysis ###
###########################################
# DMPPT2 input data prior to 2013
dat_program1 <- read.csv('Data/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2009:2012 & 
           age_group == '10+' & 
           type == 'DMPPT2' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# DHIS input data from 2013-2017 for all districts
dat_program2 <- read.csv('Data/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2013:2017 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) &
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# DHIS input data for 2018-2020 for all non-PEPFAR EC districts
dat_program3 <- read.csv('Data/zaf_circumcision_program_data.csv') %>%
  filter(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')) &
           area_level == 2 & 
           year %in% 2018:2020 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# PEPFAR input data for 2018-2020 for all PEPFAR EC districts with sharing 
dat_program4 <- read.csv('Data/prop1_4.csv') %>%
  dplyr::rename(mmc_performed = test) %>%
  dplyr::select(-c(name_district)) %>%
  filter(mmc_performed > 0 & type == 'prop4') %>%
  left_join(area_hierarchy[,c('area_id', 'area_level')], by = 'area_id') %>%
  mutate(type = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), 'Reallocated PEPFAR', 'Reported PEPFAR'),
         # age_group = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), '10+', age_group),
         age_group = '10+', 
         age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# Appending together 
dat_program <- rbind(dat_program1, dat_program2, dat_program3, dat_program4) %>%
  ddply(.(area_id, year, age_group, area_name, parent_area_id, area_level, age1, age2),
        summarise,
        mmc_performed = sum(mmc_performed))

# Removing unecessary datasets
rm(dat_program1, dat_program2, dat_program3, dat_program4)

# Aggregation matrix for the Poisson likelihood, add program data 
AggMat <- matrix(0, ncol = nrow(out), nrow = nrow(dat_program)) 

# Loop for each observation of program data 
for (i in 1:nrow(AggMat)){
  AggMat[i, which(out$year == dat_program$year[i] & 
                    out$area_id == dat_program$area_id[i] & 
                    out$circ_age >= dat_program$age1[i]& 
                    out$circ_age <= dat_program$age2[i])] <- 
    out$population[which(out$year == dat_program$year[i] & 
                           out$area_id == dat_program$area_id[i] & 
                           out$circ_age >= dat_program$age1[i]& 
                           out$circ_age <= dat_program$age2[i])]
}

# Converting to sparseMatrix 
AggMat <- as(AggMat, 'sparseMatrix')

##############################
### TMI crossover products ###
##############################
# Getting shell for the borrowing probabilities and relevant priors
probs <- subset(out, (area_id == 'ZAF_2_DC31' & year %in% 2013:2019) |
                  (area_id %in% subset(area_hierarchy, parent_area_id %in% c('ZAF_1_MP', 'ZAF_1_LIM'))$area_id & year %in% 2015:2019) |
                  (area_id %in% c('ZAF_2_BUF', 'ZAF_2_CPT', 'ZAF_2_DC10', 'ZAF_2_DC12', 'ZAF_2_DC13', 'ZAF_2_DC14', 'ZAF_2_DC15', 'ZAF_2_DC44', 'ZAF_2_NMA') & year == 2018:2019))%>%
  dplyr::select(c(area_id, area_name, year)) %>%
  unique() %>%
  dplyr::mutate(prob = seq(dplyr::n()))

# Setting priors
probs$probs_pmn <- as.numeric(NA)
probs$probs_pmn[probs$year == 2015] <- logit(0.2)
probs$probs_pmn[probs$year == 2016] <- logit(0.4)
probs$probs_pmn[probs$year == 2017] <- logit(0.6)
probs$probs_pmn[probs$year == 2018] <- logit(0.8)
probs$probs_pmn[probs$year == 2019] <- logit(0.9)
probs$probs_pmn[probs$area_name == 'Nkangala DM'] <- logit(0.9)
probs$probs_psd = 0.2
probs$probs_pmn[probs$area_id %in% c('ZAF_2_BUF', 'ZAF_2_CPT', 'ZAF_2_DC10', 'ZAF_2_DC12', 'ZAF_2_DC13', 'ZAF_2_DC14', 'ZAF_2_DC15', 'ZAF_2_DC44', 'ZAF_2_NMA') & probs$year == 2018] <- logit(0.99)
probs$probs_pmn[probs$area_id %in% c('ZAF_2_BUF', 'ZAF_2_CPT', 'ZAF_2_DC10', 'ZAF_2_DC12', 'ZAF_2_DC13', 'ZAF_2_DC14', 'ZAF_2_DC15', 'ZAF_2_DC44', 'ZAF_2_NMA') & probs$year == 2019] <- logit(0.99)
probs$probs_psd[probs$area_id %in% c('ZAF_2_BUF', 'ZAF_2_CPT', 'ZAF_2_DC10', 'ZAF_2_DC12', 'ZAF_2_DC13', 'ZAF_2_DC14', 'ZAF_2_DC15', 'ZAF_2_DC44', 'ZAF_2_NMA') & probs$year == 2018] <- 0.2
probs$probs_psd[probs$area_id %in% c('ZAF_2_BUF', 'ZAF_2_CPT', 'ZAF_2_DC10', 'ZAF_2_DC12', 'ZAF_2_DC13', 'ZAF_2_DC14', 'ZAF_2_DC15', 'ZAF_2_DC44', 'ZAF_2_NMA') & probs$year == 2019] <- 0.2

# Merging probability shell to output dataset
out <- out %>%
  left_join(probs[,c('area_id', 'area_name', 'year', 'prob')],
            by = c("area_id", 'area_name', 'year'))

# Filling in missings
out$prob[is.na(out$prob)] <- 0

# Design matrix for the stratification random effects
X_prob <- sparse.model.matrix(N ~ -1 + as.factor(prob), data = out)[,-1]

#############################
### Dataset for modelling ###
#############################
# Data for tmb model
dat_tmb <- list(
  # Design matrices 
  X_fixed_tmc       = X_fixed_tmc,
  X_space_tmc       = X_space_tmc,
  X_age_tmc         = X_age_tmc,
  X_agespace_tmc    = X_agespace_tmc,
  X_fixed_mmc_y     = X_fixed_mmc_y,
  X_space_mmc_y     = X_space_mmc_y,
  X_age_mmc_y       = X_age_mmc_y,
  X_agespace_mmc_y  = X_agespace_mmc_y,
  X_fixed_mmc_a     = X_fixed_mmc_a,
  X_space_mmc_a     = X_space_mmc_a,
  X_age_mmc_a       = X_age_mmc_a,
  X_time_mmc_a      = X_time_mmc_a,
  X_agespace_mmc_a  = X_agespace_mmc_a,
  X_agetime_mmc_a   = X_agetime_mmc_a,
  X_spacetime_mmc_a = X_spacetime_mmc_a,
  # Survival analysis matrices 
  A_mmc             = A_mmc,
  A_tmc             = A_tmc,
  B                 = B,
  C                 = C, 
  IntMat1           = IntMat1,
  IntMat2           = IntMat2,
  # Precision matrices 
  Q_space           = Q_space,
  # Program data 
  Agg               = AggMat, 
  NCirc             = dat_program$mmc_performed,
  # Borrowing matrices and priors
  X_prob            = X_prob, 
  probs_pmn         = probs$probs_pmn,
  probs_psd         = probs$probs_psd
)

###########################################
### Modelling circumcision probabilites ###
###########################################
# Compiling TMB code
compile("Code/TMBModels/FullModel.cpp")
dyn.load(dynlib("Code/TMBModels/FullModel"))

# Intial values
parameters <- list(u_fixed_tmc              = rep(-5, ncol(X_fixed_tmc)),
                   u_fixed_mmc_y            = rep(-5, ncol(X_fixed_mmc_y)),
                   u_fixed_mmc_a            = rep(-5, ncol(X_fixed_mmc_a)),
                   u_age_tmc                = rep(0, ncol(X_age_tmc)),
                   u_age_mmc_y              = rep(0, ncol(X_age_mmc_y)),
                   u_age_mmc_a              = rep(0, ncol(X_age_mmc_a)),
                   u_time_mmc_a             = rep(0, ncol(X_time_mmc_a)),
                   u_space_tmc              = rep(0, ncol(X_space_tmc)),
                   u_space_mmc_y            = rep(0, ncol(X_space_mmc_y)),
                   u_space_mmc_a            = rep(0, ncol(X_space_mmc_a)),
                   u_agespace_tmc           = matrix(0, ncol(X_age_tmc), ncol(X_space_tmc)),
                   u_agespace_mmc_y         = matrix(0, ncol(X_age_mmc_y), ncol(X_space_mmc_y)),
                   u_agespace_mmc_a         = matrix(0, ncol(X_age_mmc_a), ncol(X_space_mmc_a)),
                   u_agetime_mmc_a          = matrix(0, ncol(X_age_mmc_a), ncol(X_time_mmc_a)),
                   u_spacetime_mmc_a        = matrix(0, ncol(X_time_mmc_a), ncol(X_space_mmc_a)),
                   u_probs                  = probs$probs_pmn,
                   logsigma_age_tmc         = 0,
                   logsigma_space_tmc       = 0,
                   logsigma_agespace_tmc    = 0,
                   logsigma_age_mmc_y       = 0,
                   logsigma_space_mmc_y     = 0,
                   logsigma_agespace_mmc_y  = 0,
                   logsigma_age_mmc_a       = 0,
                   logsigma_time_mmc_a      = 0,
                   logsigma_space_mmc_a     = 0,
                   logsigma_agetime_mmc_a   = 0,
                   logsigma_agespace_mmc_a  = 0,
                   logsigma_spacetime_mmc_a = 0,
                   logitrho_tmc_age1        = 2,
                   logitrho_tmc_age2        = 2,
                   logitrho_mmc_y_age1      = 2,
                   logitrho_mmc_y_age2      = 2,
                   logitrho_mmc_a_time1     = 2,
                   logitrho_mmc_a_time2     = 2,
                   logitrho_mmc_a_time3     = 2,
                   logitrho_mmc_a_age1      = 2,
                   logitrho_mmc_a_age2      = 2,
                   logitrho_mmc_a_age3      = 2)

# Creating TMB object
obj <- MakeADFun(dat_tmb,
                 parameters,
                 random = c('u_age_tmc', 'u_age_mmc_y', 'u_age_mmc_a', 'u_time_mmc_a',
                            'u_space_tmc', 'u_space_mmc_y', 'u_space_mmc_a', 'u_agespace_tmc',
                            'u_agespace_mmc_y', 'u_agespace_mmc_a', 'u_agetime_mmc_a', 'u_spacetime_mmc_a', 'u_probs'),
                 method = "L-BFGS-B",
                 hessian = TRUE,
                 DLL = "FullModel")

# Running optimiser
opt <- do.call(optim, obj)

# Getting the TMB into "NAOMI" format to sample from using the NAOMI package
opt$par.fixed <- opt$par
opt$par.full <- obj$env$last.par
fit <- c(opt, obj = list(obj))
class(fit) <- "naomi_fit"  

# Look at standard deviation report
fit$sdreport <- sdreport(fit$obj, fit$par, getJointPrecision = TRUE)

# Generating samples 
fit <- sample_tmb(fit)

# Getting median + CI for probabilites, incidence and cumulative incidence 
out[,c('rate_mmcM','rate_mmcL','rate_mmcU')] <- t(apply(fit$sample$haz_mmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('rate_tmcM','rate_tmcL','rate_tmcU')] <- t(apply(fit$sample$haz_tmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('rateM','rateL','rateU')] <- t(apply(fit$sample$haz_mmc + fit$sample$haz_tmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('survM','survL','survU')] <- t(apply(fit$sample$surv, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('inc_tmcM','inc_tmcL','inc_tmcU')] <- t(apply(fit$sample$inc_tmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('inc_mmcM','inc_mmcL','inc_mmcU')] <- t(apply(fit$sample$inc_mmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('inc_mmctM','inc_mmctL','inc_mmctU')] <- t(apply(fit$sample$inc_mmct, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('incM','incL','incU')] <- t(apply(fit$sample$inc_tmc + fit$sample$inc_mmc + fit$sample$inc_mmct, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('cum_inc_tmcM','cum_inc_tmcL','cum_inc_tmcU')] <- t(apply(fit$sample$cum_inc_tmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('cum_inc_mmcM','cum_inc_mmcL','cum_inc_mmcU')] <- t(apply(fit$sample$cum_inc_mmc, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('cum_inc_mmctM','cum_inc_mmctL','cum_inc_mmctU')] <- t(apply(fit$sample$cum_inc_mmct, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))
out[,c('cum_incM','cum_incL','cum_incU')] <- t(apply(fit$sample$cum_inc_tmc + fit$sample$cum_inc_mmc + fit$sample$cum_inc_mmct, 1, function(x) quantile(x, probs = c(0.5, 0.025, 0.975))))

######################
### Saving results ###
######################
# Preparing for output 
out <- out %>%
  # Selecting variables 
  dplyr::select(
    # Keep stratum
    area_id, area_name, year, age = circ_age, population,
    # Keep empirical estimates
    obs_mmc, obs_tmc, cens, icens, N,  
    # Keep rate
    rate_mmcM, rate_mmcL, rate_mmcU, 
    rate_tmcM, rate_tmcL, rate_tmcU, 
    rateM, rateL, rateU, 
    # Keep survivor function
    survM, survL, survU, 
    # Keep incidence
    inc_tmcM, inc_tmcL, inc_tmcU, 
    inc_mmcM, inc_mmcL, inc_mmcU, 
    inc_mmctM, inc_mmctL, inc_mmctU, 
    incM, incL, incU, 
    # Keep Cumulative indicence 
    cum_inc_tmcM, cum_inc_tmcL, cum_inc_tmcU, 
    cum_inc_mmcM, cum_inc_mmcL, cum_inc_mmcU, 
    cum_inc_mmctM, cum_inc_mmctL, cum_inc_mmctU, 
    cum_incM, cum_incL, cum_incU)

# Saving results 
write_csv(out, file = 'Output/Predictions/Results_FullModel.csv')
save(fit, file = 'Output/Models/TMBObjects_FullModel.RData')

# Clearing Workspace
rm(list = ls())


