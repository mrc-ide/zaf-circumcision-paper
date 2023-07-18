#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd("~/Dropbox/zaf-circumcision-rates/Transfer/20230606")

# Loading package
library(dplyr)
library(readr)
library(tidyr)
require(reshape2)
require(plyr)

#################################################
### Load models and save sample probabilities ###
#################################################
#' Loading in survey only model
load("TMBObjects_DistrictAgeTime_ByType.RData")
fit_so <- fit

#' Loading in full model
load("TMBObjects_DistrictAgeTime_ByType_withProgram_withBorrowing.RData")
fit_full <- fit

# Removing unecessary objects
rm(fit)

# Extracting samples of probabilities from both models
mc_prop_so <- 1.0 - fit_so$sample$surv
tmc_prop_so <- fit_so$sample$cum_inc_tmc
mmc_prop_so <- fit_so$sample$cum_inc_mmc
mc_prop_full <- 1.0 - fit_full$sample$surv
tmc_prop_full <- fit_full$sample$cum_inc_tmc
mmc_prop_full <- fit_full$sample$cum_inc_mmc

#' Model output frame dataset
out_so <- read_csv("Results_DistrictAgeTime_ByType.csv")
out_full <- read_csv("Results_DistrictAgeTime_ByType_withProgram_withBorrowing.csv")

#' Assign row index to output frame -> identify rows in samples
out_idx <- out_so %>%
  dplyr::select(area_id, year, age) %>%
  dplyr::mutate(idx = row_number())

###############################
### Prepare survey data set ###
###############################
# Loading in area_hierarchy
areas <- read_csv("zaf_area_hierarchy.csv")

# Loading in survey datasets
survey_circumcision <- read_csv("zaf_survey_circumcision.csv", guess_max = 1E5)
survey_individuals <- read_csv("zaf_survey_individuals.csv", guess_max = 1E5)
survey_clusters <- read_csv("zaf_survey_clusters.csv", guess_max = 1E5)

# Merging all together
survey_obs <- survey_circumcision %>%
  left_join(
    survey_individuals %>%
      dplyr::select(survey_id, cluster_id, individual_id, sex, age, indweight)
  ) %>%
  left_join(
    survey_clusters %>%
      dplyr::select(c(survey_id, cluster_id, area_id = geoloc_area_id))
  )

# Remove if missing circumcision status, age, or weight
survey_obs <- survey_obs %>%
  filter(!is.na(circ),
         !is.na(age),
         !is.na(indweight),
         ! (circ == 1 & is.na(circ_where) & is.na(circ_who)))

# Assign survey year
survey_obs <- survey_obs %>%
  mutate(
    year = as.numeric(substr(survey_id, 4, 7))
  )

# Reassign 2002 survey to year 2006, which is the first year of output
survey_obs <- survey_obs %>%
  mutate(
    year = if_else(survey_id == "ZAF2002HSRC", 2006, year)
  )

# Subset to age <60
survey_obs <- survey_obs %>%
  filter(age < 60)

# Assign circumcision type
survey_obs <- survey_obs %>%
  mutate(
    type = case_when(circ_who == 'Healthcare worker' & circ_where == 'Medical' ~ 'MMC',
                     circ_who == 'Healthcare worker' & circ_where == 'Traditional' ~ 'MMC',
                     circ_who == 'Traditional practitioner' & circ_where == 'Medical' ~ 'MMC',
                     circ_who == 'Traditional practitioner' & circ_where == 'Traditional' ~ 'TMC',
                     is.na(circ_who) & circ_where == 'Medical' ~ 'MMC',
                     is.na(circ_who) & circ_where == 'Traditional' ~ 'TMC',
                     circ_who == 'Healthcare worker' & is.na(circ_where) ~ 'MMC',
                     circ_who == 'Traditional practitioner' & is.na(circ_where) ~ 'TMC',
                     is.na(circ_who) & is.na(circ_where) ~ 'Missing'))

# Roll-up area_id to district (level 2)
for(i in 1:3) {
  survey_obs <- survey_obs %>%
    left_join(
      areas %>%
        dplyr::select(area_id, area_level, parent_area_id),
      by = "area_id"
    ) %>%
    mutate(
      area_id = if_else(area_level == 2, as.character(area_id), as.character(parent_area_id))
    ) %>%
    dplyr::select(-parent_area_id, -area_level)
}

#' Assign output frame idx
survey_obs <- survey_obs %>%
  left_join(out_idx, by = c("area_id", "year", "age"))

stopifnot(!is.na(survey_obs$idx))

##############################################################
### Simulate observations from the predictive distribution ###
##############################################################
# Get posterior probability of being circumcised (any type) for each observation
survey_mc_prop_so <- mc_prop_so[survey_obs$idx, ]
survey_mc_prop_full <- mc_prop_full[survey_obs$idx, ]

# Simulate circumcised / uncircumcised as binomial 0/1
# Reshape as matrix
survey_sim_mc_so <- rbinom(length(survey_mc_prop_so), 1, survey_mc_prop_so)
survey_sim_mc_so <- matrix(survey_sim_mc_so, nrow = nrow(survey_mc_prop_so))
survey_sim_mc_full <- rbinom(length(survey_mc_prop_full), 1, survey_mc_prop_full)
survey_sim_mc_full <- matrix(survey_sim_mc_full, nrow = nrow(survey_mc_prop_full))

# Get probability of being MMC given circumcised (MMC or TMC)
survey_mmctype_prop_so <- mmc_prop_so[survey_obs$idx, ] / (tmc_prop_so[survey_obs$idx, ] + mmc_prop_so[survey_obs$idx, ])
survey_mmctype_prop_full <- mmc_prop_full[survey_obs$idx, ] / (tmc_prop_full[survey_obs$idx, ] + mmc_prop_full[survey_obs$idx, ])

# Simulate whether MMC _if_ they were circumcised
# NOTE: This is a bit inefficient because it samples for _all_ respondents
#       rather than only those who are circumcised.
survey_sim_mmctype_so <- rbinom(length(survey_mmctype_prop_so), 1, survey_mmctype_prop_so)
survey_sim_mmctype_so <- matrix(survey_sim_mmctype_so, nrow = nrow(survey_mmctype_prop_so))
survey_sim_mmctype_full <- rbinom(length(survey_mmctype_prop_full), 1, survey_mmctype_prop_full)
survey_sim_mmctype_full <- matrix(survey_sim_mmctype_full, nrow = nrow(survey_mmctype_prop_full))

# Construct matrices for simulated MMC and TMC
survey_sim_mmc_so <- survey_sim_mc_so == 1 & survey_sim_mmctype_so == 1
survey_sim_mmc_so[] <- as.integer(survey_sim_mmc_so)
survey_sim_mmc_full <- survey_sim_mc_full == 1 & survey_sim_mmctype_full == 1
survey_sim_mmc_full[] <- as.integer(survey_sim_mmc_full)
survey_sim_tmc_so <- survey_sim_mc_so == 1 & survey_sim_mmctype_so == 0
survey_sim_tmc_so[] <- as.integer(survey_sim_tmc_so)
survey_sim_tmc_full <- survey_sim_mc_full == 1 & survey_sim_mmctype_full == 0
survey_sim_tmc_full[] <- as.integer(survey_sim_tmc_full)

# Construct data frame with all outputs for observed and simulated
# MC, MMC, and TMC
survey_sim_obs <- survey_obs %>%
  left_join(
    dplyr::select(areas, area_id, province_area_id = parent_area_id)
  ) %>%
  left_join(
    dplyr::select(areas, province_area_id = area_id, province_name = area_name)
  ) %>%
  transmute(
    survey_id,
    individual_id,
    area_id,
    province_area_id,
    province_name,
    sex,
    age,
    year,
    indweight,
    idx,
    mc_obs = circ,
    mmc_obs = as.integer(circ == 1 & type == "MMC"),
    tmc_obs = as.integer(circ == 1 & type == "TMC"),
  )

# Altering column names
colnames(survey_sim_mc_so) <- sprintf("mc_sim%04d", 1:ncol(survey_sim_mc_so))
colnames(survey_sim_mmc_so) <- sprintf("mmc_sim%04d", 1:ncol(survey_sim_mmc_so))
colnames(survey_sim_tmc_so) <- sprintf("tmc_sim%04d", 1:ncol(survey_sim_tmc_so))
colnames(survey_sim_mc_full) <- sprintf("mc_sim%04d", 1:ncol(survey_sim_mc_full))
colnames(survey_sim_mmc_full) <- sprintf("mmc_sim%04d", 1:ncol(survey_sim_mmc_full))
colnames(survey_sim_tmc_full) <- sprintf("tmc_sim%04d", 1:ncol(survey_sim_tmc_full))

# Appending togetherwith the observations
survey_sim_so <- bind_cols(survey_sim_obs, survey_sim_mc_so, survey_sim_mmc_so, survey_sim_tmc_so) %>%
  mutate(model = "Survey only") %>%
  dplyr::select(model, everything())
survey_sim_full <- bind_cols(survey_sim_obs, survey_sim_mc_full, survey_sim_mmc_full, survey_sim_tmc_full) %>%
  mutate(model = "Survey + programme") %>%
  dplyr::select(model, everything())

# Row binding to append everything together
survey_sim <- bind_rows(survey_sim_full, survey_sim_so)

# Pivot longer with column for circumcision type
survey_sim <- survey_sim %>%
  pivot_longer(
    cols = c(starts_with("mc_"), starts_with("mmc_"), starts_with("tmc_")),
    names_pattern = "(.*)_(.*)",
    names_to = c("type", ".value")
  )


###############################################
### Summarising samples and getting outputs ###
###############################################
# Helper function for calculating coverage
calc_ppd_coverage <- function(interval, obs) {
  qlower <- quantile(c_across(starts_with("sim")), 0.5 - interval / 2, names = FALSE)
  qupper <- quantile(c_across(starts_with("sim")), 0.5 + interval / 2, names = FALSE)
  between(obs, qlower, qupper)
}

# Calculate coverage by survey x district x age 15-49 
mccov_district_15to49 <- survey_sim %>%
  filter(age %in% 15:49) %>%
  mutate(iso3 = 'ZAF') %>%
  summarise(across(c(obs, starts_with("sim")), \(x) weighted.mean(x, indweight)),
            .by = c(model, type, survey_id, iso3, province_area_id, area_id)) 

ppd_district_15to49 <- mccov_district_15to49 %>%
  rowwise(model, type, survey_id, iso3, province_area_id, area_id, obs) %>%
  summarise(
    ppd_mean = mean(c_across(starts_with("sim"))),
    crps = scoringutils::crps_sample(true_values = obs, predictions = matrix(c_across(starts_with("sim")), nrow = 1)),
    mae = mean(abs(c_across(starts_with("sim")) - obs)),
    rmse = sqrt(mean((c_across(starts_with("sim")) - obs)^2)),
    cov50 = calc_ppd_coverage(0.5, obs),
    cov80 = calc_ppd_coverage(0.8, obs),
    cov95 = calc_ppd_coverage(0.95, obs),
    .groups = "drop"
  )

# Calculate coverage by survey x district x age 15-49 
mccov_district_5year <- survey_sim %>%
  mutate(
    age_group = cut(age, 0:12*5, sprintf("%02d-%02d", 0:11*5, 0:11*5+4), right = FALSE)
  ) %>%
  mutate(iso3 = 'ZAF') %>%
  summarise(across(c(obs, starts_with("sim")), \(x) weighted.mean(x, indweight)),
            .by = c(model, type, survey_id, iso3, province_area_id, area_id, age_group)) 

ppd_district_5year <- mccov_district_5year %>%
  rowwise(model, type, survey_id, iso3, province_area_id, area_id, age_group, obs) %>%
  summarise(
    ppd_mean = mean(c_across(starts_with("sim"))),
    crps = scoringutils::crps_sample(true_values = obs, predictions = matrix(c_across(starts_with("sim")), nrow = 1)),
    mae = mean(abs(c_across(starts_with("sim")) - obs)),
    rmse = sqrt(mean((c_across(starts_with("sim")) - obs)^2)),
    cov50 = calc_ppd_coverage(0.5, obs),
    cov80 = calc_ppd_coverage(0.8, obs),
    cov95 = calc_ppd_coverage(0.95, obs),
    .groups = "drop"
  )

# Saving
save(mccov_district_15to49, ppd_district_15to49, mccov_district_5year, ppd_district_5year,
     file = 'ppc_DONOTDELETE.RData')

############################
### Preparing for output ###
############################
# Appending summaries
tmp <- rbind.fill(
  # Summarise overall
  ppd_district_5year %>%
    dplyr::summarise(
      dplyr::across(crps, sum),
      dplyr::across(c(mae, rmse, starts_with("cov")), mean),
      .by = c(model, type)) %>%
    mutate(Overall = 'Overall'),
  # Summarise by survey
  ppd_district_5year %>%
    dplyr::summarise(
      dplyr::across(crps, sum),
      dplyr::across(c(mae, rmse, starts_with("cov")), mean),
      .by = c(model, type, survey_id)),
  # Summarise by age group
  ppd_district_5year %>%
    dplyr::summarise(
      dplyr::across(crps, sum),
      dplyr::across(c(mae, rmse, starts_with("cov")), mean),
      .by = c(model, type, age_group)),
  # Summarise by 15-49
  ppd_district_15to49 %>%
    dplyr::summarise(
      dplyr::across(crps, sum),
      dplyr::across(c(mae, rmse, starts_with("cov")), mean),
      .by = c(model, type)) %>%
    mutate(age_group = '15-49'))

# Preparing for table 
tmp <- melt(tmp, 
     id.vars = c("Overall", "survey_id", "age_group", "model", "type"))%>%
  # Creating new variable to transpose by 
  dplyr::mutate(variable = case_when(model == 'Survey + programme' ~ paste0(variable, '_1'),
                                     model == 'Survey only' ~ paste0(variable, '_2')),
                survey_id = as.numeric(substr(survey_id, 4, 7))) %>%
  # Sorting dataset 
  arrange(type, Overall, survey_id, age_group, model, variable) %>%
  # Removing uncessary columns 
  dplyr::select(-c(model)) %>%
  # Long to wide dataset
  dcast(type + Overall  + survey_id + age_group ~ variable, 
        value.var = c("value"))%>%
  # Creating column with variables name. First non missing.
  mutate(variable = coalesce(Overall,
                             as.character(survey_id), 
                             age_group),
         space = " ")%>%
  # Ordering columns 
  dplyr::select(space, type, variable, crps_1, mae_1, 
                rmse_1, cov50_1, cov80_1, cov95_1, crps_2, 
                mae_2, rmse_2, cov50_2, cov80_2, cov95_2)


# Multiplying the coverage by 100 and rounding 
tmp[,c("cov50_1", "cov80_1", "cov95_1", "cov50_2", "cov80_2", "cov95_2")] <- 
  round(100 * tmp[,c("cov50_1", "cov80_1", "cov95_1", "cov50_2", "cov80_2", "cov95_2")], 2)

# Rounding other variables 
tmp[,c("crps_1", "mae_1", "rmse_1", "crps_2", "mae_2", "rmse_2")] <- 
  round(tmp[,c("crps_1", "mae_1", "rmse_1", "crps_2", "mae_2", "rmse_2")], 2)

# Formatting for output
tmp[,c("crps_1", "mae_1", "rmse_1", "cov50_1", "cov80_1", "cov95_1", 
       "crps_2", "mae_2", "rmse_2", "cov50_2", "cov80_2", "cov95_2")] <- 
  format(tmp[,c("crps_1", "mae_1", "rmse_1", "cov50_1", "cov80_1", "cov95_1", 
                "crps_2", "mae_2", "rmse_2", "cov50_2", "cov80_2", "cov95_2")], nsmall = 2)

########################
### Outputting table ###
########################
require(xtable)
print(xtable(tmp[which(tmp$type == 'mc'),
                 c('space', "variable", 
                   "crps_1", "mae_1", "rmse_1", "cov50_1", "cov80_1", "cov95_1", 
                   "crps_2", "mae_2", "rmse_2", "cov50_2", "cov80_2", "cov95_2")]), 
      include.rownames=FALSE)

print(xtable(tmp[which(tmp$type == 'mmc'),
                 c('space', "variable", 
                   "crps_1", "mae_1", "rmse_1", "cov50_1", "cov80_1", "cov95_1", 
                   "crps_2", "mae_2", "rmse_2", "cov50_2", "cov80_2", "cov95_2")]), 
      include.rownames=FALSE)

print(xtable(tmp[which(tmp$type == 'tmc'),
                 c('space', "variable", 
                   "crps_1", "mae_1", "rmse_1", "cov50_1", "cov80_1", "cov95_1", 
                   "crps_2", "mae_2", "rmse_2", "cov50_2", "cov80_2", "cov95_2")]), 
      include.rownames=FALSE)


  
  

