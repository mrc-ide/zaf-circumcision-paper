#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/Postdoc/zaf-circumcision-rates')

# Loading source code 
source('Code/0_Source.R')

# Model version
version <- '20210716'

###########################
### Reading in datasets ###
###########################
# Getting the district grid
zaf_district_grid <- read_csv("~/Dropbox/Downloads/zaf-district-grid.csv")

# Reading in dataset
results_agegroup <- read_csv(paste('Output/Analysis/', version, '/Results_AgeGroup_Prevalence.csv', sep = ''))

#########################
### Preparing results ###
#########################
# Only keeping prevalence estimates
results_agegroup <- subset(results_agegroup, area_level <= 2 & 
                             type %in% c('MC coverage', 'MMC coverage', 'TMC coverage', 
                                         'Number circumcised', 'Unmet need',
                                         'Change in MC coverage from 2008') & 
                             age_group == '15-49' & 
                             model == 'With program data' &
                             year %in% c(2020))

# Relabelling
results_agegroup$type[which(results_agegroup$type == 'MC coverage')] <- 'Total'
results_agegroup$type[which(results_agegroup$type == 'MMC coverage')] <- 'MMC'
results_agegroup$type[which(results_agegroup$type == 'TMC coverage')] <- 'TMC'
results_agegroup$type[which(results_agegroup$type == 'Number circumcised')] <- 'Circumcised'
results_agegroup$type[which(results_agegroup$type == 'Unmet need')] <- 'Uncircumcised'
results_agegroup$type[which(results_agegroup$type == 'Change in MC coverage from 2008')] <- 'Change in Total'

# Creating output variable
results_agegroup$val <- NA

# Output variable for prevalence estimate
results_agegroup$val[which(results_agegroup$type %in% c('Total', 'MMC', 'TMC', 'Change in Total'))] <- 
  paste(format(round(100 * results_agegroup$mean[which(results_agegroup$type %in% c('Total', 'MMC', 'TMC', 'Change in Total'))], 1), 1), 
        '% (', 
        format(round(100 * results_agegroup$lower[which(results_agegroup$type %in% c('Total', 'MMC', 'TMC', 'Change in Total'))], 1), 1), 
        '%--', 
        format(round(100 * results_agegroup$upper[which(results_agegroup$type %in% c('Total', 'MMC', 'TMC', 'Change in Total'))], 1), 1), 
        '%)',
        sep = '')

# Output variable for the prevalence estimate 
results_agegroup$val[which(results_agegroup$type %in% c('Circumcised', 'Uncircumcised'))] <- 
  paste(round(results_agegroup$mean[which(results_agegroup$type %in% c('Circumcised', 'Uncircumcised'))]/1000, 0), 
        ' (', 
        round(results_agegroup$lower[which(results_agegroup$type %in% c('Circumcised', 'Uncircumcised'))]/1000, 0), 
        '--', 
        round(results_agegroup$upper[which(results_agegroup$type %in% c('Circumcised', 'Uncircumcised'))]/1000, 0),
        ')',
        sep = '')

# Rounding population 
results_agegroup$population <- round(results_agegroup$population/1000, 1)

########################
### Outputting table ###
########################
# Transposing datasets
out <- dcast(results_agegroup, 
              parent_area_id + area_id + area_name + year + population ~ type, 
              value.var = "val")

# Mergin on nice names 
out <- merge(out, 
             zaf_district_grid[,c('code_area_id', 'name_district')],
             by.x = 'area_id',
             by.y = 'code_area_id',
             all.x = TRUE)

# Using standardised label if missing
out$area_name[!is.na(out$name_district)] <- ''

# Sorting dataset
out <- out[order(out$parent_area_id, out$name_district),]

# Ordering columns 
out <- out[, c("area_name", "name_district", 'Total','MMC', 'TMC', 'Change in Total', 'Circumcised', 'Uncircumcised')]

require(xtable)
sink('Documents/Survival/Figures/paper/Table1.txt')
print(xtable(out), include.rownames = FALSE)
sink()
