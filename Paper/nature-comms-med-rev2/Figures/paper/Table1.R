#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggridges)

# Model version
version <- '20210716'

###########################
### Reading in datasets ###
###########################
# Getting the district grid
zaf_district_grid <- read_csv("../src/zaf-district-grid.csv")

# Reading in results by age 
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

#########################
### Preparing results ###
#########################
# Only keeping prevalence estimates
results_agegroup <- subset(results_agegroup, area_level <= 2 & 
                             type %in% c('Circumcision coverage (MC)', 
                                         'Circumcision coverage (MMC)', 
                                         'Circumcision coverage (TMC)', 
                                         'Total number circumcised (MC)', 
                                         'Total number uncircumcised/Unmet need',
                                         'Change in circumcision coverage (MC) from 2008') & 
                             age_group == '15-49' & 
                             model == 'With program data' &
                             year %in% c(2020))

# Relabelling
results_agegroup$type[which(results_agegroup$type == 'Circumcision coverage (MC)')] <- 'Total'
results_agegroup$type[which(results_agegroup$type == 'Circumcision coverage (MMC)')] <- 'MMC'
results_agegroup$type[which(results_agegroup$type == 'Circumcision coverage (TMC)')] <- 'TMC'
results_agegroup$type[which(results_agegroup$type == 'Total number circumcised (MC)')] <- 'Circumcised'
results_agegroup$type[which(results_agegroup$type == 'Total number uncircumcised/Unmet need')] <- 'Uncircumcised'
results_agegroup$type[which(results_agegroup$type == 'Change in circumcision coverage (MC) from 2008')] <- 'Change in Total'

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
out$parent_area_id[grep("_1_", out$area_id)] <- out$area_id[grep("_1_", out$area_id)]
out$parent_area_id[is.na(out$parent_area_id)] <- 'ZAF'
out$tmp <- as.numeric(substr(out$area_id, 5, 5))
out$tmp[is.na(out$tmp)] <- 0
out <- out[order(out$parent_area_id, out$tmp, out$name_district),]

# Ordering columns 
out <- out[, c("area_name", "name_district", 'Total','MMC', 'TMC', 'Change in Total', 'Circumcised', 'Uncircumcised')]

# Saving as LaTeX table
require(xtable)
sink('Table1.txt')
print(xtable(out), include.rownames = FALSE)
sink()

# Removing double dash for Excel format
out$Total <- gsub('--', '-', out$Total)
out$MMC <- gsub('--', '-', out$MMC)
out$TMC <- gsub('--', '-', out$TMC)
out$`Change in Total` <- gsub('--', '-', out$`Change in Total`)
out$Circumcised <- gsub('--', '-', out$Circumcised)
out$Uncircumcised <- gsub('--', '-', out$Uncircumcised)

# Saving as Excel
write.csv(out, file = 'Table1.csv', row.names = FALSE)









