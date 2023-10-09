#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/suppmat')

# Loading source code 
source('../src/0_Source.R')

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

#####################################
### Reading in stuff for geofacet ###
#####################################
zaf_province_grid <- tribble(
  ~code, ~code_area_id, ~name_province , ~row, ~col,
  "LIM", "ZAF_1_LIM"  , "Limpopo"      , 1   , 3   ,
  "NW" , "ZAF_1_NW"   , "North West"   , 2   , 1   ,
  "GT" , "ZAF_1_GT"   , "Gauteng"      , 2   , 2   ,
  "MP" , "ZAF_1_MP"   , "Mpumalanga"   , 2   , 3   ,
  "NC" , "ZAF_1_NC"   , "Northern Cape", 3   , 1   ,
  "FS" , "ZAF_1_FS"   , "Free State"   , 3   , 2   ,
  "KZN", "ZAF_1_KZN"  , "KwaZulu-Natal", 3   , 3   ,
  "WC" , "ZAF_1_WC"   , "Western Cape" , 4   , 1   ,
  "EC" , "ZAF_1_EC"   , "Eastern Cape" , 4   , 2
)

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

###########################################
### Preparing program data for analysis ###
###########################################
# DMPPT2 input data prior to 2014
dat_program1 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2009:2012 & 
           age_group == '10+' & 
           type == 'DMPPT2' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) 

# DMPPT2 input data prior to 2014
dat_program2 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2013:2017 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0)

# DMPPT2 input data prior to 2014
dat_program3 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')) &
           area_level == 2 & 
           year %in% 2018:2019 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0)

# DMPPT2 input data prior to 2014
dat_program4 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/prop1_4.csv') %>%
  rename(mmc_performed = test) %>%
  dplyr::select(-c(name_district)) %>%
  filter(mmc_performed > 0 & type == 'prop4') %>%
  left_join(area_hierarchy[,c('area_id', 'area_level')], by = 'area_id') %>%
  mutate(type = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), 'Reallocated PEPFAR', 'Reported PEPFAR'),
         # age_group = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), '10+', age_group),
         age_group = '10+') 

# Appending together 
dat_program <- rbind(dat_program1, dat_program2, dat_program3, dat_program4) %>%
  mutate(area_id = parent_area_id,
         parent_area_id = 'ZAF',
         area_level = 1) %>%
  ddply(.(area_id, year, age_group, parent_area_id, area_level),
        summarise,
        mmc_performed = sum(mmc_performed)) %>%
  mutate(lower = NA,
         upper = NA,
         type = 'Programme data')  %>%
  left_join(area_hierarchy[,c('area_id', 'area_name')])

# Removing unecessary datasets
rm(dat_program1, dat_program2, dat_program3, dat_program4)

#
test <- subset(results_agegroup, age_group == '10+' &
                 type == 'Number of circumcisions performed (MMC)' &
                 year %in% c(2008:2019) &
                 model == 'With program data') %>%
  dplyr::select(area_id, year, type, age_group, mmc_performed = mean, lower, upper, area_name, parent_area_id, area_level)

# Appending together
tmp <- rbind(test, dat_program)

# Only keeping province level results
tmp <- subset(tmp, year %in% 2010:2019 & area_level == 1)

# Setting order of programme data and VMMCs performed
tmp$type[which(tmp$type == 'Number of circumcisions performed (MMC)')] <- 'Modelled estimates'
tmp$type <- factor(tmp$type, levels = c('Programme data', 'Modelled estimates'))

pdf('VMMCs/MMCsComparison_Province.pdf', width = 12, height = 9)
ggplot(subset(tmp, year %in% 2010:2019 & area_level == 1),
       aes(x = year,
           y = mmc_performed/1000,
           ymin = lower / 1000,
           ymax = upper / 1000,
           group = type,
           fill = type)) +
  geom_bar(stat = 'identity',
           position = position_dodge(.9)) +
  geom_errorbar(width = 0.01,
                position = position_dodge(.9),
                size = 1) +
  scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Archambault", 3)[c(1, 2)]) +
  scale_colour_manual(values = MetBrewer::met.brewer("Archambault", 3)[c(1, 2)]) + 
  labs(x = 'Year',
       y = 'Number of MMCs performed (in 1000s)',
       fill = '') +
  theme_minimal() +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_province_grid, 
            scales = 'free_y',
            label = "name_province") +  
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        legend.text = element_text(size = 16),
        panel.grid = element_blank(),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        strip.placement = "outside",
        legend.position = 'bottom')
dev.off()


