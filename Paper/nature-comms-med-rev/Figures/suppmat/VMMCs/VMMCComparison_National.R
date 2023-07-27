#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Setting working directory 
setwd('~/Dropbox/Postdoc/zaf-circumcision-rates')

# Loading source code 
source('Code/0_Source.R')

# Colours for the plot 
colourPalette <- rev(colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))(100))

# Model version
version <- '20210716'

################################################
### Preparing location/shapefile information ###
################################################
# Loading shapefiles 
area_hierarchy <- read.csv("~/Dropbox/Postdoc/zaf-subnational-hiv/data/zaf_area_hierarchy.csv")
area_boundaries <- read_sf("~/Dropbox/Postdoc/zaf-subnational-hiv/data/zaf_area_boundaries.geojson")

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
# Reading in image of SA
img <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Map_of_South_Africa_with_provinces_shaded_and_districts_numbered_%282016%29.svg/500px-Map_of_South_Africa_with_provinces_shaded_and_districts_numbered_%282016%29.svg.png"))

# Getting the district grid
zaf_district_grid <- read_csv("~/Dropbox/Downloads/zaf-district-grid.csv")

# Setting colours for each province
province_palette <- c("Western Cape" = "red",
                      "Free State" = "orange",
                      "Eastern Cape" = "darkblue",
                      "Limpopo" = "purple",
                      "Northern Cape" = "darkgreen",
                      "Gauteng" = "hotpink",
                      "Mpumalanga" = "brown",
                      "North West" = "black",
                      "KwaZulu-Natal" = "#fb8072")

# Grouping districts by their province
zaf_district_grid <- zaf_district_grid %>%
  mutate(name_province = code %>%
           fct_collapse(
             "Limpopo" = c("DC33", "DC34", "DC35", "DC36", "DC47"),
             "Mpumalanga" = c("DC30", "DC31", "DC32"),
             "Gauteng" = c("TSH", "JHB", "EKU", "DC42", "DC48"),
             "North West" = c("DC37", "DC38", "DC39", "DC40"),
             "Free State" = c("MAN", "DC16", "DC18", "DC19", "DC20"),
             "KwaZulu-Natal" = c("ETH", "DC21", "DC22", "DC23", "DC24", "DC25", "DC26", "DC27", "DC28", "DC29", "DC43"),
             "Eastern Cape" = c("BUF", "NMA", "DC10", "DC12", "DC13", "DC14", "DC15", "DC44"),
             "Northern Cape" = c("DC6", "DC7", "DC8", "DC9", "DC45"),
             "Western Cape" = c("CPT", "DC1", "DC2", "DC3", "DC4", "DC5")
           ))

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_agegroup1 <- read_csv(paste('Output/Analysis/', version, '/Results_AgeGroup_Rate.csv', sep = ''))
results_agegroup2 <- read_csv(paste('Output/Analysis/', version, '/Results_AgeGroup_Incidence.csv', sep = ''))
results_agegroup3 <- read_csv(paste('Output/Analysis/', version, '/Results_AgeGroup_Prevalence.csv', sep = ''))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Adding label-freindly district name 
results_agegroup <- merge(results_agegroup, 
                          zaf_district_grid[,c('code_area_id', 'name_district')],
                          by.x = 'area_id',
                          by.y = 'code_area_id',
                          all.x = TRUE)

tmp <- subset(results_agegroup, year == 2006)
tmp$year = 2002
results_agegroup <- rbind(tmp, results_agegroup)
tmp$year = 2005
results_agegroup <- rbind(tmp, results_agegroup)

###########################################
### Preparing program data for analysis ###
###########################################
# DMPPT2 input data prior to 2014
dat_program1 <- read.csv('Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2009:2012 & 
           age_group == '10+' & 
           type == 'DMPPT2' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) 

# DMPPT2 input data prior to 2014
dat_program2 <- read.csv('Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2013:2017 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0)

# DMPPT2 input data prior to 2014
dat_program3 <- read.csv('Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')) &
           area_level == 2 & 
           year %in% 2018:2019 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0)

# DMPPT2 input data prior to 2014
dat_program4 <- read.csv('Data/Processed/prop1_4.csv') %>%
  rename(mmc_performed = test) %>%
  dplyr::select(-c(name_district)) %>%
  filter(mmc_performed > 0 & type == 'prop4') %>%
  left_join(area_hierarchy[,c('area_id', 'area_level')], by = 'area_id') %>%
  mutate(type = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), 'Reallocated PEPFAR', 'Reported PEPFAR'),
         # age_group = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), '10+', age_group),
         age_group = '10+') 

# Appending together 
dat_program <- rbind(dat_program1, dat_program2, dat_program3, dat_program4) %>%
  mutate(area_id = 'ZAF',
         area_name = 'South Africa',
         parent_area_id = '',
         area_level = 0) %>%
  ddply(.(area_id, year, age_group, area_name, parent_area_id, area_level),
        summarise,
        mmc_performed = sum(mmc_performed)) %>%
  mutate(lower = NA,
         upper = NA,
         type = 'Programme data')
# Removing unecessary datasets
rm(dat_program1, dat_program2, dat_program3, dat_program4)

#
test <- subset(results_agegroup, age_group == '10+' &
                 type == 'MMCs performed' &
                 year %in% c(2008:2019) &
                 model == 'With program data') %>%
  dplyr::select(area_id, year, type, age_group, mmc_performed = mean, lower, upper, area_name, parent_area_id, area_level)

# Appending together
tmp <- rbind(test, dat_program)

tmp$type <- factor(tmp$type, levels = c('Programme data', 'MMCs performed'))

pdf('Documents/Survival/Figures/suppmat/VMMCs/MMCsComparison_National.pdf', width = 12, height = 9)
ggplot(subset(tmp, year %in% 2010:2019 & area_level == 0),
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
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) + 
  scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(0, NA)) + 
  labs(x = 'Year',
       y = 'Number of MMCs performed (in 1000s)',
       fill = '') +
  theme_minimal() +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        strip.placement = "outside",
        legend.position = 'bottom')
dev.off()



