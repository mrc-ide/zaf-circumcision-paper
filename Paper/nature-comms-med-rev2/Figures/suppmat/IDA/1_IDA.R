#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/suppmat')

# Loading source code 
source('../src/0_Source.R')
library(patchwork)

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
# Getting the district grid
zaf_district_grid <- read_csv("../src/zaf-district-grid.csv")

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
### Preparing data for plotting ###
######################################
# Reading in program data 
dat_program <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') 

############################
### Plots of PEPFAR data ###
############################
# Reading in program data 
tmp <- subset(dat_program, !is.na(mmc_performed) & 
                       type == 'PEPFAR' & 
                       !(age_group %in% c('Unknown Age', '00-09')) & 
                area_name %in% c("A Nzo DM","Amathole DM","Bojanala Platinum DM","Buffalo City MM","C Hani DM",
                                 "Cape Town MM","Capricorn DM","Dr K Kaunda DM","Ehlanzeni DM","Ekurhuleni MM",
                                 "eThekwini MM","G Sibande DM","Harry Gwala DM","Johannesburg MM",
                                 "King Cetshwayo DM","Lejweleputswa DM","Mopani DM","Ngaka Modiri Molema DM",
                                 "Nkangala DM","O Tambo DM","Sedibeng DM","T Mofutsanyana DM",
                                 "Tshwane MM","Ugu DM","uMgungundlovu DM","Umkhanyakude DM","Uthukela DM",
                                 "Zululand DM" ))

# Grouping over 30 age groups to standardise
tmp$age_group[which(tmp$age_group %in% c('25-49', '30-34', '35-39', '40-44', '45-49', '50+', '40-49', '30-49'))] <- '30+'

# Aggregating across all age groups 
tmp <- ddply(tmp, 
             .(area_id, age_group, year),
             summarise,
             mmc_performed = sum(mmc_performed))

# Getting missing districts
tmp <- merge(expand.grid(area_id = sort(unique(area_hierarchy$area_id[which(area_hierarchy$area_level <= 2)])),
                         year = 2015:2019,
                         age_group = c('10-14', '15-19', '20-24', '25-29', '30+')),
             tmp,
             by = c('area_id','year','age_group'),
             all.x = TRUE) %>%
  ddply(.(area_id, year),
        summarize,
        mmc_performed = sum(mmc_performed)) %>%
  left_join(area_hierarchy[,c('area_name','area_id','parent_area_id','area_level')],
            by = c('area_id')) %>%
  # Renaming area_id for geofacet
  dplyr::rename(code_area_id = area_id) %>%
  # Getting code for geofacet
  mutate(code = sub("ZAF_2_", "", code_area_id)) %>%
  # Merging geofacet
  left_join(dplyr::select(zaf_district_grid, code_area_id, name_province))

# Creating plot 
pdf('IDA/NumMMC_PEPFAR_District.pdf', width = 24, height = 21)
ggplot(subset(tmp, area_level == 2 & year < 2020),
       aes(x = year, 
           y = mmc_performed/1000)) + 
  geom_bar(stat = 'identity',
           fill = rev(RColorBrewer::brewer.pal(4, 'BrBG'))[1],
           linewidth = 0.5) + 
  labs(x = 'Year',
       y = 'VMMCs performed (in 1000s)',
       fill = '') + 
  # Minimal theme
  theme_minimal() +
  # Geofacet
  facet_geo(~ code_area_id, 
            grid = zaf_district_grid, 
            label = "name_district",
            scales = 'free') +
  # Altering plot text size
  theme(strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title = element_text(size = 28),
        panel.grid = element_blank(),
        plot.title = element_text(size = 40, hjust = 0.5),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5,vjust = 0.5),
        axis.text.y = element_text(size = 16),
        legend.position = 'bottom')
dev.off()

############################
### Plots of DMPPT2 data ###
############################
# Reading in program data 
tmp <- subset(dat_program, !is.na(mmc_performed) & 
                type == 'DMPPT2' & 
                !(age_group %in% c('10+', '15+', '00-04', '05-09')) & 
                year %in% 2010:2017)

# Grouping over 30 age groups to standardise
tmp$age_group[which(tmp$age_group %in% c('30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))] <- '30+'

# Aggregating across all age groups 
tmp <- ddply(tmp, 
             .(area_id, area_name, area_level, year),
             summarise,
             mmc_performed = sum(mmc_performed))  %>%
  # Renaming area_id for geofacet
  dplyr::rename(code_area_id = area_id) %>%
  # Getting code for geofacet
  mutate(code = sub("ZAF_2_", "", code_area_id),
         y1 = 1) %>%
  # Merging geofacet
  left_join(dplyr::select(zaf_district_grid, code_area_id, name_province))

pdf('IDA/NumMMC_DMPPT2_District.pdf', width = 24, height = 21)
ggplot(subset(tmp, area_level == 2 & year < 2020),
       aes(x = year, 
           y = mmc_performed/1000)) + 
  geom_bar(stat = 'identity',
           fill =  rev(RColorBrewer::brewer.pal(4, 'BrBG'))[1],
           linewidth = 0.5) + 
  labs(x = 'Year',
       y = 'VMMCs performed (in 1000s)',
       fill = '') + 
  # scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 5)) + 
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+
  # Minimal theme
  theme_minimal() +
  # Geofacet
  facet_geo(~ code_area_id, 
            grid = zaf_district_grid, 
            label = "name_district",
            scales = 'free') +
  # Altering plot text size
  theme(strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title = element_text(size = 28),
        panel.grid = element_blank(),
        plot.title = element_text(size = 40, hjust = 0.5),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5,vjust = 0.5),
        axis.text.y = element_text(size = 16),
        legend.position = 'bottom')
dev.off()


###########################
### Plots of DHIS2 data ###
###########################
# Reading in program data 
tmp <- subset(dat_program, !is.na(mmc_performed) & 
                type == 'DHIS') %>%
  filter((year <= 2016 & age_group == '10+') | 
           (year %in% 2016:2019 & age_group == '10+')) %>%
  # Renaming area_id for geofacet
  dplyr::rename(code_area_id = area_id) %>%
  # Getting code for geofacet
  mutate(code = sub("ZAF_2_", "", code_area_id),
         y1 = 1) %>%
  # Merging geofacet
  left_join(dplyr::select(zaf_district_grid, code_area_id, name_province))

pdf('IDA/NumMMC_DHIS_District.pdf', width = 24, height = 21)
ggplot(subset(tmp, area_level == 2),
       aes(x = year, 
           y = mmc_performed/1000)) + 
  geom_bar(stat = 'identity',
           fill = rev(RColorBrewer::brewer.pal(4, 'BrBG'))[1],
           linewidth = 0.5) + 
  labs(x = 'Year',
       y = 'VMMCs performed (in 1000s)',
       fill = '') + 
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(3, 'BrBG'))) + 
  scale_x_continuous(breaks = seq(2010, 2020, by = 1))+
  # Minimal theme
  theme_minimal() +
  # Geofacet
  facet_geo(~ code_area_id, 
            grid = zaf_district_grid, 
            label = "name_district",
            scales = 'free') +
  # Altering plot text size
  theme(strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title = element_text(size = 28),
        panel.grid = element_blank(),
        plot.title = element_text(size = 40, hjust = 0.5),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5,vjust = 0.5),
        axis.text.y = element_text(size = 16),
        legend.position = 'bottom')
dev.off()


###########################################
### Preparing program data for analysis ###
###########################################
# DMPPT2 input data prior to 2014
dat_program1 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2010:2012 & 
           age_group == '10+' & 
           type == 'DMPPT2' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# DMPPT2 input data prior to 2014
dat_program2 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(area_level == 2 & 
           year %in% 2013:2017 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# DMPPT2 input data prior to 2014
dat_program3 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
  filter(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')) &
           area_level == 2 & 
           year %in% 2018:2019 & 
           age_group == '10+' & 
           type == 'DHIS' & 
           !is.na(mmc_performed) & 
           mmc_performed > 0) %>%
  mutate(age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# DMPPT2 input data prior to 2014
dat_program4 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/prop1_4.csv') %>%
  rename(mmc_performed = test) %>%
  dplyr::select(-c(name_district)) %>%
  filter(mmc_performed > 0 & type == 'prop4') %>%
  left_join(area_hierarchy[,c('area_id', 'area_level')], by = 'area_id') %>%
  mutate(type = if_else(!(area_id %in% c('ZAF_2_DC12', 'ZAF_2_BUF', 'ZAF_2_DC44', 'ZAF_2_DC13', 'ZAF_2_DC15')), 'Reallocated PEPFAR', 'Reported PEPFAR'),
         age_group = '10+',
         age1 = as.numeric(substr(age_group, 1, 2)),
         age2 = as.numeric(substr(age_group, 4, 5))) %>%
  mutate(age2 = if_else(is.na(age2), Inf, age2))

# Appending together 
dat_program <- rbind(dat_program1, dat_program2, dat_program3, dat_program4) %>%
  ddply(.(area_id, year, age_group, type, area_name, parent_area_id, area_level, age1, age2),
        summarise,
        mmc_performed = sum(mmc_performed))

# Setting order for the plot
dat_program$type <- factor(dat_program$type, levels = c('DHIS', 'DMPPT2', 'Reallocated PEPFAR', 'Reported PEPFAR'))

# Adding label-freindly district name 
dat_program <- merge(dat_program, 
                     zaf_district_grid[,c('code_area_id', 'name_district')],
                     by.x = 'area_id',
                     by.y = 'code_area_id',
                     all.x = TRUE)

pdf('IDA/NumMMC_Model_District.pdf', width = 25, height = 21)
ggplot(dat_program,
       aes(x = year,
           y = mmc_performed/1000,
           group = type,
           fill = type)) + 
  geom_bar(stat = 'identity',
           colour = 'black',
           linewidth  = 0.5) + 
  # scale_fill_brewer(palette = 'Paired') +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, 'BrBG'))) +
  labs(x = 'Year',
       y = 'VMMCs performed (in 1000s)',
       fill = ''
       #title = 'Pooling PEPFAR MMCs from EC PEPFAR districts, reallocating to all districts',
       #subtitle = 'Reallocating done using proportion of Xhosa men'
       ) + 
  scale_x_continuous(breaks = seq(2010, 2019, by = 1))+
  # Minimal theme
  theme_minimal() +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_district_grid, 
            label = "name_district",
            scales = 'free') +
  # Altering plot text size
  theme(strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title = element_text(size = 28),
        panel.grid = element_blank(),
        plot.title = element_text(size = 40, hjust = 0.5),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 0.5,vjust = 0.5),
        axis.text.y = element_text(size = 16),
        legend.position = 'bottom')
dev.off()





















