#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggridges)

# Model version
version <- '20210716'

################################################
### Preparing location/shapefile information ###
################################################
# Loading shapefiles 
area_hierarchy <- read.csv("../src/zaf_area_hierarchy.csv")
area_boundaries <- read_sf("../src/zaf_area_boundaries.geojson")

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
## img <- readPNG(getURLContent("https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Map_of_South_Africa_with_provinces_shaded_and_districts_numbered_%282016%29.svg/500px-Map_of_South_Africa_with_provinces_shaded_and_districts_numbered_%282016%29.svg.png"))
img <- readPNG("../src/Map_of_South_Africa_with_provinces_shaded_and_districts_numbered_(2016).svg.png")

# Getting the district grid
zaf_district_grid <- read_csv("../src/zaf-district-grid.csv")

# Setting colours for each province
province_palette <- c("Western Cape" = "#bc80bd",
                      "Free State" = "#bebada",
                      "Eastern Cape" = "#8dd3c7",
                      "Limpopo" = "#80b1d3",
                      "Northern Cape" = "#b3de69",
                      "Gauteng" = "#ffffb3",
                      "Mpumalanga" = "#fdb462",
                      "North West" = "#fccde5",
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

###########################################
### Preparing program data for analysis ###
###########################################
# DMPPT2 input data prior to 2013
dat_program1 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
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

# DHIS input data for 2018-2020 for all non-PEPFAR EC districts
dat_program3 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/zaf_circumcision_program_data.csv') %>%
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
dat_program4 <- read.csv('~/Dropbox/zaf-circumcision-rates/Data/Processed/prop1_4.csv') %>%
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
  dplyr::select(area_id = parent_area_id, year, type, age_group, mmc_performed, age1, age2) %>%
  left_join(area_hierarchy %>%
              dplyr::select(area_id, area_name, area_level, parent_area_id)) %>%
  ddply(.(area_id, year, age_group, area_name, parent_area_id, area_level, age1, age2),
        summarise,
        mmc_performed = sum(mmc_performed))%>%
  subset(year %in% 2010:2019)

# Removing unecessary datasets
rm(dat_program1, dat_program2, dat_program3, dat_program4)

# Adding label-friendly district name 
dat_program <- merge(dat_program, 
                     zaf_district_grid[,c('code_area_id', 'name_district')],
                     by.x = 'area_id',
                     by.y = 'code_area_id',
                     all.x = TRUE)

##########################
### Reading in results ###
##########################
# Reading in model results by age group
results_agegroup1 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Only keeping MMCs preformed 
results <- results_agegroup %>%
  subset(age_group == '10+' & type %in% c('MMCs performed') & 
           area_level == 1 & year %in% 2010:2019)

# Adding label-friendly district name 
results <- merge(results, 
                 zaf_district_grid[,c('code_area_id', 'name_district')],
                 by.x = 'area_id',
                 by.y = 'code_area_id',
                 all.x = TRUE)

# Getting relevant columns 
results <- results[,c('area_id','area_name', 'area_level', 'name_district', 'model', 'type','year','mean', 'lower', 'upper')]

#######################################
### Plotting results (DMPPT2, total)###
#######################################
# Creating grid for geofacet
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

# Renaming model label
results$model[which(results$model == 'No program data')] <- 'Survey data only'
results$model[which(results$model == 'With program data')] <- 'With programme data'


fig8p <- ggplot(results,
                aes(x = year,
                    y = mean/1000,
                    ymin = lower/1000,
                    ymax = upper/1000,
                    group = model,
                    fill = model,
                    colour = model)) +
  # Bar plot
  geom_bar(stat = 'identity',
           width = 0.75,
           position = position_dodge(0.9)) +
  # Error bars
  geom_linerange(colour = 'black',
                 show.legend = FALSE,
                 position = position_dodge(0.75))  +
  # Adding points for observed data
  geom_point(data = dat_program,
             aes(x = year,
                 y = mmc_performed/1000),
             shape = 4,
             inherit.aes = FALSE) +
  # Setting for the axes
  scale_x_continuous(breaks = seq(2010, 2018, by = 2)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  # Colour palette
  scale_fill_manual(values = MetBrewer::met.brewer("Archambault", 3)[c(1, 2)]) +
  scale_colour_manual(values = MetBrewer::met.brewer("Archambault", 3)[c(1, 2)]) + 
  # Plot labels 
  labs(x = NULL,
       y = 'Number of MMCs performed (in 1000s)',
       colour = NULL,
       fill = NULL) +
  # Adding text labels  for the facet
  geom_text(aes(x = 2009.5, y = top, label = area_name),
            data = ddply(results, .(area_id, area_name), summarize, top = 1.1 * round(max(upper)/1000)),
            hjust = 0, vjust = 1,
            size = 2.9,
            fontface = "bold",
            inherit.aes = FALSE) + 
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_province_grid, 
            label = "name_province",
            scales = 'free_y') +
  # Minimal theme
  theme_bw(8) +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 8),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9, face = 'bold'),
        legend.position = 'none') + 
  # Altering legend
  guides(color = guide_legend(ncol = 2))

fig8l <- {fig8p +
    theme(legend.position = "right")} %>%
  ggpubr::get_legend()

library(patchwork)

fig8 <- wrap_elements(full = get_geofacet_grob(fig8p)) +
  inset_element(fig8l, 0.7, 0.05, 1.0, 0.23, align_to = "full", on_top = TRUE)

ggsave("Figure2.pdf", width = 8, height = 5.3, units = "in")
ggsave("Figure2.png", width = 8, height = 5.3, units = "in")
ggsave("Figure2.eps", width = 8, height = 5.3, units = "in", device = cairo_ps)

