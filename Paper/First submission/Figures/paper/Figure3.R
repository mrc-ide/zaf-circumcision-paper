#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Loading source code 
source('../src/0_Source.R')

# Colours for the plot 
colourPalette <- rev(colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))(100))

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
  dplyr::mutate(space = seq(dplyr::n())) %>%
  ungroup()

# Adding a unique identifier within Admin code and merging to boundaries
area_boundaries <- area_hierarchy %>% 
  group_by(area_level) %>% 
  dplyr::mutate(space = 1:dplyr::n()) %>% 
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
results_age1 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Probability.csv'))
results_age2 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Incidence.csv'))
results_age3 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Prevalence.csv'))
results_age <- rbind(results_age1, results_age2, results_age3)
rm(results_age1, results_age2, results_age3)

# Reading in results by age 
results_agegroup1 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

## # Reading in survey results
## results_survey <- read_csv('Data/Processed/zaf_circumcision_surveypoints.csv')

# Merging to shapefiles 
results_agegroup_shp <- results_agegroup %>%
  left_join(x = area_boundaries)

# Merging to shapefiles 
results_age_shp <- results_age %>%
  left_join(x = area_boundaries)

# Getting order of placement of districts within figures
tmp <- subset(area_hierarchy, area_level <= 2) %>%
  arrange(parent_area_id, area_name)

# Setting factor
results_age$area_name <- factor(results_age$area_name, levels = tmp$area_name)
results_agegroup$area_name <- factor(results_agegroup$area_name, levels = tmp$area_name)

# Removing unecessary datasets
rm(tmp)

# Copying 2006 for 2002 and 2005, no change in prevalence prior to 2006
tmp <- subset(results_agegroup_shp, year == 2006)
tmp$year = 2002
results_agegroup_shp <- rbind(tmp, results_agegroup_shp)
tmp$year = 2005
results_agegroup_shp <- rbind(tmp, results_agegroup_shp)

# Copying 2006 for 2002 and 2005, no change in prevalence prior to 2006
tmp <- subset(results_agegroup, year == 2006)
tmp$year = 2002
results_agegroup <- rbind(tmp, results_agegroup)
tmp$year = 2005
results_agegroup <- rbind(tmp, results_agegroup)

# Adding label-freindly district name 
results_age <- merge(results_age, 
                     zaf_district_grid[,c('code_area_id', 'name_district')],
                     by.x = 'area_id',
                     by.y = 'code_area_id',
                     all.x = TRUE)
results_agegroup <- merge(results_agegroup, 
                          zaf_district_grid[,c('code_area_id', 'name_district')],
                          by.x = 'area_id',
                          by.y = 'code_area_id',
                          all.x = TRUE)
## results_survey <- merge(results_survey, 
##                         zaf_district_grid[,c('code_area_id', 'name_district')],
##                         by.x = 'area_id',
##                         by.y = 'code_area_id',
##                         all.x = TRUE)

#########################
### Map of prevalence ###
#########################
# Subsetting results
tmp <- subset(results_agegroup_shp, year %in% c(2008, 2019) &
                age_group == '15-49' &
                area_level == 2 &
                model == 'With program data' &
                type %in% c('MC coverage','MMC coverage', 'TMC coverage'))

# Altering labels for the plot
tmp$type[which(tmp$type == 'MC coverage')] <- 'Total circumcision'
tmp$type[which(tmp$type == 'MMC coverage')] <- 'Medical circumcision'
tmp$type[which(tmp$type == 'TMC coverage')] <- 'Traditional circumcision'

# Ordering for the pltos 
tmp$type <-factor(tmp$type, levels = c('Total circumcision', 'Medical circumcision', 'Traditional circumcision'))

# Plotting to PDF

fig3 <- ggplot(tmp) +
  geom_sf(aes(fill = mean),
          linewidth = 0.2,
          colour = NA) +
  geom_sf(data = subset(area_boundaries, area_level == 1),
          colour = 'black',
          size = 0.2,
          fill = NA) +
  labs(fill = '') +
  scale_fill_gradientn(colours = colourPalette,
                       breaks = seq(0, 1, by = 0.1),
                       limits = c(0,1),
                       label = scales::label_percent(accuracy = 1),
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE,
                                              frame.colour = "black",
                                              ticks = TRUE,
                                              barheight = 1,
                                              barwidth = 20)) +
  facet_grid(type ~ year) +
  theme_minimal(9) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = rel(1.1), face = "bold"),
        legend.text = element_text(size = rel(1.0)),        
        panel.grid = element_blank(),
        legend.position = 'bottom')

ggsave("Figure3.pdf", fig3, width = 5.2, height = 6.8, units = "in")
ggsave("Figure3.png", fig3, width = 5.2, height = 6.8, units = "in")
ggsave("Figure3.eps", fig3, width = 5.2, height = 6.8, units = "in", device = cairo_ps)
