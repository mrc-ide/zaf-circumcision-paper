#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')

# Colours for the plot 
colourPalette <- rev(colorRampPalette(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2'))(100))

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

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Merging to shapefiles 
results_agegroup_shp <- results_agegroup %>%
  left_join(x = area_boundaries)

# Removing unecessary datasets
rm(tmp)

#########################
### Map of prevalence ###
#########################
# Subsetting results
tmp <- subset(results_agegroup_shp, year %in% c(2008, 2019) &
                age_group == '15-49' &
                area_level == 2 &
                model == 'With program data' &
                type %in% c('Circumcision coverage (MC)',
                            'Circumcision coverage (MMC)', 
                            'Circumcision coverage (TMC)'))

# Altering labels for the plot
tmp$type[which(tmp$type == 'Circumcision coverage (MC)')] <- 'Total circumcision'
tmp$type[which(tmp$type == 'Circumcision coverage (MMC)')] <- 'Medical circumcision'
tmp$type[which(tmp$type == 'Circumcision coverage (TMC)')] <- 'Traditional circumcision'

# Ordering for the pltos 
tmp$type <-factor(tmp$type, levels = c('Total circumcision', 'Medical circumcision', 'Traditional circumcision'))

# Plotting
fig5 <- ggplot(tmp) +
  geom_sf(aes(fill = mean),
          linewidth = 0.2,
          colour = NA) +
  geom_sf(data = subset(area_boundaries, area_level == 1),
          colour = 'black',
          size = 0.2,
          fill = NA) +
  labs(fill = '') +
  scale_fill_gradientn(colours = colourPalette, #rev(MetBrewer::met.brewer("Homer1", 100)),
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

# Saving plot
ggsave("Figure5.pdf", fig5, width = 5.2, height = 6.8, units = "in")
ggsave("Figure5.png", fig5, width = 5.2, height = 6.8, units = "in")
ggsave("Figure5.eps", fig5, width = 5.2, height = 6.8, units = "in", device = cairo_ps)
