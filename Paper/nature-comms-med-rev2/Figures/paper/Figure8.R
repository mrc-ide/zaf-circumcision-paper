#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggridges)

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
  mutate(space = seq(dplyr::n())) %>%
  ungroup()

# Adding a unique identifier within Admin code and merging to boundaries
area_boundaries <- area_hierarchy %>% 
  group_by(area_level) %>% 
  mutate(space = seq(dplyr::n())) %>% 
  left_join(x =  area_boundaries,
            by = 'area_id') %>%
  ungroup()

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_age1 <- read_csv('../src/Results_SingleAge_Probability.csv')
results_age2 <- read_csv('../src/Results_SingleAge_Incidence.csv')
results_age3 <- read_csv('../src/Results_SingleAge_Prevalence.csv')
results_age <- rbind(results_age1, results_age2, results_age3)
rm(results_age1, results_age2, results_age3)

# Loading in the average age at circumcision 
results_aac <- read_csv('../src/Results_AverageAgeAtCircumcision.csv')

#####################
### Creating plot ###
#####################
# Keeping relevant information
tmp <- results_age %>%
  # Only keeping relevant data
  filter(type %in% c("Number of circumcisions performed (MMC-nT)", 
                     "Number of circumcisions performed (TMIC)") &
           area_level %in% 0:1 &
           year == 2018 & 
           model == "With program data") 

# Getting density for the ridge plot
tmp <- tmp %>%  
  # Grouping for normalising
  group_by(area_id, year, type) %>%
  # Estimating density
  mutate(density = mean /(2*sum(mean))) %>%
  # Ungrouping
  ungroup() 

# Altering labels for the plot
tmp$type[which(tmp$type == "Number of circumcisions performed (MMC-nT)")] <- 'Medical'
tmp$type[which(tmp$type == "Number of circumcisions performed (TMIC)")] <- 'Traditional'

# Keepingn revelant average age at circumcision 
tmp2 <- results_aac %>%
  # Only keeping relevant data
  filter(area_level %in% 0:1 &
           year == 2018 &
           type %in% c('Average age at circumcision (MMC-nTs)',
                       'Average age at circumcision (TMIC)') &
           model == "With program data") 
  
# Altering labels for the plot
tmp2$type[which(tmp2$type == "Average age at circumcision (MMC-nT)")] <- 'Medical'
tmp2$type[which(tmp2$type == "Average age at circumcision (TMIC)")] <- 'Traditional'

# Ordering the provinces
tmp$area_name <- factor(tmp$area_name,
                        levels = rev(c("South Africa", "Limpopo", "Mpumalanga", "Gauteng", 
                                       "North West","KwaZulu-Natal", "Free State",
                                       "Northern Cape", "Eastern Cape", "Western Cape")))

# Ordering the provinces
tmp2$area_name <- factor(tmp2$area_name,
                         levels = rev(c("South Africa", "Limpopo", "Mpumalanga", "Gauteng", 
                                        "North West","KwaZulu-Natal", "Free State",
                                        "Northern Cape", "Eastern Cape", "Western Cape")))

# Fixing 9 so that it has a step at 9/10
test <- subset(tmp, age == 9 & type == 'Medical')
test$age <- test$age + 1 - 1E-6
tmp <- rbind(tmp, test)

# Removing uncessary datasets
rm(test)

# Plotting
fig8 <- ggplot(subset(tmp, age <=30),
               aes(x = age, 
                   y = area_name, 
                   height = density, 
                   fill = type, 
                   color = type)) +
  geom_density_ridges(stat = "identity", 
                      scale = 1, 
                      alpha = 0.7, 
                      color = NA)  +
  # Adding average age of circumcision
  geom_point(data = tmp2, 
             aes(x = mean, 
                 y = as.integer(area_name) - 0.05, 
                 color = type),
             inherit.aes = FALSE, 
             show.legend = FALSE) +
  # Adding uncertainty interval of average age of circumcision
  geom_segment(data = tmp2, 
               aes(x = lower, 
                   xend = upper, 
                   y = as.integer(area_name)-0.05, 
                   yend = as.integer(area_name)-0.05, 
                   color = type),
               inherit.aes = FALSE, 
               show.legend = FALSE) +
  # Colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_y_discrete(expand = expansion(0, c(0.2, 0.9))) +
  scale_x_continuous(expand = expansion(c(0, 0.01), 0),
                     breaks = seq(0, 30, by = 5)) +
  # Setting theme
  theme_minimal(8) + 
  # Splitting by circumcision type
  # facet_grid(. ~ type) + 
  # Setting labels
  labs(y = NULL, 
       x = "Age at circumcision (years)", 
       color = NULL, 
       fill = NULL) +
  coord_cartesian(clip = "off") +
  # Changing plot themes 
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.text.y = element_text(face = c(rep('plain', 9), 'bold'), vjust = 0),
    axis.ticks = element_line(colour = "grey20"),
    axis.title.x = element_text(face = "bold"),
    strip.background = element_blank(), 
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = 'bottom',
    panel.spacing = unit(0.2, "lines"))

ggsave("Figure8.pdf", fig8, height = 4.5, width = 4.5, units = "in")
ggsave("Figure8.png", fig8, height = 4.5, width = 4.5, units = "in")
ggsave("Figure8.eps", fig8, height = 4.5, width = 4.5, units = "in", device = cairo_ps)
