#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

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


######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_age1 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Probability.csv'))
results_age2 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Incidence.csv'))
results_age3 <- read_csv(paste0('~/Downloads/', version, '/Results_Age_Prevalence.csv'))
results_age <- rbind(results_age1, results_age2, results_age3)

rm(results_age1, results_age2, results_age3)

# Adding label-freindly district name 
results_age <- merge(results_age, 
                     zaf_district_grid[,c('code_area_id', 'name_district')],
                     by.x = 'area_id',
                     by.y = 'code_area_id',
                     all.x = TRUE)

#####################
### Creating plot ###
#####################
# Keeping relevant information
tmp <- results_age %>%
  # Only keeping relevant data
  filter(type %in% c("MMC-nTs performed", "TMICs performed") &
           area_level %in% 0:1 &
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
tmp$type[which(tmp$type == "MMC-nTs performed")] <- 'Medical'
tmp$type[which(tmp$type == "TMICs performed")] <- 'Traditional'


tmp2 <- tmp %>%
  group_by(area_id, area_name, type, year) %>%
  summarise(average_age = weighted.mean(age, w = density),
            average_age_lower = weighted.mean(age, w = lower),
            average_age_upper = weighted.mean(age, w = upper) 
  )

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

test <- subset(tmp, age == 9 & type == 'Medical')

test$age <- test$age + 1 - 1E-6
tmp <- rbind(tmp, test)

fig6 <- ggplot(subset(tmp, age <=30 & year == 2018), 
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
  geom_point(data = subset(tmp2, year == 2018),
             aes(x = average_age, 
                 y = as.integer(area_name) - 0.05, 
                 color = type),
             inherit.aes = FALSE, 
             show.legend = FALSE) +
  # Adding uncertainty interval of average age of circumcision
  geom_segment(data = subset(tmp2, year == 2018), 
               aes(x = average_age_lower, 
                   xend = average_age_upper, 
                   y = as.integer(area_name)-0.05, 
                   yend = as.integer(area_name)-0.05, 
                   color = type),
               inherit.aes = FALSE, 
               show.legend = FALSE) +
  # Colour palette
  scale_fill_manual("Circumcision type:", values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_colour_manual("Circumcision type:", values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_y_discrete(expand = expansion(0, c(0.2, 0.9))) +
  scale_x_continuous(expand = expansion(c(0, 0.01), 0)) +
  # Setting theme
  theme_minimal(8) + 
  # Splitting by circumcision type
  # facet_grid(. ~ type) + 
  # Setting labels
  labs(y = NULL, 
       x = "Age at circumcision", 
       color = NULL, 
       fill = NULL) +
  coord_cartesian(clip = "off") +
  # Changing plot themes 
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.text.y = element_text(face = c(rep('plain', 9), 'bold'), vjust = 0),
    axis.ticks = element_line(colour = "grey20"),
    axis.title.x = element_text(face = "bold", hjust = 0),
    strip.background = element_blank(), 
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = 'bottom',
    panel.spacing = unit(0.2, "lines"))

ggsave("Figure6.pdf", fig6, height = 4.5, width = 4.5, units = "in")
ggsave("Figure6.png", fig6, height = 4.5, width = 4.5, units = "in")
ggsave("Figure6.eps", fig6, height = 4.5, width = 4.5, units = "in", device = cairo_ps)
