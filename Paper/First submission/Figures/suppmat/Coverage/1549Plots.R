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

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- subset(results_agegroup,
               type %in% c('MC coverage') &
                 age_group == '15-49' & 
                 year %in% c(2008:2019) &
                 area_level == 2 & 
                 model == 'With program data') %>%
  dplyr::rename(mean   = mean, 
                sd.y     = sd, 
                median.y = median, 
                lower.y  = lower, 
                upper.y  = upper)

# Subsetting
tmp2 <- subset(results_agegroup,
               type %in% c('MMC-nT coverage', 'MMC-T coverage', 'TMC coverage') &
                 age_group == '15-49' & 
                 year %in% c(2008:2019) &
                 area_level == 2 & 
                 model == 'With program data') %>%
  dplyr::rename(mean.y   = mean, 
                sd.y     = sd, 
                median.y = median, 
                lower.y  = lower, 
                upper.y  = upper)

# Relabelling for plot 
tmp2$type[which(tmp2$type %in% c('MMC-nT coverage'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('MMC-T coverage'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('TMC coverage'))] <- 'TMC'


# Appending datasets together 
tmp <- rbind.fill(tmp2)


pdf('Documents/Survival/Figures/suppmat/Coverage/Coverage_1549_District.pdf', width = 24, height = 21)
ggplot(tmp, 
       aes(x = year,
           group = type,
           fill = type)) + 
  # Adding target line to prevalence
  geom_hline(yintercept = 80,
             size = 2,
             linetype = 'dashed',
             colour = 'grey50') +
  # Prevalence as area plot
  geom_area(aes(y = 100 * mean.y)) +
  
  geom_text(data = tmp1,
            aes(x = 2008,
                y = 100*mean,
                fill = NA,
                label = if_else(year %in% c(2008), percent(mean, 1), NA_character_)),
            fontface = "bold",
            vjust = 0,
            color = "grey30",
            size = 6,
            nudge_y = 0.02,
            show.legend = FALSE) +
  geom_text(data = tmp1,
            aes(x = 2019,
                y = 100*mean,
                fill = NA,
                label = if_else(year %in% c(2019), percent(mean, 1), NA_character_)),
            fontface = "bold",
            vjust = 0,
            color = "grey30",
            size = 6,
            nudge_y = 0.02,
            show.legend = FALSE) +
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2019, by = 2),
                     limits = c(2007, 2020)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     limits = c(0, 108)) + 
  # Setting colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
  # Plotting labels
  labs(x = 'Year',
       y = 'Circumcision coverage (%)',
       fill = '') +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_district_grid, 
            label = "name_district") + 
  # Minimal theme
  theme_minimal() +
  # Altering plot text size
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title = element_text(size = 28),
        panel.grid = element_blank(),
        plot.title = element_text(size = 40, hjust = 0.5),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1),
        legend.position = 'bottom')
dev.off()

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- subset(results_agegroup,
               type %in% c('MC coverage') &
                 age_group == '15-49' & 
                 year %in% c(2008:2019) &
                 area_level == 1 & 
                 model == 'With program data') %>%
  dplyr::rename(mean   = mean, 
                sd.y     = sd, 
                median.y = median, 
                lower.y  = lower, 
                upper.y  = upper)

# Subsetting
tmp2 <- subset(results_agegroup,
               type %in% c('MMC-nT coverage', 'MMC-T coverage', 'TMC coverage') &
                 age_group == '15-49' & 
                 year %in% c(2008:2019) &
                 area_level == 1 & 
                 model == 'With program data') %>%
  dplyr::rename(mean.y   = mean, 
                sd.y     = sd, 
                median.y = median, 
                lower.y  = lower, 
                upper.y  = upper)

# Relabelling for plot 
tmp2$type[which(tmp2$type %in% c('MMC-nT coverage'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('MMC-T coverage'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('TMC coverage'))] <- 'TMC'


# Appending datasets together 
tmp <- rbind.fill(tmp2)

# Altering province names for ordering
tmp$area_name[which(tmp$area_name == 'North West')] <- 'A'
tmp$area_name[which(tmp$area_name == 'Gauteng')] <- 'B'
tmp$area_name[which(tmp$area_name == 'Limpopo')] <- 'C'
tmp$area_name[which(tmp$area_name == 'Northern Cape')] <- 'D'
tmp$area_name[which(tmp$area_name == 'Free State')] <- 'E'
tmp$area_name[which(tmp$area_name == 'Mpumalanga')] <- 'F'
tmp$area_name[which(tmp$area_name == 'Western Cape')] <- 'G'
tmp$area_name[which(tmp$area_name == 'Eastern Cape')] <- 'H'
tmp$area_name[which(tmp$area_name == 'KwaZulu-Natal')] <- 'I'
tmp1$area_name[which(tmp1$area_name == 'North West')] <- 'A'
tmp1$area_name[which(tmp1$area_name == 'Gauteng')] <- 'B'
tmp1$area_name[which(tmp1$area_name == 'Limpopo')] <- 'C'
tmp1$area_name[which(tmp1$area_name == 'Northern Cape')] <- 'D'
tmp1$area_name[which(tmp1$area_name == 'Free State')] <- 'E'
tmp1$area_name[which(tmp1$area_name == 'Mpumalanga')] <- 'F'
tmp1$area_name[which(tmp1$area_name == 'Western Cape')] <- 'G'
tmp1$area_name[which(tmp1$area_name == 'Eastern Cape')] <- 'H'
tmp1$area_name[which(tmp1$area_name == 'KwaZulu-Natal')] <- 'I'

# GGPLOT
pdf('Documents/Survival/Figures/suppmat/Coverage/Coverage_1549_Province.pdf', width = 12, height = 9)
ggplot(tmp, 
       aes(x = year,
           group = type,
           fill = type)) + 
  # Adding target line to prevalence
  geom_hline(yintercept = 80,
             size = 2,
             linetype = 'dashed',
             colour = 'grey50') +
  # Prevalence as area plot
  geom_area(aes(y = 100 * mean.y)) +
  
  geom_text(data = tmp1,
            aes(x = 2008,
                y = 100*mean,
                fill = NA,
                label = if_else(year %in% c(2008), percent(mean, 1), NA_character_)),
            fontface = "bold",
            vjust = 0,
            color = "grey30",
            size = 6,
            nudge_y = 0.02,
            show.legend = FALSE) +
  geom_text(data = tmp1,
            aes(x = 2019,
                y = 100*mean,
                fill = NA,
                label = if_else(year %in% c(2019), percent(mean, 1), NA_character_)),
            fontface = "bold",
            vjust = 0,
            color = "grey30",
            size = 6,
            nudge_y = 0.02,
            show.legend = FALSE) +
  # Facet wrapping
  facet_wrap(. ~ area_name,
             labeller = as_labeller(c(A = 'North West',
                                      B = 'Gauteng', 
                                      C = 'Limpopo',
                                      D = 'Northern Cape', 
                                      E = 'Free State', 
                                      F = 'Mpumalanga',
                                      G = 'Western Cape', 
                                      H = 'Eastern Cape', 
                                      I = 'KwaZulu-Natal')))+  
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2019, by = 2),
                     limits = c(2007.5, 2019.5)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     limits = c(0, 108)) + 
  # Setting colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
  # Plotting labels
  labs(x = 'Year',
       y = 'Circumcision coverage (%)',
       fill = '') +
  # Minimal theme
  theme_minimal() +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        # strip.placement = 'outside',
        legend.position = 'bottom')
dev.off()





















