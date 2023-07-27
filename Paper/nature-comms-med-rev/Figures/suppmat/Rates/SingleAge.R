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
results_age1 <- read_csv(paste('Output/Analysis/', version, '/Results_Age_Probability.csv', sep = ''))
results_age2 <- read_csv(paste('Output/Analysis/', version, '/Results_Age_Incidence.csv', sep = ''))
results_age3 <- read_csv(paste('Output/Analysis/', version, '/Results_Age_Prevalence.csv', sep = ''))
results_age <- rbind(results_age1, results_age2, results_age3)
rm(results_age1, results_age2, results_age3)

# Adding label-freindly district name 
results_age <- merge(results_age, 
                     zaf_district_grid[,c('code_area_id', 'name_district')],
                     by.x = 'area_id',
                     by.y = 'code_area_id',
                     all.x = TRUE)

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
for (i in c('Probability of MC', 'Probability of MMC', 'Probability of TMC')){
  # Subsetting
  tmp1 <- subset(results_age,
                 type == i &
                   year %in% c(2008, 2014, 2019) &
                   area_level == 1 & 
                   model == 'With program data')
  # Altering province names for ordering
  tmp1$area_name[which(tmp1$area_name == 'North West')] <- 'A'
  tmp1$area_name[which(tmp1$area_name == 'Gauteng')] <- 'B'
  tmp1$area_name[which(tmp1$area_name == 'Limpopo')] <- 'C'
  tmp1$area_name[which(tmp1$area_name == 'Northern Cape')] <- 'D'
  tmp1$area_name[which(tmp1$area_name == 'Free State')] <- 'E'
  tmp1$area_name[which(tmp1$area_name == 'Mpumalanga')] <- 'F'
  tmp1$area_name[which(tmp1$area_name == 'Western Cape')] <- 'G'
  tmp1$area_name[which(tmp1$area_name == 'Eastern Cape')] <- 'H'
  tmp1$area_name[which(tmp1$area_name == 'KwaZulu-Natal')] <- 'I'
  
  p1 <- ggplot(tmp1, 
         aes(x = age,
             group = as.factor(year),
             fill = as.factor(year),
             colour = as.factor(year))) +
    # Prevalence as area plot
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                colour = NA,
                alpha = 0.5) +
    # Prevalence as area plot
    geom_line(aes(y = mean),
              size = 1) +
    # Setting for the axes
    scale_x_continuous(breaks = seq(0, 60, by = 10)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(0, 0.4)) + 
    # Setting colour palette
    scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
    # Plotting labels
    labs(x = 'Age',
         y = 'Probability of circumcision',
         colour = '',
         fill = '') +
    # Minimal theme
    theme_minimal() +
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
                                        I = 'KwaZulu-Natal')),
               scales = 'free')+  
    # Extra options on the plot 
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 26, hjust = 0.5),
          strip.placement = 'outside',
          legend.position = 'bottom')
  # Outputting plot
  pdf(paste('Documents/Survival/Figures/suppmat/Rates/', gsub(' ', '', i), '_SingleAge_Province.pdf', sep = ''), width = 12, height = 9)
  print(p1)
  dev.off()
}


for (i in c('Probability of MC', 'Probability of MMC', 'Probability of TMC')){
  # Subsetting
  tmp1 <- subset(results_age,
                 type == i &
                   year %in% c(2008, 2014, 2019) &
                   area_level == 2 & 
                   model == 'With program data')
  
  p1 <- ggplot(tmp1, 
               aes(x = age,
                   group = as.factor(year),
                   fill = as.factor(year),
                   colour = as.factor(year))) +
    geom_point(data = expand.grid(age = 0,
                                  area_id = unique(tmp1$area_id),
                                  year = 2008,
                                  mean = c(0,max(tmp1$upper))),
               aes(y = mean),
               colour = 'white') + 
    # Prevalence as area plot
    geom_ribbon(aes(ymin = lower,
                    ymax = upper),
                colour = NA,
                alpha = 0.5) +
    # Prevalence as area plot
    geom_line(aes(y = mean),
              size = 1) +
    # Setting for the axes
    scale_x_continuous(breaks = seq(0, 60, by = 10)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(5)) + 
    # Setting colour palette
    scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
    # Plotting labels
    labs(x = 'Age',
         y = 'Probability of circumcision',
         colour = '',
         fill = '') +
    # Minimal theme
    theme_minimal()  +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_district_grid, 
              label = "name_district",
              scales = 'free') +
    # Altering plot text size
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_text(size = 28),
          plot.title = element_text(size = 40, hjust = 0.5),
          legend.text = element_text(size = 24),
          legend.position = 'bottom')
  # Outputting plot
  pdf(paste('Documents/Survival/Figures/suppmat/Rates/', gsub(' ', '', i), '_SingleAge_District.pdf', sep = ''), width = 24, height = 21)
  print(p1)
  dev.off()
}











