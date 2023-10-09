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

# Appending the grid together 
zaf_grid <- rbind.fill(zaf_district_grid, zaf_province_grid)

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_age1 <- read_csv('../src/Results_SingleAge_Probability.csv')
results_age2 <- read_csv('../src/Results_SingleAge_Incidence.csv')
results_age3 <- read_csv('../src/Results_SingleAge_Prevalence.csv')
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
# List of coverage estimates
test1 <- c('Probability of circumcision (MC)', 'Probability of circumcision (MMC)', 'Probability of circumcision (TMC)')
test2 <- c('MC', 'MMC', 'TMC')

for (i in 1:length(test1)){
  # Subsetting
  tmp1 <- subset(results_age,
                 type == test1[i] &
                   year %in% c(2008, 2014, 2019) &
                   area_level == 1 & 
                   model == 'With program data')
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
    scale_colour_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
    scale_fill_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
    # Plotting labels
    labs(x = 'Age',
         y = 'Probability of circumcision',
         colour = '',
         fill = '') +
    # Minimal theme
    theme_minimal() +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_province_grid, 
              label = "name_province")  +
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
  pdf(paste('Rates/Probabilityof', test2[i], '_SingleAge_Province.pdf', sep = ''), width = 12, height = 9)
  print(p1)
  dev.off()
}


for (i in 1:length(test1)){
  # Subsetting
  tmp1 <- subset(results_age,
                 type == test1[i] &
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
    scale_colour_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
    scale_fill_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
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
  pdf(paste('Rates/Probabilityof', test2[i], '_SingleAge_District.pdf', sep = ''), width = 24, height = 21)
  print(p1)
  dev.off()
}











