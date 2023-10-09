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

# Reading in results by age 
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Reading in survey results
results_survey <- read.csv("../src/zaf-survey-circumcision-coverage.csv") %>%
  filter(iso3 == 'ZAF')

# Renaming age group
results_survey$age1 <- as.numeric(substr(results_survey$age_group, 2, 4))
results_survey$age2 <- as.numeric(substr(results_survey$age_group, 7, 9))
results_survey$age2[results_survey$age2 != 99] <- paste0('-', results_survey$age2[results_survey$age2 != 99])
results_survey$age2[results_survey$age2 == 99] <- '+'
results_survey$age_group <- paste0(results_survey$age1, results_survey$age2)
results_survey$age1 <- NULL
results_survey$age2 <- NULL
results_survey$year <- results_survey$survey_mid_calendar_quarter

# Getting order of placement of districts within figures
tmp <- subset(area_hierarchy, area_level <= 2) %>%
  arrange(parent_area_id, area_name)

# Setting factor
results_age$area_name <- factor(results_age$area_name, levels = tmp$area_name)
results_agegroup$area_name <- factor(results_agegroup$area_name, levels = tmp$area_name)

# REmoving unecessary datasets
rm(tmp)

# Copying 2006 results for previous years
tmp <- subset(results_agegroup, year == 2006)
tmp$year = 2002
results_agegroup <- rbind(tmp, results_agegroup)
tmp$year = 2005
results_agegroup <- rbind(tmp, results_agegroup)

# Adding label-friendly district name 
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
results_survey <- merge(results_survey, 
                          zaf_district_grid[,c('code_area_id', 'name_district')],
                          by.x = 'area_id',
                          by.y = 'code_area_id',
                          all.x = TRUE)

# Getting year 
results_survey$year <- as.numeric(substr(results_survey$survey_id, 4, 7))

#################################
### Plotting total prevalence ###
#################################
# Preparing dataset for plots
tmp1 <- subset(results_agegroup,
               type == 'Circumcision coverage (MC)' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', 
                                  '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2 <- subset(results_survey,
               indicator == 'circumcised' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', 
                                  '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))

# Ordering age groups
tmp1$age_group <- factor(tmp1$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', 
                                                    '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2$age_group <- factor(tmp2$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', 
                                                    '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))

# # Converting to numeric for GGPLOT
tmp1$age_group <- as.numeric(tmp1$age_group)
tmp2$age_group <- as.numeric(tmp2$age_group)

# Looping for each year (district)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  p1 <- ggplot(data = subset(tmp1,
                             year == i &
                               model == 'With program data' &
                               area_level == 2),
               aes(x = age_group)) +
    geom_point(data = expand.grid(area_id = unique(subset(tmp1, area_level == 2)$area_id),
                                  age_group = unique(subset(tmp1, area_level == 2)$age_group),
                                  y = 100),
               aes(y = y),
               colour = 'white') + 
    geom_point(data = subset(tmp2, year == i &
                               area_level == 2),
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 2),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE)+
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_district_grid, 
              label = "name_district",
              scales = 'free') + 
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
          axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5),
          legend.position = 'bottom')
  # Adding survey points
  pdf(paste('ModelFit/TotalPrev_5year_District_', i, '_withsurveypoints.pdf', sep = ''), width = 24, height = 21)
  print(p1)
  dev.off()
}

# Looping for each year (provicne)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  test <- subset(tmp1,
                 year == i &
                   model =='With program data' &
                   area_level == 1)
  test2 <- subset(tmp2, year == i &
                    area_level == 1)
  # Altering province names for ordering
  test$area_name <- factor(test$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                      'Northern Cape', 'Free State', 'Mpumalanga',
                                                      'Western Cape', 'Eastern Cape', 'KwaZulu-Natal'))
  test2$area_name <- factor(test2$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                        'Northern Cape', 'Free State', 'Mpumalanga',
                                                        'Western Cape', 'Eastern Cape','KwaZulu-Natal'))
  p1 <- ggplot(data = test,
               aes(x = age_group)) +
    geom_point(data = subset(tmp2, year == i &
                               area_level == 1),
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 1),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE)+
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    scale_y_continuous(breaks = seq(0, 100,  by =  20),
                       limits = c(0, 100))+
    # Minimal theme
    theme_minimal() +
    # Extra options on the plot 
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 26, hjust = 0.5),
          # strip.placement = 'outside',
          legend.position = 'bottom') +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_province_grid, 
              label = "name_province") 
  # Adding survey points
  pdf(paste('ModelFit/TotalPrev_5year_Province_', i, '_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
  print(p1)
  dev.off()
}

# National
pdf(paste('ModelFit/TotalPrev_5year_National_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
ggplot(data = subset(tmp1,
                     year %in% c(2002, 2008, 2012, 2016, 2017) &
                       model == 'With program data' &
                       area_level == 0),
       aes(x = age_group)) +
  geom_ribbon(aes(ymin = 100*lower,
                  ymax = 100*upper),
              alpha = 0.75,
              colour = NA,
              fill = 'darkgrey') +
  geom_line(aes(y = 100*mean),
            linewidth = 1,
            colour = 'black') +
  geom_point(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                             area_level == 0),
             aes(y = 100*estimate),
             colour = 'black',
             show.legend = FALSE) +
  geom_errorbar(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                                area_level == 0),
                aes(ymin = 100*ci_lower,
                    ymax = 100*ci_upper),
                width = 0,
                colour = 'black',
                show.legend = FALSE)+
  # Labels
  labs(x =  'Age group',
       y = 'Circumcision coverage (%)',
       colour = '',
       fill = '') +
  scale_x_continuous(breaks = 1:12,
                     labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
  scale_y_continuous(breaks = seq(0, 100,  by =  10),
                     limits = c(0, 100))+
  theme_minimal() +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        strip.placement = "outside",
        legend.position = 'bottom') +
  facet_wrap(. ~ year,
             scales = 'free')
dev.off()

#################################
### Plotting MMCnT prevalence ###
#################################
# Preparing dataset for plots
tmp1 <- subset(results_agegroup,
               type == 'Circumcision coverage (MMC-nT)' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                  '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2 <- subset(results_survey,
                 indicator == 'circ_medical' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                  '35-39', '40-44', '45-49', '50-54', '55-59'))

# Ordering age groups
tmp1$age_group <- factor(tmp1$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                                    '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2$age_group <- factor(tmp2$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                                    '35-39', '40-44', '45-49', '50-54', '55-59'))

# # Converting to numeric for GGPLOT
tmp1$age_group <- as.numeric(tmp1$age_group)
tmp2$age_group <- as.numeric(tmp2$age_group)

# Looping for each year (district)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  p1 <- ggplot(data = subset(tmp1,
                             year == i &
                               model == 'With program data' &
                               area_level == 2),
               aes(x = age_group)) +
    geom_point(data = expand.grid(area_id = unique(subset(tmp1, area_level == 2)$area_id),
                                  age_group = unique(subset(tmp1, area_level == 2)$age_group),
                                  y = 100),
               aes(y = y),
               colour = 'white') + 
    geom_point(data = subset(tmp2, year == i &
                               area_level == 2),
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 2),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE) +
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_district_grid, 
              label = "name_district",
              scales = 'free') + 
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
          axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5),
          legend.position = 'bottom')
  # Adding survey points
  pdf(paste('ModelFit/MMCnTPrev_5year_District_', i, '_withsurveypoints.pdf', sep = ''), width = 24, height = 21)
  print(p1)
  dev.off()
}

# Looping for each year (provicne)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  test <- subset(tmp1,
                 year == i &
                   model == 'With program data' &
                   area_level == 1)
  test2 <- subset(tmp2, year == i &
                    area_level == 1)
  # Altering province names for ordering
  test$area_name <- factor(test$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                      'Northern Cape', 'Free State', 'Mpumalanga',
                                                      'Western Cape', 'Eastern Cape', 'KwaZulu-Natal'))
  test2$area_name <- factor(test2$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                        'Northern Cape', 'Free State', 'Mpumalanga',
                                                        'Western Cape', 'Eastern Cape','KwaZulu-Natal'))
  p1 <- ggplot(data = test,
               aes(x = age_group)) +
    geom_point(data = subset(tmp2, year == i &
                               area_level == 1),
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 1),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE)+
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    scale_y_continuous(breaks = seq(0, 100,  by =  20),
                       limits = c(0, 100))+
    # Minimal theme
    theme_minimal() +
    # Extra options on the plot 
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 26, hjust = 0.5),
          # strip.placement = 'outside',
          legend.position = 'bottom') +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_province_grid, 
              label = "name_province") 
  # Adding survey points
  pdf(paste('ModelFit/MMCnTPrev_5year_Province_', i, '_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
  print(p1)
  dev.off()
}

# National
pdf(paste('ModelFit/MMCnTPrev_5year_National_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
ggplot(data = subset(tmp1,
                     year %in% c(2002, 2008, 2012, 2016, 2017) &
                       model == 'With program data' &
                       area_level == 0),
       aes(x = age_group)) +
  geom_ribbon(aes(ymin = 100*lower,
                  ymax = 100*upper),
              alpha = 0.75,
              colour = NA,
              fill = 'darkgrey') +
  geom_line(aes(y = 100*mean),
            linewidth = 1,
            colour = 'black') +
  geom_point(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                             area_level == 0),
             aes(y = 100*estimate),
             colour = 'black',
             show.legend = FALSE) +
  geom_errorbar(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                                area_level == 0),
                aes(ymin = 100*ci_lower,
                    ymax = 100*ci_upper),
                width = 0,
                colour = 'black',
                show.legend = FALSE)+
  # Labels
  labs(x =  'Age group',
       y = 'Circumcision coverage (%)',
       colour = '',
       fill = '') +
  scale_x_continuous(breaks = 1:12,
                     labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
  scale_y_continuous(breaks = seq(0, 100,  by =  10),
                     limits = c(0, 100))+
  theme_minimal() +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        strip.placement = "outside",
        legend.position = 'bottom') +
  facet_wrap(. ~ year,
             scales = 'free')
dev.off()

#################################
### Plotting TMIC prevalence ###
#################################
# Preparing dataset for plots
tmp1 <- subset(results_agegroup,
               type == 'Circumcision coverage (TMIC)' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                  '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2 <- subset(results_survey,
               indicator == 'circ_traditional' &
                 age_group %in% c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                  '35-39', '40-44', '45-49', '50-54', '55-59'))

# Ordering age groups
tmp1$age_group <- factor(tmp1$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                                    '35-39', '40-44', '45-49', '50-54', '55-59'))
tmp2$age_group <- factor(tmp2$age_group, levels = c('0-4',   '5-9',   '10-14', '15-19', '20-24', '25-29', '30-34',
                                                    '35-39', '40-44', '45-49', '50-54', '55-59'))

# # Converting to numeric for GGPLOT
tmp1$age_group <- as.numeric(tmp1$age_group)
tmp2$age_group <- as.numeric(tmp2$age_group)

# Looping for each year (district)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  p1 <- ggplot(data = subset(tmp1,
                             year == i &
                               model == 'With program data' &
                               area_level == 2),
               aes(x = age_group)) +
    geom_point(data = expand.grid(area_id = unique(subset(tmp1, area_level == 2)$area_id),
                                  age_group = unique(subset(tmp1, area_level == 2)$age_group),
                                  y = 100),
               aes(y = y),
               colour = 'white') + 
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 2),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE)+
    geom_point(data = subset(tmp2, year == i &
                               area_level == 2),
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_district_grid, 
              label = "name_district",
              scales = 'free') + 
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
          axis.text.x = element_text(angle = 90, hjust = 0.5,vjust = 0.5),
          legend.position = 'bottom')
  # Adding survey points
  pdf(paste('ModelFit/TMICPrev_5year_District_', i, '_withsurveypoints.pdf', sep = ''), width = 24, height = 21)
  print(p1)
  dev.off()
}

# Looping for each year (provicne)
for (i in c(2002, 2008, 2012, 2016, 2017)) {
  test <- subset(tmp1,
                 year == i &
                   model == 'With program data' &
                   area_level == 1)
  test2 <- subset(tmp2, year == i &
           area_level == 1)
  # Altering province names for ordering
  test$area_name <- factor(test$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                      'Northern Cape', 'Free State', 'Mpumalanga',
                                                      'Western Cape', 'Eastern Cape', 'KwaZulu-Natal'))
  test2$area_name <- factor(test2$area_name, levels = c('North West', 'Gauteng', 'Limpopo',
                                                      'Northern Cape', 'Free State', 'Mpumalanga',
                                                      'Western Cape', 'Eastern Cape','KwaZulu-Natal'))
  p1 <- ggplot(data = test,
               aes(x = age_group)) +
    geom_point(data = test2,
               aes(y = 100*estimate),
               colour = 'black',
               show.legend = FALSE) +
    geom_errorbar(data = subset(tmp2, year == i &
                                  area_level == 1),
                  aes(ymin = 100*ci_lower,
                      ymax = 100*ci_upper),
                  width = 0,
                  colour = 'black',
                  show.legend = FALSE)+
    geom_ribbon(aes(ymin = 100*lower,
                    ymax = 100*upper),
                alpha = 0.75,
                colour = NA,
                fill = 'darkgrey') +
    geom_line(aes(y = 100*mean),
              linewidth = 1,
              colour = 'black') +
    # Labels
    labs(x =  'Age group',
         y = 'Circumcision coverage (%)',
         colour = '',
         fill = '') +
    scale_x_continuous(breaks = 1:12,
                       labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
    scale_y_continuous(breaks = seq(0, 100,  by =  20),
                       limits = c(0, 100))+
    # Minimal theme
    theme_minimal() +
    # Extra options on the plot 
    theme(axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          strip.background = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 26, hjust = 0.5),
          # strip.placement = 'outside',
          legend.position = 'bottom') +
    # Geofacet
    facet_geo(~ area_id, 
              grid = zaf_province_grid, 
              label = "name_province") 
  # Adding survey points
  pdf(paste('ModelFit/TMICPrev_5year_Province_', i, '_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
  print(p1)
  dev.off()
}

# National
pdf(paste('ModelFit/TMICPrev_5year_National_withsurveypoints.pdf', sep = ''), width = 12, height = 9)
ggplot(data = subset(tmp1,
                     year %in% c(2002, 2008, 2012, 2016, 2017) &
                       model == 'With program data' &
                       area_level == 0),
       aes(x = age_group)) +
  geom_ribbon(aes(ymin = 100*lower,
                  ymax = 100*upper),
              alpha = 0.75,
              colour = NA,
              fill = 'darkgrey') +
  geom_line(aes(y = 100*mean),
            linewidth = 1,
            colour = 'black') +
  geom_point(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                             area_level == 0),
             aes(y = 100*estimate),
             colour = 'black',
             show.legend = FALSE) +
  geom_errorbar(data = subset(tmp2, year %in% c(2002, 2008, 2012, 2016, 2017) &
                                area_level == 0),
                aes(ymin = 100*ci_lower,
                    ymax = 100*ci_upper),
                width = 0,
                colour = 'black',
                show.legend = FALSE)+
  # Labels
  labs(x =  'Age group',
       y = 'Circumcision coverage (%)',
       colour = '',
       fill = '') +
  scale_x_continuous(breaks = 1:12,
                     labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59'))  +
  scale_y_continuous(breaks = seq(0, 100,  by =  10),
                     limits = c(0, 100))+
  theme_minimal() +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26, hjust = 0.5),
        strip.placement = "outside",
        legend.position = 'bottom') +
  facet_wrap(. ~ year,
             scales = 'free')
dev.off()





