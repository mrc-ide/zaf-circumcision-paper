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
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
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
               type %in% c('Circumcision coverage (MC)') &
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
               type %in% c('Circumcision coverage (MMC-nT)', 'Circumcision coverage (MMC-T)', 'Circumcision coverage (TMC)') &
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
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-nT)'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-T)'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (TMC)'))] <- 'TMC'


# Appending datasets together 
tmp <- rbind.fill(tmp2)


pdf('Coverage/Coverage_1549_District.pdf', width = 24, height = 21)
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
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3),
                    labels = c("MMC-nT" = "Medical (not trad. setting; MMC-nT)",
                               "MMC-T" = "Med. in trad. setting (MMC-T)",
                               "TMC" = "Traditional (TMC)")) + 
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
               type %in% c('Circumcision coverage (MC)') &
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
               type %in% c('Circumcision coverage (MMC-nT)', 'Circumcision coverage (MMC-T)', 'Circumcision coverage (TMC)') &
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
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-nT)'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-T)'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (TMC)'))] <- 'TMC'

# Appending datasets together 
tmp <- rbind.fill(tmp2)

# GGPLOT
pdf('Coverage/Coverage_1549_Province.pdf', width = 12, height = 9)
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
  # Adding percentages labels
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
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_province_grid, 
            label = "name_province") +  
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2019, by = 2),
                     limits = c(2007.5, 2019.5)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     limits = c(0, 108)) + 
  # Setting colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3),
                    labels = c("MMC-nT" = "Medical (not trad. setting; MMC-nT)",
                               "MMC-T" = "Med. in trad. setting (MMC-T)",
                               "TMC" = "Traditional (TMC)")) + 
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





















