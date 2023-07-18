#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Loading source code 
source('../src/0_Source.R')

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

##########################
### Reading in results ###
##########################

# Read in results 
results_survey <- read.csv("../src/zaf-survey-circumcision-coverage.csv") %>%
  filter(age_group == 'Y015_049', year >= 2005 & indicator == "circumcised")

results_survey <- results_survey %>%
  mutate(type = recode(indicator, "circumcised " = "MC coverage"))

# Reading in model results by age group
results_agegroup1 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)
results_agegroup <- results_agegroup %>%
  subset(age_group == '15-49' & type %in% c('MC coverage'))

# Adding label-friendly district name 
results_survey <- merge(results_survey, 
                        zaf_district_grid[,c('code_area_id', 'name_district')],
                        by.x = 'area_id',
                        by.y = 'code_area_id',
                        all.x = TRUE)

results_agegroup <- merge(results_agegroup, 
                          zaf_district_grid[,c('code_area_id', 'name_district')],
                          by.x = 'area_id',
                          by.y = 'code_area_id',
                          all.x = TRUE)

# Getting relevant columns 
results_agegroup <- results_agegroup[,c('area_id','area_name', 'area_level', 'name_district', 'model', 'type','year','mean', 'lower', 'upper')]

# Appending results 
## results <- results_agegroup %>%
##   left_join(results_survey[,c('area_id','year', 'type','p_ind')])

results <- results_agegroup

#######################################
### Plotting results (DMPPT2, total)###
#######################################

tmp1 <- subset(results, area_level == 1 & year %in% 2008:2019)
# Multiplying coverage by 100
tmp1$mean <- 100 * tmp1$mean
tmp1$lower <- 100 * tmp1$lower
tmp1$upper <- 100 * tmp1$upper
## tmp1$p_ind <- 100 * tmp1$p_ind

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



## # Altering province names for ordering
## tmp1$area_name[which(tmp1$area_name == 'North West')] <- 'A'
## tmp1$area_name[which(tmp1$area_name == 'Gauteng')] <- 'B'
## tmp1$area_name[which(tmp1$area_name == 'Limpopo')] <- 'C'
## tmp1$area_name[which(tmp1$area_name == 'Northern Cape')] <- 'D'
## tmp1$area_name[which(tmp1$area_name == 'Free State')] <- 'E'
## tmp1$area_name[which(tmp1$area_name == 'Mpumalanga')] <- 'F'
## tmp1$area_name[which(tmp1$area_name == 'Western Cape')] <- 'G'
## tmp1$area_name[which(tmp1$area_name == 'Eastern Cape')] <- 'H'
## tmp1$area_name[which(tmp1$area_name == 'KwaZulu-Natal')] <- 'I'


tmp1$model[which(tmp1$model == 'No program data')] <- 'Survey data only'
tmp1$model[which(tmp1$model == 'With program data')] <- 'With programme data'

survey_plot <- results_survey %>%
  semi_join(tmp1, by = "area_id")


fig7p <- ggplot(tmp1,
       aes(x = year,
           y = mean,
           ymin = lower,
           ymax = upper,
           group = model,
           fill = model,
           colour = model)) + 
  # Adding target line to prevalence
  geom_hline(yintercept = 80,
             linewidth = 0.5,
             linetype = 'dashed',
             colour = 'grey50') +
  # Lines of coverage
  geom_ribbon(colour = NA, alpha = 0.5) + 
  geom_line(linewidth = 0.5) + 
  geom_point(aes(x = year, y = 100 * estimate),
             data = survey_plot,             
             colour = 'black',
             size = 1.0,
             inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_linerange(aes(x = year, ymin = 100 * ci_lower, ymax = 100 * ci_upper),
             data = survey_plot,             
             colour = 'black',
             inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_text(aes(x = 2008, y = 100, label = area_name),
            data = distinct(tmp1, area_id, area_name),
            hjust = 0, vjust = 1,
            size = 3.0,
            fontface = "bold",
            inherit.aes = FALSE) +
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2018, by = 2),
                     labels = c("2008", "", "2012", "", "2016", "")) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(0, 100)) + 
  # Colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  labs(x = NULL,
       y = 'Circumcision coverage (%)',
       colour = NULL,
       fill = NULL) + 
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_province_grid, 
            label = "name_province") +
  # Minimal theme
  theme_bw(8) +
  # Extra options on the plot 
  theme(axis.text = element_text(size = 8),
        ## strip.text = element_text(size = 9, face = "bold"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        # strip.placement = 'outside',
        legend.position = 'none')+ 
  guides(color=guide_legend(ncol=2))


fig7l <- {fig7p +
  theme(legend.position = "right")} %>%
  ggpubr::get_legend()

library(patchwork)

fig7 <- wrap_elements(full = get_geofacet_grob(fig7p)) +
  inset_element(fig7l, 0.7, 0.05, 1.0, 0.23, align_to = "full", on_top = TRUE)

  
ggsave("Figure7.pdf", width = 5.2, height = 5.3, units = "in")
ggsave("Figure7.png", width = 5.2, height = 5.3, units = "in")
ggsave("Figure7.eps", width = 5.2, height = 5.3, units = "in", device = cairo_ps)
