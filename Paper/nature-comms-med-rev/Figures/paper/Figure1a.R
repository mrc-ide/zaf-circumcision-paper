#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev/Figures/paper')

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
  dplyr::mutate(space = seq(dplyr::n())) %>% 
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
# Reading in model results by age group
results_agegroup1 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Adding label-freindly district name 
results_agegroup <- merge(results_agegroup, 
                          zaf_district_grid[,c('code_area_id', 'name_district')],
                          by.x = 'area_id',
                          by.y = 'code_area_id',
                          all.x = TRUE)

#############################################
### Preparing survey results for plotting ###
#############################################
# Read in results 
results_survey <- read.csv("../src/zaf-survey-circumcision-coverage.csv") %>%
  filter(age_group == 'Y015_049', year >= 2005 & indicator == "circumcised")

results_survey <- results_survey %>%
  mutate(type = recode(indicator, "circumcised " = "MC coverage"))

# Adding label-friendly district name 
results_survey <- merge(results_survey, 
                        zaf_district_grid[,c('code_area_id', 'name_district')],
                        by.x = 'area_id',
                        by.y = 'code_area_id',
                        all.x = TRUE)

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- subset(results_agegroup,
               type == 'MC coverage' &
                 age_group == '15-49' & 
                 year %in% c(2008:2019) &
                 area_level == 2) 

# Renaming model label
tmp1$model[which(tmp1$model == 'No program data')] <- 'Survey data only'
tmp1$model[which(tmp1$model == 'With program data')] <- 'With programme data'

# Multiplying prevalence by 100
tmp1$mean <- 100 * tmp1$mean
tmp1$lower <- 100 * tmp1$lower
tmp1$upper <- 100 * tmp1$upper

survey_plot <- results_survey %>%
  semi_join(tmp1, by = "area_id")

# Recode district names
district_recode <- c("Ngaka Modiri Molema" = "N M Molema",
                     "Ruth Segomotsi Mompati" = "R S Mompati",
                     "Bojanala Platinum" = "Bojanala Plat.")

# Recode district names
zaf_district_grid <- zaf_district_grid %>%
  mutate(name_district = recode(name_district, !!!district_recode))

# Creating a fill palette
fill_palette <- c(setNames(MetBrewer::met.brewer("Archambault", 3)[c(1, 2)], c('With programme data', 'Survey data only')),
                  province_palette,
                  " " = NA, "  " = NA, "   " = NA)

# Factor for the model and district plots
tmp1$model <- factor(tmp1$model, names(fill_palette))
zaf_district_grid$name_province <- factor(zaf_district_grid$name_province, names(fill_palette))


fig7ap <-
  ggplot(tmp1, 
             aes(x = year,
                 group = as.factor(model),
                 fill = as.factor(model),
                 colour = as.factor(model))) +
  # Background fill
  geom_area(aes(y = y1, fill = name_province, group = NULL, colour = NULL),
            data = zaf_district_grid %>%
              mutate(area_id = code_area_id) %>%
              crossing(data.frame(year = c(2008, 2019), y1 = 100)),
            alpha = 0.2, show.legend = FALSE) +
  # Adding target line to prevalence
  annotate("segment",
           x = 2008,
           xend = 2019,
           y = 100 * 0.8,
           yend = 100 * 0.8,
           linetype = "dashed",
           color = "grey40",
           linewidth = rel(0.3)) +
  # Prevalence as area plot
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              colour = NA,
              alpha = 0.5) +
  # Prevalence as area plot
  geom_line(aes(y = mean),
            linewidth = 0.5,
            show.legend = FALSE) +
  # Survey points
  geom_point(aes(x = year, y = 100 * estimate),
             data = survey_plot,             
             colour = 'black',
             size = 1.0,
             inherit.aes = FALSE,
             show.legend = FALSE) +
  # Uncertainty on the survey points
  geom_linerange(aes(x = year, ymin = 100 * ci_lower, ymax = 100 * ci_upper),
                 data = survey_plot,             
                 colour = 'black',
                 inherit.aes = FALSE,
                 show.legend = FALSE) +
  
  # Setting colour palette
  scale_fill_manual(values = fill_palette,
                    breaks = c('With programme data', 'Survey data only'),
                    labels = c('With programme data', 'Survey data only')) +
  scale_colour_manual(values = fill_palette,
                    breaks = c('With programme data', 'Survey data only'),
                    labels = c('With programme data', 'Survey data only')) +
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2018, by = 2),
                     labels = seq(2008, 2018, by = 2),
                     limits = c(2007.75, 2019.25)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(4),
                     limits = c(0, 100),
                     expand = expansion(0, 0)) + 
  # Plotting labels
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       colour = NULL,
       title = 'A \n') +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_district_grid, 
            label = "name_district") +
  coord_cartesian(clip = "off") +
  # Minimal theme
  theme_minimal(8) +
  # Altering plot text size
  theme(axis.text = element_text(size = rel(0.9)),
        strip.text = element_text(size = rel(0.9), face = "bold", margin = margin(0, 0, 1.5, 0.0, "pt")),        
        strip.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0, size = rel(1.5), face = 'bold'), 
        axis.title = element_text(size = unit(9, "pt"), hjust = 0.0, face = "bold"),
        axis.ticks = element_line(colour = "grey20"),
        axis.ticks.length = unit(1.5, "pt"),
        axis.text.x = element_text(size = rel(0.8), angle = 50, hjust = 1, vjust = 1.1),
        axis.text.y = element_text(size = rel(0.8)),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(1.0)),
        legend.title = element_text(size = unit(9, "pt"), face = "bold"))


fig7ainset <- fig7ap +
  scale_fill_manual(values = fill_palette[-c(1:2)],
                    breaks = c(" ", "  ", "Limpopo",
                               "North West", "Gauteng", "Mpumalanga",
                               "Northern Cape", "Free State", "KwaZulu-Natal",
                               "Western Cape", "Eastern Cape", "   "),
                    labels = c(" ", "  ", "Limpopo",
                               "North\nWest", "Gauteng", "Mpumalanga",
                               "Northern\nCape", "Free\nState", "KwaZulu-\nNatal",
                               "Western\nCape", "Eastern\nCape", "   "),
                    drop = FALSE,
                    na.value = "grey97") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.25),
                             ncol = 3,
                             byrow = TRUE,
                             title = "Province:",
                             title.theme = element_text(size = 9, face = "bold"),
                             title.position = "top",
                             title.hjust = 0,
                             label.theme = element_text(size = 6.5, margin = margin()))) +
  theme(legend.spacing.x = unit(4, "pt"),
        legend.key.size = unit(1.2, "lines"),
        legend.box.background = element_rect(fill = "grey97", linewidth = NA))

fig7ainset <- ggpubr::get_legend(fig7ainset)


library(patchwork)

fig7ayaxis <- grid::textGrob("Circumcision coverage (%)", rot = 90, gp = gpar(fontsize=9, fontface = "bold"))

fig7a <- wrap_elements(full = get_geofacet_grob(fig7ap)) +
  inset_element(fig7ainset, 0, 0.81, 0.31, 0.92, align_to = "full", on_top = TRUE) +
  inset_element(fig7ayaxis, 0.07, 0.4, 0.09, 0.7)

ggsave("Figure1a.pdf", fig7a, height = 7.5, width = 7.5, units = "in")
ggsave("Figure1a.png", fig7a, height = 7.5, width = 7.5, units = "in")
ggsave("Figure1a.eps", fig7a, height = 7.5, width = 7.5, units = "in", device = cairo_ps)


save(fig7a, file = "Figure1a.RData")
