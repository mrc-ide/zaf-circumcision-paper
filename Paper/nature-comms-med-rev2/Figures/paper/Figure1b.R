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
library(patchwork)

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

##########################
### Reading in results ###
##########################
# Reading in model results by age group
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Extracting coverage
results_agegroup <- results_agegroup %>%
  subset(age_group == '15-49' & 
           type %in% c('Circumcision coverage (MMC-nT)', 'Circumcision coverage (TMIC)') & 
           model == 'With program data')

# Getting relevant columns 
results_agegroup <- results_agegroup[,c('area_id','area_name', 'area_level', 'model', 'type','year','mean', 'lower', 'upper')]

##############################
### Reading in survey data ###
##############################
# Read in results 
results_survey <- read.csv("../src/zaf-survey-circumcision-coverage.csv") %>%
  filter(age_group == 'Y015_049', year >= 2005 & indicator %in% c("circ_medical", "circ_traditional"))

# Recoding survey data
results_survey <- results_survey %>%
  mutate(type = recode(indicator, 
                       "circ_medical" = "Medical", 
                       "circ_traditional" = "Traditional"))

########################
### Plotting results ###
########################
# Getting results
tmp1 <- subset(results_agegroup, area_level == 1 & year %in% 2008:2019)

# Multiplying coverage by 100
tmp1$mean <- 100 * tmp1$mean
tmp1$lower <- 100 * tmp1$lower
tmp1$upper <- 100 * tmp1$upper

# Ordering for the province facet
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

# Relabelling
tmp1$type[which(tmp1$type == 'Circumcision coverage (MMC-nT)')] <- 'Medical'
tmp1$type[which(tmp1$type == 'Circumcision coverage (TMIC)')] <- 'Traditional'

# Merging on results to survey data
survey_plot <- results_survey %>%
  semi_join(tmp1, by = "area_id")

# Plotting 
fig1bp <-
  ggplot(tmp1,
       aes(x = year,
           y = mean,
           ymin = lower,
           ymax = upper,
           group = type,
           fill = type,
           colour = type)) + 
  # Adding target line to prevalence
  geom_hline(yintercept = 80,
             linewidth = 0.5,
             linetype = 'dashed',
             colour = 'grey50') +
  # Lines of coverage
  geom_ribbon(colour = NA, alpha = 0.5) + 
  geom_line(linewidth = 0.5,
            show.legend = FALSE) + 
  geom_point(aes(x = year, y = 100 * estimate,
                 group = type,
                 colour = type),
             data = survey_plot,   
             size = 1.0,
             inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_linerange(aes(x = year, ymin = 100 * ci_lower, ymax = 100 * ci_upper,
                     group = type,
                     colour = type),
             data = survey_plot,             
             inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_text(aes(x = 2008, y = 100, label = area_name),
            data = distinct(tmp1, area_id, area_name),
            hjust = 0, vjust = 1,
            size = 3.0,
            fontface = "bold",
            inherit.aes = FALSE) +
  # Setting for the axes
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2018, by = 2),
                     labels = seq(2008, 2018, by = 2),
                     limits = c(2007.75, 2019.25)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(0, 100)) + 
  # Colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)[c(1,3)]) +
  labs(x = NULL,
       y = 'Circumcision coverage (%)',
       colour = NULL,
       fill = NULL,
       title = 'b') + 
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
        plot.title = element_text(hjust = 0, size = rel(1.5), face = 'bold'), 
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9, face = 'bold'),
        axis.text.x = element_text(size = rel(0.8), angle = 50, hjust = 1, vjust = 1.1),
        axis.text.y = element_text(size = rel(0.8)),
        # strip.placement = 'outside',
        legend.position = 'none')+ 
  guides(color=guide_legend(ncol=2))

fig1bl <- {fig1bp +
  theme(legend.position = "right")} %>%
  ggpubr::get_legend()

fig1b <- wrap_elements(full = get_geofacet_grob(fig1bp)) +
  inset_element(fig1bl, 0.7, 0.05, 1.0, 0.23, align_to = "full", on_top = TRUE)
  
# Saving plot
ggsave("Figure1b.pdf", width = 5.2, height = 5.3, units = "in")
ggsave("Figure1b.png", width = 5.2, height = 5.3, units = "in")
ggsave("Figure1b.eps", width = 5.2, height = 5.3, units = "in", device = cairo_ps)
save(fig1b, file = "Figure1b.RData")



