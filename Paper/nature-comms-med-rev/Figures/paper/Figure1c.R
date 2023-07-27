#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggridges)

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

##########################
### Reading in results ###
##########################
# Reading in model results by age group
results_agegroup1 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Dropbox/zaf-circumcision-rates/Output/Analysis/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)
results_agegroup <- results_agegroup %>%
  subset(age_group %in% c(  '0-4',   '5-9', '10-14', '15-19', '20-24', '25-29', 
                          '30-34', '35-39', '40-44', '45-49', '50-54', '54-59') & 
           type %in% c('MC coverage') & 
           year %in% c(2008, 2012, 2016, 2017) & 
           model == 'With program data')


# Getting relevant columns 
results_agegroup <- results_agegroup[,c('area_id','area_name', 'area_level', 'age_group', 'model', 'type','year','mean', 'lower', 'upper')]
results_agegroup$age_group[results_agegroup$age_group == '54-59'] <- '55-59'
results_agegroup$age_group <- factor(results_agegroup$age_group, levels = c(  '0-4',   '5-9', '10-14', '15-19', '20-24', '25-29', 
                                                                              '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))
##############################
### Reading in survey data ###
##############################
# Read in results 
results_survey <- read.csv("../src/zaf-survey-circumcision-coverage.csv") %>%
  filter(age_group%in% c('Y000_004', 'Y005_009', 'Y010_014', 'Y015_019', 'Y020_024', 'Y025_029',
                         'Y030_034', 'Y035_039', 'Y040_044', 'Y045_049', 'Y050_054', 'Y055_059') & 
           year >= 2005 & 
           indicator %in% c("circumcised"))

# Relabelling
results_survey$age_group[which(results_survey$age_group == 'Y000_004')] <- '0-4'
results_survey$age_group[which(results_survey$age_group == 'Y005_009')] <- '5-9'
results_survey$age_group[which(results_survey$age_group == 'Y010_014')] <- '10-14'
results_survey$age_group[which(results_survey$age_group == 'Y015_019')] <- '15-19'
results_survey$age_group[which(results_survey$age_group == 'Y020_024')] <- '20-24'
results_survey$age_group[which(results_survey$age_group == 'Y025_029')] <- '25-29'
results_survey$age_group[which(results_survey$age_group == 'Y030_034')] <- '30-34'
results_survey$age_group[which(results_survey$age_group == 'Y035_039')] <- '35-39'
results_survey$age_group[which(results_survey$age_group == 'Y040_044')] <- '40-44'
results_survey$age_group[which(results_survey$age_group == 'Y045_049')] <- '45-49'
results_survey$age_group[which(results_survey$age_group == 'Y050_054')] <- '50-54'
results_survey$age_group[which(results_survey$age_group == 'Y055_059')] <- '55-59'

# Recoding survey data
results_survey <- results_survey %>%
  mutate(type = recode(indicator, "circumcised " = "MC coverage"))

results_survey$age_group <- factor(results_survey$age_group, levels = c(  '0-4',   '5-9', '10-14', '15-19', '20-24', '25-29', 
                                                                              '30-34', '35-39', '40-44', '45-49', '50-54', '55-59'))
#######################################
### Plotting results (DMPPT2, total)###
#######################################
# Getting results
tmp1 <- subset(results_agegroup, area_level == 0 & year %in% c(2008:2019))

# Multiplying coverage by 100
tmp1$mean <- 100 * tmp1$mean
tmp1$lower <- 100 * tmp1$lower
tmp1$upper <- 100 * tmp1$upper

# Merging on results to survey data
survey_plot <- results_survey %>%
  semi_join(tmp1, by = "area_id")


fig7cp <-
ggplot(tmp1,
       aes(x = age_group,
           y = mean,
           ymin = lower,
           ymax = upper,
           group = as.factor(year),
           fill = as.factor(year),
           colour = as.factor(year))) + 
  # Adding target line to prevalence
  geom_hline(yintercept = 80,
             linewidth = 0.5,
             linetype = 'dashed',
             colour = 'grey50') +
  # Lines of coverage
  geom_ribbon(colour = NA, alpha = 0.5) + 
  geom_line(linewidth = 0.5) + 
  geom_point(aes(x = age_group, y = 100 * estimate,
                 group = as.factor(year),
                 colour = as.factor(year)),
             data = survey_plot,   
             size = 1.0,
             inherit.aes = FALSE,
             show.legend = FALSE) +
  geom_linerange(aes(x = age_group, ymin = 100 * ci_lower, ymax = 100 * ci_upper,
                     group = as.factor(year),
                     colour = as.factor(year)),
                 data = survey_plot,             
                 inherit.aes = FALSE,
                 show.legend = FALSE) +
  geom_text(aes(x = "0-4", y = 100, label = year),
            data = distinct(tmp1, year),
            hjust = 0, vjust = 1,
            size = 3.0,
            fontface = "bold",
            inherit.aes = FALSE) +
  # Setting for the axes
  scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(0, 100)) + 
  # Colour palette
  scale_fill_manual(values = MetBrewer::met.brewer('Nizami', 4)) +
  scale_colour_manual(values = MetBrewer::met.brewer('Nizami', 4)) +
  labs(x = NULL,
       y = 'Circumcision coverage (%)',
       colour = NULL,
       fill = NULL,
       title = 'C') + 
  # Facet wrap
  facet_wrap(. ~ year) + 
  # Minimal theme
  theme_bw(8) +  
  # Extra options on the plot 
  theme(axis.text = element_text(size = 8),
        axis.text.x = element_text(size = rel(0.8), angle = 50, hjust = 1, vjust = 1.1),
        axis.text.y = element_text(size = rel(0.8)),
        strip.text = element_text(size = 9, face = "bold"),
        panel.grid = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title = element_text(hjust = 0, size = rel(1.5), face = 'bold'), 
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9, face = 'bold'),
        strip.placement = 'outside',
        legend.position = 'bottom')+ 
  guides(color=guide_legend(ncol=2))


ggsave("Figure1c.pdf", width = 5.2, height = 5.3, units = "in")
ggsave("Figure1c.png", width = 5.2, height = 5.3, units = "in")
ggsave("Figure1c.eps", width = 5.2, height = 5.3, units = "in", device = cairo_ps)

save(fig7cp, file = "Figure1c.RData")