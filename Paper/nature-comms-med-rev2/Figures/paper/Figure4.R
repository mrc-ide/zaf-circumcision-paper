#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')

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

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 
results_age1 <- read_csv('../src/Results_SingleAge_Probability.csv')
results_age2 <- read_csv('../src/Results_SingleAge_Incidence.csv')
results_age3 <- read_csv('../src/Results_SingleAge_Prevalence.csv')
results_age <- rbind(results_age1, results_age2, results_age3)
rm(results_age1, results_age2, results_age3)

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- subset(results_age,
               type %in% c('Probability of circumcision (MC)', 
                           'Probability of circumcision (MMC)', 
                           'Probability of circumcision (TMC)',
                           'Circumcision coverage (MC)', 
                           'Circumcision coverage (MMC)', 
                           'Circumcision coverage (TMC)') &
                 year %in% c(2008, 2014, 2019) &
                 area_level == 0 & 
                 model == 'With program data') %>%
  dplyr::rename(mean.x   = mean, 
                sd.x     = sd, 
                median.x = median, 
                lower.x  = lower, 
                upper.x  = upper)

# Multiplying prevalence by 100
tmp1$mean.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))] <- 
  100 * tmp1$mean.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))]
tmp1$lower.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))] <- 
  100 * tmp1$lower.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))]
tmp1$upper.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))] <- 
  100 * tmp1$upper.x[which(tmp1$type %in% c('Circumcision coverage (MC)', 'Circumcision coverage (MMC)', 'Circumcision coverage (TMC)'))]

# Relabelling for plot 
tmp1$type1 <- NA
tmp1$type1[which(tmp1$type %in% c('Circumcision coverage (MC)'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('Circumcision coverage (MMC)'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('Circumcision coverage (TMC)'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('Probability of circumcision (MC)'))] <- 'Z'
tmp1$type1[which(tmp1$type %in% c('Probability of circumcision (MMC)'))] <- 'Z'
tmp1$type1[which(tmp1$type %in% c('Probability of circumcision (TMC)'))] <- 'Z'
tmp1$type2 <- NA
tmp1$type2[which(tmp1$type %in% c('Circumcision coverage (MC)'))] <- 'A'
tmp1$type2[which(tmp1$type %in% c('Circumcision coverage (MMC)'))] <- 'B'
tmp1$type2[which(tmp1$type %in% c('Circumcision coverage (TMC)'))] <- 'C'
tmp1$type2[which(tmp1$type %in% c('Probability of circumcision (MC)'))] <- 'A'
tmp1$type2[which(tmp1$type %in% c('Probability of circumcision (MMC)'))] <- 'B'
tmp1$type2[which(tmp1$type %in% c('Probability of circumcision (TMC)'))] <- 'C'


# Dummy dataset for limits in each panel
dummy1 <- rbind(expand.grid(x = c(0, 60),
                            y = c(0, 0.2),
                            year = NA,
                            type2 = c('A', 'B', 'C'),
                            type1 = 'Z'), 
                expand.grid(x = c(0, 60),
                            y = c(0, 100),
                            year = NA,
                            type2 = c('A', 'B', 'C'),
                            type1 = 'Y'))

# Dummy dataset to add 80% target
dummy2 = data.frame(type2 = c('A', 'B', 'C'),
                    type1 = 'Y',
                    year = NA,
                    y = c(80, 80, 80))

# PLotting 
fig4 <-ggplot(tmp1, 
              aes(x = age,
                  group = as.factor(year),
                  fill = as.factor(year),
                  colour = as.factor(year))) +
  # Adding fake limits to the plot
  geom_point(data = dummy1,
             aes(x = x,
                 y = y),
             colour = NA) +
  # Adding target line to prevalence
  geom_hline(data = dummy2,
             aes(yintercept = y),
             linewidth = 0.5,
             linetype = 'dashed',
             colour = 'grey50') +
  # Prevalence as area plot
  geom_ribbon(aes(ymin = lower.x,
                  ymax = upper.x),
              colour = NA,
              alpha = 0.5) +
  # Prevalence as area plot
  geom_line(aes(y = mean.x),
            linewidth = 0.5) +
  # Setting for the axes
  scale_x_continuous(breaks = seq(0, 60, by = 5),
                     labels = c(0, "", 10, "", 20, "", 30, "", 40, "", 50, "", 60)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA)) + 
  # Setting colour palette
  scale_colour_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
  scale_fill_manual(values = rev(MetBrewer::met.brewer('Nizami', 3))) + 
  # Plotting labels
  labs(x = 'Male age (years)',
       y = NULL,
       colour = NULL,
       fill = NULL) +
  # Minimal theme
  theme_bw(8) +
  # Facet wrapping
  facet_grid(type1 ~ type2,
             scales = 'free', 
             labeller = as_labeller(c(A = "Total circumcision", 
                                      B = "Medical circumcision",
                                      C = "Traditional circumcision",
                                      Y = "Circumcision\ncoverage (%)",
                                      Z = "Annual probability\nof circumcision") ), 
             switch = "y")+  
  # Extra options on the plot 
  theme(axis.text = element_text(size = 8),
        strip.text = element_text(size = 9, face = "bold"),
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9, face = 'bold'),
        strip.placement = 'outside',
        legend.position = 'bottom')

# Saving plot 
ggsave("Figure4.pdf", fig4, width = 5.2, height = 3.8, units = "in")
ggsave("Figure4.png", fig4, width = 5.2, height = 3.8, units = "in")
ggsave("Figure4.eps", fig4, width = 5.2, height = 2.6, units = "in", device = cairo_ps)
