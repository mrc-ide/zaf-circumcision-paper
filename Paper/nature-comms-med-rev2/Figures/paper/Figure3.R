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
results_agegroup1 <- read_csv('../src/Results_AgeGroup_Probability.csv')
results_agegroup2 <- read_csv('../src/Results_AgeGroup_Incidence.csv')
results_agegroup3 <- read_csv('../src/Results_AgeGroup_Prevalence.csv')
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- results_agegroup %>%
  filter(
    type %in% c('Number of circumcisions performed (MMC-nT)',
                'Number of circumcisions performed (MMC-T)', 
                'Number of circumcisions performed (TMC)'),
    age_group == '10+',
    year %in% c(2008:2019),
    area_level == 0,
    model == 'With program data'
  ) %>%
  dplyr::rename(mean.x   = mean, 
                sd.x     = sd, 
                median.x = median, 
                lower.x  = lower, 
                upper.x  = upper)

# Subsetting
tmp2 <- results_agegroup %>%
  filter(
    type %in% c('Circumcision coverage (MMC-nT)', 
                'Circumcision coverage (MMC-T)', 
                'Circumcision coverage (TMC)'),
    age_group == '15-49',
    year %in% c(2008:2019),
    area_level == 0,
    model == 'With program data'
  ) %>%
  dplyr::rename(mean.y   = mean, 
                sd.y     = sd, 
                median.y = median, 
                lower.y  = lower, 
                upper.y  = upper)

# Relabelling for plot 
tmp1$type[which(tmp1$type %in% c('Number of circumcisions performed (MMC-nT)'))] <- 'MMC-nT'
tmp1$type[which(tmp1$type %in% c('Number of circumcisions performed (MMC-T)'))] <- 'MMC-T'
tmp1$type[which(tmp1$type %in% c('Number of circumcisions performed (TMC)'))] <- 'TMC'

# Relabelling for plot 
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-nT)'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (MMC-T)'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('Circumcision coverage (TMC)'))] <- 'TMC'

# Adding labels to the facet
tmp1$test <- 'B'
tmp2$test <- 'A'

# Appending datasets together 
tmp <- rbind.fill(tmp1, tmp2)

# Dummy dataset for limits in each panel
dummy1 <- rbind(expand.grid(x = c(2008, 2019),
                            y = c(0, 100),
                            type = NA,
                            test = "A"), 
                expand.grid(x = c(2008, 2019),
                            y = c(0, 600),
                            type = NA,
                            test = "B"))

# Dummy dataset to add 80% target
dummy2 = data.frame(test = c("A", "B"),
                    type = NA,
                    y = c(80, NA))

# Plotting
fig3 <- ggplot(tmp,
               aes(x = year,
                   group = type,
                   fill = type)) + 
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
  geom_area(aes(y = 100 * mean.y)) +
  # Number of MCs performed as bar chart
  geom_bar(aes(y = mean.x/1000),
           stat = 'identity') + 
  # Setting for the axes
  scale_x_continuous(breaks = seq(2008, 2019, by = 2)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(6), limits = c(0, NA)) + 
  # Setting colour palette
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3),
                    labels = c("MMC-nT" = "Medical (not trad. setting; MMC-nT)",
                               "MMC-T" = "Med. in trad. setting (MMC-T)",
                               "TMC" = "Traditional (TMC)")) + 
  # Plotting labels
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  # Minimal theme
  theme_bw(8) +
  # Facet wrapping
  facet_wrap(. ~ test,
             strip.position = "left", 
             scales = 'free', 
             labeller = as_labeller(c(A = "Circumcision coverage (%)", 
                                      B = "Circumcisions performed (1000s)") ))+  
  # Extra options on the plot 
  theme(axis.text = element_text(size = rel(1.0)),
        strip.text = element_text(size = rel(0.9), face = 'bold'),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = rel(0.9)),
        axis.title = element_text(size = rel(1.1), face = 'bold'),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.justification = c(0.9, 0.5))

# Saving plot
ggsave("Figure3.pdf", fig3, width = 5.2, height = 2.6, units = "in")
ggsave("Figure3.png", fig3, width = 5.2, height = 2.6, units = "in")
ggsave("Figure3.eps", fig3, width = 5.2, height = 2.6, units = "in", device = cairo_ps)

