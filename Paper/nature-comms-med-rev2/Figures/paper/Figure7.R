#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
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
# Subsetting
tmp1 <- subset(results_age,
               type == 'Circumcision coverage (MC)' &
                 year %in% c(2008, 2014, 2019) &
                 area_level == 2 & 
                 model == 'With program data') 

# Multiplying prevalence by 100
tmp1$mean <- 100 * tmp1$mean
tmp1$lower <- 100 * tmp1$lower
tmp1$upper <- 100 * tmp1$upper

#' Recode district names
district_recode <- c("Ngaka Modiri Molema" = "N M Molema",
                     "Ruth Segomotsi Mompati" = "R S Mompati",
                     "Bojanala Platinum" = "Bojanala Plat.")
zaf_district_grid <- zaf_district_grid %>%
  mutate(name_district = recode(name_district, !!!district_recode))

# Setting colours for the years 
fill_palette <- c(setNames(rev(MetBrewer::met.brewer('Nizami', 3)), c("2008", "2014", "2019")),
                  province_palette,
                  " " = NA, "  " = NA, "   " = NA)

# Factor for year 
tmp1$year <- factor(tmp1$year, names(fill_palette))

# Factor for province
zaf_district_grid$name_province <- factor(zaf_district_grid$name_province, names(fill_palette))

# Plotting
fig7p <- ggplot(tmp1, 
             aes(x = age,
                 group = as.factor(year),
                 fill = as.factor(year),
                 colour = as.factor(year))) +
  # Background fill
  geom_area(aes(y = y1, fill = name_province, group = NULL, colour = NULL),
            data = zaf_district_grid %>%
              mutate(area_id = code_area_id) %>%
              crossing(data.frame(age = c(0, 59), y1 = 100)),
            alpha = 0.2, show.legend = FALSE) +  
  # Adding target line to prevalence
  annotate("segment",
           x = 0,
           xend = 59,
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
            linewidth = 0.5) +
  # Setting for the axes
  scale_x_continuous("Male age (years)",
                     breaks = seq(0, 60, by = 10),
                     limits = c(-2, 62),
                     labels = seq(0, 60, by = 10),
                     expand = expansion(0, 0)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(4),
                     limits = c(0, 100),
                     expand = expansion(0, 0)) + 
  # Setting colour palette
  scale_fill_manual(values = fill_palette, breaks = c("2008", "2014", "2019")) +
  scale_color_manual(values = fill_palette, breaks = c("2008", "2014", "2019")) +
  # Plotting labels
  labs(x = NULL,
       y = NULL,
       colour = NULL,
       fill = NULL) +
  # Minimal theme
  theme_minimal(8) +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_district_grid, 
            label = "name_district") +
  coord_cartesian(clip = "off") +
  # Altering plot text size
  theme(axis.text = element_text(size = rel(0.8)),
        strip.text = element_text(size = rel(0.9), face = "bold", margin = margin(0, 0, 1.5, 0.0, "pt")),        
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = unit(9, "pt"), face = "bold"),
        axis.ticks = element_line(colour = "grey20"),
        axis.ticks.length = unit(1.5, "pt"),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(1.0)),
        legend.title = element_text(size = unit(9, "pt"), face = "bold"))

fig7inset <- fig7p +
  scale_fill_manual(values = fill_palette,
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
  guides(colour = "none",
         fill = guide_legend(override.aes = list(alpha = 0.25, colour = NA),
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

fig7inset <- ggpubr::get_legend(fig7inset)

fig7yaxis <- grid::textGrob("Circumcision coverage (%)", rot = 90, gp = gpar(fontsize=9, fontface = "bold"))

fig7 <- wrap_elements(full = get_geofacet_grob(fig7p)) +
  inset_element(fig7inset, 0, 0.81, 0.31, 1.0, align_to = "full", on_top = TRUE) +
  inset_element(fig7yaxis, 0.07, 0.4, 0.09, 0.7)

# Saving plot 
ggsave("Figure7.pdf", fig7, height = 7.5, width = 7.5, units = "in")
ggsave("Figure7.png", fig7, height = 7.5, width = 7.5, units = "in")
ggsave("Figure7.eps", fig7, height = 7.5, width = 7.5, units = "in", device = cairo_ps)
