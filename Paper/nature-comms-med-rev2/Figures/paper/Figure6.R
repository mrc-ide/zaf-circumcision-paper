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
               type %in% c('Circumcision coverage (MMC-nT)', 
                           'Circumcision coverage (MMC-T)', 
                           'Circumcision coverage (TMC)') &
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

#' Recode district names
district_recode <- c("Ngaka Modiri Molema" = "N M Molema",
                     "Ruth Segomotsi Mompati" = "R S Mompati",
                     "Bojanala Platinum" = "Bojanala Plat.")
zaf_district_grid <- zaf_district_grid %>%
  mutate(name_district = recode(name_district, !!!district_recode))

# Setting colour palette on the type
fill_palette <- c(setNames(wesanderson::wes_palette("Zissou1", 3), c("MMC-nT", "MMC-T", "TMC")),
                  province_palette,
                  " " = NA, "  " = NA, "   " = NA)

#' Don't show MMC-nT / MMC-T stratification -- too much complexity
fill_palette["MMC-T"] <- fill_palette["MMC-nT"]

# Setting factor on fill for order
tmp$type <- factor(tmp$type, names(fill_palette))

# Factor for the province
zaf_district_grid$name_province <- factor(zaf_district_grid$name_province, names(fill_palette))
                           
# Plotting
fig6p <- tmp %>%
  ggplot(aes(x = year,
             group = type,
             fill = type)) +
  # Background fill
  geom_area(aes(y = y1, fill = name_province, group = NULL),
            data = zaf_district_grid %>%
              mutate(area_id = code_area_id) %>%
              crossing(data.frame(year = c(2008, 2019), y1 = 100)),
            alpha = 0.25) +
  # Adding target line to prevalence
  annotate("segment",
           x=2008,
           xend = 2019,
           y = 100 * 0.8,
           yend = 100 * 0.8,
           linetype = "dashed",
           color = "grey40",
           linewidth = rel(0.3)) +
  # Prevalence as area plot
  geom_area(aes(y = 100 * mean.y)) +
  geom_label(data = filter(tmp1, year == 2008),
             aes(x = 2008,
                 y = 100*mean,
                 fill = NULL,
                 label = percent(mean, 1)),
             fontface = "bold",
             color = "grey30",
             size = rel(2.2),
             nudge_y = 4.0,
             nudge_x = 0.5,
             label.padding = unit(0.05, "lines"),
             label.r = unit(0.05, "lines"),
             label.size = 0,
             alpha = 0.6,
             show.legend = FALSE) +
  geom_label(data = filter(tmp1, year == 2019),
             aes(x = 2019,
                 y = 100*mean,
                 fill = NULL,
                 label = percent(mean, 1)),
             fontface = "bold",
             color = "grey30",
             size = rel(2.2),
             nudge_y = 4.0,
             nudge_x = -0.5,
             label.padding = unit(0.05, "lines"),
             label.r = unit(0.05, "lines"),
             label.size = 0,
             alpha = 0.6,
             show.legend = FALSE) +
  # Setting for the axes
  scale_x_continuous("Year",
                     breaks = seq(2008, 2018, by = 2),
                     labels = seq(2008, 2018, by = 2),
                     limits = c(2007.75, 2019.25)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(5),
                     limits = c(0, 100), expand = expansion(0, 0)) + 
  # Setting colour palette
  scale_fill_manual(values = fill_palette,
                    breaks = c("MMC-nT", "TMC"),
                    labels = c("MMC-nT" = "Medical",
                               "TMC" = "Traditional")) +
  # Plotting labels
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  # Geofacet
  facet_geo(~ area_id, 
            grid = zaf_district_grid, 
            label = "name_district") +
  coord_cartesian(clip = "off") +
  # Minimal theme
  theme_minimal(8) +
  # Altering plot text size
  theme(axis.text.y = element_blank(),
        strip.text = element_text(size = rel(0.9), face = "bold", margin = margin(0, 0, 1.5, 0.0, "pt")),
        strip.background = element_blank(),
        axis.title = element_text(size = unit(9, "pt"), face = "bold"),
        panel.grid = element_blank(),
        legend.text = element_text(size = rel(1.0)),
        legend.title = element_text(size = unit(9, "pt"), face = "bold"),
        axis.text.x = element_text(size = rel(0.8), angle = 50, hjust = 1, vjust = 1.1),
        axis.ticks.x = element_line(colour = "grey20"),
        axis.ticks.length = unit(1.5, "pt"),
        legend.position = 'bottom')

fig6inset <- fig6p +
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

fig6inset <- ggpubr::get_legend(fig6inset)

fig6yaxis <- grid::textGrob("Circumcision coverage (%)", rot = 90, gp = gpar(fontsize=9, fontface = "bold"))

fig6 <- wrap_elements(full = get_geofacet_grob(fig6p)) +
  inset_element(fig6inset, 0, 0.81, 0.31, 1.0, align_to = "full", on_top = TRUE) +
  inset_element(fig6yaxis, 0.07, 0.4, 0.09, 0.7)

# Saving plot 
ggsave("Figure6.pdf", fig6, height = 7.5, width = 7.5, units = "in")
ggsave("Figure6.png", fig6, height = 7.5, width = 7.5, units = "in")
ggsave("Figure6.eps", fig6, height = 7.5, width = 7.5, units = "in", device = cairo_ps)
