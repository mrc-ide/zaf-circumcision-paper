#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

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

######################################
### Preparing resutls for plotting ###
######################################
# Reading in results by age 

results_agegroup1 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Rate.csv'))
results_agegroup2 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Incidence.csv'))
results_agegroup3 <- read_csv(paste0('~/Downloads/', version, '/Results_AgeGroup_Prevalence.csv'))
results_agegroup <- rbind(results_agegroup1, results_agegroup2, results_agegroup3)
rm(results_agegroup1, results_agegroup2, results_agegroup3)

# Adding label-freindly district name 
results_agegroup <- results_agegroup %>%
  left_join(
    select(zaf_district_grid, code_area_id, name_district),
    by = c("area_id" = "code_area_id")
  )

###################################################
### Bar plot of number of circumcisions by type ###
###################################################
# Subsetting
tmp1 <- results_agegroup %>%
  filter(
    type %in% c('MMC-nTs performed', 'MMC-Ts performed', 'TMCs performed'),
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
    type %in% c('MMC-nT coverage', 'MMC-T coverage', 'TMC coverage'),
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
tmp1$type[which(tmp1$type %in% c('MMC-nTs performed'))] <- 'MMC-nT'
tmp1$type[which(tmp1$type %in% c('MMC-Ts performed'))] <- 'MMC-T'
tmp1$type[which(tmp1$type %in% c('TMCs performed'))] <- 'TMC'

# Relabelling for plot 
tmp2$type[which(tmp2$type %in% c('MMC-nT coverage'))] <- 'MMC-nT'
tmp2$type[which(tmp2$type %in% c('MMC-T coverage'))] <- 'MMC-T'
tmp2$type[which(tmp2$type %in% c('TMC coverage'))] <- 'TMC'

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

fig1 <- ggplot(tmp, 
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
        strip.text = element_text(size = rel(1.1)),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = rel(1.0)),
        axis.title = element_text(size = rel(1.1)),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.justification = c(0.9, 0.5))

ggsave("Figure1.pdf", fig1, width = 5.2, height = 2.6, units = "in")
ggsave("Figure1.png", fig1, width = 5.2, height = 2.6, units = "in")
ggsave("Figure1.eps", fig1, width = 5.2, height = 2.6, units = "in", device = cairo_ps)
