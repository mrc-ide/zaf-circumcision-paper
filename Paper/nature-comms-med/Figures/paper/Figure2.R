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
results_age1 <- read_csv(paste('~/Downloads/', version, '/Results_Age_Probability.csv', sep = ''))
results_age2 <- read_csv(paste('~/Downloads/', version, '/Results_Age_Incidence.csv', sep = ''))
results_age3 <- read_csv(paste('~/Downloads/', version, '/Results_Age_Prevalence.csv', sep = ''))
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
               type %in% c('Probability of MC', 'Probability of MMC', 'Probability of TMC',
                           'MC coverage', 'MMC coverage', 'TMC coverage') &
                 year %in% c(2008, 2014, 2019) &
                 area_level == 0 & 
                 model == 'With program data') %>%
  dplyr::rename(mean.x   = mean, 
                sd.x     = sd, 
                median.x = median, 
                lower.x  = lower, 
                upper.x  = upper)

# Multiplying prevalence by 100
tmp1$mean.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))] <- 
  100 * tmp1$mean.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))]
tmp1$lower.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))] <- 
  100 * tmp1$lower.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))]
tmp1$upper.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))] <- 
  100 * tmp1$upper.x[which(tmp1$type %in% c('MC coverage', 'MMC coverage', 'TMC coverage'))]

# Relabelling for plot 
tmp1$type1 <- NA
tmp1$type1[which(tmp1$type %in% c('MC coverage'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('MMC coverage'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('TMC coverage'))] <- 'Y'
tmp1$type1[which(tmp1$type %in% c('Probability of MC'))] <- 'Z'
tmp1$type1[which(tmp1$type %in% c('Probability of MMC'))] <- 'Z'
tmp1$type1[which(tmp1$type %in% c('Probability of TMC'))] <- 'Z'
tmp1$type2 <- NA
tmp1$type2[which(tmp1$type %in% c('MC coverage'))] <- 'A'
tmp1$type2[which(tmp1$type %in% c('MMC coverage'))] <- 'B'
tmp1$type2[which(tmp1$type %in% c('TMC coverage'))] <- 'C'
tmp1$type2[which(tmp1$type %in% c('Probability of MC'))] <- 'A'
tmp1$type2[which(tmp1$type %in% c('Probability of MMC'))] <- 'B'
tmp1$type2[which(tmp1$type %in% c('Probability of TMC'))] <- 'C'


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


fig2 <- ggplot(tmp1, 
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
  scale_colour_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 3)) + 
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
        axis.title = element_text(size = 9),
        strip.placement = 'outside',
        legend.position = 'bottom')

ggsave("Figure2.pdf", fig2, width = 5.2, height = 3.8, units = "in")
ggsave("Figure2.png", fig2, width = 5.2, height = 3.8, units = "in")
ggsave("Figure2.eps", fig2, width = 5.2, height = 2.6, units = "in", device = cairo_ps)
