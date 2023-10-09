#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev2/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggpubr)

# Loading figures 
load("Figure1a.RData")
load("Figure1b.RData")
load("Figure1c.RData")

# Appending figures together 
fig1 <- ggarrange(fig1a, 
          ggarrange(fig1b, fig1cp,
                    nrow = 2),
          ncol = 2,
          widths = c(2, 1))

# Saving plots 
ggsave("Figure1.pdf", fig1, height = 9, width = 14, units = "in")
ggsave("Figure1.png", fig1, height = 9, width = 14, units = "in")
ggsave("Figure1.eps", fig1, height = 9, width = 14, units = "in", device = cairo_ps)

          
