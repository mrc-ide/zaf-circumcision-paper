#####################
### Preliminaries ###
#####################
# Clearing Workspace
rm(list = ls())

# Set working directory
setwd('~/Dropbox/Github/zaf-circumcision-rates/Documents/nature-comms-rev/Figures/paper')

# Loading source code 
source('../src/0_Source.R')
require(ggpubr)


load("Figure1a.RData")
load("Figure1b.RData")
load("Figure1c.RData")


fig7 <- ggarrange(fig7a, 
          ggarrange(fig7b, fig7cp,
                    nrow = 2),
          ncol = 2,
          widths = c(2, 1))

ggsave("Figure1.pdf", fig7, height = 9, width = 14, units = "in")
ggsave("Figure1.png", fig7, height = 9, width = 14, units = "in")
ggsave("Figure1.eps", fig7, height = 9, width = 14, units = "in", device = cairo_ps)

          