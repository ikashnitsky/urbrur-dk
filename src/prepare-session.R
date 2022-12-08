#===============================================================================
# 2022-11-21 -- urbrur-dk
# prepare session
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================
library(tidyverse)
library(magrittr)
library(patchwork)
library(paletteer)
library(hrbrthemes)
library(sf)
library(rmapshaper)

library(cowplot)
# library(ggforce)
library(prismatic)

# remotes::install_github("jimjam-slam/ggflags")
library(ggflags)

source("src/fun-inset-cph-box.R")
source("src/fun-life-table.R")
source("src/fun-topals-fit.R")
source("src/fun-show-topals.R")


# visual theming ----------------------------------------------------------


# pal_3 <- c("#3C4565FF", "#4db6ac", "#FFE37CFF")
# 886457
# pal_3 <- c("#3C4565FF", "#82E2D8FF", "#B29906FF")
#
# pal_3 <- c("#03463A", "#9ccc65", "#81684D")

# pal_3 <- c("#4C1D4BFF", "#CB1B4FFF", "#F69C73FF" %>% clr_darken(.1))

# pal_3 <- viridis::viridis(3, begin = .2, end = .8, option = "F")

pal_3 <- viridis::viridis(3, begin = .05, end = .85, option = "D")

# pal_3 %>% clr_grayscale()
# pal_3 %>% clr_deutan()
# pal_3 %>% clr_deutan()%>% clr_grayscale()

theme_set(
    theme_minimal(base_family = font_rc, base_size = 15)+
        theme(
            plot.background = element_rect(fill = "#dadada", color = NA),
            legend.position = "none",
            panel.spacing = unit(1, "lines"),
            panel.grid.minor = element_blank(),
            line = element_line(lineend = "round")
        )
)
