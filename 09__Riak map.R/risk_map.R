rm(list = ls())

library(sf)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(scatterpie)
library(reshape2)


QTP <- "D:DBATP_Polygon.shp"
border <- "D:DBATP/DBATP_Line.shp"

QTP_pro <- st_read(QTP, quiet = TRUE)
border_line <- st_read(border, quiet = TRUE)


dat <- read.csv(
  "risk_total2log.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

str(dat)
head(dat)

dat$total <- as.numeric(dat$total)
dat$long  <- as.numeric(dat$long)
dat$lat   <- as.numeric(dat$lat)

dat_long <- melt(
  dat,
  id.vars = c("region", "long", "lat", "total"),
  measure.vars = c("riskI", "riskII", "riskIII"),
  variable.name = "variable",
  value.name = "value"
)

dat_long <- dat_long %>%
  filter(!is.na(value))

dat_long$total_plot <- dat_long$total


china_QTP_gg <- ggplot() +
  geom_sf(data = QTP_pro, fill = "white", color = NA) +
  geom_sf(data = border_line, color = "black", linewidth = 0.4) +
  
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  
  coord_sf(
    xlim = c(70, 105),
    ylim = c(20, 45),
    expand = FALSE
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

china_QTP_gg


figure <- china_QTP_gg +
  geom_scatterpie(
    data = dat_long,
    aes(
      x = long,
      y = lat,
      group = region,
      r = total_plot
    ),
    cols = "variable",
    long_format = TRUE,
    color = "transparent"
  ) +
  
  scale_fill_manual(
    name = "Risk",
    limits = c("riskI", "riskII", "riskIII"),
    values = c(
      "riskI" = "#F4B183",
      "riskII" = "#C55A11",
      "riskIII" = "#843C0C"
    ),
    labels = c("Risk I", "Risk II", "Risk III")
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme(
    legend.position = "right"
    # 如果你不想显示图例，就改成：
    # legend.position = "none"
  )

figure
