rm(list = ls())

library(dplyr)
library(ggplot2)
library(geosphere)
library(segmented)


sim_data <- read.csv(
  "Lake_Glacier_ARG_similarity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

sim_data$Lake <- trimws(
  gsub("\u00A0", "", sim_data$Lake)
)

sim_data$Glacier <- trimws(
  gsub("\u00A0", "", sim_data$Glacier)
)


geo <- read.csv(
  "geo.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

geo$LakeEng <- trimws(
  gsub("\u00A0", "", geo$LakeEng)
)



lake_geo <- geo %>%
  dplyr::select(
    Lake = LakeEng,
    Lake_lon = Longitude,
    Lake_lat = Latitude
  )


glacier_geo <- geo %>%
  dplyr::select(
    Glacier = LakeEng,
    Glacier_lon = Longitude,
    Glacier_lat = Latitude
  )


plot_map <- sim_data %>%
  dplyr::left_join(
    lake_geo,
    by = "Lake"
  ) %>%
  dplyr::left_join(
    glacier_geo,
    by = "Glacier"
  )


# =========================================================
# Haversine distance
# =========================================================

plot_map$Distance_km <- geosphere::distHaversine(
  
  cbind(
    plot_map$Lake_lon,
    plot_map$Lake_lat
  ),
  
  cbind(
    plot_map$Glacier_lon,
    plot_map$Glacier_lat
  )
  
) / 1000


summary(
  plot_map$Distance_km
)


# =========================================================
# LOESS
# =========================================================

loess_fit <- loess(
  Similarity ~ Distance_km,
  data = plot_map,
  span = 0.75,
  degree = 2
)


x_grid <- seq(
  min(plot_map$Distance_km),
  max(plot_map$Distance_km),
  length.out = 2000
)


y_grid <- predict(
  loess_fit,
  newdata = data.frame(
    Distance_km = x_grid
  )
)


smooth_df <- data.frame(
  x = x_grid,
  y = y_grid
) %>%
  dplyr::filter(
    !is.na(y)
  )


# =========================================================
# First derivative
# =========================================================

d1 <- diff(smooth_df$y) /
  diff(smooth_df$x)

x1 <- (
  smooth_df$x[-1] +
    smooth_df$x[-nrow(smooth_df)]
) / 2


# =========================================================
# Second derivative
# =========================================================

d2 <- diff(d1) /
  diff(x1)

x2 <- (
  x1[-1] +
    x1[-length(x1)]
) / 2


idx <- which(
  diff(
    sign(d2)
  ) != 0
)


main_inflection <- candidate_main %>%
  dplyr::slice_max(
    order_by = Curvature,
    n = 1,
    with_ties = FALSE
  )


main_inflection


cat(
  "Primary LOESS inflection =",
  round(
    main_inflection$Distance_km,
    2
  ),
  "km\n"
)


lm0 <- lm(
  Similarity ~ Distance_km,
  data = plot_map
)


seg_fit <- segmented::segmented(
  lm0,
  seg.Z = ~ Distance_km,
  psi = main_inflection$Distance_km
)


summary(seg_fit)


seg_breakpoint <- seg_fit$psi[
  ,
  "Est."
]


cat(
  "Segmented breakpoint =",
  round(seg_breakpoint, 2),
  "km\n"
)


# =========================================================
# Plot
# =========================================================

p <- ggplot(
  plot_map,
  aes(
    x = Distance_km,
    y = Similarity
  )
) +
  
  geom_point(
    size = 1.8,
    alpha = 0.5
  ) +
  
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = TRUE,
    color = "red",
    linewidth = 1.1,
    alpha = 0.2
  ) +
  
  geom_vline(
    xintercept =
      main_inflection$Distance_km,
    linetype = "dashed"
  ) +
  
  geom_point(
    data = main_inflection,
    aes(
      x = Distance_km,
      y = Similarity
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  
  labs(
    x = "Geographic distance (km)",
    y = "ARG Bray-Curtis similarity"
  ) +
  
  theme_classic()


p