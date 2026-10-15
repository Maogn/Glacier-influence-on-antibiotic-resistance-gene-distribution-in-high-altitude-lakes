# =========================================================
# MAG relative abundance + bubble plot + BH-adjusted P values
# =========================================================

rm(list = ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(openxlsx)


# =========================================================
# 1. MAG count bubble plot
# =========================================================

mag_count <- read.csv(
  "MAG-counts-f-N.csv",
  header = TRUE,
  check.names = FALSE
)

# Wide to long
mag_count_long <- mag_count %>%
  pivot_longer(
    cols = -1,
    names_to = "MAG",
    values_to = "count"
  )

# Assume the first column is genus
colnames(mag_count_long)[1] <- "genus"

# Zero values are not displayed in bubble plot
mag_count_long$count[
  mag_count_long$count == 0
] <- NA


p_bubble <- ggplot(
  mag_count_long,
  aes(
    x = MAG,
    y = genus,
    size = count
  )
) +
  geom_point(
    shape = 21,
    colour = "black",
    fill = "#EE9988"
  ) +
  scale_size_area(
    max_size = 6
  ) +
  scale_x_discrete(
    name = ""
  ) +
  scale_y_discrete(
    name = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    
    axis.title = element_text(
      size = 15,
      family = "serif"
    ),
    
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.border = element_rect(
      linewidth = 0.5
    ),
    
    panel.grid = element_blank()
  )

p_bubble


# =========================================================
# 2. Read MAG relative abundance data
# =========================================================

options(
  scipen = 200,
  digits = 22
)

mag_abund <- read.csv(
  "MAG-relative-s-N.csv",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)

design <- read.csv(
  "design-s.csv",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)


# =========================================================
# 3. Merge sample grouping and MAG abundance
# =========================================================

df <- merge(
  design,
  mag_abund,
  by = "row.names",
  all = FALSE
)

# Check merged data
head(df)


# Wide to long
df_long <- df %>%
  pivot_longer(
    cols = -c(1:2),
    names_to = "MAG",
    values_to = "Abundance"
  )


# ---------------------------------------------------------
# Important:
# Keep zero abundance for statistical testing.
# Do NOT replace zeros with NA here.
# ---------------------------------------------------------

df_long$Type <- factor(
  df_long$Type,
  levels = c(
    "freshwater",
    "saline"
  )
)

# Check group names
table(df_long$Type)


# =========================================================
# 4. Calculate mean MAG abundance
# =========================================================

df_mean <- df_long %>%
  dplyr::group_by(
    Type,
    MAG
  ) %>%
  dplyr::summarise(
    mean.abundance = mean(
      Abundance,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

df_mean$Type <- factor(
  df_mean$Type,
  levels = c(
    "freshwater",
    "saline"
  )
)

head(df_mean)


# =========================================================
# 5. Wilcoxon test for each MAG
# =========================================================

p_result <- df_long %>%
  dplyr::group_by(MAG) %>%
  dplyr::summarise(
    
    n_freshwater = sum(
      Type == "freshwater" &
        !is.na(Abundance)
    ),
    
    n_saline = sum(
      Type == "saline" &
        !is.na(Abundance)
    ),
    
    p.value = tryCatch(
      
      wilcox.test(
        Abundance ~ Type,
        exact = FALSE
      )$p.value,
      
      error = function(e) {
        NA_real_
      }
      
    ),
    
    .groups = "drop"
  )


# =========================================================
# 6. Benjamini-Hochberg correction
# =========================================================

p_result <- p_result %>%
  dplyr::mutate(
    
    p.adjust = p.adjust(
      p.value,
      method = "BH"
    ),
    
    p.value.label = dplyr::case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "<0.001",
      TRUE ~ sprintf(
        "%.3f",
        p.value
      )
    ),
    
    p.adjust.label = dplyr::case_when(
      is.na(p.adjust) ~ "NA",
      p.adjust < 0.001 ~ "<0.001",
      TRUE ~ sprintf(
        "%.3f",
        p.adjust
      )
    )
  )


# Keep original MAG order
MAG_order <- unique(
  df_long$MAG
)

p_result$MAG <- factor(
  p_result$MAG,
  levels = MAG_order
)

df_long$MAG <- factor(
  df_long$MAG,
  levels = MAG_order
)

df_mean$MAG <- factor(
  df_mean$MAG,
  levels = MAG_order
)

mag_count_long$MAG <- factor(
  mag_count_long$MAG,
  levels = MAG_order
)


# Check results
head(p_result)


# =========================================================
# 7. Prepare abundance data for plotting
# =========================================================

# log10(0) cannot be plotted.
# Therefore zero values are removed ONLY from visualization,
# but retained in statistical testing above.

df_plot <- df_long %>%
  dplyr::filter(
    !is.na(Abundance),
    Abundance > 0
  )

df_mean_plot <- df_mean %>%
  dplyr::filter(
    !is.na(mean.abundance),
    mean.abundance > 0
  )


# =========================================================
# 8. MAG relative abundance plot
# =========================================================

p_abundance <- ggplot() +
  
  # Individual samples
  geom_point(
    data = df_plot,
    aes(
      x = MAG,
      y = log10(Abundance),
      colour = Type
    ),
    shape = 16,
    alpha = 0.2,
    size = 2,
    position = position_jitterdodge(
      jitter.width = 0,
      dodge.width = 0.6
    )
  ) +
  
  # Mean abundance
  geom_point(
    data = df_mean_plot,
    aes(
      x = MAG,
      y = log10(mean.abundance),
      fill = Type
    ),
    shape = 21,
    colour = "black",
    size = 4,
    position = position_jitterdodge(
      jitter.width = 0,
      dodge.width = 0.6
    )
  ) +
  
  scale_colour_manual(
    values = c(
      freshwater = "#FEC211",
      saline = "#336699"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      freshwater = "#FEC211",
      saline = "#336699"
    )
  ) +
  
  coord_flip() +
  
  xlab(NULL) +
  
  ylab(
    expression(
      log[10] ~ "(MAG relative abundance)"
    )
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "none",
    legend.background = element_blank(),
    legend.key = element_blank(),
    
    axis.title.x = element_text(
      size = 12,
      family = "serif",
      colour = "black"
    ),
    
    axis.title.y = element_blank(),
    
    axis.text.x = element_text(
      size = 12,
      family = "serif",
      colour = "black"
    ),
    
    axis.text.y = element_text(
      size = 12,
      family = "serif",
      colour = "black"
    ),
    
    axis.line.x = element_line(
      linewidth = 1.5,
      colour = "black"
    ),
    
    axis.line.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    panel.border = element_blank(),
    
    panel.grid = element_blank()
  )

p_abundance


# =========================================================
# 9. BH-adjusted P-value panel
# =========================================================

p_bh <- ggplot(
  p_result,
  aes(
    x = MAG,
    y = 1
  )
) +
  
  geom_text(
    aes(
      label = p.adjust.label
    ),
    size = 3.5,
    family = "serif"
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    limits = c(
      0,
      2
    ),
    expand = c(
      0,
      0
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )

p_bh


# =========================================================
# 10. BH-adjusted significance stars
# Optional
# =========================================================

p_result <- p_result %>%
  dplyr::mutate(
    significance = dplyr::case_when(
      is.na(p.adjust) ~ "",
      p.adjust < 0.001 ~ "***",
      p.adjust < 0.01 ~ "**",
      p.adjust < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

head(p_result)


# =========================================================
# 11. Combine abundance, bubble and BH P-value plots
# =========================================================

figure <- plot_grid(
  p_abundance,
  p_bubble,
  p_bh,
  
  nrow = 1,
  
  rel_widths = c(
    2,
    1,
    0.7
  ),
  
  align = "h",
  axis = "tblr"
)

figure


# =========================================================
# 12. Export statistical results
# =========================================================

p_output <- p_result %>%
  dplyr::select(
    MAG,
    n_freshwater,
    n_saline,
    p.value,
    p.adjust,
    significance
  ) %>%
  dplyr::rename(
    `Raw P value` = p.value,
    `BH-adjusted P value` = p.adjust
  )

write.xlsx(
  p_output,
  file = "MAG_Wilcoxon_BH_results.xlsx",
  rowNames = FALSE
)


# =========================================================
# 13. Export figure
# =========================================================

ggsave(
  filename = "MAG_abundance_BH_plot.pdf",
  plot = figure,
  width = 12,
  height = 8
)

ggsave(
  filename = "MAG_abundance_BH_plot.png",
  plot = figure,
  width = 12,
  height = 8,
  dpi = 600
)
