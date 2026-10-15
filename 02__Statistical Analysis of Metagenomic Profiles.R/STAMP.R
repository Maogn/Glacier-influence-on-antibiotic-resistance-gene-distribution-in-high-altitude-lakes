# =========================================================
# Statistical Analysis of Metagenomic Profiles
# and Benjamini-Hochberg correction
# =========================================================

library(tidyverse)
library(broom)
library(patchwork)
library(openxlsx)
library(eoffice)

rm(list = ls())


# ---------------------------------------------------------
# 1. Load abundance data
# ---------------------------------------------------------

abund <- data.table::fread(
  "data.txt",
  data.table = FALSE
)

sample_names <- abund[, 1]

abund <- abund[, -1]
rownames(abund) <- sample_names


# ---------------------------------------------------------
# 2. Load group information
# ---------------------------------------------------------

metadata <- read.csv(
  "group.csv",
  header = TRUE,
  check.names = FALSE
)


# ---------------------------------------------------------
# 3. Prepare data
# ---------------------------------------------------------

abund_t <- t(abund)

data_all <- data.frame(
  abund_t,
  Group = metadata$Group,
  check.names = FALSE
)

data_all$Group <- as.factor(data_all$Group)


# ---------------------------------------------------------
# 4. Wilcoxon rank-sum test
# ---------------------------------------------------------

diff_result <- data_all %>%
  dplyr::select(where(is.numeric)) %>%
  purrr::map_df(
    ~ broom::tidy(
      wilcox.test(
        .x ~ Group,
        data = data_all,
        conf.int = TRUE
      )
    ),
    .id = "var"
  )


# ---------------------------------------------------------
# 5. Benjamini-Hochberg correction
# ---------------------------------------------------------

diff_result$p.adjust <- p.adjust(
  diff_result$p.value,
  method = "BH"
)


# ---------------------------------------------------------
# 6. Select variables for plotting
# ---------------------------------------------------------

# NOTE:
# p.adjust < 0.5 is used here for visualization.
# For statistical significance, p.adjust < 0.05 is usually used.

diff_sig <- diff_result %>%
  filter(p.adjust < 0.5)


# ---------------------------------------------------------
# 7. Calculate mean abundance for each group
# ---------------------------------------------------------

abun_bar <- data_all %>%
  dplyr::select(
    all_of(diff_sig$var),
    Group
  ) %>%
  pivot_longer(
    cols = -Group,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, Group) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 8. Prepare difference and confidence interval data
# ---------------------------------------------------------

diff_mean <- diff_sig %>%
  dplyr::select(
    var,
    estimate,
    conf.low,
    conf.high,
    p.value,
    p.adjust
  )

diff_mean$Group <- ifelse(
  diff_mean$estimate > 0,
  levels(data_all$Group)[1],
  levels(data_all$Group)[2]
)

diff_mean <- diff_mean %>%
  arrange(desc(estimate))


# ---------------------------------------------------------
# 9. Factor order
# ---------------------------------------------------------

abun_bar$variable <- factor(
  abun_bar$variable,
  levels = rev(diff_mean$var)
)

diff_mean$var <- factor(
  diff_mean$var,
  levels = levels(abun_bar$variable)
)


# ---------------------------------------------------------
# 10. Plot settings
# ---------------------------------------------------------

group_colors <- c(
  "#B3CDE3",
  "#DECBE4"
)

common_theme <- theme(
  panel.background = element_rect(fill = "transparent"),
  panel.grid = element_blank(),
  axis.ticks.length = unit(0.4, "lines"),
  axis.ticks = element_line(color = "black"),
  axis.line = element_line(colour = "black"),
  axis.title.x = element_text(
    colour = "black",
    size = 12,
    face = "bold"
  ),
  axis.text = element_text(
    colour = "black",
    size = 10,
    face = "bold"
  )
)


# ---------------------------------------------------------
# 11. Panel A: Mean abundance
# ---------------------------------------------------------

p1 <- ggplot(
  abun_bar,
  aes(
    x = variable,
    y = Mean,
    fill = Group
  )
) +
  coord_flip() +
  xlab("") +
  ylab("Mean proportion (%)") +
  common_theme +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12,
      face = "bold",
      colour = "black"
    ),
    legend.position = "top",
    legend.direction = "horizontal"
  )

# Alternating background
if (nrow(diff_mean) > 1) {
  for (i in seq_len(nrow(diff_mean) - 1)) {
    p1 <- p1 +
      annotate(
        "rect",
        xmin = i + 0.5,
        xmax = i + 1.5,
        ymin = -Inf,
        ymax = Inf,
        fill = ifelse(
          i %% 2 == 0,
          "white",
          "gray95"
        )
      )
  }
}

p1 <- p1 +
  geom_col(
    position = "dodge",
    width = 0.7,
    colour = "black"
  ) +
  scale_fill_manual(
    values = group_colors
  )


# ---------------------------------------------------------
# 12. Panel B: Difference between groups
# ---------------------------------------------------------

p2 <- ggplot(
  diff_mean,
  aes(
    x = var,
    y = estimate,
    fill = Group
  )
) +
  coord_flip() +
  xlab("") +
  ylab("Difference between groups (%)") +
  labs(
    title = "95% confidence intervals"
  ) +
  common_theme +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      size = 15,
      face = "bold",
      colour = "black",
      hjust = 0.5
    )
  )

# Alternating background
if (nrow(diff_mean) > 1) {
  for (i in seq_len(nrow(diff_mean) - 1)) {
    p2 <- p2 +
      annotate(
        "rect",
        xmin = i + 0.5,
        xmax = i + 1.5,
        ymin = -Inf,
        ymax = Inf,
        fill = ifelse(
          i %% 2 == 0,
          "white",
          "gray95"
        )
      )
  }
}

p2 <- p2 +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.5,
    linewidth = 0.5
  ) +
  geom_point(
    shape = 21,
    size = 3
  ) +
  scale_fill_manual(
    values = group_colors
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  )


# ---------------------------------------------------------
# 13. Panel C: Raw and BH-adjusted P values
# ---------------------------------------------------------

diff_mean <- diff_mean %>%
  mutate(
    p.value.label = signif(p.value, 3),
    p.adjust.label = signif(p.adjust, 3)
  )

p3 <- ggplot(
  diff_mean,
  aes(x = var)
) +
  
  geom_text(
    aes(
      y = 0.15,
      label = p.value.label
    ),
    hjust = 0.5,
    size = 3,
    fontface = "bold"
  ) +
  
  geom_text(
    aes(
      y = 0.65,
      label = p.adjust.label
    ),
    hjust = 0.5,
    size = 3,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = nrow(diff_mean) + 0.7,
    y = 0.15,
    label = "P-value",
    angle = 90,
    fontface = "bold",
    size = 4
  ) +
  
  annotate(
    "text",
    x = nrow(diff_mean) + 0.7,
    y = 0.65,
    label = "BH-adjusted P",
    angle = 90,
    fontface = "bold",
    size = 4
  ) +
  
  coord_flip() +
  
  ylim(-0.1, 1) +
  
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# ---------------------------------------------------------
# 14. Combine plots
# ---------------------------------------------------------

p <- p1 + p2 + p3 +
  plot_layout(
    widths = c(4, 6, 3),
    guides = "keep"
  )

p


# ---------------------------------------------------------
# 15. Export P values
# ---------------------------------------------------------

p_output <- diff_mean %>%
  dplyr::select(
    var,
    p.value,
    p.adjust
  ) %>%
  dplyr::rename(
    Variable = var,
    `P-value` = p.value,
    `BH-adjusted P` = p.adjust
  )

