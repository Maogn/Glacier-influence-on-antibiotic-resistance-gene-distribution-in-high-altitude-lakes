# =========================================================
# Procrustes analysis between MGE and ARG profiles
# Saline lakes
# =========================================================

rm(list = ls())

library(vegan)
library(dplyr)
library(ggplot2)


# ---------------------------------------------------------
# 1. Load group information
# ---------------------------------------------------------

group <- read.delim(
  "group_sal.txt",
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

colnames(group)[1] <- "clade"


# ---------------------------------------------------------
# 2. Load MGE abundance table
# ---------------------------------------------------------

mge <- read.delim(
  "MGE_table_sal.txt",
  row.names = 1,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ---------------------------------------------------------
# 3. Load ARG abundance table
# ---------------------------------------------------------

arg <- read.delim(
  "arg_table_sal.txt",
  row.names = 1,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ---------------------------------------------------------
# 4. Check sample order
# ---------------------------------------------------------

identical(
  rownames(mge)[1:113],
  rownames(arg)[1:113]
)


# ---------------------------------------------------------
# 5. Bray-Curtis dissimilarity
# ---------------------------------------------------------

mge_dist <- vegdist(
  mge[1:113, 1:310],
  method = "bray"
)

arg_dist <- vegdist(
  arg[1:113, 1:1016],
  method = "bray"
)


# ---------------------------------------------------------
# 6. NMDS
# ---------------------------------------------------------

mds_mge <- monoMDS(mge_dist)

mds_arg <- monoMDS(arg_dist)


# ---------------------------------------------------------
# 7. Procrustes analysis
# ---------------------------------------------------------

pro_fit <- procrustes(
  mds_mge,
  mds_arg,
  symmetric = TRUE
)

summary(pro_fit)

plot(
  pro_fit,
  kind = 2
)

residuals(pro_fit)


# ---------------------------------------------------------
# 8. PROTEST permutation test
# ---------------------------------------------------------

set.seed(1)

pro_test <- protest(
  mds_mge,
  mds_arg,
  permutations = 999
)

pro_test

# M2 statistic
M2 <- pro_test$ss

# P value
p_value <- pro_test$signif

M2
p_value


# ---------------------------------------------------------
# 9. Prepare plotting data
# ---------------------------------------------------------

Pro_Y <- cbind(
  data.frame(pro_fit$Yrot),
  data.frame(pro_fit$X)
)

Pro_X <- data.frame(
  pro_fit$rotation
)

Pro_Y$clade <- rownames(Pro_Y)


# ---------------------------------------------------------
# 10. Add group information
# ---------------------------------------------------------

Pro_Y <- dplyr::left_join(
  Pro_Y,
  group,
  by = "clade"
)

colnames(Pro_Y)[6] <- "type"

rownames(Pro_Y) <- Pro_Y$clade

Pro_Y <- Pro_Y[, -5]


# ---------------------------------------------------------
# 11. Procrustes plot
# ---------------------------------------------------------

p <- ggplot(data = Pro_Y) +
  
  # Connection between MGE and ARG ordinations
  geom_segment(
    aes(
      x = X1,
      y = X2,
      xend = MDS1,
      yend = MDS2
    ),
    arrow = arrow(
      length = unit(0, "cm")
    ),
    linewidth = 0.5,
    color = "darkgrey"
  ) +
  
  # MGE points
  geom_point(
    aes(
      x = X1,
      y = X2
    ),
    size = 3,
    shape = 16,
    color = "#9AC2CD"
  ) +
  
  # ARG points
  geom_point(
    aes(
      x = MDS1,
      y = MDS2
    ),
    size = 3,
    shape = 17,
    color = "#9AC2CD"
  ) +
  
  # Reference lines
  geom_vline(
    xintercept = 0,
    color = "gray",
    linetype = 2,
    linewidth = 0.3
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "gray",
    linetype = 2,
    linewidth = 0.3
  ) +
  
  # Rotation axes
  geom_abline(
    intercept = 0,
    slope = Pro_X[1, 2] / Pro_X[1, 1],
    linewidth = 0.3
  ) +
  
  geom_abline(
    intercept = 0,
    slope = Pro_X[2, 2] / Pro_X[2, 1],
    linewidth = 0.3
  ) +
  
  # Statistical annotation
  annotate(
    "text",
    label = paste0(
      "Procrustes analysis:\n",
      "M² = ",
      round(M2, 2),
      ", p-value = ",
      signif(p_value, 3)
    ),
    x = -0.2,
    y = 0.15,
    size = 4,
    hjust = 0
  ) +
  
  # Labels
  labs(
    x = "Dimension 1",
    y = "Dimension 2",
    title = "Correlation between MGEs and ARGs"
  ) +
  
  # Theme
  theme(
    panel.background = element_rect(
      color = "black",
      fill = "transparent"
    ),
    
    panel.grid = element_blank(),
    
    axis.ticks.length = unit(
      0.4,
      "lines"
    ),
    
    axis.ticks = element_line(
      color = "black"
    ),
    
    axis.line = element_line(
      colour = "black"
    ),
    
    axis.title.x = element_text(
      colour = "black",
      size = 14
    ),
    
    axis.title.y = element_text(
      colour = "black",
      size = 14
    ),
    
    axis.text = element_text(
      colour = "black",
      size = 12
    ),
    
    plot.title = element_text(
      size = 14,
      colour = "black",
      hjust = 0.5,
      face = "bold"
    )
  )


# ---------------------------------------------------------
# 12. Display plot
# ---------------------------------------------------------

p


