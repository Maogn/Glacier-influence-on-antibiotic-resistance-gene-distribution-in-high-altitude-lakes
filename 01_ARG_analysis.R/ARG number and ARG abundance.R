# =========================================================
# Violin plots for ARG number and ARG abundance
# =========================================================

library(ggpubr)
library(reshape2)

rm(list = ls())

# 配色
cols <- c("#B3CDE3", "#DECBE4")


# ---------------------------------------------------------
# 1. ARG number
# ---------------------------------------------------------

arg_number <- read.csv(
  "ARGnumber.csv",
  header = TRUE,
  check.names = FALSE
)

arg_number_long <- melt(
  arg_number,
  id.vars = "type",
  variable.name = "variable",
  value.name = "value"
)

p1 <- ggviolin(
  arg_number_long,
  x = "variable",
  y = "value",
  color = "type",
  palette = cols,
  add = c("boxplot", "jitter"),
  add.params = list(
    color = "type"
  ),
  xlab = FALSE,
  legend = "none"
)

p1


# ---------------------------------------------------------
# 2. ARG abundance
# ---------------------------------------------------------

arg_abundance <- read.csv(
  "ARGabundance.csv",
  header = TRUE,
  check.names = FALSE
)

arg_abundance_long <- melt(
  arg_abundance,
  id.vars = "type",
  variable.name = "variable",
  value.name = "value"
)

p2 <- ggviolin(
  arg_abundance_long,
  x = "variable",
  y = "value",
  color = "type",
  palette = cols,
  add = c("boxplot", "jitter"),
  add.params = list(
    color = "type"
  ),
  xlab = FALSE,
  legend = "none"
)

p2
