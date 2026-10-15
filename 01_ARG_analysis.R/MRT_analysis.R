# =========================================================
# Multivariate Regression Tree (MRT) analysis
# =========================================================

library(mvpart)

rm(list = ls())

# ---------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------

data <- read.csv(
  "ARGdataMRT.csv",
  row.names = 1,
  check.names = FALSE
)

head(data)


# ---------------------------------------------------------
# 2. MRT analysis with multiple environmental variables
# ---------------------------------------------------------

arg_cols <- 1:31

fit_glacier <- mvpart(
  data.matrix(data[, arg_cols]) ~Glacier ,
  data = data,
  xv = "1se",
  xadj = 1.5,
  yadj = 1.5,
  legend = FALSE,
  which = 4,
  branch = 1,
  minauto = TRUE,
  text.add = TRUE,
  digits = 3
)


# ---------------------------------------------------------
# 3. MRT analysis using salinity as the predictor
# ---------------------------------------------------------

fit_salinity <- mvpart(
  data.matrix(data[, arg_cols]) ~ Salinity,
  data = data,
  xv = "1se",
  xadj = 1.5,
  yadj = 1.5,
  legend = FALSE,
  which = 4,
  branch = 1,
  minauto = TRUE,
  text.add = TRUE,
  digits = 3
)
