# =========================================================
# Aggregated Boosted Tree (ABT) analysis
# =========================================================

library(dismo)
library(gbm)

rm(list = ls())


abt_data <- read.table(
  "ARGglacier_data.txt",
  header = TRUE,
  sep = "\t",
  row.names = 1,
  check.names = FALSE
)

abt_data <- as.data.frame(abt_data)



abt_saline <- gbm.step(
  data = abt_data,
  gbm.x = 1:6,          # explanatory variables
  gbm.y = 7,            # response variable
  family = "gaussian",
  tree.complexity = 1,
  learning.rate = 0.001,
  bag.fraction = 0.75
)



abt_importance_saline <- summary(abt_saline)

abt_importance_saline


abt_glaicer <- gbm.step(
  data = abt_data,
  gbm.x = 8:13,          # explanatory variables
  gbm.y = 7,            # response variable
  family = "gaussian",
  tree.complexity = 1,
  learning.rate = 0.001,
  bag.fraction = 0.75
)

abt_importance_glacier <- summary(abt_glaicer)

abt_importance_glacier



