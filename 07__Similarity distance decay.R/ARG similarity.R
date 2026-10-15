rm(list = ls())

library(vegan)
library(dplyr)

df <- read.csv(
  "glacierfreshwaterARG.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Clean names
df$LakeEng <- trimws(
  gsub("\u00A0", "", df$LakeEng)
)

df$type <- tolower(
  trimws(df$type)
)

table(df$type)


ARG <- df[
  ,
  3:ncol(df),
  drop = FALSE
]

ARG <- as.data.frame(
  lapply(
    ARG,
    as.numeric
  )
)

rownames(ARG) <- df$LakeEng


# =========================================================
# Bray-Curtis dissimilarity
# =========================================================

bray_dist <- vegan::vegdist(
  ARG,
  method = "bray"
)

bray_matrix <- as.matrix(
  bray_dist
)

sim_matrix <- 1 - bray_matrix


# =========================================================
# Get lake/glacier names
# =========================================================

lake_names <- df$LakeEng[
  df$type == "lake"
]

glacier_names <- df$LakeEng[
  df$type == "glacier"
]


cat(
  "Lake number =",
  length(lake_names),
  "\n"
)

cat(
  "Glacier number =",
  length(glacier_names),
  "\n"
)


# =========================================================
# Extract only lake × glacier similarity
# =========================================================

sim_lg <- sim_matrix[
  lake_names,
  glacier_names,
  drop = FALSE
]


dim(sim_lg)


similarity_long <- expand.grid(
  Lake = lake_names,
  Glacier = glacier_names,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

similarity_long$Similarity <- as.vector(
  sim_lg
)


write.csv(
  similarity_long,
  "Lake_Glacier_ARG_similarity.csv",
  row.names = FALSE
)