# =========================================================
# Glacier source contribution analysis using FEAST
# =========================================================

rm(list = ls())

library(geosphere)
library(dplyr)
library(tidyr)
library(FEAST)


# =========================================================
# 1. Read lake and glacier coordinates
# =========================================================

lake <- read.csv(
  "lake.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

glacier <- read.csv(
  "glacier.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# =========================================================
# 2. Generate all lake-glacier combinations
# =========================================================

result <- expand.grid(
  Lake = lake$Lake,
  Glacier = glacier$Glacier,
  stringsAsFactors = FALSE
)

# Add lake coordinates
result <- result %>%
  dplyr::left_join(
    lake %>%
      dplyr::select(
        Lake,
        Lake_lon = Longitude,
        Lake_lat = Latitude
      ),
    by = "Lake"
  )

# Add glacier coordinates
result <- result %>%
  dplyr::left_join(
    glacier %>%
      dplyr::select(
        Glacier,
        Glacier_lon = Longitude,
        Glacier_lat = Latitude
      ),
    by = "Glacier"
  )


# =========================================================
# 3. Calculate geographic distance
# =========================================================

result$Distance_km <- geosphere::distHaversine(
  cbind(
    result$Lake_lon,
    result$Lake_lat
  ),
  cbind(
    result$Glacier_lon,
    result$Glacier_lat
  )
) / 1000


# =========================================================
# 4. Select glaciers within 500 km
# =========================================================

result_500 <- result %>%
  dplyr::filter(
    Distance_km <= 500
  )

# Check
table(result_500$Lake)

result_500


# =========================================================
# 5. Read abundance table for FEAST
# =========================================================

otu <- read.table(
  "otu_table.txt",
  header = TRUE,
  row.names = 1,
  sep = "\t",
  check.names = FALSE
)

# Samples as rows, features as columns
otu <- t(otu)

dim(otu)

head(rownames(otu), 20)
head(colnames(otu), 20)


# =========================================================
# 6. Run FEAST for each lake
# =========================================================

FEAST_results <- list()

lake_names <- unique(
  result_500$Lake
)

for (lake_i in lake_names) {
  
  cat(
    "Running:",
    lake_i,
    "\n"
  )
  
  # Candidate glacier sources within 500 km
  source_i <- result_500 %>%
    dplyr::filter(
      Lake == lake_i
    ) %>%
    dplyr::pull(
      Glacier
    ) %>%
    unique()
  
  # Retain only glacier samples present in otu table
  source_i <- source_i[
    source_i %in% rownames(otu)
  ]
  
  
  # Check sink
  if (!(lake_i %in% rownames(otu))) {
    
    cat(
      "Skip:",
      lake_i,
      "- lake sample not found in OTU table\n"
    )
    
    next
  }
  
  
  # Check sources
  if (length(source_i) == 0) {
    
    cat(
      "Skip:",
      lake_i,
      "- no glacier source found\n"
    )
    
    next
  }
  
  
  # Combine sources and sink
  samples_i <- c(
    source_i,
    lake_i
  )
  
  otu_i <- otu[
    samples_i,
    ,
    drop = FALSE
  ]
  
  
  # FEAST metadata
  metadata_i <- data.frame(
    Env = samples_i,
    
    SourceSink = c(
      rep(
        "Source",
        length(source_i)
      ),
      "Sink"
    ),
    
    id = rep(
      1,
      length(samples_i)
    ),
    
    row.names = samples_i,
    stringsAsFactors = FALSE
  )
  
  
  # Run FEAST
  feast_i <- FEAST(
    C = otu_i,
    metadata = metadata_i,
    EM_iterations = 1000,
    different_sources_flag = 1,
    dir_path = getwd(),
    outfile = lake_i
  )
  
  
  FEAST_results[[lake_i]] <- feast_i
}


# =========================================================
# 7. Find FEAST output files
# =========================================================

files <- list.files(
  path = getwd(),
  pattern = "_source_contributions_matrix\\.txt$",
  full.names = TRUE
)

files


# =========================================================
# 8. Combine all FEAST results
# =========================================================

result_all <- do.call(
  rbind,
  
  lapply(
    files,
    function(f) {
      
      x <- read.table(
        f,
        header = TRUE,
        row.names = 1,
        sep = "\t",
        check.names = FALSE
      )
      
      lake_name <- sub(
        "_source_contributions_matrix\\.txt$",
        "",
        basename(f)
      )
      
      data.frame(
        Lake = lake_name,
        Glacier = colnames(x),
        Contribution = as.numeric(
          x[1, ]
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(result_all) <- NULL


# =========================================================
# 9. Convert contribution to percentage
# =========================================================

result_all <- result_all %>%
  dplyr::mutate(
    Contribution_percent =
      Contribution * 100
  )


# =========================================================
# 10. Clean duplicated FEAST source names
# =========================================================

result_all <- result_all %>%
  dplyr::mutate(
    Glacier = sub(
      "^(.+)_\\1$",
      "\\1",
      Glacier
    )
  )

head(result_all$Glacier)


# =========================================================
# 11. Add geographic distance
# =========================================================

result_all <- result_all %>%
  dplyr::left_join(
    
    result_500 %>%
      dplyr::select(
        Lake,
        Glacier,
        Distance_km
      ) %>%
      dplyr::distinct(),
    
    by = c(
      "Lake",
      "Glacier"
    )
  )
# =========================================================
# 12.Export
# =========================================================

result_export <- result_all %>%
  dplyr::filter(
    Lake != "FEAST"
  )

write.csv(
  result_export,
  "FEAST_result_500km.csv",
  row.names = FALSE
)
