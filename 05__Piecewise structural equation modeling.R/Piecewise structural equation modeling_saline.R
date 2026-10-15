# =========================================================
# Piecewise structural equation modeling
# Saline lakes
# =========================================================

rm(list = ls())

library(ggplot2)
library(piecewiseSEM)


# ---------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------

dt <- read.csv(
  "DATAsaline.csv",
  header = TRUE,
  check.names = FALSE
)

head(dt)


# =========================================================
# 2. Construct composite variables
# =========================================================


# ---------------------------------------------------------
# 2.1 Spatial factors
# ---------------------------------------------------------

Space_model <- lm(
  ARG ~ Longitude + Latitude + Altitude + Area + DevelopmentIndex,
  data = dt
)

summary(Space_model)

coefs(
  Space_model,
  standardize = "scale"
)

beta_space <- coef(Space_model)[-1]

dt$Space <-
  beta_space["Longitude"]        * dt$Longitude +
  beta_space["Latitude"]         * dt$Latitude +
  beta_space["Altitude"]         * dt$Altitude +
  beta_space["Area"]             * dt$Area +
  beta_space["DevelopmentIndex"] * dt$DevelopmentIndex

summary(
  lm(ARG ~ Space, data = dt)
)

coefs(
  lm(ARG ~ Space, data = dt),
  standardize = "scale"
)


# ---------------------------------------------------------
# 2.2 Lake physicochemical factors
# ---------------------------------------------------------

Lake_model <- lm(
  ARG ~ Temp + pH + NH4 + NO3 + NO2 + PO4,
  data = dt
)

summary(Lake_model)

coefs(
  Lake_model,
  standardize = "scale"
)

beta_lake <- coef(Lake_model)[-1]

dt$Lake <-
  beta_lake["Temp"] * dt$Temp +
  beta_lake["pH"]   * dt$pH +
  beta_lake["NH4"]  * dt$NH4 +
  beta_lake["NO3"]  * dt$NO3 +
  beta_lake["NO2"]  * dt$NO2 +
  beta_lake["PO4"]  * dt$PO4

summary(
  lm(ARG ~ Lake, data = dt)
)

coefs(
  lm(ARG ~ Lake, data = dt),
  standardize = "scale"
)


# ---------------------------------------------------------
# 2.3 Catchment factors
# ---------------------------------------------------------

Catchment_model <- lm(
  ARG ~ CMI + NPP + bare_land +
    forest_percent +
    grassland_percent +
    wetland_water_percent +
    Glacier_percent +
    FSC,
  data = dt
)

summary(Catchment_model)

coefs(
  Catchment_model,
  standardize = "scale"
)

beta_catchment <- coef(Catchment_model)[-1]

dt$Catchment <-
  beta_catchment["CMI"]                   * dt$CMI +
  beta_catchment["NPP"]                   * dt$NPP +
  beta_catchment["bare_land"]             * dt$bare_land +
  beta_catchment["forest_percent"]        * dt$forest_percent +
  beta_catchment["grassland_percent"]     * dt$grassland_percent +
  beta_catchment["wetland_water_percent"] * dt$wetland_water_percent +
  beta_catchment["Glacier_percent"]       * dt$Glacier_percent +
  beta_catchment["FSC"]                   * dt$FSC

summary(
  lm(ARG ~ Catchment, data = dt)
)

coefs(
  lm(ARG ~ Catchment, data = dt),
  standardize = "scale"
)


# ---------------------------------------------------------
# 2.4 Human influence
# ---------------------------------------------------------

Human_model <- lm(
  ARG ~ HumanFootprint,
  data = dt
)

summary(Human_model)

coefs(
  Human_model,
  standardize = "scale"
)

beta_human <- coef(Human_model)["HumanFootprint"]

dt$Human <-
  beta_human * dt$HumanFootprint

summary(
  lm(ARG ~ Human, data = dt)
)

coefs(
  lm(ARG ~ Human, data = dt),
  standardize = "scale"
)


# ---------------------------------------------------------
# 2.5 MGE
# ---------------------------------------------------------

MGE_model <- lm(
  ARG ~ MGE,
  data = dt
)

summary(MGE_model)

coefs(
  MGE_model,
  standardize = "scale"
)

beta_mge <- coef(MGE_model)["MGE"]

# Keep the original MGE variable unchanged
dt$MGE_composite <-
  beta_mge * dt$MGE

summary(
  lm(ARG ~ MGE_composite, data = dt)
)

coefs(
  lm(ARG ~ MGE_composite, data = dt),
  standardize = "scale"
)


# =========================================================
# 3. Piecewise SEM
# =========================================================

dt_psem1 <- psem(
  
  lm(
    ARG ~ MGE_composite + Lake,
    data = dt
  ),
  
  lm(
    MGE_composite ~ Lake,
    data = dt
  ),
  
  lm(
    Lake ~ Catchment,
    data = dt
  )
  
)

summary(
  dt_psem1,
  .progressBar = FALSE
)


# ---------------------------------------------------------
# 4. Plot SEM
# ---------------------------------------------------------

p1 <- plot(dt_psem1)

p1


# =========================================================
# 5. Additional regression analyses
# =========================================================

# Spatial effect
model_longitude <- lm(
  ARG ~ Longitude,
  data = dt
)

summary(model_longitude)


# Lake physicochemical factors
model_lake <- lm(
  ARG ~ Temp + NO3,
  data = dt
)

summary(model_lake)

coefs(
  model_lake,
  standardize = "scale"
)


# Catchment factors
model_catchment <- lm(
  ARG ~ bare_land +
    grassland_percent +
    wetland_water_percent,
  data = dt
)

summary(model_catchment)

coefs(
  model_catchment,
  standardize = "scale"
)
