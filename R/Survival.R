library(xlsx)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(IPMbook)
library(ggplot2)

# Read the Survival data
csoil.type <- c("MC", "peat", "S3")
cfertilized <-c("NF", "F")
cherb.type <- c("Forb", "Grass")
cspecies <- c("ACMI", "ARLU", "HEVI", "PASM", "BOGR", "NAVI", "HECO")
# , "SYER" missing?
cplantdate <- c("2025-07-10", "2025-08-14", "2025-09-11")
cCol <- c(
  "plot",
  "soil.type",
  "species",
  "plant.date",
  "fertilized",
  "planted",
  "survived",
  "survival.rate"
)

# Read the survival data from the simplified sheet.
Survival <- read.xlsx(
  file = paste0(here("ori"), "/JCOS data - MG.xlsx"),
  sheetName = "alldatasimplified",
  rowIndex = 1:85,
  colIndex = c(1:8),
  colClasses = c(
    "numeric",
    "character",
    "character",
    "Date",
    "character",
    "numeric",
    "numeric",
    "numeric"
  )
) %>%
  mutate(
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(species, levels = cspecies),
    plant.date = factor(plant.date, levels = cplantdate),
    fertilized = factor(fertilized, levels = cfertilized),
    survived = as.integer(survived)
  ) %>%
  select(all_of(cCol))

# Model selection
SrvlMdlM <- glm(
  cbind(survived, planted - survived) ~ species + soil.type + plant.date + fertilized,
  family = binomial(link = "logit"),
  data = Survival
  # ,subset = planted > 0
)
summary(SrvlMdlM)
anova(SrvlMdlM)

SrvlMdlI <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date + fertilized,
  data = Survival,
  family = binomial(link = "logit")
)
summary(SrvlMdlI)
anova(SrvlMdlI)

# Negative Hessian
SrvlMdlLME <- glmmTMB(
  cbind(survived, planted - survived) ~ 1 + (species|soil.type) + plant.date + fertilized,
  # (species|plant.date)
  data = Survival,
  family = binomial(link = "logit")
)
summary(SrvlMdlLME)
anova(SrvlMdlLME)

SrvlMdlnF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date,
  data = Survival,
  family = binomial(link = "logit")
)
summary(SrvlMdlnF)
anova(SrvlMdlnF)

SrvlMdlF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type * plant.date + fertilized,
  data = Survival,
  binomial(link = "logit")
)
summary(SrvlMdlF)
anova(SrvlMdlF)

# The interaction with fertilized is best.
AIC(SrvlMdlM, SrvlMdlI, SrvlMdlnF, SrvlMdlF)

# Make an expanded grid of species and soil types

# Make an expanded grid of species, soil types, and plant dates
CmbFac <- expand_grid(
  species = cspecies,
  soil.type = csoil.type,
  plant.date = cplantdate,
  fertilized = cfertilized
) %>%
  mutate(
    soil.type = factor(soil.type, csoil.type),
    plant.date = factor(plant.date, cplantdate),
    fertilized = factor(fertilized, levels = cfertilized),
    herb.type = factor(ifelse(
      str_sub(species, 1, 4) %in% c("ACMI", "SYER", "ARLU", "HEVI"),
      "Forb",
      "Grass"
    ), levels = cherb.type),
    species = factor(species, cspecies)
  )

# can work these to get the herb type and soil type statistics and plots.
fitterms <- predict(
  object = SrvlMdlI,
  newdata = CmbFac,
  type = "terms",
  se.fit = TRUE
)

intercept <- attr(fitterms$fit, "constant")
tfit <- fitterms$fit
colnames(tfit) <- paste(colnames(tfit), "Mn", sep = "")
tse <- fitterms$se.fit
colnames(tse) <- paste(colnames(tse), "SE", sep = "")

Fit <- bind_cols(CmbFac, tfit, tse)

# Make the marginal matrices
# Species
# Note that must be < sqrt(p * (1 - p))
# HEVI and HECO are sparse.

SpeciesFit <- Fit %>%
  distinct(species, herb.type, speciesMn, speciesSE) %>%
  mutate(
    lgt.species = intercept + speciesMn,
    lgt.speciesSE = speciesSE,
    pspecies = 1 / (1 + exp(-lgt.species)),
    MxSE = 0.8 * sqrt(pspecies * (1 - pspecies)),
    pspeciesSE = pmin(MxSE, pspecies * (1 - pspecies) * lgt.speciesSE),
    Median = qbeta2(0.5, mean = pspecies, sd = pspeciesSE),
    CI05 = qbeta2(0.05, mean = pspecies, sd = pspeciesSE),
    CI95 = qbeta2(0.95, mean = pspecies, sd = pspeciesSE)
  ) %>%
  select(
    species,
    herb.type,
    lgt.species,
    lgt.speciesSE,
    pspecies,
    pspeciesSE,
    Median,
    CI05,
    CI95
  )



