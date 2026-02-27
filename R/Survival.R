library(xlsx)
library(here)
library(dplyr)
library(stringr)
# Read the Survival data
csoil.type <- c("MC", "peat", "S3")
cherb.type <- c("Forb", "Grass")
cspecies <- c("ACMI", "SYER", "ARLU", "HEVI", "PASM", "BOGR", "NAVI", "HECO")
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
    survived = as.integer(survived)
  ) %>%
  select(all_of(cCol))

SrvlMdlM <- glm(
  cbind(survived, planted - survived) ~ species + soil.type + plant.date + fertilized,
  family = binomial(link = "logit"),
  data = Survival,
  subset = planted > 0
)
summary(SrvlMdlM)
anova(SrvlMdlM)


SrvlMdlI <- glm(
  cbind(count, 98 - count) ~ species + soil.type + species *
    soil.type,
  data = Survival,
  binomial(link = "logit")
)
summary(SrvlMdlI)
anova(SrvlMdlI)

AIC(SrvlMdlM, SrvlMdlI)

# Make an expanded grid of species and soil types
expan
pred <- predict(model, newdata, type="link", se.fit=TRUE)