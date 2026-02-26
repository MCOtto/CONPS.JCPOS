library(xlsx)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(IPMbook)
# Read the germination data
csoil.type <- c("Peat", "Mineral/Compost")

# Grass BOGR Blue gramma
# Grass HECO Hesperostipa comata
# Forb HEVI Helleborus viridis
# Grass? NAVI ?
# Grass PASM Paspalum genus n the Poaceae family
# Forb  SYER White heath aster (Symphyotrichum ericoides)
cfrb.grs <- c("Forb", "Forb", "Grass", "Grass", "Forb", "Grass", "Grass", "Forb")
cspecies <- c("ACMI", "ARLU", "BOGR", "HECO", "HEVI", "NAVI", "PASM", "SYER")
Germination <- read.xlsx(
  file = paste0(here("ori"), "/JCOS data - MG.xlsx"),
  sheetName = "germination rates",
  rowIndex = 1:17,
  colIndex = c(1, 2, 4),
  colClasses = c("text", "text", "numeric")
) %>%
  mutate(
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(
      str_sub(species, 1, 4),
      levels = cspecies
    )
  )

GrmnMdlM <- glm(cbind(count, 98 - count) ~ species + soil.type,
                data = Germination,
                binomial(link = "logit"))
summary(GrmnMdlM)


GrmnMdlI <- glm(
  cbind(count, 98 - count) ~ species + soil.type + species *
    soil.type,
  data = Germination,
  binomial(link = "logit")
)
summary(GrmnMdlI)
anova(GrmnMdlI)

AIC(GrmnMdlM, GrmnMdlI)


# Make an expanded grid of species and soil types
CmbFac <- expand_grid(species = cspecies, soil.type = csoil.type) %>%
  mutate(
    soil.type = factor(soil.type, csoil.type),
    species = factor(species, cspecies)
  )

pred <- predict(
  object = GrmnMdlM,
  newdata = CmbFac,
  type = "response",
  se.fit = TRUE
)

Pred <- bind_cols(CmbFac,Mean = pred$fit, SE = pred$se.fit) %>%
  mutate(
    Median = qbeta2(0.5, mean = Mean, sd = SE),
    CI05 = qbeta2(0.05, mean = Mean, sd = SE),
    CI95 = qbeta2(0.95, mean = Mean, sd = SE)
  )


