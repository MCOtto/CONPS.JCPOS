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
  cbind(survived, planted - survived) ~ species + soil.type * fertilized +  plant.date,
  family = binomial(link = "logit"),
  data = Survival
  ,subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlM)
anova(SrvlMdlM)

SrvlMdlSF <- glm(
  cbind(survived, planted - survived) ~ species *  soil.type * fertilized  +  plant.date,
#  cbind(survived, planted - survived) ~ species * soil.type + soil.type * fertilized +  plant.date ,
  family = binomial(link = "logit"),
  data = Survival
  ,subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlSF)
anova(SrvlMdlSF)

SrvlMdlI <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date + fertilized,
  data = Survival,
  family = binomial(link = "logit")
  ,subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlI)
anova(SrvlMdlI)

# Negative Hessian
# SrvlMdlLME <- glmmTMB(
#   #   SrvlMdlLME <- glmer(
#     cbind(survived, planted - survived) ~ 1 + (species|soil.type) + plant.date + fertilized,
#   # (species|plant.date)
#   data = Survival,
#   family = binomial(link = "logit")
#   ,subset = !species %in% c("HEVI", "HECO")
# )
# summary(SrvlMdlLME)
# anova(SrvlMdlLME)

SrvlMdlnF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date,
  data = Survival,
  family = binomial(link = "logit")
  ,subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlnF)
anova(SrvlMdlnF)

SrvlMdlF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type * plant.date + fertilized,
  data = Survival,
  binomial(link = "logit")
  ,subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlF)
anova(SrvlMdlF)

# The interaction with fertilized is best.
AIC(SrvlMdlM, SrvlMdlI, SrvlMdlnF, SrvlMdlF, SrvlMdlSF)

# Make an expanded grid of species and soil types

# Make an expanded grid of species, soil types, and plant dates
CmbFac <- expand_grid(
  species = cspecies[c(1:2,4:6)],
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
    species = factor(species, cspecies[c(1:2,4:6)])
  )

iVar <- c("species", "soil.type")
jVar <- paste(iVar,collapse = ":")
cVar <- "Species by Soil Type"
# Plot survival by species and soil.type with plant.date = "2025-09-11"
iCmbFac <- CmbFac %>%
  filter(plant.date == "2025-09-11" & fertilized == "NF")

# can work these to get the herb type and soil type statistics and plots.
pred <- predict(
  object = SrvlMdlI,
  newdata = iCmbFac,
  type = "response",
  se.fit = TRUE
)

Pred <- bind_cols(iCmbFac,Mean = pred$fit, SE = pred$se.fit) %>%
  mutate(
    Median = qbeta2(0.5, mean = Mean, sd = SE),
    CI05 = qbeta2(0.05, mean = Mean, sd = SE),
    CI95 = qbeta2(0.95, mean = Mean, sd = SE)
  )

# Plot predicted germination rates by soil type with 95% CI
iPlot <- Pred %>%
  ggplot(aes(x = Median, y = .data[[iVar[1]]]
             , colour = .data[[iVar[2]]]
  )) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(xmin = CI05, xmax = CI95),
    #           orientation = "x",
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    x = "Median Survival Rates",
    y = cVar,
    title = paste("Survival Rates by", cVar),
    subtitle = "90% Confidence Intervals"
  ) +
  theme_minimal(base_size = 14)

assign(x = paste("Survival", gsub(" ", "", cVar), sep = "."), value = Pred)
ggsave(filename = here::here(paste("doc/images/Survival", gsub(" ", "", cVar), "png", sep = ".")),
       iPlot)
