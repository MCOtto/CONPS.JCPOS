# Field-survival analysis for the CoNPS/JCPOS native plant study.
#
# Data: ori/JCOS data - MG.xlsx, sheet "alldatasimplified". One row per
# plot x species: plants installed and plants alive at the last monitoring
# visit (October 23, 2025).
#
# Figures are written to doc/images/ and estimate tables to doc/tables/.

library(xlsx)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(IPMbook)
library(ggplot2)

source(here("R", "Helpers.R"))

# Factor levels ---------------------------------------------------------------
csoil.type <- c("MC", "peat", "S3")
cfertilized <- c("NF", "F")
cherb.type <- c("Forb", "Grass")
cforb <- c("ACMI", "ARLU", "HEVI")
# SYER was removed from the field study: mixed leaf and flower forms suggested
# hybridization, and wild Symphyotrichum colonized the plots.
cspecies <- c(cforb, "PASM", "BOGR", "NAVI", "HECO")
cplantdate <- c("2025-07-10", "2025-08-14", "2025-09-11")
cCol <- c("plot", "soil.type", "species", "plant.date", "fertilized",
          "planted", "survived", "survival.rate")

# Read the survival data from the simplified sheet -------------------------------
Survival <- read.xlsx(
  file = here("ori", "JCOS data - MG.xlsx"),
  sheetName = "alldatasimplified",
  rowIndex = 1:85,
  colIndex = c(1:8),
  colClasses = c("numeric", "character", "character", "Date",
                 "character", "numeric", "numeric", "numeric")
) %>%
  mutate(
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(species, levels = cspecies),
    plant.date = factor(as.character(plant.date), levels = cplantdate),
    fertilized = factor(fertilized, levels = cfertilized),
    # One count is fractional (ACMI, plot 1: 32.5); as.integer() truncates it to 32.
    survived = as.integer(survived)
  ) %>%
  select(all_of(cCol))

# Which species x soil x date combinations actually had plants installed.
Observed <- Survival %>%
  filter(!is.na(planted), planted > 0) %>%
  distinct(species, soil.type, plant.date) %>%
  mutate(observed = TRUE)

# Planting layout: plants installed by species, date, and soil.
Survival %>%
  filter(!is.na(planted), planted > 0) %>%
  group_by(species, plant.date, soil.type) %>%
  summarise(planted = sum(planted), .groups = "drop") %>%
  pivot_wider(names_from = c(plant.date, soil.type), values_from = planted, values_fill = 0)

# Observed survival ---------------------------------------------------------------
ObsSurvival <- Survival %>%
  filter(!is.na(planted), planted > 0) %>%
  group_by(species, soil.type, plant.date) %>%
  summarise(planted = sum(planted), survived = sum(survived), .groups = "drop") %>%
  mutate(rate = survived / planted,
         species = factor(species, levels = rev(cspecies)),
         in.model = !species %in% c("HEVI", "HECO"))

ObsPlot <- ObsSurvival %>%
  ggplot(aes(x = rate, y = species, colour = soil.type, size = planted)) +
  geom_point(aes(alpha = in.model), position = position_dodge(width = 0.6)) +
  facet_wrap(~ plant.date, nrow = 1,
             labeller = as_labeller(c("2025-07-10" = "July 10", "2025-08-14" = "August 14",
                                      "2025-09-11" = "September 11"))) +
  scale_colour_manual(values = soil.colours, labels = c(MC = "MC", peat = "Peat", S3 = "S3")) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
  scale_size_area(max_size = 7, breaks = c(10, 50, 100)) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(x = "Observed survival proportion (planting to October 23, 2025)", y = NULL,
       colour = "Soil type", size = "Planted",
       title = "Observed survival by species, soil type, and planting date",
       subtitle = "Pooled over fertilization; faded species (HEVI, HECO) are not in the model") +
  theme_jcpos()
save_figure(ObsPlot, "Survival.Observed.png", width = 8, height = 4.5)

# Model selection ------------------------------------------------------------------
# HEVI and HECO are excluded: too few plants in too few cells to support
# species x soil and species x date terms.
SrvlMdlM <- glm(
  cbind(survived, planted - survived) ~ species + soil.type * fertilized + plant.date,
  family = binomial(link = "logit"), data = Survival,
  subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlM)
anova(SrvlMdlM)

SrvlMdlSF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type * fertilized + plant.date,
  family = binomial(link = "logit"), data = Survival,
  subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlSF)
anova(SrvlMdlSF)

SrvlMdlI <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date + fertilized,
  family = binomial(link = "logit"), data = Survival,
  subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlI)
anova(SrvlMdlI)

# A random plot effect was tried but glmmTMB reported a non-positive-definite
# Hessian; with 12 unreplicated plots there is too little information.
# SrvlMdlLME <- glmmTMB(
#   cbind(survived, planted - survived) ~ 1 + (species|soil.type) + plant.date + fertilized,
#   data = Survival, family = binomial(link = "logit"),
#   subset = !species %in% c("HEVI", "HECO")
# )

SrvlMdlnF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type + species * plant.date,
  family = binomial(link = "logit"), data = Survival,
  subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlnF)
anova(SrvlMdlnF)

SrvlMdlF <- glm(
  cbind(survived, planted - survived) ~ species * soil.type * plant.date + fertilized,
  family = binomial(link = "logit"), data = Survival,
  subset = !species %in% c("HEVI", "HECO")
)
summary(SrvlMdlF)
anova(SrvlMdlF)

# SrvlMdlI, with the species interactions plus fertilized, has the lowest AIC.
AIC(SrvlMdlM, SrvlMdlI, SrvlMdlnF, SrvlMdlF, SrvlMdlSF)

# Overdispersion ----------------------------------------------------------------
# Pearson chi-square / residual df is about 2.9, so the binomial standard errors
# are too small. Refit as quasi-binomial for F tests and wider standard errors.
sum(residuals(SrvlMdlI, type = "pearson")^2) / df.residual(SrvlMdlI)
SrvlMdlQ <- update(SrvlMdlI, family = quasibinomial(link = "logit"))
# (R notes that zero-planted rows are not used for the dispersion; that is expected.)
summary(SrvlMdlQ)
anova(SrvlMdlQ, test = "F")

# Estimated survival -------------------------------------------------------------
# Grid of every modeled species x soil x date x fertilization.
CmbFac <- expand_grid(
  species = cspecies[c(1:2, 4:6)],
  soil.type = csoil.type,
  plant.date = cplantdate,
  fertilized = cfertilized
) %>%
  mutate(
    soil.type = factor(soil.type, levels = csoil.type),
    plant.date = factor(plant.date, levels = cplantdate),
    fertilized = factor(fertilized, levels = cfertilized),
    species = factor(species, levels = cspecies[c(1:2, 4:6)])
  )

# predict() warns that the fit is rank deficient: ARLU x 2025-09-11 is not
# estimable. The other predictions are unaffected.
pred <- suppressWarnings(
  predict(SrvlMdlI, newdata = CmbFac, type = "response", se.fit = TRUE)
)
Pred <- bind_cols(CmbFac, beta_interval(pred$fit, pred$se.fit)) %>%
  left_join(Observed, by = c("species", "soil.type", "plant.date")) %>%
  mutate(observed = coalesce(observed, FALSE))

# Two views, both for unfertilized plants:
#   species x soil type at the September 11 planting, and
#   species x planting date in MC soil.
# Open points mark combinations where no plants were installed: those estimates
# come entirely from the model's additive structure.
plot_survival <- function(Pred, colourVar, colourVals, colourLabs, cVar, subtitle) {
  Pred %>%
    mutate(species = factor(species, levels = rev(levels(species))),
           status = factor(ifelse(observed, "Observed", "No plants (extrapolated)"),
                           levels = c("Observed", "No plants (extrapolated)"))) %>%
    ggplot(aes(x = Median, y = species, colour = .data[[colourVar]],
               group = .data[[colourVar]])) +
    geom_errorbar(aes(xmin = CI05, xmax = CI95), width = 0.2,
                  position = position_dodge(width = 0.6)) +
    geom_point(aes(shape = status), size = 3, fill = "white", stroke = 1.2,
               position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = colourVals, labels = colourLabs) +
    scale_shape_manual(values = c("Observed" = 16, "No plants (extrapolated)" = 21), drop = FALSE) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = "Estimated survival rate", y = "Species",
         colour = cVar, shape = NULL,
         title = paste("Survival rates by species and", tolower(cVar)),
         subtitle = subtitle) +
    theme_jcpos()
}

SoilPred <- Pred %>% filter(plant.date == "2025-09-11", fertilized == "NF")
SoilPlot <- plot_survival(SoilPred, "soil.type", soil.colours,
                          c(MC = "MC", peat = "Peat", S3 = "S3"), "Soil type",
                          "September 11 planting, unfertilized; error bars are 90% intervals")
save_figure(SoilPlot, "Survival.SpeciesbySoilType.png")

DatePred <- Pred %>% filter(soil.type == "MC", fertilized == "NF")
DatePlot <- plot_survival(DatePred, "plant.date", date.colours,
                          c("2025-07-10" = "July 10", "2025-08-14" = "August 14",
                            "2025-09-11" = "September 11"), "Planting date",
                          "MC soil, unfertilized; error bars are 90% intervals")
save_figure(DatePlot, "Survival.SpeciesbyPlantingDate.png")

# Tables for the report ---------------------------------------------------------
write_table(bind_rows(SoilPred, DatePred %>% filter(plant.date != "2025-09-11")),
            "SurvivalEstimates.csv")
