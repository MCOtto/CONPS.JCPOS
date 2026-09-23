# Germination analysis for the CoNPS/JCPOS native plant study.
#
# Data: ori/JCOS data - MG.xlsx, sheet "germination rates". One SC10 tray
# (98 cells) per species x soil type; count = cells with a seedling on
# June 1, 2025. Forbs were sown 1 seed per cell, grasses ~10 seeds per cell.
#
# Figures are written to doc/images/ and estimate tables to doc/tables/.

library(xlsx)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(IPMbook)   # qbeta2(): beta quantiles from a mean and sd
library(ggplot2)

source(here("R", "Helpers.R"))

# Factor levels ---------------------------------------------------------------
csoil.type <- c("Peat", "Mineral/Compost")
cherb.type <- c("Forb", "Grass")
cforb <- c("ACMI", "SYER", "ARLU", "HEVI")
# Forb  ACMI Achillea millefolium, common yarrow
# Forb  SYER Symphyotrichum ericoides, white heath aster
# Forb  ARLU Artemisia ludoviciana, white sage / silver wormwood
# Forb  HEVI Heterotheca villosa, hairy goldenaster
# Grass PASM Pascopyrum smithii, western wheatgrass
# Grass BOGR Bouteloua gracilis, blue grama
# Grass NAVI Nassella viridula, green needlegrass
# Grass HECO Hesperostipa comata, needle-and-thread grass
cspecies <- c(cforb, "PASM", "BOGR", "NAVI", "HECO")
nCells <- 98

# Read the germination data -----------------------------------------------------
# The species column holds e.g. "ACMI Peat"; the first four characters are the code.
Germination <- read.xlsx(
  file = here("ori", "JCOS data - MG.xlsx"),
  sheetName = "germination rates",
  rowIndex = 1:17,
  colIndex = c(1, 2, 4),
  colClasses = c("text", "text", "numeric")
) %>%
  mutate(
    herb.type = factor(ifelse(str_sub(species, 1, 4) %in% cforb, "Forb", "Grass"),
                       levels = cherb.type),
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(str_sub(species, 1, 4), levels = cspecies)
  ) %>%
  select(species, herb.type, soil.type, count)

# Observed germination ---------------------------------------------------------
ObsPlot <- Germination %>%
  mutate(rate = count / nCells,
         species = factor(species, levels = rev(cspecies))) %>%
  ggplot(aes(x = rate, y = species, colour = soil.type)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 4.5, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = soil.colours) +
  scale_x_continuous(limits = c(0.4, 1)) +
  labs(x = "Observed germination proportion (cells of 98)", y = NULL,
       colour = "Soil type",
       title = "Observed germination by species and soil type",
       subtitle = "Forbs above the dashed line, grasses below") +
  theme_jcpos()
save_figure(ObsPlot, "GerminationObserved.png")

# Models -------------------------------------------------------------------------
GrmnMdlSS <- glm(cbind(count, nCells - count) ~ species + soil.type,
                 data = Germination, family = binomial(link = "logit"))
summary(GrmnMdlSS)
anova(GrmnMdlSS)

# Same fit as GrmnMdlSS, but nesting species in herb type lets the sequential
# deviance table separate forbs vs. grasses from the variation among species.
GrmnMdlM <- glm(cbind(count, nCells - count) ~ soil.type + herb.type / species,
                data = Germination, family = binomial(link = "logit"))
summary(GrmnMdlM)
anova(GrmnMdlM)

# Saturated: one parameter per tray.
GrmnMdlI <- glm(cbind(count, nCells - count) ~ herb.type / species + species * soil.type,
                data = Germination, family = binomial(link = "logit"))
summary(GrmnMdlI)
anova(GrmnMdlI)

AIC(GrmnMdlSS, GrmnMdlM, GrmnMdlI)

# Dispersion check: Pearson chi-square / residual df (about 1.3 here, so fine).
sum(residuals(GrmnMdlM, type = "pearson")^2) / df.residual(GrmnMdlM)

# Estimated rates ------------------------------------------------------------------
# Grid of every species x soil type.
CmbFac <- expand_grid(species = cspecies, soil.type = csoil.type) %>%
  mutate(
    herb.type = factor(ifelse(species %in% cforb, "Forb", "Grass"), levels = cherb.type),
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(species, levels = cspecies)
  )

# Soil type: average the logits over all eight species.
# Herb type: average the logits over the four species of each type and both soils.
# NOTE: predict(type = "terms") is not used for herb type because, with species
# nested in herb type, the herb.type term is only the HECO-vs-ACMI contrast.
SoilFit <- marginal_rates(GrmnMdlM, CmbFac, by = "soil.type")
HerbFit <- marginal_rates(GrmnMdlM, CmbFac, by = "herb.type")
SoilFit
HerbFit

SoilPlot <- SoilFit %>%
  ggplot(aes(x = Median, y = soil.type, colour = soil.type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = CI05, xmax = CI95), width = 0.2) +
  scale_colour_manual(values = soil.colours, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  labs(x = "Estimated germination rate", y = "Soil type",
       title = "Germination rates by soil type",
       subtitle = "Averaged over species; error bars are 90% intervals") +
  theme_jcpos()
save_figure(SoilPlot, "GerminationSoilType.png", width = 5.5, height = 2.8)

HerbPlot <- HerbFit %>%
  ggplot(aes(x = Median, y = herb.type, colour = herb.type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = CI05, xmax = CI95), width = 0.2) +
  scale_colour_manual(values = herb.colours, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  labs(x = "Estimated germination rate", y = "Herb type",
       title = "Germination rates by herb type",
       subtitle = "Averaged over species and soils; error bars are 90% intervals") +
  theme_jcpos()
save_figure(HerbPlot, "GerminationHerbType.png", width = 5.5, height = 2.8)

# Species, in mineral/compost.
pred <- predict(GrmnMdlM, newdata = CmbFac, type = "response", se.fit = TRUE)
Pred <- bind_cols(CmbFac, beta_interval(pred$fit, pred$se.fit))

SpeciesPlot <- Pred %>%
  filter(soil.type == "Mineral/Compost") %>%
  mutate(species = factor(species, levels = rev(cspecies))) %>%
  ggplot(aes(x = Median, y = species, colour = herb.type)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = CI05, xmax = CI95), width = 0.2) +
  scale_colour_manual(values = herb.colours) +
  labs(x = "Estimated germination rate", y = "Species", colour = "Herb type",
       title = "Germination rates by species, mineral/compost",
       subtitle = "Error bars are 90% intervals") +
  theme_jcpos()
save_figure(SpeciesPlot, "GerminationSpecies.png")

# Tables for the report ---------------------------------------------------------
write_table(
  bind_rows(
    SoilFit %>% transmute(group = "Soil type", level = as.character(soil.type), Mean, SE, Median, CI05, CI95),
    HerbFit %>% transmute(group = "Herb type", level = as.character(herb.type), Mean, SE, Median, CI05, CI95),
    Pred %>% filter(soil.type == "Mineral/Compost") %>%
      transmute(group = "Species (Mineral/Compost)", level = as.character(species), Mean, SE, Median, CI05, CI95)
  ),
  "GerminationEstimates.csv"
)
