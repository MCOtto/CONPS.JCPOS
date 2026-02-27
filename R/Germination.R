library(xlsx)
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(IPMbook)
library(ggplot2)

# Read the germination data
csoil.type <- c("Peat", "Mineral/Compost")

# Grass BOGR Blue gramma
# Grass HECO Hesperostipa comata
# Forb HEVI Helleborus viridis
# Grass? NAVI ?
# Grass PASM Paspalum genus n the Poaceae family
# Forb  SYER White heath aster (Symphyotrichum ericoides)
cherb.type <- c("Forb", "Grass")
cspecies <- c("ACMI", "SYER", "ARLU", "HEVI", "PASM", "BOGR", "NAVI", "HECO")
Germination <- read.xlsx(
  file = paste0(here("ori"), "/JCOS data - MG.xlsx"),
  sheetName = "germination rates",
  rowIndex = 1:17,
  colIndex = c(1, 2, 4),
  colClasses = c("text", "text", "numeric")
) %>%
  mutate(
    herb.type = factor(ifelse(
      str_sub(species, 1, 4) %in% c("ACMI", "SYER", "ARLU", "HEVI"), "Forb", "Grass"), 
      levels = cherb.type),
    soil.type = factor(soil.type, levels = csoil.type),
    species = factor(str_sub(species, 1, 4), levels = cspecies)
  ) %>%
  select(species, herb.type, soil.type, count)

GrmnMdlSS <- glm(cbind(count, 98 - count) ~ species + soil.type,
                data = Germination,
                binomial(link = "logit"))
summary(GrmnMdlSS)
anova(GrmnMdlSS)

GrmnMdlM <- glm(cbind(count, 98 - count) ~ soil.type + herb.type / species,
                data = Germination,
                binomial(link = "logit"))
summary(GrmnMdlM)
anova(GrmnMdlM)

GrmnMdlI <- glm(
  cbind(count, 98 - count) ~ herb.type / species + species * soil.type,
  data = Germination,
  binomial(link = "logit")
)
summary(GrmnMdlI)
anova(GrmnMdlI)

AIC(GrmnMdlSS, GrmnMdlM, GrmnMdlI)

# Make an expanded grid of species and soil types
CmbFac <- expand_grid(species = cspecies, soil.type = csoil.type) %>%
  mutate(
    herb.type = factor(ifelse(
      str_sub(species, 1, 4) %in% c("ACMI", "SYER", "ARLU", "HEVI"),
      "Forb",
      "Grass"
    ), levels = cherb.type),
    soil.type = factor(soil.type, csoil.type),
    species = factor(species, cspecies)
  )

# can work these to get the herb type and soil type statistics and plots.
fitterms <- predict(
  object = GrmnMdlM,
  newdata = CmbFac,
  type = "terms",
  se.fit = TRUE
)

intercept <- attr(fitterms$fit, "constant")
Fit <- bind_cols(
  CmbFac,
  lgt.herb = intercept + as.double(fitterms$fit[, "herb.type"]),
  lgt.herbSE = as.double(fitterms$se.fit[, "herb.type"]),
  lgt.soil = intercept + as.double(fitterms$fit[, "soil.type"]),
  lgt.soilSE = as.double(fitterms$se.fit[, "soil.type"])
)

SoilFit <- Fit %>%
  distinct(soil.type, lgt.soil, lgt.soilSE) %>%
  mutate(
    psoil = 1 / (1 + exp(-lgt.soil)),
    psoilSE = psoil * (1 - psoil) * lgt.soilSE,
    Median = qbeta2(0.5, mean = psoil, sd = psoilSE),
    CI05 = qbeta2(0.05, mean = psoil, sd = psoilSE),
    CI95 = qbeta2(0.95, mean = psoil, sd = psoilSE)
  )

# Plot predicted germination rates by soil type with 95% CI
SoilFit %>%
  ggplot(aes(x = Median,
             y = soil.type)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = CI05,
                    xmax = CI95),
                #           orientation = "x",
                height = 0.2,
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Median Germination Rates",
    y = "Soil Type",
    title = "Germination Rates by Soil Type",
    subtitle = "Error bars represent 90% confidence intervals"
  ) +
  theme_minimal(base_size = 14)

HerbFit <- Fit %>%
  distinct(herb.type, lgt.herb, lgt.herbSE) %>%
  mutate(
    pherb = 1 / (1 + exp(-lgt.herb)),
    pherbSE = pherb * (1 - pherb) * lgt.herbSE,
    Median = qbeta2(0.5, mean = pherb, sd = pherbSE),
    CI05 = qbeta2(0.05, mean = pherb, sd = pherbSE),
    CI95 = qbeta2(0.95, mean = pherb, sd = pherbSE)
  )

HerbFit %>%
  ggplot(aes(x = Median,
             y = herb.type)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = CI05,
                    xmax = CI95),
                #           orientation = "x",
                height = 0.2,
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Median Germination Rates",
    y = "Herb Type",
    title = "Germination Rates by Herb Type",
    subtitle = "Error bars represent 90% confidence intervals"
  ) +
  theme_minimal(base_size = 14)
  
# Plot differences by species
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

# Plot predicted germination rates with 95% CI
Pred %>%
  filter(soil.type == "Mineral/Compost") %>%
  ggplot(aes(x = Median,
           y = species, 
           colour = herb.type)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = CI05,
                     xmax = CI95),
     #           orientation = "x",
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  labs(
    x = "Median Germination Rates",
    y = "Species",
    colour = "Herb Type",
    title = "Predicted Germination Rates by Species and Mineral/Compost",
    subtitle = "Error bars represent 90% confidence intervals"
  ) +
  theme_minimal(base_size = 14)
