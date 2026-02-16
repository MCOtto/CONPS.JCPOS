library(xlsx)
library(here)
library(dplyr)
library(stringr)
# Read the germination data
csoil.type <- c("Peat", "Mineral/Compost")
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

GrmnMdlM <- glm(
  cbind(count, 98 - count) ~ species + soil.type,
  data = Germination,
  binomial(link = "logit")
)
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
expan
pred <- predict(model, newdata, type="link", se.fit=TRUE)