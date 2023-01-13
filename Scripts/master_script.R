library(tidyverse)
library(psych)
library(corrplot)
library(EFAtools)
library(lavaan)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---------- Descriptives -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load and clean raw data OR load clean data directly
# source("Scripts/0_clean_data.R") # raw data
df <- read_csv("CleanData/cleanData_230105.csv") # clean data

# Load helper functions
source("Scripts/helper_functions.R")

# extra csv file with the wording for all items 
itemWording <- read_csv("Materials/BANG_EFA_item_labels.csv") %>%
  mutate(item = paste0(label, ": ", item))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---------- Descriptives -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corrplot(cor(df[,12:103]), order = "original", tl.col='black', tl.cex=.75) 

# histograms for each variable
df[,12:103] %>% 
  pivot_longer(cols = 1:92) %>%
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  theme_minimal() +
  facet_wrap(~name)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- Sampling adequacy checks -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
EFAtools::KMO(df[,12:103])
EFAtools::BARTLETT(df[,12:103])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA results -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explore how many factors to extract
fa.parallel(df[,12:103], fm = "minres", nfactors = 6)
# N_FACTORS(df[,12:103])

# Fit model
efa1 <- fa(df[,12:103], nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa1)

# Inspect results
efa1tab <- fa_table(efa1)

labelOrder1 <- match(itemWording$label, efa1tab[["ind_table"]][["_data"]][["Indicator"]])
efa1tab[["ind_table"]][["_data"]][["Indicator"]][labelOrder1] <- itemWording$item
efa1tab$ind_table
efa1tab$f_table

gtsave(efa1tab$ind_table, "efa1_table.html")

#Plot loadings of the first 2 factors against one another
load <- efa1$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(df[,12:103]),cex=.7) # add variable names

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- Interpretation of EFA 1 results -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# factors that were easily extracted and > 4 strong candidate items retained:
# RS
# CF
# AS
# AF
# factors that were muddled or left with two few items
# RF (split into two, namely toxic community and loneliness)
# CS (many crossloadings with autonomy satisfaction)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA 2 -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Second EFA, after item pruning (this seems like questionable practice, but is what they did in the PXI)
# Selected items have loadings of > .5 on intended factor, and < .3 on all others
retained <- c("as05", "as06", "as01", "as09",
              "af04", "af02", "af16", "af01",
              "cs06", "cs03", "cs02", "cs12",
              "cf01", "cf04", "cf05", "cf14",
              "rs10", "rs14", "rs02", "rs13",
              "rf07", "rf08", "rf05", "rf02")
dfReduced <- df %>% 
  select(all_of(retained))

itemWordingReduced <- itemWording %>%
  filter(label %in% retained)

efa2 <- fa(dfReduced, nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa2)

efa2tab <- fa_table(efa2)
labelOrder2 <- match(itemWordingReduced$label, efa2tab[["ind_table"]][["_data"]][["Indicator"]])
efa2tab[["ind_table"]][["_data"]][["Indicator"]][labelOrder2] <- itemWordingReduced$item
efa2tab$ind_table
efa2tab$f_table

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- CFA results -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cfaMod <- '
  AS =~ as05 + as06 + as01 + as09
  AF =~ af04 + af02 + af16 + af01
  CS =~ cs06 + cs03 + cs02 + cs12
  CF =~ cf01 + cf04 + cf05 + cf14
  RS =~ rs10 + rs14 + rs02 + rs13
  RF =~ rf07 + rf08 + rf05 + rf02
'

cfa1 <- cfa(cfaMod, df[,12:103])
summary(cfa1)
fitMeasures(cfa1)
