library(tidyverse)
library(psych)
library(corrplot)
library(EFAtools)
library(lavaan)
library(semTools)
library(dynamic) # for dynamix fit index cutoffs
library(webshot2) # for saving gt tables

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ---------- Descriptives -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load and clean raw data OR load clean data directly
# source("Scripts/0_clean_data.R") # raw data
df <- read_csv("CleanData/cleanData_230131.csv") # clean data

# Second df with only items that were included in both EFA waves
df2 <- df %>%
  select(where(~sum(is.na(.)) < 100))

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
  geom_histogram(binwidth = 1) + 
  theme_minimal() +
  facet_wrap(~name)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- Sampling adequacy checks -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Initial sample
EFAtools::KMO(df %>% filter(!is.na(condition)) %>% select(12:103))
EFAtools::BARTLETT(df %>% filter(!is.na(condition)) %>% select(12:103))

# Second wave with slightly reduced item pool
EFAtools::KMO(df2 %>% select(8:83))
EFAtools::BARTLETT(df2 %>% select(8:83))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA results -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Interpretation
# Results support a 6-factor model, though 5 and 7 are equally plausible. 
# factors that were easily extracted and > 4 strong candidate items retained: AS, AF, CF, RS
# 
# factors that were muddled or left with two few items: RF (split into two, namely toxic community and loneliness), 
# CS (many crossloadings with autonomy satisfaction)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explore how many factors to extract
fa.parallel(df[,12:103], fm = "minres", nfactors = 6)

# Fit model
efa1 <- fa(df %>% filter(!is.na(condition)) %>% select(12:103), nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa1)

# Inspect results
# Red rows are those where loadings on the primary and secondary factors are within .10 of each other
efa1tab <- fa_table(efa1)
efa1tab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$item[match(efa1tab[["ind_table"]][["_data"]][["Indicator"]],
                                                                           itemWording$label)]
efa1tab$ind_table
efa1tab$f_table

gtsave(efa1tab$ind_table, "TablesFigures/EFA/efa1_full_table.html")

#Plot loadings of the first 2 factors against one another
load <- efa1$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(df[,12:103]),cex=.7) # add variable names

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA 1 Iterative Pruning ---------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the list of worst performers is developed iteratively over 6 rounds; in each case, an EFA model is fit, the worst-performing items 
# are identified and dropped, and a new model with fewer items is fit until all remaining items are high-performing. 
worst_performers <- c(
  # Round 1
  "as14", "rs11","as15", "cs08", "rf15", "rf14", "cf10", "rf10", "as10", "af12", "af07", "cf15", "rf06", "cs15", "af03", "as13", "rf13", "rf12"
  # Round 2
  ,"rs12","rs06","cs14","cs13","rf03","af13","rf11"
  # Round 3 
  ,"rs03","cs10","cf13","cs07","as08","as07","cs09","as03"
  # Round 4
  ,"rs07","af06","as02","cs05"
  # Round 5
  ,"as11","as12","af15","af16","af11","cf06"
  # Round 6
  ,"rf04","cf11","af08","af14","af01","af09"
)

dfReduced <- df[,12:103] %>% 
  # select(all_of(best_performers)) %>%
  select(-all_of(worst_performers))


efa1reduced <- fa(dfReduced, nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa1reduced)

efa1reducedTab <- fa_table(efa1reduced)
efa1reducedTab[["ind_table"]][["_data"]][["Indicator"]] <- itemWordingReduced$item[match(efa1reducedTab[["ind_table"]][["_data"]][["Indicator"]], 
                                                                                         itemWordingReduced$label)]
efa1reducedTab$ind_table
efa1reducedTab$f_table

gtsave(efa1reducedTab$ind_table, "TablesFigures/EFA/efa1_reduced_table.html")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA - Wave 2 -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explore how many factors to extract
fa.parallel(df2[,8:83], fm = "minres", nfactors = 6)
# N_FACTORS(df2[,8:83]) # compares many factor extraction methods, but very slow

# Fit model
efa2 <- fa(df2[,8:83], nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa2)

# Inspect results
# Red rows are those where the loading on the primary factor and the loading on the secondary factor are within .10 of each other
efa2tab <- fa_table(efa2)
efa2tab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$item[match(efa2tab[["ind_table"]][["_data"]][["Indicator"]], 
                                                                           itemWording$label)]
efa2tab$ind_table
efa2tab$f_table

gtsave(efa2tab$ind_table, "TablesFigures/EFA/efa2_full_table.html")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- EFA 2 Iterative Pruning -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

worstPerformers2 <- c(
  # Round 1
  "rf06","as10","cf06","af14","af03","as14","cs08","as13","cf15","af13","af07","af06","rf14"
  # Round 2
  ,"cs07","cs09","af08"
  # # Round 3
  ,"cs10","cf16","as15","af11","as11","cs05","cf04"
)

df2Reduced <- df2[,8:83] %>% 
  select(-all_of(worstPerformers2))

efa2reduced <- fa(df2Reduced, nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa2reduced)

efa2reducedTab <- fa_table(efa2reduced)
efa2reducedTab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$item[match(efa2reducedTab[["ind_table"]][["_data"]][["Indicator"]], 
                                                                                  itemWording$label)]
efa2reducedTab$ind_table
efa2reducedTab$f_table

gtsave(efa2reducedTab$ind_table, "TablesFigures/EFA/efa2_reduced_table5.html")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- Reliability analysis with pruned items -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

alpha(df[,c("as01","as04","as05","as06","as09")])
alpha(df[,c("af01","af02","af04","af05","af08","af09","af10","af14")])
alpha(df[,c("cs01","cs02","cs03","cs04","cs06","cs11","cs12")])
alpha(df[,c("cf01","cf02","cf03","cf04","cf05","cf07","cf08","cf09","cf11","cf12","cf14","cf16")])
alpha(df[,c("rs01","rs02","rs04","rs05","rs08","rs09","rs10","rs13","rs14","rs15")])
alpha(df[,c("rf01","rf02","rf05","rf07","rf08","rf09")])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --------- CFA results -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We observe high correlations between first-order factors (particularly AS ~~ CS and CS ~~ RS); as a result, 
# we add second-order need satisfaction and need frustration factors, 
cfaMod <- '
  AS =~ as01 + as06 + as09 + as05
  AF =~ af02 + af16 + af04 + af01
  CS =~ cs13 + cs02 + cs06 + cs11
  CF =~ cf01 + cf09 + cf14 + cf05
  RS =~ rs13 + rs05 + rs04 + rs03
  RF =~ rf08 + rf02 + rf07 + rf05
# 
#   NS =~ AS + CS + RS
#   NF =~ AF + CF + RF
# 
#   AS ~~ CS
#   AS ~~ RS
#   CS ~~ RS
# 
#   AF ~~ CF
#   AF ~~ RF
#   CF ~~ RF

'

df3 <- df %>%
  # filter(!is.na(condition)) %>%
  select(as01, as06, as09, as05,
         af01, af16, af04, af02,
         cs13, cs02, cs06, cs11,
         cf01, cf09, cf14, cf05,
         rs13, rs05, rs04, rs03,
         rf08, rf05, rf07, rf02)

efa3 <- fa(df3, nfactors = 6, fm = "ml", rotate = "oblimin")
efa3 <- fa_table(efa3)
efa3[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$item[match(efa3[["ind_table"]][["_data"]][["Indicator"]], 
                                                                                  itemWording$label)]
efa3$ind_table

cfa1 <- cfa(cfaMod, df)
summary(cfa1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa1,c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
modificationindices(cfa1)
compRelSEM(cfa1, return.total = TRUE) # check factor reliabilities
compRelSEM(cfa1, tau.eq = TRUE, return.total = TRUE) # check factor reliabilities as alpha 
omega(df %>% select(af01,af16,af04))

# Get dynamic fit index cutoffs, rather than relying on standards (see McNeish & Wolf 2021, https://psyarxiv.com/v8yru/)
# cfaHB returns a table with cutoffs that correctly accept ≥95% of true models and reject ≥95% of misspecified models, given 
# a certain degree of misspecification. Misspecification takes the form of additional cross-loadings present in the true model but 
# omitted from the model---in this case, 1--5 additional cross-loadings. The magnitude column indicates the strength of the omitted
# cross-loading in the population model.
# 
# Dynamic cutoffs are only available for the non-hierarchical version of the model 
cfaHB(cfa1)