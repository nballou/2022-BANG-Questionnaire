library(tidyverse)
library(psych)
library(corrplot)
library(EFAtools)
library(lavaan)
library(semTools)
library(dynamic) # for dynamic fit index cutoffs
library(webshot2) # for saving gt tables
library(ggthemes)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data and calculate means
df <- read_csv("CleanData/combinedData.csv", guess_max = 1400) %>%
  
  # save (and duplicate) the items included in the final measure for convenient referencing
  mutate(
    bang_1 = as06,
    # bang_2 = as09,
    bang_2 = as01,
    bang_3 = as05,
    bang_4 = af01,
    bang_5 = af16,
    bang_6 = af04,
    bang_7 = cs13,
    bang_8 = cs02,
    bang_9 = cs06,
    bang_10 = cf01,
    bang_11 = cf05,
    bang_12 = cf09,
    bang_13 = rs05,
    bang_14 = rs13,
    bang_15 = rs03,
    bang_16 = rf07,
    bang_17 = rf05,
    bang_18 = rf08
  ) %>%
  
  # calculate (sub)scale means
  rowwise() %>%
  mutate(
    mean_as = mean(c(bang_1, bang_2, bang_3)),
    mean_af = mean(c(bang_4, bang_5, bang_6)),
    mean_cs = mean(c(bang_7, bang_8, bang_9)),
    mean_cf = mean(c(bang_10, bang_11, bang_12)),
    mean_rs = mean(c(bang_13, bang_14, bang_15)),
    mean_rf = mean(c(bang_16, bang_17, bang_18)),
    
    mean_as_bpnsfs = mean(c(bpnsfs_1 + bpnsfs_7 + bpnsfs_13 + bpnsfs_19)),
    mean_af_bpnsfs = mean(c(bpnsfs_2 + bpnsfs_8 + bpnsfs_14 + bpnsfs_20)),
    mean_cs_bpnsfs = mean(c(bpnsfs_5 + bpnsfs_11 + bpnsfs_17 + bpnsfs_23)),
    mean_cf_bpnsfs = mean(c(bpnsfs_6 + bpnsfs_12 + bpnsfs_18 + bpnsfs_24)),
    mean_rs_bpnsfs = mean(c(bpnsfs_3 + bpnsfs_9 + bpnsfs_15 + bpnsfs_21)),
    mean_rf_bpnsfs = mean(c(bpnsfs_4 + bpnsfs_10 + bpnsfs_16 + bpnsfs_22)),
    
    mean_as_pxi = mean(c(pxi_4, pxi_5, pxi_6)),
    mean_cs_pxi = mean(c(pxi_1, pxi_2, pxi_3)),
    
    mean_im_umi = mean(c(umi_1, umi_2, umi_3))
  )


# Save separate dfs for each component study, for convenience
dfEFA <- df %>% filter(study == "study1efa" & !possibleCareless) %>% select_if(~sum(is.na(.)) < 100) # for concision, drop the items only shown to the first sub-sample
dfGEN <- df %>% filter(study == "study2xbox" & !possibleCareless)
dfPiped <- df %>% filter(study == "study3piped" & !possibleCareless)

# Load helper functions
source("Scripts/helper_functions.R")

# extra csv file with the wording for all items 
itemWording <- read_csv("Materials/BANG_EFA_item_labels.csv") %>%
  mutate(wording = paste0(label, ": ", item)) %>%
  mutate(bangqWording = paste0(label, " (", bangqLabel, "): ", item))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Specify Models ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bangqMod <- '
  AS =~ bang_1 + bang_2 + bang_3
  AF =~ bang_4 + bang_5 + bang_6 
  CS =~ bang_7 + bang_8 + bang_9
  CF =~ bang_10 + bang_11 + bang_12
  RS =~ bang_13 + bang_14 + bang_15
  RF =~ bang_16 + bang_17 + bang_18
'

bangqMod4item <- '
  AS =~ bang_1 + bang_2 + bang_3 + as01
  AF =~ bang_4 + bang_5 + bang_6 + af02
  CS =~ bang_7 + bang_8 + bang_9 + cs11
  CF =~ bang_10 + bang_11 + bang_12 + cf14
  RS =~ bang_13 + bang_14 + bang_15 + rs04
  RF =~ bang_16 + bang_17 + bang_18 + rf02
'



bangqModBifactor <- '
  g =~ bang_1 + bang_2 + bang_3 + bang_4 + bang_5 + bang_6 + bang_7 + bang_8 + bang_9 + bang_10 + bang_11 + bang_12 + bang_13 + bang_14 + bang_15 + bang_16 + bang_17 + bang_18
  AS =~ bang_1 + bang_2 + bang_3
  AF =~ bang_4 + bang_5 + bang_6 
  CS =~ bang_7 + bang_8 + bang_9
  CF =~ bang_10 + bang_11 + bang_12
  RS =~ bang_13 + bang_14 + bang_15
  RF =~ bang_16 + bang_17 + bang_18

  g ~~ 0*AS
  g ~~ 0*AF
  g ~~ 0*CS
  g ~~ 0*CF
  g ~~ 0*RS
  g ~~ 0*RF
'

bangqModHierarchical <- '
  NS =~ bang_1 + bang_2 + bang_3 + bang_7 + bang_8 + bang_9 + bang_13 + bang_14 + bang_15
  NF =~ bang_4 + bang_5 + bang_6 + bang_10 + bang_11 + bang_12 + bang_16 + bang_17 + bang_18 

  AS =~ bang_1 + bang_2 + bang_3
  AF =~ bang_4 + bang_5 + bang_6 
  CS =~ bang_7 + bang_8 + bang_9
  CF =~ bang_10 + bang_11 + bang_12
  RS =~ bang_13 + bang_14 + bang_15
  RF =~ bang_16 + bang_17 + bang_18

  NS ~~ 0*AS
  NS ~~ 0*AF
  NS ~~ 0*CS
  NS ~~ 0*CF
  NS ~~ 0*RS
  NS ~~ 0*RF
  NF ~~ 0*AS
  NF ~~ 0*AF
  NF ~~ 0*CS
  NF ~~ 0*CF
  NF ~~ 0*RS
  NF ~~ 0*RF
  
  NS ~~ NF
'

bpnsfsMod <- '
  AS =~ bpnsfs_1 + bpnsfs_7 + bpnsfs_13 + bpnsfs_19
  AF =~ bpnsfs_2 + bpnsfs_8 + bpnsfs_14 + bpnsfs_20
  RS =~ bpnsfs_3 + bpnsfs_9 + bpnsfs_15 + bpnsfs_21
  RF =~ bpnsfs_4 + bpnsfs_10 + bpnsfs_16 + bpnsfs_22
  CS =~ bpnsfs_5 + bpnsfs_11 + bpnsfs_17 + bpnsfs_23
  CF =~ bpnsfs_6 + bpnsfs_12 + bpnsfs_18 + bpnsfs_24

  # NS =~ AS + CS + RS
  # NF =~ AF + CF + RF
'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study 1 EFA ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Descriptives #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

corrplot(cor(dfEFA %>% select(af01:rs15), 
             use = "pairwise.complete.obs"),
         order = "original", tl.col='black', tl.cex=.75) 

# histograms for each variable
dfEFA %>% 
  select(af01:rs15) %>%
  # select(bang_1:bang_18) %>% # for looking at only items selected for final measure, comment out above line if using
  pivot_longer(cols = 1:ncol(.)) %>%
  # mutate(name = factor(name, levels = paste0("bang_", 1:18))) %>% # re-order when only looking at final items
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1) + 
  theme_minimal() +
  facet_wrap(~name)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sampling adequacy #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EFAtools::KMO(dfEFA %>% select(af01:rs15), use = "complete.obs")
EFAtools::BARTLETT(df %>% select(af01:rs15), use = "complete.obs")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## EFA Results #####
#
# Results support a 6-factor model, though 5 and 7 are equally plausible. 
# factors that were easily extracted and > 4 strong candidate items retained: AS, AF, CF, RS
# 
# factors that were muddled or left with two few items: RF (split into two, namely toxic community and loneliness), 
# CS (many crossloadings with autonomy satisfaction)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explore how many factors to extract
fa.parallel(dfEFA %>% select(af01:rs15), fm = "ml", nfactors = 6)

# Fit model
efa1 <- fa(dfEFA %>% select(af01:rs15), nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa1)

# Inspect results
# Red rows are those where loadings on the primary and secondary factors are within .10 of each other
efa1tab <- fa_table(efa1)
efa1tab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$wording[match(efa1tab[["ind_table"]][["_data"]][["Indicator"]],
                                                                              itemWording$label)]
efa1tab$ind_table
efa1tab$f_table

gtsave(efa1tab$ind_table, "TablesFigures/EFA/efa1_full_table.html")

#Plot loadings of the first 2 factors against one another
load <- efa1$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(dfEFA %>% select(af01:rs15)),cex=.7) # add variable names

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## EFA Iterative Pruning ####
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

dfReduced <- df %>%
  select(af01:rs15) %>%
  # select(all_of(best_performers)) %>%
  select(-all_of(worst_performers))


efa1reduced <- fa(dfReduced, nfactors = 6, fm = "ml", rotate = "oblimin")
summary(efa1reduced)

efa1reducedTab <- fa_table(efa1reduced)
efa1reducedTab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$wording[match(efa1reducedTab[["ind_table"]][["_data"]][["Indicator"]], 
                                                                                     itemWording$label)]
efa1reducedTab$ind_table
efa1reducedTab$f_table

gtsave(efa1reducedTab$ind_table, "TablesFigures/EFA/efa1_reduced_table.html")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reliability with pruned items ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

alpha(df[,c("as01","as04","as05","as06","as09")])
alpha(df[,c("af01","af02","af04","af05","af08","af09","af10","af14")])
alpha(df[,c("cs01","cs02","cs03","cs04","cs06","cs11","cs12")])
alpha(df[,c("cf01","cf02","cf03","cf04","cf05","cf07","cf08","cf09","cf11","cf12","cf14","cf16")])
alpha(df[,c("rs01","rs02","rs04","rs05","rs08","rs09","rs10","rs13","rs14","rs15")])
alpha(df[,c("rf01","rf02","rf05","rf07","rf08","rf09")])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CFA results ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cfa1 <- cfa(bangqMod, dfEFA, estimator = "MLM")
summary(cfa1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa1,c("cfi.robust","tli","rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr_bentler "))
modificationindices(cfa1)
cfaHB(cfa1)

MBESS::ci.reliability(dfEFA %>% select(bang_1:bang_3), B = 1000)
MBESS::ci.reliability(dfEFA %>% select(bang_4:bang_6), B = 1000)
MBESS::ci.reliability(dfEFA %>% select(bang_7:bang_9), B = 1000)
MBESS::ci.reliability(dfEFA %>% select(bang_10:bang_12), B = 1000)
MBESS::ci.reliability(dfEFA %>% select(bang_13:bang_15), B = 1000)
MBESS::ci.reliability(dfEFA %>% select(bang_16:bang_18), B = 1000)
reliability(cfa1)

# Get dynamic fit index cutoffs, rather than relying on standards (see McNeish & Wolf 2021, https://psyarxiv.com/v8yru/)
# cfaHB returns a table with cutoffs that correctly accept ≥95% of true models and reject ≥95% of misspecified models, given 
# a certain degree of misspecification. Misspecification takes the form of additional cross-loadings present in the true model but 
# omitted from the model---in this case, 1--5 additional cross-loadings. The magnitude column indicates the strength of the omitted
# cross-loading in the population model.
# 
# Dynamic cutoffs are only available for the non-hierarchical version of the model 
cfaHB(cfa1)

# EFA diagnostics/inspection of same items
efa2 <- fa(dfEFA %>% select(starts_with("bang")), nfactors = 6, fm = "ml", rotate = "oblimin")
efa2 <- fa_table(efa3)
efa2[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$item[match(efa2[["ind_table"]][["_data"]][["Indicator"]], 
                                                                        itemWording$bangqLabel)]
efa2$ind_table


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study 2 Game General ####
# Multigroup CFA on the Xbox data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EFAtools::KMO(dfGEN %>% select(starts_with("bang")))
EFAtools::BARTLETT(dfGEN %>% select(starts_with("bang")))

## CFA #####
cfa2 <- cfa(bangqMod, dfGEN, estimator = "MLM", group = "wave")
summary(cfa2, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa2,c("cfi.robust","tli.robust","rmsea.ci.lower.robust","rmsea.robust","rmsea.ci.upper.robust","srmr"))

## Reliability ####
MBESS::ci.reliability(dfGEN %>% select(bang_1:bang_3), B = 1000)
MBESS::ci.reliability(dfGEN %>% select(bang_4:bang_6), B = 1000)
MBESS::ci.reliability(dfGEN %>% select(bang_7:bang_9), B = 1000)
MBESS::ci.reliability(dfGEN %>% select(bang_10:bang_12), B = 1000)
MBESS::ci.reliability(dfGEN %>% select(bang_13:bang_15), B = 1000)
MBESS::ci.reliability(dfGEN %>% select(bang_16:bang_18), B = 1000)
reliability(cfa2)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study 3 Piped Text ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sampling adequacy #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EFAtools::KMO(dfPiped %>% select(starts_with("bang")))
EFAtools::BARTLETT(dfPiped %>% select(starts_with("bang")))

corrplot(cor(dfPiped %>% select(bang_1:bang_18)), order = "original", tl.col='black', tl.cex=.75) 

# histograms for each variable
dfPiped %>% 
  select(bang_1:bang_18) %>%
  pivot_longer(cols = 1:ncol(.)) %>%
  mutate(name = factor(name, levels = paste0("bang_", 1:24)),
         need = factor(rep(rep(c("AS","AF","CS","CF","RS","RF"), each = 3), times = nrow(dfPiped)),
                       levels = c("AS","AF","CS","CF","RS","RF"))) %>%
  ggplot(aes(x = value, fill = need)) + 
  scale_fill_viridis_d() + 
  geom_histogram(binwidth = 1) + 
  theme_few() +
  scale_x_continuous(breaks = 1:7) +
  facet_wrap(~name, ncol = 3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CFA #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cfa3 <- cfa(bangqMod, dfPiped, estimator = "MLM")
summary(cfa3, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa3,c("cfi.robust","tli.robust","rmsea.ci.lower.robust","rmsea.robust","rmsea.ci.upper.robust","srmr"))
modificationindices(cfa3, sort = TRUE, free.remove = TRUE, minimum.value = 10)
cfaHB(cfa3) # dynamic fit indices, takes a few min to compute

## Reliability ####
MBESS::ci.reliability(dfPiped %>% select(bang_1:bang_3), B = 1000)
MBESS::ci.reliability(dfPiped %>% select(bang_4:bang_6), B = 1000)
MBESS::ci.reliability(dfPiped %>% select(bang_7:bang_9), B = 1000)
MBESS::ci.reliability(dfPiped %>% select(bang_10:bang_12), B = 1000)
MBESS::ci.reliability(dfPiped %>% select(bang_13:bang_15), B = 1000)
MBESS::ci.reliability(dfPiped %>% select(bang_16:bang_18), B = 1000)
reliability(cfa3)

# Reliability of non-BANGQ item
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(pxi_1:pxi_3), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(pxi_4:pxi_6), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(umi_1:umi_3), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_1, bpnsfs_7, bpnsfs_13, bpnsfs_19), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_2, bpnsfs_8, bpnsfs_14, bpnsfs_20), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_3, bpnsfs_9, bpnsfs_15, bpnsfs_21), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_4, bpnsfs_10, bpnsfs_16, bpnsfs_22), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_5, bpnsfs_11, bpnsfs_17, bpnsfs_23), B = 1000)
MBESS::ci.reliability(dfPiped %>% filter(!is.na(pxi_1)) %>% select(bpnsfs_6, bpnsfs_12, bpnsfs_18, bpnsfs_24), B = 1000)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Diagnostics (EFA) #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# efa3 <- fa(dfPiped %>% select(bang_1:bang_28) %>% select(-bang_10),
#            nfactors = 6, fm = "ml", rotate = "oblimin")
efa3 <- fa(dfPiped %>% select(bang_1:bang_18),
           nfactors = 6, fm = "ml", rotate = "oblimin")
efa3tab <- fa_table(efa3)
efa3tab[["ind_table"]][["_data"]][["Indicator"]] <- itemWording$bangqWording[match(efa3tab[["ind_table"]][["_data"]][["Indicator"]], 
                                                                                   itemWording$bangqLabel)]
efa3tab$ind_table


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study 4 Invariance ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Test invariance across context

fit_configural <- cfa(bangqMod, df, group = "study", estimator = "MLM")
summary(fit_configural, fit.measures = TRUE, standardized = TRUE)

fit_metric <- cfa(bangqMod, df, group = "study", estimator = "MLM", group.equal = c("loadings"))
summary(fit_metric, fit.measures = TRUE, standardized = TRUE)

fit_scalar <- cfa(bangqMod, df, group = "study", estimator = "MLM", group.equal = c("loadings", "intercepts"))
summary(fit_scalar, fit.measures = TRUE, standardized = TRUE)

summary(compareFit(fit_configural, fit_metric, fit_scalar))

# Test longitudinal invariance between two survey waves
# Results are good --- no evidence to reject configural, metric, or scalar invariance
fit_long_configural <- cfa(bangqMod, dfGEN, group = "wave", estimator = "MLM")
fit_long_metric <- cfa(bangqMod, dfGEN, group = "wave", estimator = "MLM", group.equal = c("loadings"))
fit_long_scalar <- cfa(bangqMod, dfGEN, group = "wave", estimator = "MLM", group.equal = c("loadings", "intercepts"))

summary(compareFit(fit_long_configural, fit_long_metric, fit_long_scalar))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Study 5 Convergent, divergent, and criterion validity ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cfa4 <- cfa(bangqMod, df %>% filter(!possibleCareless), estimator = "MLM")
summary(cfa4, fit.measures = TRUE, standardized = TRUE)
fitMeasures(cfa4,c("cfi.robust","tli.robust","rmsea.ci.lower.robust","rmsea.robust","rmsea.ci.upper.robust","srmr"))
reliability(cfa4)


# For saving image
png("corrPlot.png", 
    width = 10, height = 10,
    res = 300,
    units = "in",
    bg = "transparent")         

corrplot(cor(df %>% 
               filter(!possibleCareless) %>%
               select(starts_with("mean")) %>%
               rename(
                 "Auto Sat (BANGQ)" = mean_as,
                 "Auto Frus (BANGQ)" = mean_af,
                 "Comp Sat (BANGQ)" = mean_cs,
                 "Comp Frus (BANGQ)" = mean_cf,
                 "Relat Sat (BANGQ)" = mean_rs,
                 "Relat Frus (BANGQ)" = mean_rf,
                 "Auto Sat (BPNSFS)*" = mean_as_bpnsfs, 
                 "Auto Frus (BPNSFS)*" = mean_af_bpnsfs, 
                 "Comp Sat (BPNSFS)*" = mean_cs_bpnsfs, 
                 "Comp Frus (BPNSFS)*" = mean_cf_bpnsfs, 
                 "Relat Sat (BPNSFS)*" = mean_rs_bpnsfs, 
                 "Relat Frus (BPNSFS)*" = mean_rf_bpnsfs, 
                 "Auto Sat (PXI)*" = mean_as_pxi,
                 "Mastery (PXI)*" = mean_cs_pxi,
                 "Intrinsic Motivation (UMI)*" = mean_im_umi
               ), 
             use = "pairwise.complete.obs"),
         order = "original", tl.col='black',
         method = "color",
         addCoef.col = 'black',
         diag = FALSE,
         type = "lower",
         tl.srt = 45,
         tl.cex = 1.2) 
dev.off() 
 
summary(lm(mean_im_umi ~ mean_as + mean_af + mean_cs + mean_cf + mean_rs + mean_rf, data = dfPiped))

summary(lm(mean_im_umi ~ mean_as_bpnsfs + mean_af_bpnsfs + mean_cs_bpnsfs + mean_cf_bpnsfs + 
             mean_rs_bpnsfs + mean_rf_bpnsfs, data = dfPiped))

# Predictive validity of actual playtime
summary(lmerTest::lmer(rank(playtimePrev2Weeks) ~ rank(needSatGames_centered) + rank(needFrusGames_centered) + (1|randomID),
                       data = dflFinal))
