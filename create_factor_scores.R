# Load libraries
library(lavaan)
library(tidyverse)

# Load sample data
sample_data <- read_csv("https://osf.io/download/zv4cn/") |> 
  # calculate (sub)scale means
  rowwise() |> 
  mutate(
    mean_AS = mean(c(bang_1, bang_2, bang_3)),
    mean_AF = mean(c(bang_4, bang_5, bang_6)),
    mean_CS = mean(c(bang_7, bang_8, bang_9)),
    mean_CF = mean(c(bang_10, bang_11, bang_12)),
    mean_RS = mean(c(bang_13, bang_14, bang_15)),
    mean_RF = mean(c(bang_16, bang_17, bang_18))
  )

# Specify factor model
bangMod <- '
  lat_AS =~ 1*bang_1 + bang_2 + bang_3
  lat_AF =~ 1*bang_4 + bang_5 + bang_6 
  lat_CS =~ 1*bang_7 + bang_8 + bang_9
  lat_CF =~ 1*bang_10 + bang_11 + bang_12
  lat_RS =~ 1*bang_13 + bang_14 + bang_15
  lat_RF =~ 1*bang_16 + bang_17 + bang_18
'

# Fit CFA model with sample data
fit <- cfa(bangMod, sample_data)
summary(fit, fit.measures = TRUE)

# Inspect factor scores and attach to original data
#
# Note that the same subscale item responses will not necessarily result in exactly the same latent factor scores -
# This is because the model also takes into account correlations between the latent factors
lavPredict(fit, type = "lv")
sample_data2 <- cbind(sample_data, lavPredict(fit))

# In practice, factor scores are very closely related to raw means - scores correlate almost perfectly (see below)
#
# However, the latent factor score method can be included within a structural equation model, 
# allowing you to account for measurement error within a model - a mean score implicitly assumes no measurement error
cor(sample_data2$mean_AS, sample_data2$lat_AS)
cor(sample_data2$mean_RS, sample_data2$lat_RS)
plot(sample_data2$mean_AF, sample_data2$lat_AF)

hist(sample_data2$mean_AF, breaks = 15)
hist(sample_data2$lat_AF, breaks = 15)
