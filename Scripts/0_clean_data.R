library(tidyverse)
library(careless)

raw_df <- read_csv("RawData/BANG_raw_wave1_230105.csv")[-(1:2),] %>%
  filter(DistributionChannel != "preview")

raw_df_2 <- read_csv("RawData/BANG_raw_wave2_230131.csv")[-(1:2),] %>%
  filter(DistributionChannel != "preview")

item_labels <- c("cf08","as07","cf07","cf01","rf15","rs08","af12","cs14","rf11","cf12","rf12","rf07","as15","cs04","af13","as04","cs09","rs04","cs05","rf06",
                 "as05","as01","as06","rs09","cf11","af03","cf13","af11","cs08","rs01","rf14","rs15","rs10","as14","cf02","rs11","cs13","rs13","cf10",
                 "af06","as09","rf02","cf04","rs05","af10","rf01","rs06","af05","cf14","rf08","rs02","cf06","cf16","af02","cf09","cs15","af08","as10","cs07",
                 "cs01","cf05","af04","af14","cs12","rf04","cf03","rs14","af16","as12","cs11","rf03","as02","rf10","af07","rs12","cs02","as08","rs07","as13",
                 "rs03","cf15","af15","rf13","rf05","af01","rf09","cs03","as03","af09","cs06","as11","cs10")

item_labels_2 <- c("rs10","cf01","af05","rf08","cf09","rf04","cs12","rs09","af03","cf07","af16","af04","as08","cs06","af01","rf01","cf16","cs01",
                   "cf15","cf14","rs04","af14","rs13","af02","af06","cf12","cs10","cs07","as13","cs14","cf05","cs02","cf13","as04","rs05","rf09",
                   "rs01","rf06","af09","rf05","as14","af10","rf07","af07","as06","cs05","rs12","cs03","af15","cf08","cs09","cs13","cs11","as05",
                   "rf14","cs08","cf03","rs10_dup","as11","as09","cf02","cf06","rs03","as07","as01","af08","af13","rs02","af11","rs08","as15",
                   "rs14","rf02","as10","cs04","cf04","rs15","cf01_dup")

clean_df <- raw_df %>%
  
  # Rename to match item labels
  rename_at(vars(`bang-1_1`:`bang-4_23`), ~ item_labels) %>%
  rename(cf08_dup = `bang-4_24`,
         as07_dup = `bang-4_25`) %>%
  mutate(wave = 1) %>%
  mutate_at(vars(cf08:as07_dup), as.numeric) %>%
  mutate(duration = as.numeric(`Duration (in seconds)`), .keep = "unused") %>%
  mutate(condition = ifelse(!is.na(`recall-pos-text`), "positive experience", "negative experience")) %>%
  
  # Reorder for easier inspection
  select(order(colnames(.))) %>%
  select(-ResponseId, -Finished, -Status, -StartDate, -EndDate, -Progress, -DistributionChannel, -UserLanguage, 
         -`Create New Field or Choose From Dropdown...`, -consent) %>%
  select(RecordedDate, age, gender, gender_5_TEXT, country, duration, condition, `recall-pos-text`, `recall-neg-text`, 
         trust, trust_3_TEXT, PROLIFIC_PID, everything()) %>%
  
  # identify potential careless responders 
  mutate(possibleCareless = duration < 400 | (abs(cf08-cf08_dup) > 1 & abs(as07-as07_dup) > 1 | PROLIFIC_PID == "613baa22050360ec21d4437f"))

clean_df_2 <- raw_df_2 %>%
  
  mutate(wave = 1) %>%
  
  # Rename to match item labels
  mutate_at(vars(`bang-1_1`:`bang-4_20`), as.numeric) %>%
  rename_at(vars(`bang-1_1`:`bang-4_20`), ~ item_labels_2) %>%
  mutate(duration = as.numeric(`Duration (in seconds)`), .keep = "unused") %>%
  
  # Reorder for easier inspection
  select(order(colnames(.))) %>%
  select(-ResponseId, -Finished, -Status, -StartDate, -EndDate, -Progress, -DistributionChannel, -UserLanguage, 
         -`Create New Field or Choose From Dropdown...`, -consent) %>%
  select(RecordedDate, age, gender, gender_5_TEXT, country, duration, `recall-pos-text`, PROLIFIC_PID, everything()) %>%
  
  # identify potential careless responders 
  mutate(possibleCareless = duration < 300 | (abs(rs10-rs10_dup) > 1 & abs(cf01-cf01_dup) > 1))

  
df <- bind_rows(clean_df, clean_df_2) %>%
  # exclude automatically- and manually-identified potential careless responders
  unite("recallText",`recall-pos-text`,`recall-neg-text`, na.rm = TRUE) %>%
  relocate(c(cf08_dup, as07_dup, rs10_dup, cf01_dup), .after = last_col())

write_csv(df, "CleanData/EFAdata.csv")
