library(tidyverse)
library(careless)

raw_df <- read_csv("RawData/BANG_raw_230105.csv")[-(1:2),] %>%
  filter(DistributionChannel != "preview")

item_labels <- c("cf08","as07","cf07","cf01","rf15","rs08","af12","cs14","rf11","cf12","rf12","rf07","as15","cs04","af13","as04","cs09","rs04","cs05","rf06",
                 "as05","as01","as06","rs09","cf11","af03","cf13","af11","cs08","rs01","rf14","rs15","rs10","as14","cf02","rs11","cs13","rs13","cf10",
                 "af06","as09","rf02","cf04","rs05","af10","rf01","rs06","af05","cf14","rf08","rs02","cf06","cf16","af02","cf09","cs15","af08","as10","cs07",
                 "cs01","cf05","af04","af14","cs12","rf04","cf03","rs14","af16","as12","cs11","rf03","as02","rf10","af07","rs12","cs02","as08","rs07","as13",
                 "rs03","cf15","af15","rf13","rf05","af01","rf09","cs03","as03","af09","cs06","as11","cs10")

clean_df <- raw_df %>%
  
  # Rename to match item labels
  rename_at(vars(`bang-1_1`:`bang-4_23`), ~ item_labels) %>%
  rename(cf08_dup = `bang-4_24`,
         as07_dup = `bang-4_25`) %>%
  mutate_at(vars(cf08:as07_dup), as.numeric) %>%
  mutate(duration = as.numeric(`Duration (in seconds)`), .keep = "unused") %>%
  
  # Reorder for easier inspection
  select(order(colnames(.))) %>%
  select(-ResponseId, -Finished, -Status, -StartDate, -EndDate, -Progress, -DistributionChannel, -UserLanguage, 
         -`Create New Field or Choose From Dropdown...`, -consent) %>%
  select(RecordedDate, age, gender, gender_5_TEXT, country, duration, `recall-pos-text`, `recall-neg-text`, 
         trust, trust_3_TEXT, PROLIFIC_PID, everything()) %>%
  
  # identify potential careless responders 
  mutate(possibleCareless = duration < 400 | (abs(cf08-cf08_dup) > 1 & abs(as07-as07_dup) > 1))

  
df <- clean_df %>%
  # exclude automatically- and manually-identified potential careless responders
  filter(!possibleCareless & !(PROLIFIC_PID %in% c("60fd5bc629663dec819d54e3","5f9a0f1dba641d12282c8e41","5ea9d5a43d8c331224cf4b61",
                                                   "614fb54a636d939d60da5818","60f23a36acf5f6c70e990efd","613baa22050360ec21d4437f"))) %>%
  relocate(cf08_dup, .after = last_col()) %>%
  relocate(as07_dup, .after = last_col())

write_csv(df, "CleanData/cleanData_230105.csv")
