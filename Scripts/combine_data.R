library(tidyverse)

efa <- read_csv("RawData/EFAdata.csv") %>%
  mutate(study = "study1efa") %>%
  select(study, age, gender, condition, recallText, possibleCareless, af01:rs15) %>%
  mutate(bang_1 = as01,
         bang_2 = as06,
         bang_3 = as09,
         bang_4 = as05,
         bang_5 = af01,
         bang_6 = af16,
         bang_7 = af04,
         bang_8 = af02,
         bang_9 = cs13,
         bang_10 = cs02,
         bang_11 = cs06,
         bang_12 = cs11,
         bang_13 = cf01,
         bang_14 = cf05,
         bang_15 = cf09,
         bang_16 = cf14,
         bang_17 = rs05,
         bang_18 = rs04,
         bang_19 = rs13,
         bang_20 = rs03,
         bang_21 = rf07,
         bang_22 = rf05,
         bang_23 = rf08,
         bang_24 = rf02) %>%
  mutate(condition = ifelse(is.na(condition), "any experience", condition))

gen <- read_csv("RawData/XboxData.csv") %>%
  mutate(randomID = as.character(randomID),
         possibleCareless = isCareless,
         study = "study2xbox") %>%
  mutate(as01 = bang_1,
         as06 = bang_2,
         as09 = bang_3,
         as05 = bang_4,
         af01 = bang_5,
         af16 = bang_6,
         af04 = bang_7,
         af02 = bang_8,
         cs13 = bang_9,
         cs02 = bang_10,
         cs06 = bang_11,
         cs11 = bang_12,
         cf01 = bang_13,
         cf05 = bang_14,
         cf09 = bang_15,
         cf14 = bang_16,
         rs05 = bang_17,
         rs04 = bang_18,
         rs13 = bang_19,
         rs03 = bang_20,
         rf07 = bang_21,
         rf05 = bang_22,
         rf08 = bang_23,
         rf02 = bang_24) %>%
  filter(!is.na(bang_1)) %>%
  select(study, randomID, possibleCareless, wave, age, gender, bang_1:bang_24, as01:rf02, 
         playtimePrevWeek, playtimePrev2Weeks, playtimeNextWeek, playtimeNext2Weeks) %>%
  arrange(randomID, wave)


piped <- read_csv("RawData/CFAdata.csv")[-(1:2),] %>% 
  mutate(across(starts_with("bang"), ~recode(., 
                                             "1 - Strongly disagree" = 1,
                                             "2" = 2,
                                             "3" = 3,
                                             "4 - Neither agree nor disagree" = 4,
                                             "5" = 5,
                                             "6" = 6,
                                             "7 - Strongly agree" = 7))) %>%
  mutate(as01 = bang_1,
         as06 = bang_2,
         as09 = bang_3,
         as05 = bang_4,
         af01 = bang_5,
         af16 = bang_6,
         af04 = bang_7,
         af02 = bang_8,
         cs13 = bang_9,
         cs02 = bang_10,
         cs06 = bang_11,
         cs11 = bang_12,
         cf01 = bang_13,
         cf05 = bang_14,
         cf09 = bang_15,
         cf14 = bang_16,
         rs05 = bang_17,
         rs04 = bang_18,
         rs13 = bang_19,
         rs03 = bang_20,
         rf07 = bang_21,
         rf05 = bang_22,
         rf08 = bang_23,
         rf02 = bang_24) %>%
  mutate(across(starts_with("pxi"), ~recode(.,
                                            "-3\n(strongly disagree)" = -3,
                                            "-2\n(disagree)" = -2,
                                            "-1\n(slightly disagree)" = -1,
                                            "0\n(neutral)" = 0,
                                            "+1\n(slightly agree)" = 1,
                                            "+2\n(agree)" = 2,
                                            "+3\n(strongly agree)" = 3))) %>%
  rename_with(.cols = starts_with("UMI"), .fn = ~tolower(.)) %>%
  mutate(across(starts_with("umi"), ~recode(.,
                                            "Strongly disagree" = 1,
                                            "Disagree" = 2,
                                            "Somewhat disagree" = 3,
                                            "Neither agree nor disagree" = 4,
                                            "Somewhat agree" = 5,
                                            "Agree" = 6,
                                            "Strongly Agree" = 7))) %>%
  mutate(across(starts_with("bpnsfs"), ~recode(.,
                                               "1 - not at all true" = 1,
                                               "2" = 2,
                                               "3" = 3,
                                               "4" = 4,
                                               "5 - completely true" = 5))) %>%
  mutate(study = "study3piped") %>%
  left_join(read_csv("RawData/prolificDemographics.csv"),
            by = c("PROLIFIC_PID" = "Participant id")) %>%
  rename(age = Age, 
         gender = Sex,
         recallText = `recall-pos-text`) %>%
  mutate(possibleCareless = ifelse(abs(pxi_7-pxi_1) > 1 | nchar(game) > 25, TRUE, FALSE)) %>%
  select(study, age, game, gender, recallText, possibleCareless, as01:rf02, umi_1:bpnsfs_24, -pxi_7)


xboxPiped <- read_csv("RawData/XboxDataPiped.csv")[-(1:2),] %>%
  mutate(possibleCareless = ifelse(as.integer(`Duration (in seconds)`) < 400, 
                                   TRUE,
                                   FALSE)) %>%
  select(randomID, game, possibleCareless, bangq_piped_1:bangq_piped_28) %>%
  rename_with(.cols = starts_with("BANG"), ~gsub("bangq_piped", "bang", .)) %>%
  filter(!is.na(bang_25)) %>%
  mutate(across(starts_with("bang"), ~recode(., 
                                             "1 - Strongly disagree" = 1,
                                             "2" = 2,
                                             "3" = 3,
                                             "4 - Neither agree nor disagree" = 4,
                                             "5" = 5,
                                             "6" = 6,
                                             "7 - Strongly agree" = 7))) %>%
  mutate(as01 = bang_1,
         as06 = bang_2,
         as09 = bang_3,
         as05 = bang_4,
         af01 = bang_5,
         af16 = bang_6,
         af04 = bang_7,
         af02 = bang_8,
         cs13 = bang_9,
         cs02 = bang_10,
         cs06 = bang_11,
         cs11 = bang_12,
         cf01 = bang_13,
         cf05 = bang_14,
         cf09 = bang_15,
         cf14 = bang_16,
         rs05 = bang_17,
         rs04 = bang_18,
         rs13 = bang_19,
         rs03 = bang_20,
         rf07 = bang_21,
         rf05 = bang_22,
         rf08 = bang_23,
         rf02 = bang_24,
         af10 = bang_25,
         af05 = bang_26,
         af09 = bang_27,
         af15 = bang_28) %>%
  select(-starts_with("bang_")) %>%
  mutate(study = "study3piped") %>%
  left_join(read_csv("RawData/XboxDemographics.csv") %>% select(randomID, age, gender),
            by = "randomID") %>%
  mutate(age = as.integer(age))

test <- bind_rows(piped, xboxPiped)

dfc <- bind_rows(efa, gen, piped, xboxPiped) %>%
  arrange(study) %>%
  mutate(possibleCareless = ifelse(is.na(possibleCareless), FALSE, possibleCareless)) %>%
  mutate(gender = case_when(
    gender == "Female" ~ "Woman",
    gender == "Male" ~ "Man",
    gender %in% c("Non-binary", "Prefer not to say", "Prefer to specify") ~ "Non-binary/prefer not to say",
    TRUE ~ gender
  )) %>%
  select(-starts_with("bang"))

write_csv(dfc, "CleanData/combinedData.csv")

