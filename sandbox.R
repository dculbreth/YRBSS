library(tidyverse)
library(readxl)
raw <- read_xlsx("Data/YRBSS.xlsx", col_names = T)
raw2 <- read_xlsx("Data/XXHqn.xlsx", col_names = T, na = "")
#import, sample, & rename vars
set.seed(28640)
YRBSS <- raw |> 
  sample_n(1000) |> 
  select(q1, q2, q3, q5, q6, bmipct, q10, q31, q35, q66, q71) |>
  rename(age = q1, 
         sex = q2,
         grade = q3, 
         height = q5, 
         weight = q6,
         texting = q10,
         smoking = q31,
         vaping = q35, 
         fruit = q66,
         soda = q71
         )
#clean vars
YRBSS_clean <- YRBSS |> 
  mutate(
    #age
    age = case_match(age,
                     "1" ~ "12",
                     "2" ~ "13",
                     "3" ~ "14",
                     "4" ~ "15",
                     "5" ~ "16",
                     "6" ~ "17",
                     "7" ~ "18",
                     .default = age), # Keep everything else the same
    #sex
    sex = case_match(sex,
                     "1" ~ "F",
                     "2" ~ "M",
                     .default = sex),
    #grade
    grade = case_match(grade,
                     "1" ~ "9",
                     "2" ~ "10",
                     "3" ~ "11",
                     "4" ~ "12",
                     "5" ~ "",
                     .default = grade),
    #texting
    texting = case_match(texting,
                     "1" ~ "I did not drive a car or other vehicle during the past 30 days",
                     "2" ~ "I drove a car or other vehicle, but did not text or e-mail while driving",
                     "3" ~ "1 or 2 days",
                     "4" ~ "3 to 5 days",
                     "5" ~ "6 to 9 days",
                     "6" ~ "10 to 19 days",
                     "7" ~ "20 to 29 days",
                     "8" ~ "All 30 days",
                     .default = texting),
    #smoking
     smoking = case_match(smoking,
                     "1" ~ "Yes",
                     "2" ~ "No",
                     .default = smoking),
    #vaping
    vaping = case_match(vaping,
                         "1" ~ "Yes",
                         "2" ~ "No",
                         .default = vaping))
#coerce $age to double class
YRBSS_clean$age <- as.double(YRBSS_clean$age)

#export
write.csv(YRBSS_clean, "Data/YRBSS.csv", na = "")
