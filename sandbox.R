library(tidyverse)
library(readxl)
raw <- read_xlsx("Data/YRBSS.xlsx", col_names = T, )
option1 <- raw |> 
  sample_n(1000) |> 
  select(q1, q2, q3, q5, q6, q10, q31, q35) |>
  rename(age = q1, 
         sex = q2,
         grade = q3, 
         height = q5, 
         weight = q6, 
         texting = q10,
         smoking = q31,
         vaping = q35)
option1 <- option1 |> 
  mutate(age = 
           replace(age, age == "4","15"))

write.csv(option1, "Data/YRBSS_Sample.csv") 
