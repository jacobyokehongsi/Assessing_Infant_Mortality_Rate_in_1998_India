# loading libraries
library(tidyverse)
library(pointblank)

# reading csv files
table_tibble_1 = read_csv("inputs/data/raw_data1.csv")
table_tibble_2 = read_csv("inputs/data/raw_data2.csv")
table_tibble_3 = read_csv("inputs/data/raw_data3.csv")

# cleaning and preparing data
# table 1
table_tibble_1 <- 
  table_tibble_1 %>% 
  separate_rows(raw_data, sep = "\\n", convert = FALSE)

table_tibble_1_sep <- 
  table_tibble_1 %>% 
  separate(col = raw_data, 
           into = c("fill",
                    "state",
                    "population", 
                    "fill2",
                    "att_school",
                    "urban",
                    "no_media",
                    "electricity",
                    "piped_pump_water",
                    "no_toilet",
                    "iodized_salt",
                    "healthcare_decisions",
                    "married_by18"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

table_tibble_1_cleaned <- 
  table_tibble_1_sep %>% 
  select(state, population, att_school, electricity, piped_pump_water, no_toilet, healthcare_decisions) %>% 
  slice(13:50) %>% 
  na.omit() %>% 
  mutate(state = str_replace_all(state, "India#", "India"),
         ) %>% 
  mutate_at(c(2:7), as.numeric)

# table 2
table_tibble_2 <- 
  table_tibble_2 %>% 
  separate_rows(raw_data, sep = "\\n", convert = FALSE)

table_tibble_2_sep <- 
  table_tibble_2 %>% 
  separate(col = raw_data, 
           into = c("state",
                    "infant_mortality", 
                    "fill1",
                    "antenatal",
                    "injections", 
                    "iron_folic",
                    "deli_med_inst",
                    "deli_health_pro",
                    "fill2",
                    "fill3",
                    "breastfed"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

table_tibble_2_cleaned <- 
  table_tibble_2_sep %>% 
  select(state, infant_mortality, antenatal, injections, iron_folic, deli_med_inst, 
         deli_health_pro, breastfed) %>% 
  slice(13:50) %>% 
  na.omit() %>%
  mutate_at(c(2:8), as.numeric) %>% 
  mutate(state = trimws(state)
         )
table_tibble_2_cleaned[13, 8] = 33.9


# table 3
table_tibble_3 <- 
  table_tibble_3 %>% 
  separate_rows(raw_data, sep = "\\n", convert = FALSE)

table_tibble_3_sep <- 
  table_tibble_3 %>% 
  separate(col = raw_data, 
           into = c("state",
                    "child_milkfood", 
                    "f2",
                    "child_anaemia",
                    "underweight", 
                    "f4",
                    "f5",
                    "below_BMI",
                    "women_anaemia",
                    "f7",
                    "f8"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

table_tibble_3_cleaned <- 
  table_tibble_3_sep %>% 
  select(state, child_milkfood, child_anaemia, underweight, below_BMI, women_anaemia) %>% 
  slice(11:48) %>% 
  na.omit() %>%
  mutate_at(c(2:6), as.numeric) %>%
  mutate(state = trimws(state)
  )
table_tibble_3_cleaned[13, 2] = 60.2
table_tibble_3_cleaned[17, 2] = 74.2
table_tibble_3_cleaned[20, 2] = 65.4

temp_table <- inner_join(table_tibble_1_cleaned, table_tibble_2_cleaned, by="state")
cleaned_data <- inner_join(temp_table, table_tibble_3_cleaned, by="state")

# perform tests using pointblank
agent <-
  create_agent(tbl = cleaned_data) |>
  col_is_numeric(columns = 2:19) |> # test that the variables that should be numeric are
  col_is_character(columns = 1) |> # test that the variables that should be characters are
  interrogate()

# writing csv
write_csv(cleaned_data, "inputs/data/cleaned_data.csv")
