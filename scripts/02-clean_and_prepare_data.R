library(tidyverse)
table_tibble = read_csv("inputs/data/raw_data.csv")

table_tibble <- 
  table_tibble %>% 
  separate_rows(raw_data, sep = "\\n", convert = FALSE)

title_tibble <-
  table_tibble %>% 
  slice(5:6)

age_tibble <-
  table_tibble %>% 
  slice(7:12)

marital_tibble <-
  table_tibble %>% 
  slice(14:16)

residence_tibble <-
  table_tibble %>% 
  slice(18:20)

project_tibble <-
  table_tibble %>% 
  slice(22:24)

region_tibble <-
  table_tibble %>% 
  slice(26:33)

education_tibble <-
  table_tibble %>% 
  slice(35:40)

total_tibble <-
  table_tibble %>% 
  slice(42)

# age table
age_tibble <- 
  age_tibble %>% 
  separate(col = raw_data, 
           into = c("age", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

age_tibble_cleaned <- 
  age_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(age = str_remove_all(age, "\\s"),
         knows_of_aids = str_replace_all(knows_of_aids, "89,4", "89.4"),
         knows_of_aids = str_replace_all(knows_of_aids, "913", "91.3"),
         knows_of_aids = str_replace_all(knows_of_aids, "926", "92.6"),
         knows_of_aids = str_replace_all(knows_of_aids, "893", "89.3"),
         radio = str_replace_all(radio, "549", "54.9"),
         pamphlet = str_replace_all(pamphlet, "45", "4.5"),
         health_worker = str_replace_all(health_worker, "28", "2.8"),
         church_s_temple = str_replace_all(church_s_temple, "00", "0.0"),
         church_s_temple = str_replace_all(church_s_temple, "0 1", "0.1"),
         friend_s_relative = str_replace_all(friend_s_relative, "I I. I", "11.1"),
         work_place = str_replace_all(work_place, "18", "1.8"),
         other = str_replace_all(other, "00", "0.0"),
         other = str_replace_all(other, "07", "0.7"),
         number_of_women = str_replace_all(number_of_women, "1.016", "1016"),
         number_of_women = str_replace_all(number_of_women, "2.331", "2331"),
         number_of_women = str_replace_all(number_of_women, "1.457", "1457"),
         ) %>% 
  mutate_at(c(2:14), as.numeric)


# marital table
marital_tibble <- 
  marital_tibble %>% 
  separate(col = raw_data, 
           into = c("current_marital_status", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

marital_tibble_cleaned <- 
  marital_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(current_marital_status = str_remove_all(current_marital_status, "\\s"),
         current_marital_status = str_replace_all(current_marital_status, "Cnrrenllynlarried", "Currently Married"),
         current_marital_status = str_replace_all(current_marital_status, "Formerlymarried", "Formerly Married"),
         knows_of_aids = str_replace_all(knows_of_aids, "912", "91.2"),
         radio = str_replace_all(radio, "689", "68.9"),
         radio = str_replace_all(radio, "587", "58.7"),
         tv = str_replace_all(tv, "764", "76.4"),
         newspaper = str_replace_all(newspaper, "226", "22.6"),
         community_meeting = str_replace_all(community_meeting, "68", "6.8"),
         work_place = str_replace_all(work_place, "17", "1.7"),
         other = str_replace_all(other, "06", "0.6"),
         number_of_women = str_replace_all(number_of_women, "5.340", "5340"),
  ) %>% 
  mutate_at(c(2:14), as.numeric)

# residence table
residence_tibble <- 
  residence_tibble %>% 
  separate(col = raw_data, 
           into = c("residence", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

residence_tibble_cleaned <- 
  residence_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(residence = str_remove_all(residence, "\\s"),
         radio = str_replace_all(radio, "765", "76.5"),
         tv = str_replace_all(tv, "919", "91.9"),
         newspaper = str_replace_all(newspaper, "226", "22.6"),
         church_s_temple = str_replace_all(church_s_temple, "04", "0.4"),
         friend_s_relative = str_replace_all(friend_s_relative, "136", "13.6"),
         other = str_replace_all(other, "05", "0.5"),
         other = str_replace_all(other, "03", "0.3"),
         number_of_women = str_replace_all(number_of_women, "1.069", "1069"),
         number_of_women = str_replace_all(number_of_women, "4.595", "4595"),
  ) %>% 
  mutate_at(c(2:14), as.numeric)


# project table
project_tibble <- 
  project_tibble %>% 
  separate(col = raw_data, 
           into = c("project_province", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

project_tibble_cleaned <- 
  project_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(project_province = str_remove_all(project_province, "\\s"),
         knows_of_aids = str_replace_all(knows_of_aids, "913", "91.3"),
         radio = str_replace_all(radio, "682", "68.2"),
         tv = str_replace_all(tv, "759", "75.9"),
         newspaper = str_replace_all(newspaper, "23 0", "23.0"),
         newspaper = str_replace_all(newspaper, "20 6", "20.6"),
         pamphlet = str_replace_all(pamphlet, "60", "6.0"),
         pamphlet = str_replace_all(pamphlet, "41", "4.1"),
         health_worker = str_replace_all(health_worker, "29", "2.9"),
         church_s_temple = str_replace_all(church_s_temple, "0 I", "0.1"),
         friend_s_relative = str_replace_all(friend_s_relative, "14 I", "14.1"),
         other = str_replace_all(other, "02", "0.2"),
         number_of_women = str_remove_all(number_of_women, "\\."),
  ) %>% 
  mutate_at(c(2:14), as.numeric)


# region table
region_tibble <- 
  region_tibble %>% 
  separate(col = raw_data, 
           into = c("region", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

region_tibble_cleaned <- 
  region_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(region = str_remove_all(region, "\\s"),
         region = str_replace_all(region, "NotlhernUplands", "Northern Uplands"),
         region = str_replace_all(region, "RedRiverDelta", "Red River Delta"),
         region = str_replace_all(region, "NorthCenlral", "North Central"),
         region = str_replace_all(region, "CentralCoasl", "Central Coast"),
         region = str_replace_all(region, "CemralIlighlands", "Central Highlands"),
         region = str_replace_all(region, "Southeasl", "Southeast"),
         region = str_replace_all(region, "MekougRiverDeha", "Mekong River Delta"),
         
         knows_of_aids = str_replace_all(knows_of_aids, "86.I", "86.1"),
         knows_of_aids = str_replace_all(knows_of_aids, "972", "97.2"),
         knows_of_aids = str_replace_all(knows_of_aids, "954", "95.4"),
         knows_of_aids = str_replace_all(knows_of_aids, "902", "90.2"),
         
         radio = str_replace_all(radio, "685", "68.5"),
         radio = str_replace_all(radio, "793", "79.3"),
         radio = str_replace_all(radio, "726", "72.6"),
         radio = str_replace_all(radio, "615", "61.5"),
         radio = str_replace_all(radio, "350", "35.0"),
         
         tv = str_replace_all(tv, "930", "93.0"),
         tv = str_replace_all(tv, "611", "61.1"),
         
         newspaper = str_replace_all(newspaper, "22 0", "22.0"),

         pamphlet = str_replace_all(pamphlet, "46", "4.6"),
         pamphlet = str_replace_all(pamphlet, "I00", "10.0"),
         pamphlet = str_replace_all(pamphlet, "37", "3.7"),
         pamphlet = str_replace_all(pamphlet, "27", "2.7"),
         
         health_worker = str_replace_all(health_worker, "40", "4.0"),
         health_worker = str_replace_all(health_worker, "24", "2.4"),
         
         church_s_temple = str_replace_all(church_s_temple, "O0", "0.0"),
         church_s_temple = str_replace_all(church_s_temple, "00", "0.0"),
         church_s_temple = str_replace_all(church_s_temple, "07", "0.7"),
         
         school = str_replace_all(school, "17", "1.7"),
         school = str_replace_all(school, "09", "0.9"),
         school = str_replace_all(school, "12", "1.2"),
         
         community_meeting = str_replace_all(community_meeting, "130", "13.0"),
         community_meeting = str_replace_all(community_meeting, "78", "7.8"),
         community_meeting = str_replace_all(community_meeting, "58", "5.8"),
         
         friend_s_relative = str_replace_all(friend_s_relative, "217", "21.7"),
         friend_s_relative = str_replace_all(friend_s_relative, "75", "7.5"),
         friend_s_relative = str_replace_all(friend_s_relative, "112", "11.2"),
         friend_s_relative = str_replace_all(friend_s_relative, "192", "19.2"),
         
         work_place = str_remove_all(work_place, "\\|"),
         work_place = str_replace_all(work_place, "4", "4.1"),
         
         other = str_replace_all(other, "03", "0.3"),
         other = str_replace_all(other, "01", "0.1"),
         
         number_of_women = str_remove_all(number_of_women, "\\."),
  ) %>% 
  mutate_at(c(2:14), as.numeric)


# education table
education_tibble <- 
  education_tibble %>% 
  separate(col = raw_data, 
           into = c("education", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

education_tibble_cleaned <- 
  education_tibble %>% 
  slice(-1) %>% 
  select(-raw_data) %>% 
  mutate(education = str_remove_all(education, "\\s"),
         education = str_replace_all(education, "Noeducation", "No education"),
         education = str_replace_all(education, "Somepdnmr.~", "Some primary"),
         education = str_replace_all(education, "Completedprimar~", "Completed primary"),
         education = str_replace_all(education, "Con~pletedIt~erseeo~dar~", "Completed lower secondary"),
         education = str_replace_all(education, "Completedhighersecorldar\\)+", "Completed higher secondary"),
         
         knows_of_aids = str_replace_all(knows_of_aids, "911", "91.1"),
         knows_of_aids = str_replace_all(knows_of_aids, "969", "96.9"),
         
         radio = str_replace_all(radio, "284", "28.4"),
         radio = str_replace_all(radio, "537", "53.7"),
         radio = str_replace_all(radio, "760", "76.0"),
         
         newspaper = str_replace_all(newspaper, "I. I", "1.1"),
         
         health_worker = str_replace_all(health_worker, "18", "1.8"),
         
         church_s_temple = str_replace_all(church_s_temple, "O0", "0.0"),
         church_s_temple = str_replace_all(church_s_temple, "00", "0.0"),
         church_s_temple = str_replace_all(church_s_temple, "07", "0.7"),
         
         school = str_replace_all(school, "17", "1.7"),
         school = str_replace_all(school, "09", "0.9"),
         school = str_replace_all(school, "12", "1.2"),
         
         community_meeting = str_replace_all(community_meeting, "IL5", "8.5"),
         community_meeting = str_replace_all(community_meeting, "I0.0", "10.0"),
         
         friend_s_relative = str_replace_all(friend_s_relative, "I 1.3", "11.3"),
         
         work_place = str_replace_all(work_place, "0,8", "0.8"),
         work_place = str_replace_all(work_place, "1,4", "1.4"),
         work_place = str_replace_all(work_place, "5,4", "5.4"),
         
         other = str_replace_all(other, "03", "0.3"),
         other = str_replace_all(other, "01", "0.1"),
         
         number_of_women = str_remove_all(number_of_women, "\\."),
  ) %>% 
  mutate_at(c(2:14), as.numeric)


# total table
total_tibble <- 
  total_tibble %>% 
  separate(col = raw_data, 
           into = c("total", 
                    "knows_of_aids",
                    "radio",
                    "tv", 
                    "newspaper",
                    "pamphlet",
                    "health_worker",
                    "church_s_temple",
                    "school",
                    "community_meeting",
                    "friend_s_relative",
                    "work_place",
                    "other",
                    "number_of_women"), 
           sep = "\\s{2,}", 
           remove = FALSE,
           fill = "right"
  )

total_tibble_cleaned <- 
  total_tibble %>% 
  select(-raw_data) %>% 
  mutate(total = str_remove_all(total, "\\s"),
         total = str_replace_all(total, "folal", "total"),
         
         radio = str_replace_all(radio, "683", "68.3"),
         
         newspaper = str_replace_all(newspaper, "223", "22.3"),
         
         church_s_temple = str_replace_all(church_s_temple, "OI", "0.1"),
         
         work_place = str_replace_all(work_place, "18", "1.8"),

         other = str_replace_all(other, "03", "0.3"),
         
         number_of_women = str_remove_all(number_of_women, "\\."),
  ) %>% 
  mutate_at(c(2:14), as.numeric)

# writing csv

write_csv(age_tibble_cleaned, "inputs/data/cleaned_age_data.csv")
write_csv(marital_tibble_cleaned, "inputs/data/cleaned_marital_data.csv")
write_csv(residence_tibble_cleaned, "inputs/data/cleaned_residence_data.csv")
write_csv(project_tibble_cleaned, "inputs/data/cleaned_project_data.csv")
write_csv(region_tibble_cleaned, "inputs/data/cleaned_region_data.csv")
write_csv(education_tibble_cleaned, "inputs/data/cleaned_education_data.csv")
write_csv(total_tibble_cleaned, "inputs/data/cleaned_total_data.csv")