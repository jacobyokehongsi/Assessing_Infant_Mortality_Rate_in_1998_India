# loading libraries
library(pdftools)
library(tidyverse)

# In this R script, I extract the table data from the NFHS-2 report. I then 
# select the desired tables I require and write them into a .csv format.


pdf <- pdf_text("inputs/data/FRIND2.pdf")

pdf_tibble <- tibble(raw_data = pdf)

table_tibble_1 <- 
  pdf_tibble %>% 
  slice(463)

table_tibble_2 <- 
  pdf_tibble %>% 
  slice(465)

table_tibble_3 <- 
  pdf_tibble %>% 
  slice(466)

write_csv(table_tibble_1, "inputs/data/raw_data1.csv")
write_csv(table_tibble_2, "inputs/data/raw_data2.csv")
write_csv(table_tibble_3, "inputs/data/raw_data3.csv")
