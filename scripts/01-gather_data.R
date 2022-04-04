library(pdftools)
library(tidyverse)

pdf <- pdf_text("inputs/data/FR104.pdf")

pdf_tibble <- tibble(raw_data = pdf)

table_tibble <- 
  pdf_tibble %>% 
  slice(133)

write_csv(table_tibble, "inputs/data/raw_data.csv")
