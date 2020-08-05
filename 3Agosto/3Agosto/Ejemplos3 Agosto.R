library(readr)
library(readxl)

excel <- read_excel("data/example_1.xlsx")
str(excel)

excel_2 <- read_excel("data/example_1.xlsx", sheet =2)
str(excel_2)


csv_leido <- read_csv("data/example_2.csv")

csv_leido_delimitador <- read_delim("data/example_3.txt", delim = ";")

csv_leido_delimitador_4 <- read_delim("data/example_4.txt", delim = "|")


library(tidyverse)
library(tidytext)

dorian <- read_lines("data/dorian_gray.txt", skip_empty_rows = TRUE)
dorian_frame <- tibble(dorian)
dorian_words <- unnest_tokens(dorian_frame, output = word, input = dorian, token="words")
