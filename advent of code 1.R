library(here)
library(tidyverse)


input <- readRDS(here("input_day1.rds"))


turns <- tibble(turn = input) |>
  separate_rows(turn, sep = "\n") |>
  mutate(sign = ifelse(substr(turn, 1, 1) == "L", -1, 1),
         number = substr(turn, 2, str_length(turn)),
         number = as.numeric(number),
         move = sign * number)


count1 <- 0
position <- 50

for (i in 1:nrow(turns)) {
  position <- (position + turns$move[i]) %% 100
  
  count1 <- ifelse(position == 0, count1 + 1, count1)
}

count2 <- 0
position <- 50

for (i in 1:nrow(turns)) {
  count2 <- ifelse(position + turns$move[i] > 99, count2 + floor((position + turns$move[i]) / 100), count2)
  count2 <- ifelse(position + turns$move[i] < 1, count2 + 1 + floor(abs(position + turns$move[i]) / 100), count2)
  count2 <- ifelse(position + turns$move[i] < 1 && position == 0, count2 - 1 , count2)
  print(c(position + turns$move[i], (position + turns$move[i]) %% 100))
  
  position <- (position + turns$move[i]) %% 100
  
  
}