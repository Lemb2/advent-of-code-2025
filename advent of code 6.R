library(tidyverse)
library(here)


input <- readRDS(here("input_day6.rds"))


input_tibble <- read.table(text = input)

answer1 <- 0

for (i in 1:ncol(input_tibble)) {
  
    
    answer1 <- ifelse(input_tibble[[i]][[nrow(input_tibble)]] == "+", answer1 + sum(as.numeric(head(input_tibble[[i]], nrow(input_tibble) - 1))),
                     answer1 + prod(as.numeric(head(input_tibble[[i]], nrow(input_tibble) - 1))))
    

}

answer2 <- 0


#the way I'm reading it in creates a row with just a space
input_tibble2 <- tibble(a = input) |> separate_rows(a, sep = "\n") |> head(-1)

operations <- unlist(strsplit(tail(input_tibble2$a, 1), split = ""))
operations <- operations[operations != " "]

input_tibble2 <- matrix(unlist(strsplit(head(input_tibble2$a, nrow(input_tibble2) - 1), split = "")), ncol = str_length(input_tibble2$a[[1]]), byrow = TRUE)

numbers <- apply(input_tibble2, 2, function(x) paste(x, collapse = ""))

position <- 1

for (i in 1:length(operations)) {
  to_add <- ifelse(operations[i] == "+", 0, 1)
  while (position <= length(numbers)) {
   if(is.na(as.numeric(numbers[position]))  ) {
     position <- position + 1
     break()
   } else {
    to_add <- ifelse(operations[i] == "+", to_add + as.numeric(numbers[position]), to_add * as.numeric(numbers[position]))
    position <- position + 1
   }
  }
  print(to_add)
  answer2 <- answer2 + to_add
}

answer2