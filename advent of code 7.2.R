library(tidyverse)
library(here)

input<- readRDS(here("input_day7.rds"))



grid_tibble <- tibble(state = input) |>
  separate_rows(state, sep = "\n")

row_length <- str_length(grid_tibble$state[[1]])
column_height <- nrow(grid_tibble)

grid_tibble <- grid_tibble |>
  separate_rows(state, sep = "") |>
  filter(state != "") |>
  mutate(position = 0 + 0i)



for (k in 1:row_length) {
  for (j in 1:column_height) {
    grid_tibble$position[k + row_length * (j - 1)] <- complex(real = k, imaginary = j)
  }
  
}

first_beam_id <- (grid_tibble |> filter(state == "S"))$position[1] + complex(real = 0, imaginary = 1)

grid_tibble <- grid_tibble |> mutate(state = ifelse(position == first_beam_id, 1, state)) |> mutate(state = ifelse(state == ".", 0, state))

split_count <- 0

update_states <- function(x, this_row) {
  beams <- filter(x, is.numeric(as.numeric(state))) |> filter(state > 0) |> filter(Im(position) == this_row)
  output <- x
  
  for (i in 1:nrow(beams)) {
    beam_position <- beams$position[i]
    timeline_number <- as.numeric(output$state[match(beam_position , output$position)])

    if(output$state[match(beam_position + complex(real = 0, imaginary = 1), output$position)] %in% c("^", "<")) {
      output$state[output$position == (beam_position + complex(real = 1, imaginary = 1))] <- as.numeric(output$state[output$position == (beam_position + complex(real = 1, imaginary = 1))]) +timeline_number
      output$state[output$position == (beam_position + complex(real = -1, imaginary = 1))] <- as.numeric(output$state[output$position == (beam_position + complex(real = -1, imaginary = 1))]) + timeline_number
      
      split_count <<- ifelse(output$state[match(beam_position + complex(real = 0, imaginary = 1), output$position)] == "^", split_count + 1, split_count)
      output$state[output$position %in% (beam_position + c(complex(real = 0, imaginary = 1)))] <- "<"
    } else {
      output$state[output$position %in% (beam_position + c(complex(real = 0, imaginary = 1)))] <- as.numeric(output$state[output$position %in% (beam_position + c(complex(real = 0, imaginary = 1)))]) + timeline_number
    }
  }
  return(output)
}

for (i in 2:(column_height - 1)) {
  grid_tibble <- update_states(grid_tibble, i)
  print(i)
}


answer2 <- sum(as.numeric(matrix(grid_tibble$state, nrow = row_length)[,142]))



