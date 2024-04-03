library(vote)
library(tidyverse)
source("clean_raw_results.R")

# Get cleaned results
all.results <- clean_raw_results()

# `vote::rcv()` wants RCV results to be Voter x Candidate where each 
# row is a choice
results.pivoted <- all.results |>
  pivot_longer(-id) |>
  filter(!is.na(value)) |>
  # Create columns for race name and choice (rank)
  mutate(choice = str_sub(name, start = -1),
         name = str_sub(name, start = 1, end = -3))

rcv_results <- function(every.result = results.pivoted, position = "large_senator", seats = 4){
  # Get results for an individual race
  # every.result (tbl): long df of all results with columns name, choice, id
  # position (str): name of position
  # seats (int): number of seats elected in race
  # -> rcv results (tbl)
  
  every.result |>
    # Filter just the position and drop column 
    filter(name == position) |>
    select(-name) |>
    
    # Remove rows with missing choice and coerce to int
    filter(!is.na(choice)) |>
    mutate(choice = as.integer(choice)) |>
    
    # Pivot to form required by `vote::stv()` and drop id col
    pivot_wider(id_cols = id, names_from = value, values_from = choice) |>
    select(-id)  |>
    vote::stv(nseats = seats, 
              invalid.partial = TRUE,  
              complete.ranking = FALSE,
              quiet = T)
}

