library(tidyverse)

extract_columns <- function(name) {
  # Clean column names from RCV VikingCentral Election
  # name (str): a column name containing a senate position and
  #             rank number, along with some other stuff
  # -> column name (str): position and rank like "first_year_senator_1"
  
  
  # If it's the ID column, rename to ID
  if (name == "SubmissionId") {
    return("id")
  }
  
  # Extract RCV rank
  rank <- name |>
    str_extract(pattern = "[- 0-9]{1}$")
  
  # Extract senate name
  name <- name |>
    
    # Clean string bc I'm bad at regex
    tolower() |>
    str_remove("-") |>
    str_remove(" year") |>
    str_replace_all(" ", "_") |>
    
    # Match "word senator"
    str_extract(pattern = "[a-z]*[_]{0,1}(?:senator)")
  
  # Return "position_rank"
  return(paste0(name, "_", rank))
}


clean_raw_results <-function(path.to.raw.results = "./data/SP_24_ElectionResults.csv", ignore.exec = T) {
  # Clean ingest raw data RCV VikingCentral Election
  # path.to.raw.results (str): a path leading to raw VikingCentral results
  # -> all.results (tbl): a tibble of results with cleaned names
    all.results <- read_csv(path.to.raw.results, skip = 1, show_col_types = FALSE) 
    
    
    if(ignore.exec)
    all.results <- all.results |>
      # Remove rows for President/Secretary
      select(!c(contains("Pres"), contains("Secr")))
    
    # Set column names to extract.columns() on each name
    colnames(all.results) <-
      map(colnames(all.results), extract_columns)
    
    # Get list of positions in election
    positions <- colnames(all.results |> select(-id)) |>
      # Remove rank and get unique positions
      str_remove_all("[_][0-9]{1}") |>
      unique()
    
    return(all.results)
}
