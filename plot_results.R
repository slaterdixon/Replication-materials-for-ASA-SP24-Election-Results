### Functions for plotting RCV results


library(ggtext)
library(ggthemes)
library(kableExtra)

get_position_name <- function(cleaned.position.name){
  # Lookup vector for position names for plot title
  positions.names <- c(
    "International Senator" = "international_senator",
    "Fourth Year Senator" = "fourth_senator",
    "Third Year Senator" = "third_senator",
    "Second Year Senator" = "second_senator",
    "At Large Senator" = "large_senator"
  )
  
  return(names(positions.names)[which(positions.names == cleaned.position.name)])
}

get_results_plot <- function(rcv.results.to.plot, position = "large_senator", seats = 4){
  # Tabulate results for each individual race
  # every.result (tbl): long df of all results to pass to pivoted.results()
  # position (str): name of position
  # -> output (csv): output of vote::RCV() giving round results and winners
  # -> results_plot (png): bar graph showing actions for each round
  
  # Get cutoffs for each round
  cutoffs <- tibble(cutoff = rcv.results.to.plot$quotas) |>
    rowid_to_column(var = "stage")
  
  # Create table of results and join to cutoffs 
  stages <- rcv.results.to.plot$preferences |> 
    as_tibble() |>
    rowid_to_column(var = "stage") |>
    pivot_longer(-stage) |>
    left_join(cutoffs) |>
    mutate(vote_delta = value - cutoff) 
  
  # Get number of IR rounds for plot title
  no.rounds <- stages |> distinct(stage) |> nrow()
  # Get number of candidates for plot title
  no.candidates <- rcv.results.to.plot$preferences |> colnames() |> length()
  
  # Elected candidates for each round are those candidates whose vote count is largest
  # compared to the cutoff 
  elected <- stages |>
    group_by(stage) |>
    filter(vote_delta == max(vote_delta)) |>
    filter(value > cutoff) |>
    select(stage, name) |>
    mutate(action = "Elected")
  
  # Eliminated candidates for each round are those whose vote count is smallest
  # compared to the cutofff
  eliminated <- stages |>
    group_by(stage) |>
    filter(vote_delta == min(vote_delta)) |>
    select(stage, name) |>
    mutate(action = "Eliminated") 
  
  
  
  incumbents <- read_csv("./data/senators23-24.csv", show_col_types = FALSE) |> pull(Name)
  
  
  # Plot the results
  stages <- stages |>
    
    # Join with elected and eliminated tables and create color columns
    left_join(elected) |>
    left_join(eliminated, by = c("stage", "name")) |>
    
    # Coalesce eliminated and elected actions
    mutate(color = coalesce(action.x, action.y)) |>
    # For some candidates nothing happened in a particular round...
    # ...action is "None" for those candidates 
    mutate(color = replace_na(color, "None"))  |>
    
    # Get the last name of each candidate
    mutate(name = str_replace(name, "Suyash SR", "Suyash-SR")) |>
    mutate(name.label = str_split_i(name, pattern = " ", i = 2)) |>
    
    # For candidates who won, paste markdown bold tags around name
    mutate(name.label = case_when(
      name %in% elected$name ~ paste0("**", name.label, "**"),
      .default = name.label
    )) |>
    # For candidates who are incumbents, paste asterick by name
    mutate(name.label = case_when(
      (name %in% incumbents) ~ paste0(name.label, "&ast;"),
      .default = name.label
    )) 
  
  # Make subtitle string
  # "x senators elected from y candidates over z instant runoff rounds"
  elaborate.subtitle <- paste0(seats, 
                               " senators elected from ", 
                               no.candidates, " candidates over ", 
                               no.rounds, " instant runoff rounds")
  
  stages.plot <- stages |>
    
    # Make bar plot for each round
    ggplot(aes(x = name.label, y = value)) +
    geom_bar(stat = "identity", color = "black", linewidth = .25, aes(fill = as.factor(color))) +
    
    # Custom colors for each bar
    scale_fill_manual(values = c(Eliminated = "#E54E21", Elected = "#6C8645", None = "#273046")) +
    # Create individual grids for each round
    facet_wrap(vars(stage)) +
    # Add cutoffs for each round
    geom_abline(data = cutoffs, aes(intercept = cutoff, slope = 0)) + 
    # Put names on Y axis and Votes on X for legibility
    coord_flip() +
    
    labs(
      x = "Name",
      y = "Votes",
      fill = "Action",
      title = get_position_name(position),
      subtitle = elaborate.subtitle,
      caption = "*&ast;Incumbent*"
    ) +
    theme_tufte() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.y = ggtext::element_markdown(lineheight = .25),
          plot.caption = ggtext::element_markdown(hjust = 0),
          legend.position = "bottom",
          legend.key.size = unit(.5, "cm"), 
          strip.text.x = element_text(
            size = 10, color = "#404040", face = "italic"
          )) 
  
  return(stages.plot)
  
}

get_results_table <- function(results.df){
  ### Table of winner results in order of when they won 
  
  tibble(Candidate = results.df$elected) |>
    rowid_to_column("Elected") |>
    kbl(booktabs = T) |>
    kable_styling(position = "center", latex_options = "HOLD_position")
}

rank_distribution_plot <- function(results.df, position = "large_senator", bold.winners = T){
  ### A distribution plot showing how many of each RCV choice a candidate got
  
  results.df$data |>
    as_tibble() |>
    pivot_longer(everything()) |>
    # If we're bolding winners' names, paste markdown bold tags around name
    mutate(name = if_else((name %in% results.df$elected) & bold.winners, paste0("**", name, "**"), name)) |>
    filter(value != 0) |>
    
    # Make the choice distribution plot
    ggplot() +
    geom_bar(aes(x = value, fill = name), color = "black", linewidth = .24) +
    facet_wrap(vars(name), strip.position="bottom") +
    theme_tufte() +
    scale_fill_manual(values =  c("#D87CAC", "#009FB7", "#e54e21ff", "#6c8645ff", "#273046ff", "#ffbd00ff", "#9e0059ff")) +
    # Plot every choice on the x axis, no skips
    scale_x_continuous(breaks = seq.int(1, length(colnames(results.df$data)), by = 1)) +
    labs(
      title = get_position_name(position),
      subtitle = "Choice distribution for each candidate",
      x = "Rank",
      y = "Count",
      fill = ""
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.y = element_markdown(lineheight = .25),
          plot.caption = element_markdown(),
          strip.text.x = element_markdown(),
          legend.position = "none")
}


