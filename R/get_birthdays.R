library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(httr)
library(janitor)
library(glue)

get_birthday_players <- function(month,day){

  cli::cli_alert("Month: {month} day: {day}")
  raw_birthday_table <- read_html(
    glue("https://www.pro-football-reference.com/friv/birthdays.cgi?month={month}&day={day}")) |>
    html_element("#birthdays") |>
    html_table(header = FALSE)

  table_head <- raw_birthday_table |>
    slice(1:2) |>
    t() |>
    apply(1,FUN = paste,collapse = " ") |>
    janitor::make_clean_names()

  birthday_table <- raw_birthday_table |>
    slice(-1,-2) |>
    set_names(table_head) |>
    select(-rk) |>
    arrange(desc(w_av)) |>
    mutate(across(c(-player,-pos), as.numeric))

  return(birthday_table)
}

get_birthday_players(4,20)

x <- tibble(
  date = seq.Date(as.Date("2000-01-01"),as.Date("2000-12-31"),by = "day")
) |>
  mutate(
    month = lubridate::month(date),
    day = lubridate::day(date),
    players = map2(month,day,get_birthday_players)
  ) |>
  select(-date) |>
  unnest(players) |>
  arrange(month,day,-w_av)

saveRDS(x,"data/birthdays.rds")




