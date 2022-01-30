library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(forcats)

player_data <- readRDS("data/birthdays.rds") |> ungroup()

birthdays <- distinct(player_data, month,day)

quarterbacks <- player_data |>
  filter(str_detect(pos,"QB"))

skill_pos <- player_data |>
  filter(str_detect(pos,"RB|HB|FB|TB|WR|TE|DB|S")) |>
  arrange(-w_av)

not_skill_pos <- player_data |>
  anti_join(quarterbacks, by = c("player","born","pos","month","day")) |>
  anti_join(skill_pos, by = c("player","born","pos","month","day")) |>
  arrange(-w_av)

#' Seven positions

#' Quarterbacks: best quarterback by career AV, with a 20% AV bonus for having a good career rushing yards total

#' One RB: best RB by career AV. Non-RB skill players should be downgraded by 20% AV.

#' Two WRs: best non-RB by career AV - RBs should be downgraded by 20% AV

#' Three not-skill-pos, sorted by AV.

score_qb <-quarterbacks |>
  select(month,day,player,pos,from,to,w_av,rushing_att, rushing_yds, rushing_td, g) |>
  mutate(
    rush_ypg = rushing_yds / g,
    qb_score = w_av * case_when(rush_ypg < 20 ~ 1,
                                rush_ypg < 30 ~ 1.5,
                                rush_ypg < 40 ~ 2,
                                rush_ypg < 50 ~ 3)) |>
  select(-contains("rush"))

score_skill <- skill_pos |>
  anti_join(score_qb) |>
  select(month,day,player,pos,born,from,to,w_av) |>
  mutate(qb_score = w_av * ifelse(str_detect(pos,"RB|HB|FB|TB"),0.25,0))

select_qb <- bind_rows(score_qb,score_skill) |>
  group_by(month,day) |>
  slice_max(qb_score,n = 1,with_ties = FALSE) |>
  ungroup() |>
  mutate(select = "QB")

select_skill <- score_skill |>
  anti_join(select_qb, by = c("player","pos","born")) |>
  group_by(month,day) |>
  slice_max(w_av, n = 3, with_ties = FALSE) |>
  ungroup() |>
  mutate(select = "skill")

select_nonskill <- not_skill_pos |>
  select(month,day,player,pos,born,from,to,w_av) |>
  group_by(month,day) |>
  slice_max(w_av,n=3,with_ties = FALSE) |>
  ungroup() |>
  mutate(select = "nonskill")


player_summary <- bind_rows(select_qb,select_skill, select_nonskill) |>
  mutate(
    select = forcats::fct_relevel(select,"QB","skill","nonskill"),
    label = glue::glue("{player} - {pos} ({from}-{to})")
  ) |>
  arrange(month,day,select) |>
  group_by(month,day) |>
  summarise(
    qb = label[as.logical(select=="QB")],
    qb_score = sum(as.numeric(select == "QB") * qb_score, na.rm = TRUE),
    skill = label[as.logical(select == "skill")] |> paste(collapse = "<br>"),
    skill_score = sum(as.numeric(select == "skill") * w_av, na.rm = TRUE),
    nonskill = label[as.logical(select == "nonskill")] |> paste(collapse = "<br>"),
    nonskill_score = sum(as.numeric(select == "nonskill") * w_av, na.rm = TRUE) * 0.75,
    score = qb_score + skill_score + nonskill_score
  ) |>
  ungroup() |>
  arrange(-score)

player_summary |>
  reactable::reactable(defaultColDef = reactable::colDef(html = TRUE),resizable = TRUE,sortable = TRUE)

