### eco decisions
library(tidyverse)

data <- read_csv(here::here("economy.csv"))
head(data)

## reshape data into long form

## everything for team 1
data %>% 
  select(date, match_id, event_id, team = team_1, map = `_map`, start = t1_start,
         ends_with("_t1")) %>% 
  pivot_longer(ends_with("_t1"),
               values_to = "spent",
               names_to = "round",
               names_pattern = "(.*)_t") %>% 
  mutate(round = parse_number(round),
         side = case_when(
           start == 't' & round <16 ~ 't',
           start == 't' & round >=16 ~ 'ct',
           start == 'ct' & round <16 ~ 'ct',
           start == 'ct' & round >=16 ~ 't',
           TRUE ~ NA_character_
  )) %>% 
  left_join(
    data %>% 
      select(match_id, event_id, team = team_1, ends_with("_t2"), map = `_map`) %>% 
      pivot_longer(ends_with("_t2"),
                   values_to = "opp_spent",
                   names_to = "round",
                   names_pattern = "(.*)_t") %>% 
      mutate(round = parse_number(round))
  ) %>% 
  left_join(
    data %>% 
      select(match_id, event_id,team =  team_1,map = `_map`, ends_with("_winner")) %>% 
      pivot_longer(ends_with("_winner"),
                   values_to = "win",
                   names_to = "round",
                   names_pattern = "(.*)_winner") %>% 
      mutate(win = ifelse(win == 1, 1, 0),
             round = parse_number(round))
  ) ->
  team_1_data

## everything for team 2
data %>% 
  select(date, match_id, event_id, team = team_2, map = `_map`, start = t2_start,
         ends_with("_t2")) %>% 
  pivot_longer(ends_with("_t2"),
               values_to = "spent",
               names_to = "round",
               names_pattern = "(.*)_t") %>% 
  mutate(round = parse_number(round),
         side = case_when(
           start == 't' & round <16 ~ 't',
           start == 't' & round >=16 ~ 'ct',
           start == 'ct' & round <16 ~ 'ct',
           start == 'ct' & round >=16 ~ 't',
           TRUE ~ NA_character_
         )) %>% 
  left_join(
    data %>% 
      select(match_id, event_id, team = team_2, ends_with("_t1"), map = `_map`) %>% 
      pivot_longer(ends_with("_t1"),
                   values_to = "opp_spent",
                   names_to = "round",
                   names_pattern = "(.*)_t") %>% 
      mutate(round = parse_number(round))
  ) %>% 
  left_join(
    data %>% 
      select(match_id, event_id,team =  team_2,map = `_map`, ends_with("_winner")) %>% 
      pivot_longer(ends_with("_winner"),
                   values_to = "win",
                   names_to = "round",
                   names_pattern = "(.*)_winner") %>% 
      mutate(win = ifelse(win == 2, 1, 0),
             round = parse_number(round))
  ) ->
  team_2_data

full_data <- team_1_data %>% 
  bind_rows(team_2_data)

## need at least three things to predict available cash
## loss streak, win streak, min money for those things
ff = function(x)
{
  cs = cumsum(x)
  cs - cummax((x == 0) * cs)
}

full_data %>% 
  filter(!is.na(spent)) %>% 
  group_by(match_id, event_id, map, team, side) %>% 
  mutate(loss_score = ifelse(cumsum(ifelse(win == 0, 1, -1))),
         loss_streak = cumsum(win),
         win_streak = cumsum(win == 1)) %>% 
  group_by(loss_streak, .add = T) %>% 
  mutate(current_loss_streak = cumsum(win==0)) %>% 
  group_by(match_id, event_id, map, team, side, win_streak) %>% 
  mutate(current_win_streak = cumsum(win)) %>% 
  ungroup() %>% 
  slice(1:100) %>% view

## need to see how many rounds in the future the spend is correlated
## could look at buy diff and how it corrs, probably better
.temp <- tibble()

for(i in 1:8) {
  full_data %>% 
    filter(!is.na(spent)) %>% 
    group_by(match_id, event_id, map, team, side) %>% 
    mutate(spend_diff = spent - opp_spent,
           next_diff = lead(spent,i)) %>% 
    filter(!is.na(next_diff), spend_diff < 0 ) %>% 
    ungroup() %>% 
    sample_n(10000) %>% 
    summarise(m = cor(spent, next_diff)) %>% 
    bind_rows(.temp) ->
    .temp
}
.temp %>% 
  mutate(rounds = 8:1) %>% 
  ggplot(aes(rounds, m))+
  geom_line()
  

full_data %>% 
  group_by(match_id, event_id, map, team) %>% 
  mutate(to_win = 16 - (cumsum(lag(win, default = 0))),
         opp_to_win = 16 - (cumsum(lag(win == 0, default = 0)))) %>% 
  group_by(side, .add = T) %>% 
  mutate(next_two = win + lead(win),
         next_round_money = lead(spent)) %>% 
  mutate(opp_buy = case_when(
    opp_spent > 20000 ~ "full",
    opp_spent > 10000 ~ "semibuy",
    opp_spent > 5000 ~ "semieco",
    TRUE ~ "eco"
  ),
  buy = case_when(
    spent > 20000 ~ "full",
    spent > 10000 ~ "semibuy",
    spent > 5000 ~ "semieco",
    TRUE ~ "eco"
  ))->
  clean_data

