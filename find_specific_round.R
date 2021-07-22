### eco decisions
library(tidyverse)

# source("D:/hltvxDK/get_eco_data.R")

## group buys into categories by situation and result
clean_data %>% 
  filter(!round %in% c(1,16)) ->
  no_pistols

## win 

## How much money does a team need to buy again in next round?

clean_data %>% 
  mutate(side_wins = lag(cumsum(win),default = 0)/(1:n()),
         side_round = 1:n(),
         streak = lag(cumsum(win == 0), default = 0)) %>% 
  group_by(streak, .add = T) %>% 
  mutate(win_streak_length = n()) %>% 
  filter(spent > 20000, win == 0) %>% 
  mutate(rebought = next_round_money > 20000) %>% 
  group_by(side_round, side_wins, win_streak_length) %>% 
  summarise(rebought = mean(rebought,na.rm = T),
            n = n()) %>% 
  mutate(rebought = ifelse(n < 200, side_wins, rebought)) ->
  will_they_rebuy

## find rounds that match a set of parameters
spent_ = 14000
opp_buy_ = 'full'


# same economic situation
clean_data %>% 
  filter(map == "Dust2", side == "t") %>% 
  mutate(side_wins = lag(cumsum(win),default = 0),
         side_round = 1:n()) %>% 
  # group_by(streak, .add = T) %>% 
  # mutate(win_streak_length = n()) %>% 
  mutate(next_three = win+lead(win)+lead(win,2)) %>% 
  filter(!round %in% c(1,16)) %>% # no pistols
  filter(between(spent, 0, spent_ + 2000), #max spend
         between(opp_spent, 25000, 30000),
         side_round == 4,
         side_wins == 1,
         # opp_buy == opp_buy_, #same opp buy
         to_win > 2, # not last round of game
         opp_to_win > 2) %>% 
  mutate(spent_bins = cut(spent, breaks = c(0,5000,20000))) %>%
  filter(win == 0, between(spent, 10000,15000)) %>% 
  # group_by(spent_bins) %>% 
  # summarise(win = mean(win, na.rm = T),
  #           two_wins = mean(next_two, na.rm = T),
  #           three_wins = mean(next_three, na.rm = T),
  #           n = n()) %>% 
  view()
  pivot_longer(c(win, two_wins, three_wins)) %>% 
  group_by(spent_bins) %>% 
  mutate(x = 1:n()) %>% 
  filter(!is.na(spent_bins)) %>% 
  ggplot(aes(x, value, size = n,color = spent_bins))+
  geom_point()

## lets look at how many rounds until wins even out after an eco
clean_data %>% 
  mutate(next_round_win =  lead(win),
         two_rounds_ahead = lead(win, 2),
         three_rounds_ahead = lead(win, 3),
         four_rounds_ahead =lead(win, 4)) %>% 
  mutate(spent_bins = cut(spent, breaks = seq(100, 30000, 400))) %>%
  group_by(spent_bins) %>% 
  summarise(this = mean(win, na.rm = T),
            one = mean(next_round_win,na.rm = T),
            two = mean(two_rounds_ahead,na.rm = T),
            three = mean(three_rounds_ahead,na.rm = T),
            four = mean(four_rounds_ahead,na.rm = T)) ->
  bin_wins

bin_wins %>% 
  mutate(wins)