# L02-transformations ----
# Stat 301-1

# load packages ----
library(tidyverse)
library(nycflights13)
library(skimr)
library(styler)
library(knitr)

# load data ----
college_rankings <- read_csv("data/college_rankings.csv")
college_rankings_codebook <- read_csv("data/college_rankings_codebook.csv")
data("flights")
tinder_data <- read_csv("data/tinder_data.csv")

# NYC flights in class ----
# data quality check
flights |> 
  naniar::miss_var_summary()

#  flights arriving from ohare and midway, calc mean and std dev
# departure and arrival times by dest, arranged w info largest dept time at top

# Exercises ----

# Ex 1----

# a. Had a tuition greater than $50,000
college_rankings |> 
  filter(tuition_fees >= 50000)

# b. Are located in Illinois and ranked in the top 50
college_rankings |> 
  filter(state_abbr == "IL" & rank_2023 <= 50)

# c. have a median ACT score greater than 32 or mean SAT score greater than 1430
college_rankings |> 
  filter(act_median > 32 | sat_avg > 1430)

# d. have a median ACT score greater than 32 or mean SAT score greater than 1430 AND are ranked in the top 50
college_rankings |> 
  filter(act_median > 32 & rank_2023 <= 50 | sat_avg > 1430 & rank_2023 <= 50)

# EX 2 ----
# or operator
college_rankings |> 
  filter(rank_2023 == 2 | rank_2023 == 3 | rank_2023 == 4)

# %in% 
college_rankings |> 
  filter(rank_2023 %in% c(2, 3, 4))

# between() 
college_rankings |> 
  filter(between(rank_2023, 2, 4))

# EX 3 ----
# use distinct to see if all states + DC are represented
college_rankings |> 
  distinct(state_abbr)

# EX 4 ----

# Arrange the data to find the most expensive colleges
# Use the slice function to select the 5 most expensive colleges
college_rankings |> 
  arrange(desc(tuition_fees)) |> 
  slice_head(n = 5)

# EX 5 ----

college_rankings |> 
  mutate(
    avg_financial_aid = tuition_fees + room_board - avg_net_price) |> 
    select(institution, tuition_fees, room_board, avg_financial_aid)
    # |>
     # filter(avg_financial_aid >= 0)

# EX 6 ----
# what happens when call same thing twice in select() call?
college_rankings |> 
  select(institution, rank_2023, institution)

# nothing?

# EX 7 ----

# list of variables
variables <- c("institution", "state", "rank_2023", "overall_score")

college_rankings |> 
  select(any_of(variables))

# EX 8 ----

college_rankings |> 
  select("institution", contains("score"))

# EX 9 ----

college_rankings

college_rankings |> 
  relocate(rank_2023, .after = institution) |>
    rename(rank = rank_2023) 

# EX 10 ----

#  public vs. private: who has higher tuition, overall score, acceptance rate,
# and undergraduate enrollment?

private_college_rankings <- 
  college_rankings |> 
    filter(public == "private")

public_college_rankings <- 
  college_rankings |> 
  filter(public == "public")

skim_without_charts(private_college_rankings)

# average tuition: 42060
# overall score: 54.9
# acceptance rate: 0.659
# undergraduate enrollment: 3345
# total schools: 454

skim_without_charts(public_college_rankings)

# average tuition: 23779
# overall score: 47.5
# acceptance rate: 0.776
# undergraduate enrollment: 14494
# total schools: 336


# EX 11 ----
# part a

# puesdo code
# data |> 
#   summarize(
#     mean = mean(x),
#     mean = mean(y),
#     mean = mean(z)
#   )

# doesn't work because uses the word "mean" to describe calcualted average value for x, y and z
# code can't tell it apart

# actual code
# data |> 
#   summarize(
#     x_mean = mean(x),
#     y_mean = mean(y),
#     z_mean = mean(z)
#   )

# part b

# data |> 
#   group_by(c) |> 
#   summarize(mean(x)) |> 
#   arrange(desc(x))

# this code doesn't work because it does not assign the mean value of x 
# to anything within summarize. It also does not work because the arrange(desc(x)) 
# is trying to have the mean values arranged in descending order of the x values, 
# not the average of the x values

#  actual code
# data |> 
#   group_by(c) |> 
#   summarize(x_mean = mean(x)) |> 
#   arrange(desc(x_mean))

# this code calcualtes mean value of x (x_mean) for each group c and arranges 
# them in descending order based on the value of x_mean

# EX 12 ----

college_rankings |>
  group_by(state_abbr) |> # group by state
  count() |> # count number of colleges per state
  arrange(desc(n)) # arrange in descending order


# Case Study ----

# objective 1

# determine the distribution of the percent of one message conversations
 
# step 1: remove no conversation users

tinder_data_clean <-
  tinder_data |> 
  filter(longest_convo_msgs != 0)

# step 2: create a new variable to show percent of user conversations that
# were 1 message long
tinder_data_clean <-
  tinder_data_clean |> 
    mutate(
      pct_one_msg = number_one_msg_conversations / number_conversations
    )

# step 3: summary stats

skim(tinder_data_clean$pct_one_msg)

# step 4: graph

# histogram
one_msg_histogram <-
  ggplot(data = tinder_data_clean, aes(x = pct_one_msg)) +
    geom_histogram(color = "white") +
    labs(
      title = "Distribution of Percent of 1 Message Conversations for 1,176 Tinder Users",
      x = "Percent of Conversations Were 1 Message Long",
      y = "Count",
      caption = "Source: Swipestats.io"
    )

ggsave(
  filename = "plots/one_msg_histogram.png",
  plot = one_msg_histogram,
  width = 10,
  units = "in"
)

# boxplot
one_msg_boxplot <-
  ggplot(data = tinder_data_clean, aes(x = pct_one_msg)) +
    geom_boxplot() +
    labs(
      title = "Distribution of Percent of 1 Message Conversations for 1,176 Tinder Users",
      x = "Percent of Conversations Were 1 Message Long",
      caption = "Source: Swipestats.io"
    )

ggsave(
  filename = "plots/one_msg_boxplot.png",
  plot = one_msg_boxplot,
  width = 10,
  units = "in"
)

# objective 2

# Arrange the data to see the users with the most matches and output the top 5 users

tinder_data_clean <- 
  tinder_data_clean |> 
  filter(matches != 0) |> 
  mutate(
    ratio_matches_to_likes = matches / swipes_like
  )

top_matches <-
  tinder_data_clean |> 
  arrange(desc(matches)) |> 
  slice_head(n = 5)

top_matches <- 
  top_matches |> 
  select(user_id, user_gender, matches:swipes_pass, ratio_matches_to_likes)

top_matches <- janitor::clean_names(top_matches, "title")

kable(top_matches)

# Normalized

top_matches_normalized <-
  tinder_data_clean |> 
  arrange(desc(matches_to_likes)) |> 
  slice_head(n =5)

top_matches_normalized <- 
  top_matches_normalized |> 
  select(user_id, user_gender, matches:swipes_pass, matches_to_likes)

top_matches_normalized <- janitor::clean_names(top_matches_normalized, "title")

kable(top_matches_normalized)




# - Compare your two results and comment on why your method is a "best" metric to quantify "popularity/likability".

