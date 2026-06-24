# Initial peek at the data

# load libraries
library(tidyverse)

# load data
games <- read_csv("data/games.csv")
genre_sum <- read_csv("data/genre_summary.csv")
plat_sum <- read_csv("data/platform_summary.csv")
pub_sum <- read_csv("data/publisher_summary.csv")
vg_sales <- read_csv("data/video_games_sales.csv")
y_trends <- read_csv("data/yearly_trends.csv")

