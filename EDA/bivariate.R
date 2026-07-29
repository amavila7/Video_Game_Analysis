# Bivariate


# load libraries
library(tidyverse)
library(ggthemes)
library(showtext)
library(here)

# free font from google thats similar to Akkurate Pro
font_add_google("Noto Serif")


# showtext auto and add font
showtext_auto()

# load data
games_cleaned <- read_csv("data/games.csv") |>
  mutate(
    platform = as.factor(platform),
    platform_type = as.factor(platform_type),
    platform_maker = as.factor(platform_maker),
    platform_generation = as.factor(platform_generation),
    genre = as.factor(genre),
    publisher_region = as.factor(publisher_region),
    publisher_tier = as.factor(publisher_tier),
    esrb_rating = as.factor(esrb_rating),
    launch_price_usd = as.factor(launch_price_usd),
    is_sequel = as.factor(is_sequel),
    is_sequel = fct_recode(is_sequel, "Yes" = "1", "No" = "0"),
    online_multiplayer = factor(online_multiplayer),
    online_multiplayer = fct_recode(online_multiplayer, "Yes" = "1", "No" = "0"),
    dlc_released = as.factor(dlc_released),
    dlc_released = fct_recode(dlc_released, "Yes" = "1", "No" = "0"),
    microtransactions = as.factor(microtransactions),
    microtransactions = fct_recode(microtransactions, "Yes" = "1", "No" = "0"),
    loot_boxes = as.factor(loot_boxes),
    loot_boxes = fct_recode(loot_boxes, "Yes" = "1", "No" = "0"),
    game_pass_available = as.factor(game_pass_available),
    game_pass_available = fct_recode(game_pass_available, "Yes" = "1", "No" = "0"),
    vr_support = as.factor(vr_support),
    vr_support = fct_recode(vr_support, "Yes" = "1", "No" = "0"),
    goty_nominated = as.factor(goty_nominated),
    goty_nominated = fct_recode(goty_nominated, "Yes" = "1", "No" = "0"),
    goty_won = as.factor(goty_won),
    goty_won = fct_recode(goty_won, "Yes" = "1", "No" = "0"),
  )

# Genre vs Year

y_vs_genre <- games_cleaned |>
  ggplot(aes(x = year, y = genre)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Year Released vs Genre",
    x = "Year",
    y = "Genre"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )



# Platform vs Year
plat_vs_year <- games_cleaned |>
  ggplot(aes(x = year, y = platform)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Year Released vs Platform Played",
    x = "Year",
    y = "Platform"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# shows the lifetimes of all the platforms 
# obvi the newer platforms have more popularity more recently 
# PC has been going strong since the start


   
# Do certain genres usually sell more than others? ----


# global sales
gsales_vs_genre <- games_cleaned |>
  ggplot(aes(x = global_sales_million, y = genre)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Global Sales vs Genre",
    x = "Global Sales\n(millions)",
    y = "Genre"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# highest sales
# Sports, Shooter, Sandbox, Fighting, & Action have the greatest sales globally

# lowest sales
# visual novel, strategy, rhythm, & Idle/clicker


games_cleaned |>
  ggplot(aes(x = global_sales_million, fill = launch_price_usd)) +
  geom_histogram() +
  geom_rug()

# launch price vs genre
lp_vs_genre <- games_cleaned |>
  ggplot(aes(x = launch_price_usd, y = genre)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Launched Price vs Genre",
    x = "Launch Price \n(USD)",
    y = "Genre",
    fill = "Platform Type"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# games_cleaned <- games_cleaned |>
#   mutate(launch_price_usd = as.factor(launch_price_usd))

# launch price vs genre
genre_w_lp <- games_cleaned |>
  ggplot(aes(fill = launch_price_usd, y = genre)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Genre with Launched Price",
    x = "",
    y = "Genre",
    fill = "Launch Price \n(USD)"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# the spread per genre is quite evenly centered around $40
# survival horror, sports, simulation, shooter, puzzle, MMORPG, and Idle/clicker 
# are around $50
# the entire range is $0-$80 without any visual outliers

# No genre is cheaper than the other
# all genres have a large range of prices
# there are fewer video games for different genres
# action, sports, shooter, and role-playing are the most popular


# launch price vs global sales
games_cleaned |>
  ggplot(aes(x = global_sales_million, y = launch_price_usd)) +
  geom_point()

# launch price seems to be more categorical 
# the best sales are the free games or games under $10
# there is an increase of global sales for games sold at $50+


# boxplot - interesting how it changes to more exact pricing
lp_vs_global_sales <- games_cleaned |>
  ggplot(aes(x = global_sales_million, y = launch_price_usd)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Genre with Launched Price",
    x = "Global Sales \n(Millions USD)",
    y = "Launch Price \n(USD)"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# adding all 3
games_cleaned |>
  ggplot() +
  geom_violin(aes(x = global_sales_million, y = genre)) +
  geom_boxplot(aes(x = global_sales_million, y = genre, fill = launch_price_usd))



# Do multiplayer games increase sales? ----
   
gs_vs_multi <- games_cleaned |>
  ggplot(aes(x = global_sales_million, y = online_multiplayer)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Global Sales \nvs Mutlitplayer",
    x = "Global Sales \n(Millions USD)",
    y = "Multiplayer"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) +
  scale_x_continuous(limits = c(0, 500))



nas_vs_multi <- games_cleaned |>
  ggplot(aes(x = na_sales_million, y = online_multiplayer)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in North America \nvs Mutlitplayer",
    x = "Sales in North America \n(Millions USD)",
    y = "Multiplayer"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )


jps_vs_multi <- games_cleaned |>
  ggplot(aes(x = jp_sales_million, y = online_multiplayer)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in Japan \nvs Mutlitplayer",
    x = "Sales in Japan \n(Millions USD)",
    y = "Multiplayer"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 

eus_vs_multi <- games_cleaned |>
  ggplot(aes(x = eu_sales_million, y = online_multiplayer)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in Europe \nvs Mutlitplayer",
    x = "Sales in Europe \n(Millions USD)",
    y = "Multiplayer"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 

others_vs_multi <- games_cleaned |>
  ggplot(aes(x = other_sales_million, y = online_multiplayer)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Other Sales \nvs Mutlitplayer",
    caption = "Other sales for video games around the world not in North America, Europe, or Japan",
    x = "Other Sales \n(Millions USD)",
    y = "Multiplayer"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 

# even distribution of online multiplayer games throughout the time period
games_cleaned |>
  ggplot(aes(x = year, y = online_multiplayer)) +
  geom_boxplot()


# How do microtransactions in a video game impact sales and launch price?----

# launched price vs microtransactions
lp_w_micro <- games_cleaned |>
  ggplot(aes(y = launch_price_usd, fill = microtransactions)) +
  geom_bar(alpha = 0.7) +
  theme_minimal() +
  scale_fill_discrete(palette = c("green", "darkgreen")) +
  labs(
    title = "Distribution of Launch Price with Microtransactions",
    x = "Count",
    y = "Launch Price \n(USD)",
    fill = "Microtransactions"
  ) +
  theme(
    legend.title = element_text(family = "Noto Serif"),
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 

# majority of the games don't have microtransactions
# some free games do have microtransactions which is ironic since the game 
# itself was free

# global sales vs micro
gs_vs_micro <- games_cleaned |>
  ggplot(aes(x = global_sales_million, y = microtransactions)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Global Sales \nvs Microtransactions",
    x = "Global Sales \n(Millions USD)",
    y = "Microtransactions"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) +
  scale_x_continuous(limits = c(0, 500))


# eu sales vs micro
eus_vs_micro <- games_cleaned |>
  ggplot(aes(x = eu_sales_million, y = microtransactions)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in Europe \nvs Microtransactions",
    x = "Sales in Europe \n(Millions USD)",
    y = "Microtransactions"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 


# jp sales vs micro
jps_vs_micro <- games_cleaned |>
  ggplot(aes(x = jp_sales_million, y = microtransactions)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in Japan \nvs Microtransactions",
    x = "Sales in Japan \n(Millions USD)",
    y = "Microtransactions"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  ) 

# na sales vs micro
nas_vs_micro <- games_cleaned |>
  ggplot(aes(x = na_sales_million, y = microtransactions)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Sales in North America \nvs Microtransactions",
    x = "Sales in North America \n(Millions USD)",
    y = "Microtransactions"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )



## Other finds

y_vs_pt <- games_cleaned |>
  ggplot(aes(x = year, y = platform_type)) +
  geom_boxplot(aes(color = platform_type, fill = platform_type), alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Distribution of Year Released vs Platform Type",
    x = "Year",
    y = "Platform Type"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel",
    legend.position = "none"
  )


y_vs_pubtier <- games_cleaned |>
  ggplot(aes(x = year, y = publisher_tier)) +
  geom_boxplot(aes(color = publisher_tier, fill = publisher_tier), alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Distribution of Year Released vs Publisher Tier",
    x = "Year",
    y = "Publisher Tier"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel",
    legend.position = "none"
  )


# not all genres have games with the various ratings
# majority for all genres is T followed by E & M

esrb_vs_g <- games_cleaned |>
  ggplot(aes(y = genre, fill = esrb_rating)) +
  geom_bar(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Distribution of Genre vs ESRB Rating",
    x = "Number of Video Games",
    y = "Genre",
    fill = "ESRB Rating"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )


lp_vs_esrb <- games_cleaned |>
  ggplot(aes(y = launch_price_usd, x = esrb_rating)) +
  geom_boxplot(aes(color = esrb_rating, fill = esrb_rating), alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Distribution of Launch Price vs ESRB Rating",
    x = "ESRB Rating",
    y = "Launch Price",
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel",
    legend.position = "none"
  )


# Saving out all the figures ----

ggsave(here("figures/y_vs_genre.png"), plot = y_vs_genre)
ggsave(here("figures/plat_vs_year.png"), plot = plat_vs_year)
ggsave(here("figures/gsales_vs_genre.png"), plot = gsales_vs_genre)
ggsave(here("figures/lp_vs_genre.png"), plot = lp_vs_genre)
ggsave(here("figures/genre_w_lp.png"), plot = genre_w_lp)
ggsave(here("figures/lp_vs_global_sales.png"), plot = lp_vs_global_sales)
ggsave(here("figures/gs_vs_multi.png"), plot = gs_vs_multi)
ggsave(here("figures/nas_vs_multi.png"), plot = nas_vs_multi)
ggsave(here("figures/jps_vs_multi.png"), plot = jps_vs_multi)
ggsave(here("figures/eus_vs_multi.png"), plot = eus_vs_multi)
ggsave(here("figures/others_vs_multi.png"), plot = others_vs_multi)
ggsave(here("figures/lp_w_micro.png"), plot = lp_w_micro)
ggsave(here("figures/gs_vs_micro.png"), plot = gs_vs_micro)
ggsave(here("figures/eus_vs_micro.png"), plot = eus_vs_micro)
ggsave(here("figures/jps_vs_micro.png"), plot = jps_vs_micro)
ggsave(here("figures/nas_vs_micro.png"), plot = nas_vs_micro)
ggsave(here("figures/y_vs_pt.png"), plot = y_vs_pt)
ggsave(here("figures/y_vs_pubtier.png"), plot = y_vs_pubtier)
ggsave(here("figures/esrb_vs_g.png"), plot = esrb_vs_g)
ggsave(here("figures/lp_vs_esrb.png"), plot = lp_vs_esrb)


write_csv(games_cleaned, file = here("data/games_cleaned.csv"))
