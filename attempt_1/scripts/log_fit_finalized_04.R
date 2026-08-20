# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_1/results/log_fit.rda"))
load(here("attempt_1/datasets/train_upsampled.rda"))
load(here("attempt_1/datasets/game_testing.rda"))


# finalize workflow ----
final_wflow_log <- log_fit |> 
  extract_workflow(log_fit) |>  
  finalize_workflow(select_best(log_fit, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(98315)
final_fit_log <- fit(final_wflow_log, train_upsampled)

# saving final fit
# save(final_fit_log, file = here("attempt_2/results/final_fit.rda"))

#################################################################
# predicting

game_test <- game_testing |>
  mutate(
    platform = as.factor(platform),
    platform_type = as.factor(platform_type),
    platform_maker = as.factor(platform_maker),
    platform_generation = as.factor(platform_generation),
    genre = as.factor(genre),
    publisher_region = as.factor(publisher_region),
    publisher_tier = as.factor(publisher_tier),
    esrb_rating = as.factor(esrb_rating),
    is_sequel = as.factor(is_sequel),
    online_multiplayer = factor(online_multiplayer),
    dlc_released = as.factor(dlc_released),
    microtransactions = as.factor(microtransactions),
    loot_boxes = as.factor(loot_boxes),
    game_pass_available = as.factor(game_pass_available),
    vr_support = as.factor(vr_support),
    goty_nominated = as.factor(goty_nominated),
    goty_won = as.factor(goty_won)
  )

pred_log <- game_test |>
  bind_cols(predict(final_fit_log, new_data = game_test, type = "prob")) |>
  select(game_id, predicted = .pred_1)

# save results
save(pred_log, file = "attempt_1/final_results/pred_log.rda")
