# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_1/results/knn_tuned.rda"))
load(here("attempt_1/datasets/train_upsampled.rda"))
load(here("attempt_1/datasets/game_testing.rda"))

# finalize workflow ----
final_wflow_knn <- knn_tuned |> 
  extract_workflow(knn_tuned) |>  
  finalize_workflow(select_best(knn_tuned, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(8347134)
final_fit_knn <- fit(final_wflow_knn, train_upsampled)

# saving final fit
# save(final_fit_knn, file = here("attempt_1results/final_fit.rda"))

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

pred_knn <- game_test |>
  bind_cols(predict(final_fit_knn, new_data = game_test, type = "prob")) |>
  select(game_id, predicted = .pred_1)


# save results
save(pred_knn, file = "attempt_1/final_results/pred_knn.rda")
