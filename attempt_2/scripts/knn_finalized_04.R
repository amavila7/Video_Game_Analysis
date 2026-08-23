# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_2/results/knn_tuned.rda"))
load(here("attempt_2/datasets/train_upsampled.rda"))
load(here("attempt_2/datasets/game_testing.rda"))

# finalize workflow ----
final_wflow_knn <- knn_tuned |> 
  extract_workflow(knn_tuned) |>  
  finalize_workflow(select_best(knn_tuned, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(63215)
final_fit_knn <- fit(final_wflow_knn, train_upsampled)

# saving final fit
# save(final_fit_knn, file = here("attempt_1results/final_fit.rda"))

#################################################################
# predicting

game_test <- game_testing |>
  mutate(
    goty_won = as.factor(goty_won)
  )

pred_knn <- game_test |>
  bind_cols(predict(final_fit_knn, new_data = game_test, type = "prob")) |>
  select(game_id, title, goty_won, predicted = .pred_1)


# save results
save(pred_knn, file = "attempt_2/final_results/pred_knn.rda")
