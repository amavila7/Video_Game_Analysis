# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_2/results/en_tuned.rda"))
load(here("attempt_2/datasets/train_upsampled.rda"))
load(here("attempt_2/datasets/game_testing.rda"))

# finalize workflow ----
final_wflow_en <- en_tuned |> 
  extract_workflow(en_tuned) |>  
  finalize_workflow(select_best(en_tuned, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(134895)
final_fit_en <- fit(final_wflow_en, train_upsampled)

# saving final fit
# save(final_fit_en, file = here("attempt_1results/final_fit.rda"))

#################################################################
# predicting

game_test <- game_testing |>
  mutate(
    goty_won = as.factor(goty_won)
  )

pred_en <- game_test |>
  bind_cols(predict(final_fit_en, new_data = game_test, type = "prob")) |>
  select(game_id, title, goty_won, predicted = .pred_1)


# save results
save(pred_en, file = "attempt_2/final_results/pred_en.rda")
