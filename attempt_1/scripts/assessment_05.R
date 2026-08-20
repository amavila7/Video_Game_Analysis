# Model assessment ---- 

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(bonsai)
library(yardstick)

# handle common conflicts
tidymodels_prefer()

# load required objects here
load(here("attempt_1/final_results/pred_log.rda"))
load(here("attempt_1/final_results/pred_en.rda"))
load(here("attempt_1/final_results/pred_knn.rda"))
load(here("attempt_1/final_results/pred_rf.rda"))
load(here("attempt_1/final_results/pred_bt.rda"))


# collect roc_auc from all the results
log_ra <- pred_log |>
  roc_auc(goty_won, predicted)

en_ra <- pred_en |>
  roc_auc(goty_won, predicted)

knn_ra <- pred_knn |>
  roc_auc(goty_won, predicted)

rf_ra <- pred_rf |>
  roc_auc(goty_won, predicted)

bt_ra <- pred_bt |>
  roc_auc(goty_won, predicted)

# creating performance table  
model_perf_tbl <- tibble(
  `Logistic` = log_ra |> pull(.estimate),
  `Elastic Net` = en_ra |> pull(.estimate),
  `K-Nearest Neighbors` = knn_ra |> pull(.estimate),
  `Random Forest` = rf_ra |> pull(.estimate),
  `Boosted Trees` = bt_ra |> pull(.estimate)
) |>
  round(digits = 4) |>
  knitr::kable()

# save out tbl

save(model_perf_tbl, file = here("figures/model_perf_tbl.rda"))

# building plot
performance_plot <- ggplot(pred_log, aes(x = goty_won, y = predicted)) +
  geom_point(alpha = .2) +
  coord_fixed() +
  theme_minimal() +
  labs(
    x = "Actual Winners",
    y = "Predicted Winners 
    (Logistic model/workflow)"
  )

# save out plot

ggsave("figures/performance_plot.png", plot = performance_plot)

