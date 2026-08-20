# Model assessment ---- 

# Load package(s)
library(tidymodels)
library(tidyverse)
library(here)
library(bonsai)

# handle common conflicts
tidymodels_prefer()

# load required objects here
load(here("attempt_1/final_results/pred_log.rda"))
load(here("attempt_1/final_results/pred_log.rda"))
load(here("attempt_1/final_results/pred_log.rda"))
load(here("attempt_1/final_results/pred_log.rda"))
load(here("attempt_1/final_results/pred_log.rda"))


# collect roc_auc from all the results
pred_log |>
  roc_auc(goty_won, .pred)

# creating performance table  
#model_perf_tbl <- tibble