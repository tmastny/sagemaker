# xgboost predictions for first 100 rows of the abalone dataset.

abalone_pred <- readr::read_csv("data-raw/abalone_pred.csv") %>%
  dplyr::select(.pred = X1)

usethis::use_data(abalone_pred)
