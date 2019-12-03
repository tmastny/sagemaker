library(readr)
library(dplyr)
library(recipes)

data_file <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
abalone <- readr::read_csv(file = data_file, col_names = FALSE)

names(abalone) <- c(
  'sex', 'length', 'diameter', 'height', 'whole_weight',
  'shucked_weight', 'viscera_weight', 'shell_weight', 'rings'
)

abalone <- abalone %>%
  select(rings, everything())

abalone <- recipe(~ ., data = abalone) %>%
  step_dummy(sex, one_hot = TRUE) %>%
  prep(training = abalone) %>%
  bake(new_data = abalone)

usethis::use_data(abalone)
