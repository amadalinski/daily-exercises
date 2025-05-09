library(tidymodels)
library(palmerpenguins)
library(ranger)

set.seed(123)
(split <- initial_split(penguins, prop = 0.7)) 

penguin_train <- training(split)
glimpse(penguin_train)

penguin_test <- testing(split)
glimpse(penguin_test)

folds <- vfold_cv(penguin_train, v = 10)
glimpse(folds)

#Daily Assignment 16

species_formula <- species ~.

penguins_recipe <- recipe(species_formula, data = penguins)

log_mod <- logistic_reg() |>
  set_engine("glm")

rf_mod <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

#Option 1
wf_set <- workflow_set(
  preproc = list(species_formula = species_formula),
  models = list(logistic = log_mod, random_forest = rf_mod)
)
  