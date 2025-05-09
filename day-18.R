# Reading in the data

library(tidyverse)
library(tidymodels)

# URLs
covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
pop_url   <- 'https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv'

# Ingest
data   <- readr::read_csv(covid_url)
census <- readr::read_csv('https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv')

# Clean Census Data
census = census |> 
  filter(COUNTY == "000") |>
  mutate(fips = STATE) |>     
  select(fips, contains("2021"))

# Process COVID-19 Data
state_data <-  data |> 
  group_by(fips) |> 
  mutate(
    new_cases  = pmax(0, cases - lag(cases)),   
    new_deaths = pmax(0, deaths - lag(deaths))
  ) |> 
  ungroup() |> 
  left_join(census, by = "fips") |> 
  distinct(state, deaths, .keep_all = TRUE) |>
  ungroup() |>
  select(state, deaths, POPESTIMATE2021, BIRTHS2021, DEATHS2021) |> 
  drop_na() |>  
  mutate(logC = log(deaths +1))

# Data Summary
skimr::skim(state_data)  

# Data Splitting for Modeling
split <- initial_split(state_data, prop = 0.8)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 10) 

rec = recipe(logC ~ . , data = train) |> 
  step_rm(state) |> 
  step_dummy(all_nominal()) |>
  step_scale(all_numeric_predictors()) |> 
  step_center(all_numeric_predictors())


# Define Regression Models
lm_mod <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

rf_model <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_model2 <- rand_forest() |> 
  set_engine("randomForest") |> 
  set_mode("regression")

b_mod <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression")

nn_mod <- mlp(hidden_units = 10) |> 
  set_engine("nnet") |> 
  set_mode("regression")

# Create Workflow Set
wf = workflow_set(list(rec), list(lm_mod, 
                                  rf_model, 
                                  rf_model2,
                                  b_mod, 
                                  nn_mod
)) |> 
  workflow_map(resamples = folds)  

# Visualize Model Performance
autoplot(wf)

# Fit Selected Model (Neural Network)
fit <- workflow() |> 
  add_recipe(rec) |> 
  add_model(nn_mod) |> 
  fit(data = train)

# Feature Importance
vip::vip(fit)

# Model Evaluation
predictions <- augment(fit, new_data = test) |> 
  mutate(diff = abs(logC - .pred))  # Compute absolute differences

metrics(predictions, truth = logC, estimate = .pred)  # Compute regression metrics

# Visualization of Predictions vs. Actual Values
ggplot(predictions, aes(x = logC, y = .pred)) + 
  geom_point() + 
  geom_abline() +
  #geom_label(aes(label = paste(state, season), nudge_x = 0.1, nudge_y = 0.1)) +
  labs(title = "Neural Net Model", 
       x = "Actual (Log10)", 
       y = "Predicted (Log10)") + 
  theme_minimal()