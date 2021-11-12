library(tidyverse)
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(arrow)
library(modeltime.ensemble)

df_rookie <- readr::read_csv("data/individual_players_7.csv") %>%
  dplyr::group_by(player_name) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::filter(Lge != "NHL") %>%
  dplyr::mutate(diff = c(1, diff(id))) %>%
  dplyr::filter(diff == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate_at(vars(GP:PIM_playoffs), ~ ifelse(. == "--" | is.na(.), 0, .)) %>%
  dplyr::mutate_at(vars(GP:PIM_playoffs), ~as.numeric(.)) %>%
  dplyr::mutate_at(vars(GP:PIM_playoffs), ~ .*id) %>%
  dplyr::group_by(player_name) %>%
  dplyr::summarise_at(vars(GP:PIM_playoffs), ~mean(., na.rm = TRUE)) %>%
  dplyr::mutate(
    playoffs = dplyr::select(
      ., GP_playoffs:PIM_playoffs
    ) %>% base::rowSums(na.rm = T),
    playoffs = ifelse(playoffs == 0, 0, 1)
  )
View(df_rookie)


final_df <- readr::read_csv("data/nhl_all.csv") %>%
  dplyr::select(player, pos, pts) %>%
  dplyr::inner_join(df_rookie, by = c("player" = "player_name")) %>%
  na.omit() %>%
  dplyr::select(-player)

## -----------------------------------------------------------------------------------------------------------------------
set.seed(1234)
splits <- rsample::initial_split(final_df, prop = 0.8)
recipes_rf <- recipes::recipe(pts ~., data = rsample::training(splits)) %>%
  recipes::step_normalize(all_numeric(), -all_outcomes()) %>%
  recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) %>%
  recipes::step_nzv(all_predictors()) %>%
  recipes::step_naomit(dplyr::everything())
model_spec_rf <- parsnip::boost_tree(
  mode = "regression",
  mtry = tune::tune(),
  trees = tune::tune(),
  min_n = tune::tune(),
  learn_rate = tune::tune(),
  loss_reduction = tune::tune(),
  tree_depth = tune::tune()
) %>%
  parsnip::set_engine("xgboost")
df_rf <- recipes_rf %>% prep() %>% juice()

set.seed(123)
resamples_kfold <- rsample::vfold_cv(
  rsample::training(splits),
  v = 5
)

grid_spec_rf <-
  dials::grid_latin_hypercube(
    dials::mtry(range = c(1, ncol(df_rf) - 1)),
    min_n(),
    trees(),
    tree_depth(),
    learn_rate(range = c(-2.5, -0.5)),
    loss_reduction(),
    size = 100
  )

library(doFuture)
registerDoFuture()
n_cores <- parallel::detectCores()

plan(
  strategy = cluster,
  workers = parallel::makeCluster(n_cores)
)

start_rf <- Sys.time()
tune_results_rf_kfold <- workflows::workflow() %>%
  workflows::add_recipe(recipes_rf) %>%
  workflows::add_model(model_spec_rf) %>%

  tune::tune_grid(
    resamples = resamples_kfold,
    grid      = grid_spec_rf,
    metrics   = modeltime::default_forecast_accuracy_metric_set(),
    control   = tune::control_grid(verbose = FALSE, save_pred = TRUE)
  )
end_rf <- Sys.time()
end_rf - start_rf

tune_results_rf_kfold %>%
  tune::show_best()
g <- tune_results_rf_kfold %>%
  autoplot() +
  geom_smooth(se = F)
plotly::ggplotly(g)

workflow_fit_rf_kfold <- workflows::workflow() %>%
  workflows::add_recipe(recipes_rf) %>%
  workflows::add_model(model_spec_rf) %>%
  tune::finalize_workflow(
    tune_results_rf_kfold %>%
      tune::show_best(metric = "mae") %>%
      dplyr::slice(1)
  ) %>%
  parsnip::fit(rsample::training(splits))

last_df <- last_fit(workflow_fit_rf_kfold, split = splits)
last_df$.predictions
last_df$.metrics

model_spec_lm <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")
wf_lm <- workflows::workflow() %>%
  workflows::add_recipe(recipes_rf) %>%
  workflows::add_model(model_spec_lm) %>%
  parsnip::fit(rsample::training(splits))
last_df <- last_fit(wf_lm, split = splits)
last_df$.predictions
last_df$.metrics

lm(pts ~., data = rsample::training(splits)) %>%
  summary()

ggplot(rsample::training(splits), aes(x = log((pts + 1)))) +
  geom_density()

library(MASS)
bc <- MASS::boxcox((pts + 0.1) ~., data = rsample::training(splits))
(lambda <- bc$x[which.max(bc$y)])
lm_bc <- lm((((pts+0.1)^lambda-1)/lambda) ~., data = rsample::training(splits))
plot(lm_bc)


lm_bc <- lm(pts ~., data = rsample::training(splits) %>%
              dplyr::filter(pts != 0))
plot(lm_bc)



bc <- MASS::boxcox(pts ~., data = rsample::training(splits) %>%
                     dplyr::filter(pts != 0))
(lambda <- bc$x[which.max(bc$y)])
lm_bc <- lm((((pts)^lambda-1)/lambda) ~., data = rsample::training(splits) %>%
              dplyr::filter(pts != 0))
plot(lm_bc)
lm((((pts)^lambda-1)/lambda) ~., data = rsample::training(splits) %>%
     dplyr::filter(pts != 0)) %>%
  summary()
