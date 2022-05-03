.libPaths("~/R/rlib-4.0.5")

library(mikropml)
library(furrr)
library(tidyverse)

plan("multisession")

genus_merged = read_tsv("~/merged-adult-child-feature-table-genus.tsv") %>%
  pivot_longer(-1, names_to = "SampleID", values_to = "rel_abund") %>%
  pivot_wider(names_from = "Genus", values_from = "rel_abund")
metadata_merged = read_tsv("~/merged-adult-child-metadata.txt")

genus_merged_meta = metadata_merged %>%
  inner_join(genus_merged) %>%
  mutate(outcome =
           case_when(Race == "Asian/Pacific Islander" ~ "AAPI",
                     Race == "Black" ~ "Black",
                     Race == "White" ~ "White"))
genus_merged_down = replicate(n=10, genus_merged_meta %>%
                                group_by(Race, Group) %>%
                                slice_sample(n=50) %>%
                                ungroup(), simplify = F)
genus_merged_down_go = lapply(genus_merged_down, function(down) {
  down %>% dplyr::select(outcome, 5:1504) %>%
    preprocess_data(outcome_colname = "outcome")
})
dat = lapply(genus_merged_down_go, function(processed) {
  processed$dat_transformed
})
grps = lapply(genus_merged_down, function(down) {
  down$Group
})

saveRDS(dat, "dat_down.rds")
saveRDS(grps, "groups_down.rds")

final_hp = list(mtry = c(38, 40, 41, 42, 43, 44, 46))

genus_race_merged_down_1 = function(seed) {
  run_ml(dataset = dat[[1]],
         method = "rf",
         outcome_colname = "outcome",
         cv_times = 100,
         find_feature_importance = T,

         hyperparameters = final_hp,
         seed = seed,
         groups = grps[[1]],
         group_partitions = list(train = c('C'),
                                 test = c('A')))
}

merged_down_ml_results_1 = future_map(1:100, genus_race_merged_down_1,
                                      .options = furrr_options(seed = T))

genus_race_merged_down_1 = function(seed) {
  run_ml(dataset = dat[[1]],
         method = "rf",
         outcome_colname = "outcome",
         cv_times = 100,
         find_feature_importance = T,

         hyperparameters = final_hp,
         seed = seed,
         groups = grps[[1]],
         group_partitions = list(train = c('C'),
                                 test = c('A')))
}

merged_down_ml_results_1 = future_map(1:100, genus_race_merged_down_1,
                                      .options = furrr_options(seed = T))


genus_race_merged_down_2 = function(seed) {
  run_ml(dataset = dat[[2]],
    method = "rf",
    outcome_colname = "outcome",
    cv_times = 100,
    find_feature_importance = T,
    hyperparameters = final_hp,
    seed = seed,
    groups = grps[[2]],
    group_partitions = list(train = c('C'),
    test = c('A')))
}

merged_down_ml_results_2 = future_map(1:100, genus_race_merged_down_2,
  .options = furrr_options(seed = T))


genus_race_merged_down_3 = function(seed) {
  run_ml(dataset = dat[[3]],
    method = "rf",
    outcome_colname = "outcome",
    cv_times = 100,
    find_feature_importance = T,
    hyperparameters = final_hp,
    seed = seed,
    groups = grps[[3]],
    group_partitions = list(train = c('C'),
    test = c('A')))
}

merged_down_ml_results_3 = future_map(1:100, genus_race_merged_down_3,
.options = furrr_options(seed = T))


genus_race_merged_down_4 = function(seed) {
run_ml(dataset = dat[[4]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[4]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_4 = future_map(1:100, genus_race_merged_down_4,
.options = furrr_options(seed = T))


genus_race_merged_down_5 = function(seed) {
run_ml(dataset = dat[[5]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[5]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_5 = future_map(1:100, genus_race_merged_down_5,
.options = furrr_options(seed = T))


genus_race_merged_down_6 = function(seed) {
run_ml(dataset = dat[[6]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[6]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_6 = future_map(1:100, genus_race_merged_down_6,
.options = furrr_options(seed = T))


genus_race_merged_down_7 = function(seed) {
run_ml(dataset = dat[[7]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[7]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_7 = future_map(1:100, genus_race_merged_down_7,
.options = furrr_options(seed = T))


genus_race_merged_down_8 = function(seed) {
run_ml(dataset = dat[[8]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[8]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_8 = future_map(1:100, genus_race_merged_down_8,
.options = furrr_options(seed = T))


genus_race_merged_down_9 = function(seed) {
run_ml(dataset = dat[[9]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[9]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_9 = future_map(1:100, genus_race_merged_down_9,
.options = furrr_options(seed = T))

genus_race_merged_down_10 = function(seed) {
run_ml(dataset = dat[[10]],
method = "rf",
outcome_colname = "outcome",
cv_times = 100,
find_feature_importance = T,
hyperparameters = final_hp,
seed = seed,
groups = grps[[10]],
group_partitions = list(train = c('C'),
test = c('A')))
}

merged_down_ml_results_10 = future_map(1:100, genus_race_merged_down_10,
.options = furrr_options(seed = T))

genus_race_merged_all = c(genus_race_merged_down_1, genus_race_merged_down_2, genus_race_merged_down_3, genus_race_merged_down_4,
genus_race_merged_down_5, genus_race_merged_down_6, genus_race_merged_down_7, genus_race_merged_down_8, genus_race_merged_down_9,
genus_race_merged_down_10)

saveRDS(genus_race_merged_all, "~/genus_race_merged_all.rds")
