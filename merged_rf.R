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

new_hp = list(mtry = c(18, 22, 25, 27, 29, 32, 36, 50))
test_hp = function(seed) {
  run_ml(dataset = dat[[1]],
         method = "rf",
         outcome_colname = "outcome",
         cv_times = 100,
         hyperparameters = new_hp,
         seed = seed,
         groups = grps[[1]],
         group_partitions = list(train = c('C'),
                                 test = c('A')))
}

iterative_run_ml_results = future_map2(1:50, test_hp,
                                      .options = furrr_options(seed = T))

saveRDS(iterative_run_ml_results, "~/iterative_merged_down_ml.rds")
