## code to prepare `DATASET` dataset goes here

avia <- data.table::fread("~/Downloads/avia_par_lu.tsv.gz") %>%
  as_tibble()

usethis::use_data(avia, overwrite = TRUE)

