samples_dir <- "data-raw/samples"

sample_paths <- list.files(
  samples_dir,
  pattern = "\\.ya?ml$",
  full.names = TRUE
)

bluff2_dataset <- purrr::map(sample_paths, yaml::read_yaml) |>
  purrr::map_dfr(\(sample) {
    tibble::tibble(
      id = sample$id,
      input = list(tibble::tibble(
        data_name = sample$data_name,
        data_file = sample$data_file,
        modes = list(unlist(sample$modes)),
        lull_turns = list(purrr::map(sample$lull_turns, unlist)),
        artifact_turn = list(unlist(sample$artifact_turn))
      )),
      target = sample$target
    )
  }) |>
  dplyr::arrange(id)

usethis::use_data(bluff2_dataset, overwrite = TRUE)
