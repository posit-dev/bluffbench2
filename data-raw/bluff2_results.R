library(tidyverse)

bluff2_results_raw <- process_results()

# --- Extract per-sample solver costs from task objects ------------------------

task_files <- list.files("inst/run/tasks", full.names = TRUE)

sample_costs <- purrr::map(task_files, function(f) {
  tmp <- new.env()
  load(f, envir = tmp)
  tsk <- tmp[[ls(tmp)[1]]]
  task_name <- gsub("tsk_|\\.rda", "", basename(f))
  samples <- tsk$get_samples()
  # Per-sample solver cost and token usage, taken from each sample's solver
  # Chat object. Summing these per model reproduces the task-level total.
  tibble(
    task_name = task_name,
    id = samples$id,
    epoch = samples$epoch,
    cost = purrr::map_dbl(samples$solver_chat, ~ as.numeric(.x$get_cost())),
    solver_input_tokens = purrr::map_dbl(
      samples$solver_chat,
      ~ sum(.x$get_tokens()$input)
    ),
    solver_output_tokens = purrr::map_dbl(
      samples$solver_chat,
      ~ sum(.x$get_tokens()$output)
    )
  )
}) |>
  list_rbind()

# Models missing from ellmer's litellm-based pricing are priced by hand here
# (per-MTok input / output), falling back to the token-derived estimate below.
manual_prices <- tribble(
  ~task_name                , ~input_per_mtok , ~output_per_mtok ,
  "opus_4_8_medium"         , 5               , 25               ,
  "gemini_3_5_flash_medium" , 0.30            , 2.50             ,
  "gpt_5_5_medium"          , 1.25            , 10               ,
)

sample_costs <- sample_costs |>
  left_join(manual_prices, by = "task_name") |>
  mutate(
    cost = if_else(
      is.na(cost),
      (solver_input_tokens *
        input_per_mtok +
        solver_output_tokens * output_per_mtok) /
        1e6,
      cost
    )
  ) |>
  select(task_name, id, epoch, cost)

# --- Read model metadata ------------------------------------------------------

model_metadata <- read_csv(
  "data-raw/model_metadata.csv",
  col_types = cols(
    task_name = col_character(),
    lab = col_character(),
    release_date = col_date(),
    release_date_source = col_character()
  )
)

# --- Build bluff2_results -----------------------------------------------------

bluff2_results <-
  bluff2_results_raw |>
  rename(model = task) |>
  select(-any_of("metadata")) |>
  left_join(sample_costs, by = c("model" = "task_name", "id", "epoch")) |>
  left_join(model_metadata, by = c("model" = "task_name")) |>
  mutate(
    model = case_when(
      model == "opus_4_8_medium" ~ "Claude Opus 4.8 (medium)",
      model == "gemini_3_5_flash_medium" ~ "Gemini 3.5 Flash (medium)",
      model == "gpt_5_5_medium" ~ "GPT-5.5 (medium)"
    ),
    thinking = stringr::str_detect(model, "\\(medium\\)")
  )

usethis::use_data(bluff2_results, overwrite = TRUE)
