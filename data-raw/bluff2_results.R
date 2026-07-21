library(tidyverse)

bluff2_results_raw <- process_results()

# --- Extract per-sample solver costs ------------------------------------------

# Per-sample solver cost and token usage, taken from each sample's solver
# Chat object. Summing these per model reproduces the task-level total.
sample_costs <- bluff2_results_raw |>
  transmute(
    task_name = task,
    id,
    epoch,
    cost = purrr::map_dbl(solver_chat, ~ as.numeric(.x$get_cost())),
    solver_input_tokens = purrr::map_dbl(
      solver_chat,
      ~ sum(.x$get_tokens()$input)
    ),
    solver_cached_tokens = purrr::map_dbl(
      solver_chat,
      ~ sum(.x$get_tokens()$cached_input)
    ),
    solver_output_tokens = purrr::map_dbl(
      solver_chat,
      ~ sum(.x$get_tokens()$output)
    )
  )

# Models missing from ellmer's litellm-based pricing are priced by hand here
# (per-MTok input / cached-read / output), falling back to the token-derived
# estimate below. `input` counts uncached input; cached reads are billed
# separately at the cached rate.
manual_prices <- tribble(
  ~task_name                , ~input_per_mtok , ~cached_per_mtok , ~output_per_mtok ,
  "opus_4_8_medium"         , 5               , 0.50             , 25               ,
  "fable_5_medium"          , 10              , 1                , 50               ,
  "sonnet_5_medium"         , 3               , 0.30             , 15               ,
  "gemini_3_5_flash_medium" , 1.50            , 0.15             , 9                ,
  "gemini_3_6_flash_medium" , 1.50            , 0.15             , 7.50             ,
  "gpt_5_5_medium"          , 5               , 0.50             , 30               ,
  "gpt_5_6_terra_medium"    , 2.50            , 0.25             , 15               ,
  "gpt_5_6_sol_medium"      , 5               , 0.50             , 30               ,
)

sample_costs <- sample_costs |>
  left_join(manual_prices, by = "task_name") |>
  mutate(
    cost = if_else(
      is.na(cost),
      (solver_input_tokens * input_per_mtok +
        solver_cached_tokens * cached_per_mtok +
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
  select(-any_of(c("metadata", "solver_chat"))) |>
  left_join(sample_costs, by = c("model" = "task_name", "id", "epoch")) |>
  left_join(model_metadata, by = c("model" = "task_name")) |>
  mutate(
    model = case_when(
      model == "opus_4_8_medium" ~ "Claude Opus 4.8 (medium)",
      model == "fable_5_medium" ~ "Claude Fable 5 (medium)",
      model == "sonnet_5_medium" ~ "Claude Sonnet 5 (medium)",
      model == "gemini_3_5_flash_medium" ~ "Gemini 3.5 Flash (medium)",
      model == "gemini_3_6_flash_medium" ~ "Gemini 3.6 Flash (medium)",
      model == "gpt_5_5_medium" ~ "GPT-5.5 (medium)",
      model == "gpt_5_6_terra_medium" ~ "GPT-5.6 Terra (medium)",
      model == "gpt_5_6_sol_medium" ~ "GPT-5.6 Sol (medium)"
    ),
    thinking = stringr::str_detect(model, "\\(medium\\)")
  )

usethis::use_data(bluff2_results, overwrite = TRUE)
