library(tidyverse)

bluff2_results_raw <- process_results()

# --- Extract solver costs from task objects -----------------------------------

task_files <- list.files("inst/run/tasks", full.names = TRUE)

task_costs <- purrr::map(task_files, function(f) {
  tmp <- new.env()
  load(f, envir = tmp)
  tsk <- tmp[[ls(tmp)[1]]]
  cost_df <- tsk$get_cost()
  task_name <- gsub("tsk_|\\.rda", "", basename(f))
  solver <- cost_df |> dplyr::filter(source == "solver")
  tibble(
    task_name = task_name,
    solver_input_tokens = solver$input,
    solver_output_tokens = solver$output,
    cost = as.numeric(gsub("\\$", "", solver$price))
  )
}) |>
  list_rbind()

# Models missing from ellmer's litellm-based pricing are priced by hand here
# (per-MTok input / output), falling back to the token-derived estimate below.
manual_prices <- tribble(
  ~task_name         , ~input_per_mtok , ~output_per_mtok ,
  "claude_4_8_opus"  , 5               , 25               ,
  "gemini_3_5_flash" , 0.30            , 2.50             ,
  "gpt_5_5"          , 1.25            , 10               ,
)

task_costs <- task_costs |>
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
  select(task_name, cost)

# --- Build bluff2_results -----------------------------------------------------

bluff2_results <-
  bluff2_results_raw |>
  rename(model = task) |>
  select(-any_of("metadata")) |>
  left_join(task_costs, by = c("model" = "task_name")) |>
  mutate(
    model = case_when(
      model == "claude_4_8_opus" ~ "Claude Opus 4.8 (medium)",
      model == "gemini_3_5_flash" ~ "Gemini 3.5 Flash (medium)",
      model == "gpt_5_5" ~ "GPT-5.5 (medium)"
    ),
    thinking = stringr::str_detect(model, "\\(medium\\)")
  )

usethis::use_data(bluff2_results, overwrite = TRUE)
