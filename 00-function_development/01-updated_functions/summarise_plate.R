print_variable_summary <-
  function(layout, variable_name, ...) {
    # get stuff like "cmpd1 in 24 wells"
    var_counts <-
      layout |>
      select(all_of(c("well", variable_name))) |>
      distinct() |>
      group_by(.data[[variable_name]]) |>
      tally() |>
      mutate("msg" = paste0(
        "--- {blue ", .data[[variable_name]],
        "} in ", .data$n, " wells\n"
      )) |>
      pull("msg")

    # don't include variable summaries which cannot be glued
    tryCatch(
      {
        var_name <- glue_col(.literal = TRUE, "{yellow {bold {variable_name}}} (type: {glue_collapse(typeof(layout[[variable_name]]))}), includes: \n {glue_col({glue_collapse(var_counts)})}\n")
      },
      error = function(e) {
        var_name <- glue_col(.literal = TRUE, "{yellow {bold {variable_name}}} (type: {glue_collapse(typeof(layout[[variable_name]]))}) -- variable couldn't be printed. Does it contain special characters? \n")
      },
      finally = function(f) {
        var_name <- glue_col("variable could not be printed")
      }
    )
  }

print_summary <- function(layout, plate_notes = "none", ...) {
  # get a few important values
  n_wells <- n_distinct(layout$well)
  n_variables <- length(names(layout)[!names(layout) %in% c("well", "row", "column", "condition")])
  variables <- names(layout)[!names(layout) %in% c("well", "row", "column", "condition")]

  # get plate heading
  plate_heading <- glue_col(.literal = TRUE, "Experiment containing {green {n_wells} wells}, with {green {n_variables}} {yellow experimental variables}")

  plate_msgs <- lapply(variables, print_variable_summary, layout = layout)

  all_notes <- tryCatch(glue_col(.literal = TRUE, "{glue_collapse(plate_notes, sep = '\n ')}"), error = function(e) {})

  var_summary <- glue_col(.literal = TRUE, "{plate_heading} \n{glue_collapse(plate_msgs, sep = '\n \n')} \n \n{magenta {bold Notes:}} \n{plate_notes}")

  var_summary
}
