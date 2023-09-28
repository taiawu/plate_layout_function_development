#' Read a plate layout file into a tibble
#'
#' read_layout() reads a raw plate layout file (.csv, .txt, .xls, or .xlsx), and returns it as a formatted tibble. Variables are coerced to numeric if indicated by readr::parse_guess(). The originally required "Type" column heading is now optional.
#'
#'
#'
#' @param filepath Path to a file containing a plate layout. Accepts .csv, .txt, .xlsx, and .xls.
#' @param ... Unused, for extensibility.
#'
#' @return Returns a tibble, mapping experimental variables to well positions. All outputs contain the columns: row, column, and well. Additionally, a single column is added for each user-defined variable in the plate layout file, from which one additional "condtion" column is created, which contains all experimental variables. If all experimental variables are defined in the layout, wells with identical entries in the "condition" column are technical replicates.
#'
#'
#' @importFrom tidyselect everything
#' @importFrom tools file_ext
#' @importFrom readr read_csv read_tsv parse_guess
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom dplyr filter if_all mutate across slice relocate
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom rlang .data
#' @importFrom rlang .data
#'
#' @export
read_layout <- function(filepath, ...) {
  # Read file based on extension  --------------------------------------------------------------
  ext <- file_ext(filepath)

  raw <- switch(ext,
    csv = read_csv(filepath, col_names = FALSE),
    txt = read_tsv(filepath, col_names = FALSE), # fails with UTF-16
    xlsx = read_excel(filepath, col_names = FALSE),
    xls = read_excel(filepath, col_names = FALSE)
  ) %>% suppressMessages()

  # handle files with or without the "Type" header
  first_cell <- raw[1, 1][[1]] # extract value of top-left cell
  out <- switch(first_cell,
    Type = raw[-1, ], # if it includes "Type", drop this unnecessary top row
    raw # otherwise, proceed unchanged
  )

  # Format as layout  --------------------------------------------------------------
  # get column names
  plate_col_names <- c("variable", "row", slice(out, 1)[-c(1, 2)])

  # convert into layout form
  out |>
    set_names(plate_col_names) |>
    # prepare plate-format data for pivoting
    filter(
      .data$row %in% c(base::letters[1:16], base::LETTERS[1:16]),
      !if_all(.cols = -c("variable", "row"), ~ is.na(.x))
    ) |> # drop empty columns before pivot
    mutate(across(everything(), as.character)) |> # make all character, to prevent issues in pivot

    # give each experimental variable a column
    pivot_longer(-c("variable", "row"), names_to = "column", values_to = "value") |>
    pivot_wider(names_from = "variable", values_from = "value") |>
    # add helpful columns: row, column, condition
    mutate(well = paste0(.data$row, .data$column)) |> # make well column
    unite(condition, -c("row", "column", "well"), sep = "__", remove = FALSE) |>
    # drop user-defined empty wells
    filter(!if_all(.cols = -c("row", "column", "condition", "well"), ~ .x %in% c("Empty", "empty") | is.na(.x))) |>
    # guess / coerce variables to numeric or character
    mutate(across(everything(), parse_guess)) |> # convert likely numeric variables to numeric

    # rearrange columns for human readability
    relocate("well") |> # well as first column
    relocate(any_of(c("row", "column")), .after = last_col()) # separate row and column info as last columns
}
