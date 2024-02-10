utils::globalVariables(c("year", "STATE", "MONTH", "n"))
#' Create tibble data from csv file
#'
#' This function reads file given and converts to tibble format
#'
#' @param filename A character string for filename along with its path
#'
#' @return This function returns tibble dataframe.
#'
#' @details It will give error if file is not present
#'
#' @examples
#' \dontrun{
#' fars_read("C://abc/file2.csv")
#' fars_read("file1.csv")
#' }
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates filename
#'
#' This function creates filename based on year
#'
#' @param year A character string or number for year
#'
#' @return This function returns name o file in bz2 format
#'
#' @details
#' Function inserts year given and creates "accident_year.csv.bz2". year will be replaced from input supplied
#'
#' @examples
#' make_filename("2021")
#' make_filename(2021)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads files with given
#'
#' This function reads all files with years given and creates list of dataframe with year and month
#'
#' @inherit make_filename
#' @inherit fars_read
#' @param year A vector with years
#'
#' @return This function returns list of dataframe with years and month
#'
#' @details
#' if there is no file with year prefixed after accident_ and suffixed with .csv.bz2, it will give error
#'
#'
#' @examples
#' \dontrun{
#' fars_read_years("2021")
#' fars_read_years(c(2021,2022))
#' }
#'
#' @importFrom dplyr mutate mutate
#' @importFrom magrittr %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dat %>%
      dplyr::mutate(year = year) %>%
        dplyr::mutate(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Gives number of data points in each year given
#'
#' This function reads all files with years given and gives number of rows in each year
#'
#' @inherit fars_read_years

#' @param year A vector with years
#'
#' @return This function returns dataframe with years in column and number of rows of data as values
#'
#' @details
#' if there is no file with year prefixed after accident_ and suffixed with .csv.bz2, it will give error
#'
#'
#' @examples
#' \dontrun{
#' fars_summarize_years("2021")
#' fars_summarize_years(c(2021,2022))
#' }
#'
#' @importFrom dplyr mutate mutate
#' @importFrom magrittr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots longitude and latitude of state number given present in given years of accident
#'
#' This function reads all files with years, filters on state ID and plots longitude and latitudes.
#' It only plots longitude greater than 900 and latitude greater than 90
#'
#'
#' @param state.num A character with state ID
#' @param year A vector with years
#'
#' @return This function returns plot with longitude and latitudes with accidents
#'
#' @details
#' if there is no file with year prefixed after accident_ and suffixed with .csv.bz2, it will give error
#'
#'
#' @examples
#' \dontrun{
#' fars_map_state("100","2021")
#' fars_map_state("100",c(2021,2022))
#' }
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
