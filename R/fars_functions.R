#' Read `fars` data from file
#'
#' This function takes a path to a file containing `fars` data, reads
#' the data from the file, and returns it as a tibble
#' ([dplyr::tbl_df]).
#'
#' @param filename Path to the file containing the data.
#'
#' @return A tibble ([dplyr::tbl_df]) with the data read from the file.
#'
#' @note The function is stopped and errors out if the given path does
#'     not exist or is not a file.
#'
#' @examples
#' \dontrun{fars_read("data/accident_2013.csv.bz2")}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @md
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make data file name 
#'
#' This function takes a year, and returns the name of a file
#' containing the data from that year.
#'
#' @param year Year the data concerns.
#'
#' @return Returns a string of the form `accident_{year}.csv.bz2`
#'     where year is the parameter given to the function.
#'
#' @note Parameter `year` is converted to an integer with the
#'     [as.integer] function.
#'
#' @examples
#' make_filename(2020)
#' make_filename(2020.10) # Note how the year is converted to an integer
#'
#' @md
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads `fars` and filters data for the given years
#'
#' This function takes a list of years and tries to read data from
#' files concerning those years. The data read contains only the
#' `MONTH` and `year` columns from the original data files.
#'
#' @param years A list of years.
#'
#' @return Returns a list of tibbles ([dplyr::tbl_df]), one for each
#'     year given to the function.
#'
#' @note This function expects that each year's file is in the working
#'     directory under the name `accident_{year}.csv.bz2`. If a year's
#'     data can not be read that year is ignored and a warning is
#'     issued, but all other data is still read.
#'
#' @seealso [make_filename(year)], [fars_read(filename)]
#'
#' @examples
#' \dontrun{fars_read_years(c(2013,2014,2015))}
#'
#' @importFrom dplyr mutate select
#'
#' @md
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summary of fatal occurences per month/year
#'
#' Takes a list of years, and returns a summary of the number of
#' occurences fatal occurences per month and year.
#'
#' @param years A list of years.
#'
#' @return A tibble ([dplyr::tbl_df]) where each row corresponds to a
#'     month of the year, and every column other than the first
#'     corresponds to a year.
#'
#' @note Years whose data file cannot be bound are ignored.
#'
#' @seealso [fars_read_years(years)]
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014,2015))}
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @md
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots the fatal occurences for a given state and year
#'
#' This function takes a state number and year, and plots the location
#' of each occurence during that year and on that state on a map using
#' the [maps::map] function.
#'
#' @param state.num A number for the state.
#' @param year A number.
#'
#' @note The functions is stopped and an error is returned if the file
#'     containing that year's data cannot be read, or if the state's
#'     number is invalid.
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @md
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
