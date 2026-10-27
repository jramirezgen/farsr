#' farsr: Read, summarize and map FARS data
#'
#' Functions to work with the data of the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System (FARS), a census of fatal
#' injuries in motor vehicle traffic crashes.
#'
#' @keywords internal
"_PACKAGE"

## Variables used with non-standard evaluation in dplyr, tidyr and with()
utils::globalVariables(c("MONTH", "year", "n", "STATE", "LONGITUD", "LATITUDE"))

#' Read a FARS data file
#'
#' Reads one yearly data file of the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System (FARS) and returns it
#' as a tibble. The file is read with \code{readr::read_csv}, so it can be a
#' plain or compressed (e.g. \code{.csv.bz2}) comma separated file. Messages
#' printed by \code{read_csv} while parsing the columns are suppressed and no
#' progress bar is shown.
#'
#' @param filename A character string with the path to the file to read. A
#'   relative path is resolved against the current working directory.
#'
#' @return A tibble (\code{tbl_df}) with one row per fatal accident and one
#'   column per variable of the file (for example \code{STATE}, \code{MONTH},
#'   \code{LONGITUD} and \code{LATITUDE}).
#'
#' @section Errors:
#' Stops with the message \code{"file '<filename>' does not exist"} if
#' \code{filename} does not point to an existing file. Errors raised by
#' \code{readr::read_csv} for a file that exists but cannot be parsed are not
#' caught.
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#'
#' @examples
#' old_dir <- setwd(system.file("extdata", package = "farsr"))
#' accidents_2013 <- fars_read("accident_2013.csv.bz2")
#' head(accidents_2013)
#' setwd(old_dir)
#'
#' \dontrun{
#' # A file that does not exist gives an error
#' fars_read("accident_1900.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        tibble::as_tibble(data)
}

#' Build the name of a FARS data file
#'
#' Creates the file name used for the FARS data of a given year, following the
#' pattern \code{accident_<year>.csv.bz2}. The function only builds the name:
#' it does not check that the file exists.
#'
#' @param year A number, or a vector of numbers, giving the year(s). Anything
#'   that \code{as.integer} can convert is accepted, for example \code{2013}
#'   or \code{"2013"}.
#'
#' @return A character vector with one file name per element of \code{year},
#'   such as \code{"accident_2013.csv.bz2"}.
#'
#' @section Errors:
#' It does not stop on a bad input. If \code{year} cannot be converted to an
#' integer, \code{as.integer} gives a warning ("NAs introduced by coercion")
#' and the result contains \code{"accident_NA.csv.bz2"}.
#'
#' @examples
#' make_filename(2013)
#' make_filename(c(2013, 2014, 2015))
#' make_filename("2014")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read the month and year of the accidents for several years
#'
#' For each year in \code{years} the function reads the corresponding FARS file
#' (see \code{\link{make_filename}} and \code{\link{fars_read}}), adds a
#' \code{year} column and keeps only the \code{MONTH} and \code{year} columns.
#'
#' @param years A vector of years (numbers or character strings that can be
#'   converted to integer).
#'
#' @return A list with one element per year, in the same order as
#'   \code{years}. Each element is a tibble with the columns \code{MONTH} and
#'   \code{year}, with one row per accident. For a year whose file cannot be
#'   read the element is \code{NULL}.
#'
#' @section Errors:
#' It does not stop for an invalid year. Any error while reading the file of a
#' year (for instance, the file is not in the working directory) is caught,
#' a warning \code{"invalid year: <year>"} is given and \code{NULL} is
#' returned for that year. The same happens if the file has no \code{MONTH}
#' column.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' old_dir <- setwd(system.file("extdata", package = "farsr"))
#' fars_read_years(c(2013, 2014))
#'
#' # 1999 has no file: warning and a NULL element
#' fars_read_years(c(2013, 1999))
#' setwd(old_dir)
#'
#' @export
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

#' Count the accidents per month and year
#'
#' Reads the FARS files of the given years with
#' \code{\link{fars_read_years}} and counts the number of fatal accidents in
#' each month of each year.
#'
#' @param years A vector of years (numbers or character strings that can be
#'   converted to integer).
#'
#' @return A tibble in wide format: one row per month (column \code{MONTH},
#'   1 to 12) and one column per year with the number of accidents of that
#'   month. Years whose file could not be read are left out, with a warning.
#'
#' @section Errors:
#' Warnings \code{"invalid year: <year>"} are given for the years that cannot
#' be read (see \code{\link{fars_read_years}}). If none of the years can be
#' read the function stops with an error from \code{dplyr::group_by}, because
#' the columns \code{year} and \code{MONTH} do not exist.
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' old_dir <- setwd(system.file("extdata", package = "farsr"))
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013, 2014, 2015))
#' setwd(old_dir)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the accidents of a state in a year
#'
#' Draws on a map of the United States the location of the fatal accidents of
#' one state in one year. The data are read from the FARS file of the year (see
#' \code{\link{make_filename}} and \code{\link{fars_read}}). Longitudes
#' greater than 900 and latitudes greater than 90 are treated as missing values
#' and are not plotted. The map is limited to the range of coordinates of the
#' accidents.
#'
#' @param state.num A number, or a string that can be converted to integer,
#'   with the state code used in the \code{STATE} column of the FARS data (for
#'   example \code{1} for Alabama).
#' @param year A number giving the year of the file to read.
#'
#' @return Called for its side effect, the plot. It returns \code{NULL}. If
#'   the state has no accidents in that year it shows the message
#'   \code{"no accidents to plot"} and returns \code{NULL} invisibly, without
#'   drawing.
#'
#' @section Errors:
#' Stops with the message \code{"invalid STATE number: <state.num>"} if
#' \code{state.num} is not one of the values in the \code{STATE} column of the
#' file. It also stops if the file for \code{year} does not exist (see
#' \code{\link{fars_read}}), or if the data lack the columns \code{STATE},
#' \code{LONGITUD} or \code{LATITUDE}.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' old_dir <- setwd(system.file("extdata", package = "farsr"))
#' fars_map_state(1, 2013)
#' setwd(old_dir)
#'
#' \dontrun{
#' # A state code that is not in the data gives an error
#' fars_map_state(99, 2013)
#' }
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