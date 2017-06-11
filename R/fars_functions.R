#' fars_read
#'
#' This function reads a (compressed) csv file with data of fatal injuries in motor vehicle traffic crashes in the USA.
#'
#' @param filename A character string denoting the (compressed) file name 
#' 
#' @return This function returns the input data in a data frame table
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @note 
#' This function will give a message "file <filename> does not exist" when the input file does not exist
#'
#' @examples
#' fars_read("accident_2013.csv")
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2013.csv.zip")
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename
#'
#' This function creates, with the given input, the name of the data file with fatal injuries in motor vehicle traffic crashes in the USA.
#'
#' @param year An integer or character string denoting a year
#' 
#' @return This function returns the character string "accident_<input as numeric>.csv.bz2" 
#'         with <input as numeric> the given input converted to numeric
#'
#' @examples
#' make_filename(2013)
#' make_filename("2014")
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("data/accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function uses the data file with fatal injuries in motor vehicle traffic crashes in the USA based on the input and selects
#' all the months (via the MONTH field) contained in the data file with the year(s), which is/are used as input.
#'
#' @param years An array of or single integer or character string denoting an array of years or a single year
#' 
#' @return This function returns a list with repeating the month and per month the year 
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @note 
#' This function will give a message "invalid year: <year>" when the input file based on the input cannot be processed
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years("2014")
#' fars_read_years(c(2013,2014))
#'
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

#' fars_summarize_years
#'
#' This function uses the data file with fatal injuries in motor vehicle traffic crashes in the USA to summarizes the total number 
#' of fatal injuries per month/year.
#'
#' @param years An array of or single integer or character string denoting an array of years or a single year
#' 
#' @return This function returns a list with per month the total casualties per (input) year 
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years("2014")
#' fars_summarize_years(c(2013,2014))
#'
#' @export
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state 
#'
#' This function gives for a certain year a graphical display of the location of fatal injuries within a particular state based on the data 
#' of fatal injuries in motor vehicle traffic crashes in the USA.
#'
#' @param state.num	An integer or character string denoting the state number
#' @param year An integer or character string denoting a year
#' 
#' @return This function shows a graphical representation of the state and the locations within a state where fatal injuries occured 
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note 
#' This function will give a message "invalid STATE number: <state.num>" when the statenumber cannot be found within the data file based on the input of the year and will stop processing
#' This function will give a message "no accidents to plot" when no fatal injuries took place in a certain state in a certain year
#'
#' @examples
#' fars_map_state(1,2013)
#' fars_map_state("1",2013)
#' fars_map_state("1","2013")
#'
#' @export
#'
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
