#' Quality function for dataset using tifyverse functions
#'
#' @param df a dataframe
#'
#' @return a data frame with the number of NA, NaN and infinite values per variables.
#' @export
#' @importFrom purrr map_dfc
#' @importFrom magrittr %>% 
#' @importFrom dplyr mutate select everything
#'
#' @examples
#' tidy_qual(iris)

tidy_qual <- function(df){
        purrr::map_dfc(.x = df, 
                       .f = function(f){ c(sum(is.na(f)),
                                           sum(is.nan(f)),
                                           sum(is.infinite(f))
                       ) }) %>%  
                dplyr::mutate(indic = c("n_na", "n_nan", "n_inf") ) %>% 
                dplyr::select(indic, dplyr::everything() )
}
