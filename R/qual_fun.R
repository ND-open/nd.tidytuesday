#' Quality function for dataset using base functions
#'
#' @param df a dataframe
#'
#' @return a data frame with the number of NA, NaN and infinite values per variables.
#' @export
#'
#' @examples
#' qual_fun(iris)

qual_fun <- function(df){
        ll <- lapply(df, function(var) c(n_na = sum(is.na(var)),
                                  n_nan = sum(is.nan(var)),
                                  n_inf = sum(is.infinite(var))
        )
        )
        ll <- cbind.data.frame(ll)
        
        return(ll)
}