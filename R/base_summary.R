#' Statistical summary using mostly base functions 
#'
#' @param df a dataframe
#'
#' @return a data.frame with summary statistics
#' @export
#' @importFrom magrittr %>% 
#' @importFrom tibble as_tibble
#'
#' @examples
#' base_summary(mtcars)

base_summary <- function(df){
        
        if(sum( unlist( lapply(df, is.numeric) ) ) < ncol(df)){stop("Some variable is not numeric.")}
        
        lapply(df, function(x) c("mean" = round(mean(x), 2), 
                                "var" = round(var(x), 2),
                                "sd" = round(sd(x), 2),
                                "min" = min(x),
                                "1q" = quantile(x, .25, names = FALSE),
                                "median" = quantile(x, .5, names = FALSE),
                                "3q" = quantile(x, .75, names = FALSE),
                                "max" = max(x)
        )
        ) %>% 
                cbind.data.frame() %>% 
                t() %>% tibble::as_tibble(rownames = "variable")
}