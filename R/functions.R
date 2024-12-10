
#' Creates descriptive statistics
#'
#' @param data
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(dplyr::across( # given these columns do these actions
            value,
            list(
                mean = mean,
                sd = sd
            )
        )) %>%
        dplyr::mutate(dplyr::across(
            where(is.numeric),
            ~round(.x, digits = 1)
        ))
}
