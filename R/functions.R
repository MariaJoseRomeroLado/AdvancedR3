
#' Creates descriptive statistics
#'
#' @param data
#'
#' @return A data.frame/tibble
#'
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(dplyr::across( # given these columns do these actions
            value,
            list(
                mean = mean,
                sd = sd,
                median = median,
                iqr = IQR
            )
        )) %>%
        dplyr::mutate(dplyr::across(
            where(is.numeric),
            ~round(.x, digits = 1)
        ))
}

#' Plot for basic distribution of metabolite data.
#'
#' @param data The lipidomics dataset.
#'
#' @return A ggplot2 graph.
#'
plot_distributions <- function(data) {
    ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}
