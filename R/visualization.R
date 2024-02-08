#' Function to plot the proportion of missing predictors
#'
#' @details Function to plot the proportion of missing predictors
#' @param data DataFrame to plot
#' @importFrom rlang .data
#' @export
f_plot_missing_data <- function(data) {

  data <- data |>
    dplyr::mutate(dplyr::across(-.data$Year, ~dplyr::if_else(is.na(.), 0, 1))) |>
    dplyr::group_by(.data$Year) |>
    dplyr::summarise_all(.funs = mean, na.rm = TRUE) |>
    tidyr::pivot_longer(-.data$Year, names_to="variable", values_to="value")

  # plot missing data
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data$Year, y =
                                                      forcats::fct_rev(.data$variable))) +
    ggplot2::geom_tile(ggplot2::aes(fill=.data$value), colour = "grey80", show.legend = TRUE) +
    ggplot2::ylab("") +
    ggplot2::scale_fill_gradientn(colours = c("red", "green"),
                         values = scales::rescale(c(0, 0.6, 1)),
                         labels = scales::percent,
                         limits = c(0, 1)) +
    ggplot2::facet_grid(~.data$Year, space="free", scales="free", switch="x") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_text(colour="grey20")) +
    ggplot2::labs(fill = "% of district months with complete predictor data") +
    ggplot2::theme(strip.placement = "outside",
          strip.background = ggplot2::element_rect(fill=NA, colour="grey50"),
          panel.spacing = ggplot2::unit(0,"cm"), strip.text.y = ggplot2::element_text(angle = 0),
          strip.text.x = ggplot2::element_text(vjust=0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.key.width = ggplot2::unit(2, "cm")) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5))

  return(plot)
}
