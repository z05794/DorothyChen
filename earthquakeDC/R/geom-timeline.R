#' Timeline of earthquakes
#'
#' @inheritParams ggplot2::geom_point
#'
#' @export
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(country %in% c("USA", "MEXICO"), year(datetime) > 2000) %>%
#'    ggplot(aes(x = datetime,
#'               y = country,
#'               color = as.numeric(deaths),
#'               size = as.numeric(eq_primary)
#'    )) +
#'    geom_timeline() +
#'    theme_timeline()
#' }

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @importFrom ggplot2 draw_key_blank
#' @importFrom dplyr %>% group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList

GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                               shape = 21, fill = "grey", stroke = 0.5),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {
      
      if (!("y" %in% colnames(data))) {
        data$y <- 0.15
      }
      coords <- coord$transform(data, panel_scales)
      
      points <- grid::pointsGrob(
        coords$x, coords$y,
        pch = coords$shape, size = unit(coords$size / 4, "char"),
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(coords$colour, coords$alpha)
        )
      )
      y_lines <- unique(coords$y)
      
      lines <- grid::polylineGrob(
        x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
        y = unit(c(y_lines, y_lines), "npc"),
        id = rep(seq_along(y_lines), 2),
        gp = grid::gpar(col = "grey",
                        lwd = .pt)
      )
      grid::gList(points, lines)
    }
  )

