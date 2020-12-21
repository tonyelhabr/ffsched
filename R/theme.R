
#' Theme for ggplots
#' 
#' @param ... Extra arguments to pass to `ggplot2::theme_update`
#' @export
theme_set_update_ffsched <- function(...) {
  if (!requireNamespace('ggplot2', quietly = TRUE)) {
    stop('`{ggplot2}` needed for this function to work. Please install it.', call. = FALSE)
  }
  
  if (requireNamespace('extrafont', quietly = TRUE)) {
    extrafont::loadfonts('win', quiet = TRUE)
    font <- 'Karla'
  } else {
    font <- ''
  }
  ggplot2::theme_set(ggplot2::theme_minimal())
  ggplot2::theme_update(
    ...,
    text = ggplot2::element_text(family = font),
    title = ggplot2::element_text(font, size = 14, color = 'gray20'),
    plot.title = ggplot2::element_text(font, face = 'bold', size = 18, color = 'gray20'),
    plot.title.position = 'plot',
    axis.text = ggplot2::element_text(font, size = 14, color = 'gray20'),
    axis.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.99),
    axis.line = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = 'gray80'),
    panel.grid.minor = ggplot2::element_line(color = 'gray80'),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10),
    plot.caption = ggplot2::element_text(font, size = 10, color = 'gray20', hjust = 0),
    plot.caption.position = 'plot',
    plot.tag = ggplot2::element_text(font, size = 12, color = 'gray20', hjust = 0), 
    legend.text = ggplot2::element_text(size = 14)
  )
  ggplot2::update_geom_defaults('text', list(family = font, size = 4, color = 'white'))
}