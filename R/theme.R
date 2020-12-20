
#' Theme for ggplots
#' 
#' @export
theme_update_ff <- function() {
  ggplot2::theme_update(
    text = ggplot2::element_text(family = 'Karla', color = 'white'),
    title = ggplot2::element_text('Karla', size = 22, color = 'white'),
    plot.title = ggplot2::element_text(face = 'bold', size = 22),
    plot.subtitle = ggplot2::element_text(face = 'bold', size = 14, color = 'white'),
    # plot.margin = margin(10, 10, 10, 10),
    # panel.grid = element_blank(),
    axis.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.99),
    axis.text = ggplot2::element_text(family = 'Karla', color = 'white'),
    plot.caption = ggtext::element_markdown('Karla', size = 12, hjust = 0),
    plot.caption.position = 'plot',
    panel.spacing = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = 'gray30'),
    panel.grid.minor = ggplot2::element_line(color = 'gray30'),
    plot.background = ggplot2::element_rect(fill = 'gray10', color = NA),
    plot.tag = ggtext::element_markdown(size = 12, hjust = 1),
    plot.tag.position = c(1, 0.01),
    panel.background = ggplot2::element_blank()
  )
  ggplot2::update_geom_defaults('text', list(family = 'Karla', size = 5, color = 'white'))
}