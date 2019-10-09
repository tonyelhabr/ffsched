

scale_color_ff <- function(...) {
  # ggthemes::scale_color_hc()
  # ggthemes::scale_color_gdocs()
  ggthemes::scale_color_tableau(palette = 'Classic 10 Light', ...)
}

scale_fill_ff <- function(...) {
  ggthemes::scale_fill_tableau(palette = 'Classic 10 Light')
}

theme_ff <-
  function(...,
           family = 'Arial',
           plot_title_size = 16,
           size = 14,
           margin = 7.5,
           plot_margin = margin(2 * margin, 2 * margin, 2 * margin, 2 * margin, unit = 'pt')) {
    hrbrthemes::theme_modern_rc(
      base_family = family,
      subtitle_family = family,
      caption_family = family,
      plot_title_margin = margin,
      subtitle_margin = margin,
      caption_margin = margin,
      plot_margin = margin(2 * margin, margin, margin, margin),
      plot_title_size = plot_title_size,
      subtitle_size = size,
      base_size = size,
      axis_title_size = size,
      caption_size = size,
      ...
    )
  }
