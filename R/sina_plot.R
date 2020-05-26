#' Make a sina plot of the Shapley values computed using shapr.
#'
#' @param explanation shapr list containing an explanation produced by shapr::explain
#'
#' @author Ioan Gabriel Bucur
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @seealso \link[SHAPforxgboost]{shap.plot.summary}
#' 
#' @details Function inspired by \link[SHAPforxgboost]{shap.plot.summary}. 
#' Copyright © 2020 - Yang Liu & Allan Just
#' 
sina_plot <- function(explanation) {
  
  shapley_values <- explanation$dt[, -1, drop = FALSE]
  X_values <- explanation$x_test
  
  data_long <- explanation$x_test %>%
    as.data.table() %>%
    pivot_longer(everything()) %>%
    bind_cols(explanation$dt %>%
                select(-none) %>%
                pivot_longer(everything()) %>%
                select(-name) %>%
                rename(shap = value)) %>%
    group_by(name) %>%
    arrange(name) %>%
    mutate(mean_value = mean(value)) %>%
    mutate(std_value = (value - min(value)) / (max(value) - min(value)))
  
  x_bound <- max(abs(max(data_long$shap)), abs(min(data_long$shap))) * 1.1
  
  ggplot(data = data_long) + 
    coord_flip(ylim = c(-x_bound, x_bound)) + 
    geom_hline(yintercept = 0) + 
    ggforce::geom_sina(
      aes(x = name, y = shap, color = std_value), 
      method = "counts", maxwidth = 0.7, alpha = 0.7
    ) +
    ggthemes::theme_clean() + theme(
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "bottom", legend.title = element_text(size = 10), 
      legend.text = element_text(size = 8), axis.title.x = element_text(size = 10)
    ) + 
    geom_text(
      data = unique(data_long[, c("name", "mean_value")]), 
      aes(x = name, y = -Inf, label = sprintf("%.3f", mean_value)), 
      size = 3, alpha = 0.7, hjust = -0.3, vjust = -0.7, fontface = "bold"
    ) +
    scale_color_gradient(
      low = "#FF0000", high = "#FFFF00", 
      breaks = c(0, 1), labels = c(" Low", "High "), 
      guide = guide_colorbar(barwidth = 12, barheight = 0.3)
    ) +
    # scale_x_discrete(
    #   limits = rev(levels(data_long$variable)), 
    #   labels = label.feature(rev(levels(data_long$variable)))) + 
    labs(y = "SHAP value (impact on model output)", 
         x = "", color = "Feature value  ")
}

