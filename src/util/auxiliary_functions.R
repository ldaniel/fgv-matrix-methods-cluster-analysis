## ---- ClearRStudioEnvironment

# functions -----------------------------------------------------------
ClearRStudioEnvironment <- function() {
  
  # clear environment and memory
  rm(list=ls())
  invisible(gc())
  
  # clear console screen
  cat("\014")
  
  # clear plots
  while (!is.null(dev.list()))  
    dev.off()
}

## ---- end-of-ClearRStudioEnvironment


## ---- find_best_k_kmeans

find_best_k_kmeans <- function(x, k_limit, nstart) {
  
  metrics <- tibble()
  output <- list()
  
  for (k in seq(1, k_limit, 1)) {
    temp <- kmeans(x, k, nstart = nstart)
    df_temp <- tibble(k = k,
                      withinss = temp$tot.withinss,
                      betweenss = temp$betweenss,
                      best_fit = abs(withinss - betweenss))
    metrics <- bind_rows(metrics, df_temp)
  }

  output$best_k <- which.min(metrics$best_fit)
  
  plot_best_k <- ggplot(data = metrics, aes(x = k)) +
    geom_point(aes(y = withinss), color = 'blue') +
    geom_point(aes(y = betweenss), color = 'red') +
    geom_line(aes(y = withinss), color = 'blue') +
    geom_line(aes(y = betweenss), color = 'red') +
    geom_segment(aes(x = output$best_k,
                     y = metrics$withinss[output$best_k], 
                     xend = output$best_k, 
                     yend = metrics$betweenss[output$best_k]),
                 linetype = "dashed", color = "black") +
    geom_label(aes(x = output$best_k + as.integer((max(metrics$k) - output$best_k) * 0.15),
                   y = metrics$withinss[output$best_k] + 
                     as.integer(
                       abs(metrics$withinss[output$best_k] - metrics$betweenss[output$best_k]) * 0.5
                       ),
                   label = paste('best k is ', output$best_k))) +
    labs(title = 'Best k-means cluster size',
         y = 'withinss vs betweenss',
         x = 'k = cluster size')
  
  output$metrics <- metrics
  
  output$plot <- plot_best_k

  return(output)
  
}

## ---- end-of-find_best_k_kmeans
