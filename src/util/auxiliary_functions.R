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


## ---- plot_exploratory_analysis

plot_exploratory_analysis <- function(target_data) {
  
  ds_aceitar_convite  <- target_data %>% 
    group_by(escala = aceitar_convite) %>% 
    summarize(total_aceitar_convite = n()) 
  ds_gostar_doces     <- target_data %>% 
    group_by(escala = gostar_doces) %>% 
    summarize(total_gostar_doces = n()) 
  ds_emocionar        <- target_data %>% 
    group_by(escala = emocionar) %>% 
    summarize(total_emocionar = n()) 
  ds_conhecer_pessoas <- target_data %>% 
    group_by(escala = conhecer_pessoas) %>% 
    summarize(total_conhecer_pessoas = n()) 
  ds_diversao_amigos  <- target_data %>% 
    group_by(escala = diversao_amigos) %>% 
    summarize(total_diversao_amigos = n()) 
  ds_gostar_fotos     <- target_data %>% 
    group_by(escala = gostar_fotos) %>% 
    summarize(total_gostar_fotos = n()) 
  ds_cansar_cerimonia <- target_data %>% 
    group_by(escala = cansar_cerimonia) %>% 
    summarize(total_cansar_cerimonia = n()) 
  ds_casamento_civil  <- target_data %>% 
    group_by(escala = casamento_civil) %>% 
    summarize(total_casamento_civil = n()) 
  
  plot_aceitar_convite <- ggplot(ds_aceitar_convite, 
                                 aes(x = escala, y = total_aceitar_convite)) +
    geom_bar(stat = "identity", width = .5, fill = "brown1") +
    labs(title = "Sempre aceito convites", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_gostar_doces <- ggplot(ds_gostar_doces, 
                              aes(x = escala, y = total_gostar_doces)) +
    geom_bar(stat="identity", width = .5, fill = "cadetblue") +
    labs(title = "O melhor são os doces", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_emocionar <- ggplot(ds_emocionar, 
                           aes(x = escala, y = total_emocionar)) +
    geom_bar(stat="identity", width = .5, fill = "chocolate") +
    labs(title = "Me emociono com casamentos", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_conhecer_pessoas <- ggplot(ds_conhecer_pessoas, 
                                  aes(x = escala, y = total_conhecer_pessoas)) +
    geom_bar(stat="identity", width = .5, fill = "cornflowerblue") +
    labs(title = "Oportunidade de conhecer pessoas", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_diversao_amigos <- ggplot(ds_diversao_amigos, 
                                 aes(x = escala, y = total_diversao_amigos)) +
    geom_bar(stat = "identity", width = .5, fill = "darkorchid") +
    labs(title = "Me divirto com amigos", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_gostar_fotos <- ggplot(ds_gostar_fotos, 
                              aes(x = escala, y = total_gostar_fotos)) +
    geom_bar(stat="identity", width = .5, fill = "aquamarine4") +
    labs(title = "Gosto de vídeos e fotos", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_cansar_cerimonia <- ggplot(ds_cansar_cerimonia, 
                                  aes(x = escala, y = total_cansar_cerimonia)) +
    geom_bar(stat="identity", width = .5, fill = "darkmagenta") +
    labs(title = "Cerimônias longas me cansam", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  plot_casamento_civil <- ggplot(ds_casamento_civil, 
                                 aes(x = escala, y = total_casamento_civil)) +
    geom_bar(stat="identity", width = .5, fill = "tomato3") +
    labs(title = "Casamento no civil não vale", 
         subtitle = "1 (discordo totalmente) a 5 (concordo totalmente)",
         y = '') +
    theme(plot.subtitle = element_text(size = 6))
  
  ggarrange(plot_aceitar_convite,
            plot_gostar_doces, 
            plot_emocionar,
            plot_conhecer_pessoas,
            plot_diversao_amigos,
            plot_gostar_fotos,
            plot_cansar_cerimonia,
            plot_casamento_civil,
            ncol = 2, 
            nrow = 4,
            label.y = '',
            common.legend = TRUE)
}

## ---- end-of-plot_exploratory_analysis
