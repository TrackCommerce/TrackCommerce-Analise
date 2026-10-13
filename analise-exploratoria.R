library(ggplot2)
library(dplyr)
library(patchwork)
library(scales) 

caminho_silver <- "./track-commerce/silver"

pasta_destino <- "./"

meus_arquivos <- list.files(path = caminho_silver, 
                            pattern = ".*_sistema\\.csv", 
                            full.names = TRUE, 
                            recursive = TRUE)

normalizar <- function(x) {
  if(max(x, na.rm=TRUE) == min(x, na.rm=TRUE)) return(rep(0, length(x)))
  return((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) * 100)
}

for (ficheiro in meus_arquivos) {
  
  df <- read.csv(ficheiro, sep = ";")
  df[is.na(df)] <- 0
  df$timestamp <- as.POSIXct(df$timestamp)
  nome_maquina <- unique(df$hostname)[1]
  
  df$rede_bytes_total <- df$rede_upload_bytes_s + df$rede_download_bytes_s
  
  df_filtrado <- df %>%
    arrange(timestamp) %>%
    head(50)
  
 # Gráficos individuais 
  
  p_ind1 <- ggplot(df_filtrado, aes(x = timestamp, y = cpu_percent)) +
    geom_line(color = "blue", size = 1) + geom_point(color = "darkblue") +
    scale_x_datetime(date_labels = "%H:%M:%S") + 
    scale_y_continuous(limits = c(0, max(df_filtrado$cpu_percent, 1, na.rm = TRUE))) +
    labs(title = paste("1. Uso de CPU -", nome_maquina), x = "Horário", y = "CPU (%)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_ind2 <- ggplot(df_filtrado, aes(x = timestamp, y = ram_percent)) +
    geom_line(color = "red", size = 1) + geom_point(color = "darkred") +
    scale_x_datetime(date_labels = "%H:%M:%S") + 
    scale_y_continuous(limits = c(0, max(df_filtrado$ram_percent, 1, na.rm = TRUE))) +
    labs(title = "2. Uso de RAM", x = "Horário", y = "RAM (%)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_ind3 <- ggplot(df_filtrado, aes(x = timestamp, y = swap_percent)) +
    geom_line(color = "purple", size = 1) + geom_point(color = "purple4") +
    scale_x_datetime(date_labels = "%H:%M:%S") + 
    scale_y_continuous(limits = c(0, max(df_filtrado$swap_percent, 1, na.rm = TRUE))) +
    labs(title = "3. Uso de Swap", x = "Horário", y = "Swap (%)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_ind4 <- ggplot(df_filtrado, aes(x = timestamp, y = disco_iops_total)) +
    geom_line(color = "darkgreen", size = 1) + geom_point(color = "green") +
    scale_x_datetime(date_labels = "%H:%M:%S") + 
    scale_y_continuous(limits = c(0, max(df_filtrado$disco_iops_total, 1, na.rm = TRUE))) +
    labs(title = "4. Operações de Disco", x = "Horário", y = "Total IOPS") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_ind5 <- ggplot(df_filtrado, aes(x = timestamp, y = rede_conn_total)) +
    geom_line(color = "orange", size = 1) + geom_point(color = "darkorange") +
    scale_x_datetime(date_labels = "%H:%M:%S") + 
    scale_y_continuous(limits = c(0, max(df_filtrado$rede_conn_total, 1, na.rm = TRUE))) +
    labs(title = "5. Conexões de Rede", x = "Horário", y = "Total Conexões") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grafico_individuais <- p_ind1 / p_ind2 / p_ind3 / p_ind4 / p_ind5
  nome_saida_ind <- paste0(pasta_destino, "graficos_individuais_", nome_maquina, ".png")
  ggsave(nome_saida_ind, plot = grafico_individuais, width = 10, height = 25, dpi = 300)
  
 
  # Gráficos Cruzados 
  
  df_norm <- df_filtrado %>%
    mutate(
      swap_norm = normalizar(swap_percent),
      disco_iops_norm = normalizar(disco_iops_total),
      iowait_norm = normalizar(cpu_iowait_pct),
      latencia_norm = normalizar(disco_latencia_ms),
      load_norm = normalizar(load_1m),
      mem_norm = normalizar(ram_percent),
      cpu_sys_norm = normalizar(cpu_sys_pct),
      cached_norm = normalizar(ram_cached_gb),
      disco_leit_norm = normalizar(disco_iops_leitura),
      rede_total_norm = normalizar(rede_bytes_total),
      cpu_norm = normalizar(cpu_percent),
      close_wait_norm = normalizar(rede_close_wait),
      rede_down_norm = normalizar(rede_download_bytes_s),
      disco_escr_norm = normalizar(disco_escrita_bytes_s)
    )
  
  p_cruz1 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = swap_norm, color = "RAM"), size = 1) +
    geom_line(aes(y = disco_iops_norm, color = "Disco"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "1. RAM ↔ Disco (Falta de Memória)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz2 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = iowait_norm, color = "CPU"), size = 1) +
    geom_line(aes(y = latencia_norm, color = "Disco"), size = 1) +
    geom_line(aes(y = load_norm, color = "Load Average"), size = 1, linetype = "dashed") +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "2. Disco ↔ CPU (Gargalo no Disco)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz3 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = mem_norm, color = "RAM"), size = 1) +
    geom_line(aes(y = cpu_sys_norm, color = "CPU"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "3. RAM ↔ CPU (Pressão/Vazamento)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz4 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = cached_norm, color = "RAM"), size = 1) +
    geom_line(aes(y = disco_leit_norm, color = "Disco"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "4. RAM ↔ Disco (Perda de Cache)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz5 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = rede_total_norm, color = "Rede"), size = 1) +
    geom_line(aes(y = cpu_norm, color = "CPU"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "5. Rede ↔ CPU (Carga de Acessos)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz6 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = close_wait_norm, color = "Rede"), size = 1) +
    geom_line(aes(y = mem_norm, color = "RAM"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "6. Rede ↔ RAM (Vazamento de Conexões)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  p_cruz7 <- ggplot(df_norm, aes(x = timestamp)) +
    geom_line(aes(y = rede_down_norm, color = "Rede"), size = 1) +
    geom_line(aes(y = disco_escr_norm, color = "Disco"), size = 1) +
    scale_x_datetime(date_labels = "%H:%M:%S") + scale_y_continuous(limits = c(0, 100)) +
    labs(title = "7. Rede ↔ Disco (Uploads pesando no Storage)", x = "Horário", y = "Pico %") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
  
  grafico_cruzados <- p_cruz1 / p_cruz2 / p_cruz3 / p_cruz4 / p_cruz5 / p_cruz6 / p_cruz7
  nome_saida_cruz <- paste0(pasta_destino, "graficos_cruzados_", nome_maquina, ".png")
  ggsave(nome_saida_cruz, plot = grafico_cruzados, width = 12, height = 30, dpi = 300)
  
  message(paste("Gerado:", nome_saida_ind))
  message(paste("Gerado:", nome_saida_cruz))
}