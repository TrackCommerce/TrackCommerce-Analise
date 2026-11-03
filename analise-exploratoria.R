library(ggplot2)
library(dplyr)

# 1. Leitura dos dados
df_normal <- read.csv("dia_normal_sistema_limpo.csv", sep = ";")
df_bf <- read.csv("black_friday_sistema_limpo.csv", sep = ";")

# Converter timestamps e definir o texto do cabeçalho azul
df_normal$timestamp <- as.POSIXct(df_normal$timestamp)
df_normal$Cabecalho <- "Dia Normal"

df_bf$timestamp <- as.POSIXct(df_bf$timestamp)
df_bf$Cabecalho <- "Black Friday"


tema_cabecalho_azul <- theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#2C3E50", color = "#2C3E50"), 
    strip.text = element_text(color = "white", face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15))
  )


# 1. GRÁFICO CPU - DIA NORMAL
p_cpu_normal <- ggplot(df_normal, aes(x = timestamp, y = cpu_percent)) +
  geom_line(color = "#2C3E50", linewidth = 0.6) + 
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "orange", linewidth = 0.6) +
  facet_wrap(~ Cabecalho) + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Comparativo de Estresse da CPU: Operação Padrão vs Black Friday",
       subtitle = "Linha Laranja: Atenção (60%) | Linha Vermelha: Gargalo Crítico (80%)",
       x = "Horário da Operação", y = "Uso de CPU (%)") +
  tema_cabecalho_azul

ggsave("1_cpu_dia_normal.png", plot = p_cpu_normal, width = 12, height = 7, dpi = 300)

# 2. GRÁFICO CPU - BLACK FRIDAY
p_cpu_bf <- ggplot(df_bf, aes(x = timestamp, y = cpu_percent)) +
  geom_line(color = "#C0504D", linewidth = 0.6) + 
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "orange", linewidth = 0.6) +
  facet_wrap(~ Cabecalho) + 
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Comparativo de Estresse da CPU: Operação Padrão vs Black Friday",
       subtitle = "Linha Laranja: Atenção (60%) | Linha Vermelha: Gargalo Crítico (80%)",
       x = "Horário da Operação", y = "Uso de CPU (%)") +
  tema_cabecalho_azul

ggsave("2_cpu_black_friday.png", plot = p_cpu_bf, width = 12, height = 7, dpi = 300)

# 3. GRÁFICO CRUZADO RAM E CPU

p_correlacao_bf <- ggplot(df_bf, aes(x = timestamp)) +
  geom_line(aes(y = ram_percent, color = "1. Uso de Memória RAM"), linewidth = 0.6) + 
  geom_line(aes(y = cpu_percent, color = "2. Uso de CPU"), linewidth = 0.6) + 
  
  geom_hline(yintercept = 90, linetype = "dashed", color = "darkred", linewidth = 0.6) +
  
  geom_hline(yintercept = 75, linetype = "dashed", color = "orange", linewidth = 0.6) +
  
  facet_wrap(~ Cabecalho) + 
  scale_y_continuous(limits = c(0, 100)) +
  
  scale_color_manual(values = c("1. Uso de Memória RAM" = "#DF5800", 
                                "2. Uso de CPU" = "#2C3E50")) +
  
  labs(title = "Causa e Efeito: RAM ↔ CPU",
       subtitle = "O esgotamento da memória força o consumo da CPU ao limite",
       x = "Horário da Operação", 
       y = "Utilização de Recursos (%)",
       color = "Métrica Monitorada:") +
  tema_cabecalho_azul +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14)
  )

ggsave("3_correlacao_ram_cpu_bf.png", plot = p_correlacao_bf, width = 12, height = 7, dpi = 300)
