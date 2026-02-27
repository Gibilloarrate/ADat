# Liberías que vamos a usar.
{
  install.packages(
    c(
      "ggplot2",
      "ggpubr",
      "dplyr",
      "mapSpain",
      "sf",
      "ggcorrplot",
      "editrules",
      "VIM",
      "plotly",
      "crosstalk",
      "patchwork",
      "scales"
    )
  )
  
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(mapSpain)
  library(sf)
  library(ggcorrplot)
  library(editrules)
  library(VIM)
  library(plotly)
  library(stringr)
  library(crosstalk)
  library(patchwork)
  library(scales)
}

# Leemos la tabla.
{
  elecciones_total <- read.table(
    file = "https://raw.githubusercontent.com/Gibilloarrate/ADat/main/datos/a_usar.tsv",
    header = TRUE,
    skip = 3,
    dec = ',',
    sep = '\t',
    quote = "",
    fileEncoding = "UTF-8",
    na.strings = c("#N/A", "N/A", "#N/D", "N/D", "", "NA"),
    comment.char = "",
    fill = TRUE,
    stringsAsFactors = TRUE,
  )
  
  # Eliminamos los datos de las secciones censales en el extranjero.
  elecciones_total <- elecciones_total[elecciones_total$secc_exterior != "Sí", ]
  
  # Nos quedamos con los datos que más nos interesan.
  elecciones <- elecciones_total[, c(1:8, 13:14, 21:22, 27, 31:47, 50, 52)]
}

# Arreglando variables:
{
  # Cualitativas nominales:
  
  nominales <- c("cod_sscc", "cod_ccaa", "cod_prov", "cod_mun", "cod_dist")
  elecciones[nominales] <- lapply(elecciones[nominales], factor)
  
  # Cualitativas ordinales:
  
  niveles1 <- c(
    "Rural pequeño",
    "Rural mediano",
    "Rural grande",
    "Urbano pequeño",
    "Urbano mediano",
    "Urbano grande"
  )
  elecciones$nivel_mun <- factor(elecciones$nivel_mun,
                                 levels = niveles1,
                                 ordered = TRUE)
  
  niveles2 <- c(
    "Primaria e inferior",
    "Primera etapa de Educación Secundaria y similar",
    "Segunda etapa de Educación Secundaria y  Postsecundaria no Superior",
    "Educación superior"
  )
  elecciones$nivel_comun_edu <- factor(elecciones$nivel_comun_edu,
                                       levels = niveles2,
                                       ordered = TRUE)
  
  niveles3 <- c(
    "Ocupaciones elementales",
    "Trabajadores cualificados y oficiales/operarios de nivel bajo",
    "Directores/gerentes y profesionales/técnicos de nivel medio o alto"
  )
  elecciones$nivel_comun_ocupados <- factor(elecciones$nivel_comun_ocupados,
                                            levels = niveles3,
                                            ordered = TRUE)
  
  # Identificando como continua una marcada como discreta:
  elecciones$renta <- as.numeric(elecciones$renta)
}

# Generamos variables y las reordenamos:
{
  elecciones$derecha_23 <- elecciones$pp + elecciones$vox
  elecciones$izquierda_23 <- elecciones$psoe + elecciones$sumar
  elecciones$derecha_19 <- elecciones$pp_19 + elecciones$vox_19 + elecciones$cs_19
  elecciones$izquierda_19 <- elecciones$psoe_19 + elecciones$up_19
  
  names(elecciones)[names(elecciones) == "abs"] <- "abstencion_23"
  names(elecciones)[names(elecciones) == "resto"] <- "resto_23"
  names(elecciones)[names(elecciones) == "abs_19"] <- "abstencion_19"
  primeras_12 <- names(elecciones)[1:12]
  
  variables_extra <- c(
    "parados",
    "censo",
    "pob_total_est",
    "edad",
    "renta",
    "derecha_23",
    "izquierda_23",
    "abstencion_23",
    "resto_23",
    "derecha_19",
    "izquierda_19",
    "abstencion_19",
    "resto_19"
  )
  
  columnas_finales <- c(primeras_12, variables_extra)
  
  elecciones <- elecciones[, columnas_finales]
}

# Vemos que los datos esten bien, y qué datos hay qué imputar.
{
  colSums(is.na(elecciones))
  str(elecciones)
  dim(elecciones)
  summary(elecciones)
}

# Imputacion de datos
{
  variables_id <- c(
    "cod_sscc",
    "cod_ccaa",
    "nom_ccaa",
    "cod_prov",
    "nom_prov",
    "cod_mun",
    "nom_mun",
    "cod_dist"
  )
  
  elecciones_a_imputar <- elecciones[, !(names(elecciones) %in% variables_id)]
  elecciones_a_imputar$nivel_mun <- factor(elecciones_a_imputar$nivel_mun, ordered = FALSE)
  elecciones_a_imputar$nivel_comun_edu <- factor(elecciones_a_imputar$nivel_comun_edu, ordered = FALSE)
  elecciones_a_imputar$nivel_comun_ocupados <- factor(elecciones_a_imputar$nivel_comun_ocupados, ordered = FALSE)
  
  elecciones_imputadas_temp <- kNN(elecciones_a_imputar, k = 5, imp_var = FALSE)
  
  elecciones_final <- cbind(elecciones[, variables_id], elecciones_imputadas_temp)
  elecciones_final$nivel_mun <- factor(elecciones_final$nivel_mun,
                                       levels = niveles1,
                                       ordered = TRUE)
  elecciones_final$nivel_comun_edu <- factor(elecciones_final$nivel_comun_edu,
                                             levels = niveles2,
                                             ordered = TRUE)
  elecciones_final$nivel_comun_ocupados <- factor(elecciones_final$nivel_comun_ocupados,
                                                  levels = niveles3,
                                                  ordered = TRUE)
  colSums(is.na(elecciones_final))
}

# Vemos que todo este bien
{
  summary(elecciones_final)
  
  
  reglas <- editmatrix(
    c(
      "renta >= 0",
      "edad >= 0",
      "edad <= 120",
      "censo >= 0",
      "parados >= 0",
      "ocupados_total >= 0",
      "pob_total_est >= 0",
      "abstencion_23 >= 0",
      "derecha_23 >= 0",
      "izquierda_23 >= 0",
      "resto_23 >= 0",
      "abstencion_23 <= censo"
    )
  )
  
  comprobacion <- violatedEdits(reglas, elecciones)
  summary(comprobacion)
}

# Estudio Outliers
elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
{
  evaluar_outliers <- function(x) {
    if (is.numeric(x)) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      limite_inf <- Q1 - 1.5 * IQR_val
      limite_sup <- Q3 + 1.5 * IQR_val
      num_outliers <- sum(x < limite_inf |
                            x > limite_sup, na.rm = TRUE)
      porcentaje <- round((num_outliers / length(na.omit(x))) * 100, 2)
      return(c(Cantidad = num_outliers, Porcentaje_pct = porcentaje))
    }
  }
  
  tabla_outliers <- as.data.frame(t(sapply(elecciones_cuant, evaluar_outliers)))
  tabla_outliers <- tabla_outliers[order(-tabla_outliers$Cantidad), ]
  print(tabla_outliers)
  
  #Hay bastantes variables con un alto porcentajo de outliers, sin embago tiene sentido las
  #dos primeras por las comunidades mas nacionalistas, la tercerda por barrios ricos..., el
  #resto tienen menos del 4% que no esta mal, pero todos estos datos son reales asi q no vamos a
  #eliminar ninguno.
}

# Cálculo correlaciones
{
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  matriz_cor_general <- cor(elecciones_cuant, method = "spearman")
  
  matriz_cor_general_redondeada <- round(matriz_cor_general, 2)
  
  print(matriz_cor_general_redondeada)
  
  
  ggcorrplot(
    matriz_cor_general,
    hc.order = TRUE,
    type = "lower",
    lab = TRUE,
    lab_size = 2.5,
    colors = c("red", "white", "blue"),
    title = "Matriz de Correlación de Spearman",
    ggtheme = theme_minimal()
  ) +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ))
}

#Calculamos la correlacion(dependencia con la prueba del chi cuadrado)
tabla_cruce <- table(elecciones_final$nivel_mun,
                     elecciones_final$nivel_comun_edu)
test_chi <- chisq.test(tabla_cruce)
print(test_chi)

# Estudio normalidad algunas variables
{
  #Histograma renta
  hist_renta <- gghistogram(
    elecciones_final$renta,
    fill = "lightblue",
    color = "darkblue",
    title = "Distribución de la Renta",
    xlab = "Renta Media (€)",
    ylab = "Frecuencia"
  )
  
  # El QQ-Plot renta
  qq_renta <- ggqqplot(
    elecciones_final$renta,
    title = "QQ-Plot de la Renta",
    ylab = "Muestra",
    xlab = "Teórico"
  )
  
  # Juntamos los dos gráficos en uno solo
  ggarrange(hist_renta, qq_renta, ncol = 2, nrow = 1)
  
  test_renta <- lillie.test(elecciones_final$renta)
  print(test_renta)
  #Concluimos q la renta no sigue una distribucion normal
}

# Análisis de la variable EDAD
{
  hist_edad <- gghistogram(
    elecciones_final$edad,
    fill = "lightgreen",
    color = "darkgreen",
    title = "Distribución de la Edad",
    xlab = "Edad Media",
    ylab = "Frecuencia"
  )
  
  qq_edad <- ggqqplot(
    elecciones_final$edad,
    title = "QQ-Plot de la Edad",
    ylab = "Muestra",
    xlab = "Teórico"
  )
  
  ggarrange(hist_edad, qq_edad, ncol = 2, nrow = 1)
  
  test_renta <- lillie.test(elecciones_final$edad)
  print(test_renta)
  #Concluimos q tampoco es normal pq hay una cantidad desproporcionada de secciones censales con una edad media altísima
}

# Calculamos algunas tasas
{
  elecciones_final$tasa_paro <- (
    elecciones_final$parados / (elecciones_final$ocupados_total + elecciones_final$parados)
  ) * 100
  elecciones_final$tasa_abs <- (elecciones_final$abstencion_23 / elecciones_final$censo) * 100
}

# Grafico con renta media por tipo de municipio
{
  # Primero calculamos la media exacta para cada grupo
  renta_por_municipio <- elecciones_final %>%
    filter(!is.na(nivel_mun)) %>%
    group_by(nivel_mun) %>%
    summarise(renta_media = weighted.mean(renta, w = censo, na.rm = TRUE)) # Media ponderada por el censo
  
  # Gráfico 1
  ggplot(renta_por_municipio,
         aes(x = nivel_mun, y = renta_media, fill = nivel_mun)) +
    geom_col(color = "black", alpha = 0.8)  +
    theme_minimal() + labs(
      title = "Renta Media según el Tipo de Municipio",
      subtitle = "",
      x = "",
      y = "Renta Media (€)"
    )
}

# Dispersión vs abstención en regresión lineal
{
  ggplot(elecciones_final, aes(x = tasa_paro, y = tasa_abs)) +
    geom_point(color = "darkgrey", alpha = 0.2) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(
      title = "Afecta el desempleo a la participación electoral",
      subtitle = "Relación entre la Tasa de Paro y la Tasa de Abstención en 2023",
      x = "Tasa de Paro (%)",
      y = "Tasa de Abstención (%)"
    )
}

# Abstención según renta, nube de puntos y por deciles.
{
  # 1. Preparación de datos (Igual que antes)
  datos_estaticos <- elecciones_final %>%
    filter(!is.na(renta), !is.na(tasa_abs), !is.na(censo)) %>%
    arrange(renta) %>%
    mutate(
      pob_acumulada = cumsum(as.numeric(censo)),
      pct_pob_acumulada = pob_acumulada / sum(as.numeric(censo)),
      decil_pob_renta = cut(
        pct_pob_acumulada,
        breaks = seq(0, 1, by = 0.1),
        labels = paste0("D", 1:10),
        include.lowest = TRUE
      )
    )
  
  color_fondo <- "gray92"
  
  # 2. Gráfico IZQUIERDA: Dispersión con COLORES POR DECIL (Discretos)
  p1 <- ggplot(datos_estaticos,
               aes(x = renta, y = tasa_abs, color = decil_pob_renta)) +
    # Aumentamos un poco el alpha y el tamaño para que el color sea más sólido
    geom_point(alpha = 0.5, size = 1.2) +
    scale_x_log10(labels = label_number(suffix = "€", big.mark = ".")) +
    # --- CLAVE: scale_color_viridis_d (discreta) en lugar de _c (continua) ---
    scale_color_viridis_d(option = "inferno") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = color_fondo, color = NA),
      panel.background = element_rect(fill = color_fondo, color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "none" # La leyenda la da el gráfico de la derecha
    ) +
    labs(
      title = "La participación electoral crece con la renta, pero es un efecto mucho más perceptible entre las rentas más extremas.",
      subtitle = "Renta neta media per cápita por sección censal comparada con su nivel de abstención en las elecciones generales de 2023.",
      x = "Renta (escala logarítimica, €)",
      y = "Abstención (%)"
    )
  
  # 3. Gráfico DERECHA: Barras (Igual para mantener consistencia)
  p2 <- datos_estaticos %>%
    group_by(decil_pob_renta) %>%
    summarise(abstencion_media = weighted.mean(tasa_abs, w = censo)) %>%
    ggplot(aes(x = decil_pob_renta, y = abstencion_media, fill = decil_pob_renta)) +
    geom_col(color = "white", linewidth = 0.8) +
    scale_fill_viridis_d(option = "inferno") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = color_fondo, color = NA),
      panel.background = element_rect(fill = color_fondo, color = NA),
      legend.position = "none"
    ) +
    labs(x = "Deciles de renta", y = "Abstención (%)")
  
  # Unir
  (p1 + p2) + plot_layout(widths = c(2, 1))
}

# Mapa interactivo
{
  abst_prov <- aggregate(cbind(abstencion_23, censo) ~ cod_prov,
                         data = elecciones_final,
                         FUN = sum)
  abst_prov$tasa <- (abst_prov$abstencion_23 / abst_prov$censo) * 100
  
  abst_prov$cod_prov <- str_pad(abst_prov$cod_prov, width = 2, pad = "0")
  
  mapa_sf <- esp_get_prov() %>% esp_move_can()
  mapa_listo <- merge(mapa_sf, abst_prov, by.x = "cpro", by.y = "cod_prov")
  
  mapa_base <- ggplot(mapa_listo) +
    geom_sf(aes(
      fill = tasa,
      text = paste(ine.prov.name, "| Abstención:", round(tasa, 1), "%")
    ),
    color = "white",
    linewidth = 0.2) +
    scale_fill_distiller(palette = "Blues",
                         direction = 1,
                         name = "% Abst.") +
    theme_void() +
    labs(title = "Abstención Electoral por Provincias (2023)")
  
  mapa_interactivo <- ggplotly(mapa_base, tooltip = "text") %>%
    layout(hovermode = "closest",
           hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
    style(hoveron = "fills") #Si no, sitúa mal el cursor
  
  mapa_interactivo
}
