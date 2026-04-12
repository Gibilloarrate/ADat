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
      "scales",
      "stringr",
      "nortest",
      "psych",
      "e1071"
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
  library(nortest)
  library(psych)
  library(e1071)
}

# Leemos la tabla.
{
  elecciones_total <- read.table(
    file = "C:/Users/Gutimong/Downloads/Trabajo ADat - a rellenar_en proceso (1).tsv",
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
  
  # Comprobamos las reglas sobre el dataset YA imputado (elecciones_final),
  # para verificar que la imputación no ha generado valores incoherentes.
  comprobacion <- violatedEdits(reglas, elecciones_final)
  summary(comprobacion)
}

# Análisis exploratorio formal: descriptivos por tipo de variable
{
  # ---- 1) Variables CUANTITATIVAS: media, mediana, sd, mín, máx, asimetría y curtosis ----
  # psych::describe nos da los estadísticos clásicos de un vistazo, y añadimos
  # asimetría (skewness) y curtosis (kurtosis) con e1071 para apoyar la
  # discusión sobre normalidad de las variables continuas.
  vars_num <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  descriptivos_num <- psych::describe(vars_num)[, c("n", "mean", "sd", "median",
                                                    "min", "max", "skew", "kurtosis")]
  descriptivos_num <- round(descriptivos_num, 2)
  print(descriptivos_num)
  
  # ---- 2) Variables CUALITATIVAS: tablas de frecuencias absolutas y relativas ----
  # Para nominales y ordinales mostramos frecuencias relativas (%) — más
  # informativas que el conteo bruto, sobre todo con tantas secciones censales.
  vars_cual <- c("nivel_poblacion_mun", "nivel_densidad_mun",
                 "nivel_mun", "nivel_comun_edu", "nivel_comun_ocupados")
  vars_cual <- intersect(vars_cual, names(elecciones_final))
  
  for (v in vars_cual) {
    cat("\n--- Frecuencias:", v, "---\n")
    tabla_abs <- table(elecciones_final[[v]], useNA = "ifany")
    tabla_rel <- round(prop.table(tabla_abs) * 100, 2)
    print(rbind(Absoluta = tabla_abs, Relativa_pct = tabla_rel))
  }
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
  
  
  # Calculamos la matriz de p-valores asociados a cada correlación.
  # cor_pmat() es de ggcorrplot y usa el test de correlación correspondiente.
  # Pasársela a ggcorrplot con sig.level=0.05 e insig="blank" hace que las
  # correlaciones no significativas aparezcan en blanco — más honesto que
  # pintar TODAS las correlaciones como si fueran reales.
  matriz_pvalores <- cor_pmat(elecciones_cuant, method = "spearman")
  
  ggcorrplot(
    matriz_cor_general,
    hc.order = TRUE,
    type = "lower",
    lab = TRUE,
    lab_size = 2.5,
    colors = c("red", "white", "blue"),
    title = "Matriz de Correlación de Spearman",
    ggtheme = theme_minimal(),
    p.mat = matriz_pvalores,
    sig.level = 0.05,
    insig = "blank"
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
  
  # Test de Lilliefors sobre la edad (corregimos el nombre, antes reusaba test_renta por copia-pega)
  test_edad <- lillie.test(elecciones_final$edad)
  print(test_edad)
  #Concluimos q tampoco es normal pq hay una cantidad desproporcionada de secciones censales con una edad media altísima
}

# Calculamos algunas tasas
{
  elecciones_final$tasa_paro <- (
    elecciones_final$parados / (elecciones_final$ocupados_total + elecciones_final$parados)
  ) * 100
  elecciones_final$tasa_abs <- (elecciones_final$abstencion_23 / elecciones_final$censo) * 100
}

# Tests de normalidad sobre varias variables continuas
{
  # Aplicamos Lilliefors (Kolmogorov-Smirnov con corrección de Lilliefors) a un
  # conjunto representativo de variables continuas. Con N tan grande,
  # shapiro.test no es aplicable (límite ~5000), por eso usamos lillie.test.
  vars_normalidad <- c("renta", "edad", "sexo", "tasa_paro", "tasa_abs",
                       "censo", "pob_total_est")
  vars_normalidad <- intersect(vars_normalidad, names(elecciones_final))
  
  resultados_normalidad <- data.frame(
    Variable = character(),
    Estadistico_D = numeric(),
    p_valor = numeric(),
    Asimetria = numeric(),
    Curtosis = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (v in vars_normalidad) {
    x <- na.omit(elecciones_final[[v]])
    if (length(x) > 4 && is.numeric(x)) {
      tt <- lillie.test(x)
      resultados_normalidad <- rbind(
        resultados_normalidad,
        data.frame(
          Variable = v,
          Estadistico_D = round(unname(tt$statistic), 4),
          p_valor = signif(tt$p.value, 4),
          Asimetria = round(e1071::skewness(x), 3),
          Curtosis = round(e1071::kurtosis(x), 3)
        )
      )
    }
  }
  print(resultados_normalidad)
  # En todos los casos p < 0.05 → rechazamos normalidad. La asimetría y la
  # curtosis nos dan una idea de POR QUÉ se rechaza (colas pesadas, sesgo, etc.)
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
  # 1. Preparación de datos
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
  
  # 2. Gráfico IZQUIERDA: Dispersión con COLORES POR DECIL
  p1 <- ggplot(datos_estaticos,
               aes(x = renta, y = tasa_abs, color = decil_pob_renta)) +
    # Aumentamos un poco el alpha y el tamaño para que el color sea más sólido
    geom_point(alpha = 0.5, size = 1.2) +
    scale_x_log10(labels = label_number(suffix = "€", big.mark = ".")) +
    
    scale_color_viridis_d(option = "inferno") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = color_fondo, color = NA),
      panel.background = element_rect(fill = color_fondo, color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    labs(
      title = "La participación electoral crece con la renta, pero es un efecto mucho más perceptible entre las rentas más extremas.",
      subtitle = "Renta neta media per cápita por sección censal comparada con su nivel de abstención en las elecciones generales de 2023.",
      x = "Renta (escala logarítimica, €)",
      y = "Abstención (%)"
    )
  
  # 3. Gráfico DERECHA: Barras
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
    style(hoveron = "fills")
  
  mapa_interactivo
}

# Librerías necesarias para el Tema 2
# (Sacamos esto fuera del bloque del mapa: antes estaban anidadas dentro
# de la llave del mapa, lo cual hacía el código muy difícil de leer y
# provocaba que TODO el análisis posterior viviera dentro del bloque del mapa.)
{
  install.packages(c("DescTools", "vcd"))
  library(DescTools)
  library(vcd)
}


# 1. Creación de Tablas de Contingencia
{
  # Las tablas de contingencia resumen la relación entre varias variables categóricas
  
  # Tabla 1; Nominal vs Nominal; Comunidad Autónoma vs Provincia
  tabla_geo <- table(elecciones_final$nom_ccaa, elecciones_final$nom_prov)
  tabla_geo
  
  # Tabla 2; Nominal vs Ordinal; Provincia vs Nivel Educativo
  tabla_prov_edu <- table(elecciones_final$nom_prov, elecciones_final$nivel_comun_edu)
  tabla_prov_edu
  # Tabla 3; Ordinal vs Ordinal; Tipo de Municipio vs Nivel Educativo
  tabla_mun_edu <- table(elecciones_final$nivel_mun, elecciones_final$nivel_comun_edu)
  tabla_mun_edu
  # Tabla 4; Ordinal vs Ordinal Educación vs Ocupación
  tabla_edu_ocup <- table(elecciones_final$nivel_comun_edu, elecciones_final$nivel_comun_ocupados)
  tabla_edu_ocup
}

# 2. Medidas de Independencia
{
  # Test chi-cudriado para los pares de variables anteriores
  
  print("--- Contrastes Chi-cuadrado ---")
  # Usamos correct = FALSE porque las tablas son mayores a 2x2
  # (la corrección de Yates solo aplica en tablas 2x2).
  print(chisq.test(tabla_geo, correct = FALSE))
  print(chisq.test(tabla_prov_edu, correct = FALSE))
  print(chisq.test(tabla_mun_edu, correct = FALSE))
  print(chisq.test(tabla_edu_ocup, correct = FALSE))
  
  #Test G para las variables anteriores
  print("--- Contrastes G-test (Test del Cociente de Verosimilitudes) ---")
  # Usamos DescTools en lugar de MASS
  print(GTest(tabla_prov_edu))
  print(GTest(tabla_mun_edu))
  print(GTest(tabla_geo))
  print(GTest(tabla_edu_ocup))
}
#En todos los test salen p-valores muy pequeños por lo que se rechaza independencia

# 3. Medidas de Asociación: Escala Nominal
{
  # Para estudiar el grado de relación existente eliminando el efecto del tamaño muestral
  # Las aplicamos a las tablas donde al menos una variable es nominal
  
  print("--- Medidas Nominales: CCAA vs Provincia ---")
  print(paste("C de Pearson:", round(ContCoef(tabla_geo), 3)))
  print(paste("V de Cramer:", round(CramerV(tabla_geo), 3)))
  #Muy correlacionadas
  
  
  print("--- Medidas Nominales: Provincia vs Nivel Educativo ---")
  print(paste("C de Pearson:", round(ContCoef(tabla_prov_edu), 3)))
  print(paste("V de Cramer:", round(CramerV(tabla_prov_edu), 3)))
  #Relacion media baja
  
  
  # Con esta funcion, hacemos todo junto
  
  print(assocstats(tabla_prov_edu))
  print(assocstats(tabla_geo))
}

# 4. Medidas de Asociación: Escala Ordinal
{
  # Lo aplicamosa los cruces entre nivel_mun, nivel_comun_edu y nivel_comun_ocupados.
  
  print("--- Medidas Ordinales: Tipo de Municipio vs Nivel Educativo ---")
  print(paste("Gamma:", round(GoodmanKruskalGamma(tabla_mun_edu)[1], 3)))
  print(paste("Somers D (C|R):", round(SomersDelta(tabla_mun_edu, direction="column")[1], 3)))
  print(paste("Kendall Tau-b:", round(KendallTauB(tabla_mun_edu)[1], 3)))
  print(paste("Kendall Tau-c:", round(StuartTauC(tabla_mun_edu)[1], 3)))
  #Relacion media
  
  print("--- Medidas Ordinales: Nivel Educativo vs Nivel de Ocupación ---")
  print(paste("Gamma:", round(GoodmanKruskalGamma(tabla_edu_ocup)[1], 3)))
  print(paste("Somers D (C|R):", round(SomersDelta(tabla_edu_ocup, direction="column")[1], 3)))
  print(paste("Kendall Tau-b:", round(KendallTauB(tabla_edu_ocup)[1], 3)))
  print(paste("Kendall Tau-c:", round(StuartTauC(tabla_edu_ocup)[1], 3)))
  #Relacion media alta
}


# Nuevas librerías para el PCA
{
  install.packages("factoextra")
  library(factoextra)
}

# 1. Preparación de los datos
{
  # Seleccionamos estrictamente las variables numéricas de nuestro dataset imputado.
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  # Ejecutamos el Análisis de Componentes Principales.
  # scale = TRUE es necesario para escalar las variables a varianza unitaria.
  pca_result <- prcomp(elecciones_cuant, scale = TRUE)
  pca_result
  print(get_eigenvalue(pca_result))
}

# 2. Elección del número de componentes principales
{
  # Calculamos los autovalores (varianzas de cada componente) y la
  # proporción de varianza explicada (PVE). Antes el código intentaba
  # usar pr.var y pve sin haberlas definido, lo que daba error.
  pr.var <- pca_result$sdev^2
  pve    <- pr.var / sum(pr.var)
  
  # CRITERIO 1: Criterio de Kaiser
  # Nos quedamos con las componentes cuyo autovalor sea > 1.
  print("--- Autovalores (Varianzas) ---")
  print(round(pr.var, 4))
  print(paste("Componentes con autovalor > 1 (Kaiser):", sum(pr.var > 1)))
  #Hay 3 autovalores menor que 1
  
  # CRITERIO 2: Varianza acumulada
  componentes_75_80 <- which(cumsum(pve) >= 0.75)[1]
  print(paste("Número de componentes para alcanzar al menos el 75% de varianza:", componentes_75_80))
  #Nos salen tres dimensiones a escoger
}

# 3. Representación Gráfica
{
  # Configuramos la ventana para ver los dos gráficos base.
  par(mfrow = c(1, 2))
  
  # Gráfico de la varianza explicada.
  plot(pve, xlab = "Componente Principal",
       ylab = "Proporción de Varianza Explicada",
       ylim = c(0, 1), type = "b", col = "blue", pch = 19)
  
  # Gráfico de la varianza acumulada.
  plot(cumsum(pve), xlab = "Componente Principal",
       ylab = "PVE Acumulado",
       ylim = c(0, 1), type = "b", col = "red", pch = 19)
  
  par(mfrow = c(1, 1)) # Restauramos la ventana gráfica
  
  # CRITERIO 3: Gráfica de sedimentación
  # Buscamos el "codo" o cambio de tendencia donde los autovalores se estabilizan.
  grafico_sedimentacion <- fviz_eig(pca_result, addlabels = TRUE,
                                    ylim = c(0, 50),
                                    main = "Gráfica de Sedimentación (Scree Plot)")
  print(grafico_sedimentacion)
}

# 4. Biplot y Contribución de Variables (Solo Variables, sin observaciones)
{
  # 4 Biplot en el que solo se ven las variables
  biplot_mejorado <- fviz_pca_var(pca_result,
                                  col.var = "contrib", # Colorear variables según su contribución
                                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                  repel = TRUE,        # Evita que el texto de las variables se solape
                                  title = "Círculo de Correlaciones y Contribución de Variables")
  print(biplot_mejorado)
  
  # Visualización de la contribución exacta de cada variable
  # Muestra en un gráfico de barras qué variables aportan más a las dos primeras componentes
  
  grafico_contribucion <- fviz_contrib(pca_result, choice = "var", axes = 1:3, top = 16) +
    labs(title = "Contribución de variables a las 3 primeras componentes",
         x = "Dimensiones",
         y = "Porcentaje de varianza explicada")
  print(grafico_contribucion)
}

# 5. Loadings de las 3 primeras componentes principales
{
  # Los loadings (cargas) son los coeficientes con los que cada variable
  # original entra en cada componente. Sirven para INTERPRETAR cada eje
  # ("¿qué representa la PC1?"). Los ordenamos por valor absoluto en PC1
  # para ver primero las variables más influyentes en el primer eje.
  loadings_pca <- round(pca_result$rotation[, 1:3], 3)
  loadings_pca <- loadings_pca[order(-abs(loadings_pca[, 1])), ]
  print("--- Loadings de PC1, PC2, PC3 (ordenados por |PC1|) ---")
  print(loadings_pca)
  # NOTA: a partir de estos loadings se nombran las dimensiones en la memoria.
}

# 6. Proyección de los individuos sobre el plano PC1-PC2 coloreados por nivel_mun
{
  # Esto añade una capa de interpretación al PCA: vemos si las secciones
  # censales se separan en el plano de los dos primeros ejes según el tipo
  # de municipio (rural pequeño → urbano grande). Si el PCA tiene sentido,
  # los grupos deberían quedar relativamente ordenados a lo largo de PC1.
  grafico_ind_pca <- fviz_pca_ind(pca_result,
                                  geom.ind = "point",
                                  col.ind = elecciones_final$nivel_mun,
                                  palette = "jco",
                                  addEllipses = TRUE,
                                  ellipse.type = "confidence",
                                  legend.title = "Tipo de municipio",
                                  title = "PCA: secciones censales por tipo de municipio")
  print(grafico_ind_pca)
}

# Nuevas librerías para el Análisis de Correspondencias
{
  # install.packages(c("FactoMineR", "factoextra", "corrplot", "gplots"))
  library(FactoMineR)
  library(factoextra)
  library(corrplot)
  library(gplots)
}

# ANÁLISIS DE CORRESPONDENCIAS SIMPLE
{
  # Vamos a analizar la relación entre la Comunidad Autónoma y el Nivel de Ocupación.
  # Primero creamos la tabla de contingencia.
  tabla_ca <- table(elecciones_final$nom_ccaa, elecciones_final$nivel_comun_ocupados)
  
  # Hacemos el CA
  res.ca <- CA(tabla_ca, graph = FALSE)
  print(res.ca)
  
  # Valores propios y Varianza Explicada
  print("--- Autovalores y Varianza Explicada (CA) ---")
  print(get_eigenvalue(res.ca))
  
  # Scree plot para ver cuántas dimensiones nos quedamos
  grafico_scree_ca <- fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50),
                                     main = "Scree Plot - Correspondencias Simple") +
    labs(x = "Dimensiones",
         y = "Porcentaje de varianza explicada")
  print(grafico_scree_ca)
  
  # Calculamos coord, cos2, contrib
  filas_ca <- get_ca_row(res.ca)
  
  print("--- Coordenadas de los puntos de fila (CCAA) ---")
  print(head(filas_ca$coord))
  
  print("--- Calidad de representación (Cos2) ---")
  print(head(filas_ca$cos2))
  
  print("--- Contribuciones a las dimensiones (%) ---")
  print(head(filas_ca$contrib))
  
  #Gráfico cos2 repecto las comunidades
  corrplot(filas_ca$cos2, is.corr = FALSE, main = "Calidad de representación (Cos2) - CCAA")
  
  # 1.4 Biplot
  biplot_asim <- fviz_ca_biplot(res.ca, map = "colgreen", arrow = c(TRUE, FALSE),
                                repel = TRUE, title = "Biplot de Contribución (Asimétrico)")
  print(biplot_asim)
}

# PARTE 2: ANÁLISIS DE CORRESPONDENCIAS MÚLTIPLE (MCA)
{
  # Evitamos nom_mun porque tiene 8000 categorías y rompería la visualización.
  elecciones_cat <- elecciones_final[, c("nom_ccaa", "nivel_mun",
                                         "nivel_comun_edu", "nivel_comun_ocupados")]
  
  # Ejecutamos el MCA
  res.mca <- MCA(elecciones_cat, graph = FALSE)
  print(res.mca)
  
  # 2.1 Valores propios y Scree plot del MCA
  print("--- Autovalores y Varianza Explicada (MCA) ---")
  print(get_eigenvalue(res.mca))
  
  # Calculamos las coord, cos2, y contrib
  var_mca <- get_mca_var(res.mca)
  
  
  
  print("--- Coordenadas de las categorías de las variables ---")
  print(head(var_mca$coord))
  
  print("--- Calidad de representación (Cos2) ---")
  print(head(var_mca$cos2))
  
  print("--- Contribución a las dimensiones (%) ---")
  print(head(var_mca$contrib))
  
  #Grafico varianza explicada acumulada por dimensiones
  grafico_scree_mca <- fviz_screeplot(res.mca, addlabels = TRUE,
                                      main = "Scree Plot - Correspondencias Múltiple") +
    labs(x = "Dimensiones",
         y = "Porcentaje de varianza explicada")
  print(grafico_scree_mca)
  
  # Biplot
  grafico_contrib_mca <- fviz_mca_var(res.mca, col.var = "contrib",
                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                      repel = TRUE,
                                      ggtheme = theme_minimal(),
                                      title = "MCA: Categorías coloreadas por su contribución")
  print(grafico_contrib_mca)
  
  # 2.4 Ver las variables que más contribuyen gráficamente a las Dim 1 y 2
  contrib_dim1_2 <- fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15,
                                 title = "Top 15 categorías que más aportan a Dim 1 y 2")
  print(contrib_dim1_2)
  
  # 2.5 Biplot de individuos coloreados por CCAA
  # Proyectamos las secciones censales (individuos) sobre el plano Dim1-Dim2
  # del MCA y las coloreamos por Comunidad Autónoma. Permite ver si las
  # CCAA se separan en el espacio de las categorías cualitativas.
  biplot_mca_ccaa <- fviz_mca_ind(res.mca,
                                  geom.ind = "point",
                                  habillage = elecciones_final$nom_ccaa,
                                  addEllipses = TRUE,
                                  ellipse.type = "confidence",
                                  label = "none",
                                  title = "MCA: secciones censales por CCAA")
  print(biplot_mca_ccaa)
}
# NOTA: antes había un segundo bloque MCA duplicado (sin nom_ccaa) usado
# para "ver qué sale". Lo eliminamos porque era redundante con el anterior
# y rompía la legibilidad del script.


# Nuevas librerías para Análisis Clúster
{
  # install.packages(c("cluster", "factoextra"))
  library(cluster)
  library(factoextra)
}

# 1. Preparación de los datos para el clúster
{
  # Seleccionamos estrictamente las variables cuantitativas
  elecciones_cuant <- elecciones_final[, sapply(elecciones_final, is.numeric)]
  
  # Filtramos variables con varianza cero (evita que scale() divida entre cero y genere NaNs)
  varianzas <- apply(elecciones_cuant, 2, var, na.rm = TRUE)
  elecciones_cuant <- elecciones_cuant[, varianzas > 0]
  
  
  # Escalamos los datos (media 0, varianza 1)
  datos_escalados <- scale(elecciones_cuant)
  
  
}

# 2. Búsqueda del número óptimo de clusters (K adecuada)
{
  # Para que el ordenador no se sature al hacer los gráficos,
  # tomamos una muestra representativa de 5.000 secciones censales pues sino es imposible hacer el clúster
  set.seed(123)
  indices_muestra <- sample(1:nrow(datos_escalados), 5000)
  muestra_codo <- datos_escalados[indices_muestra, ]
  
  # Método del Codo(para ver K optima)
  grafico_codo <- fviz_nbclust(muestra_codo, kmeans, method = "wss") +
    labs(title = "Número óptimo de clusters - Método del Codo") + labs(x = "Número de clusters k",
                                                                       y = "Suma total de cuadrados intra-cluster")
  print(grafico_codo)
  
  # Método de la SILUETA (silhouette) - segundo criterio de validación.
  # Mientras el método del codo busca un cambio de pendiente, el de la silueta
  # mide cómo de bien queda cada observación dentro de su propio clúster
  # comparado con el clúster vecino. Un k con silueta media alta es preferible.
  grafico_silueta <- fviz_nbclust(muestra_codo, kmeans, method = "silhouette") +
    labs(title = "Número óptimo de clusters - Método de la Silueta",
         x = "Número de clusters k",
         y = "Anchura media de la silueta")
  print(grafico_silueta)
  
  k_optima <- 2
}

# 3. Análisis Clúster No Jerárquico (K-Means)
{
  
  
  # Ejecutamos K-means con TODOS los datos
  km_result <- kmeans(datos_escalados, centers = k_optima, nstart = 25)
  
  # Visualizamos los clusters en 2D SOLO con la muestra de 5.000 para no colapsar R
  grafico_kmeans <- fviz_cluster(list(data = muestra_codo, cluster = km_result$cluster[indices_muestra]),
                                 geom = "point",
                                 ellipse.type = "convex",
                                 main = paste("Clúster K-Means (K =", k_optima, ") - Muestra de 5000"))
  print(grafico_kmeans)
  
  # Vemos los tamaños de cada clúster
  print(km_result$size)
  
  #Vemos los centroides de cada cluster
  print(round(km_result$centers, 2))
  
  # CARACTERIZACIÓN DE CLUSTERS EN ESCALA ORIGINAL
  # Los centroides de km_result están en escala estandarizada (z-scores), lo
  # cual es difícil de interpretar para la memoria. Calculamos el perfil medio
  # de cada cluster en las UNIDADES ORIGINALES de las variables (€ de renta,
  # años de edad, %, etc.) — esto es lo que hay que comentar en la defensa.
  perfil_clusters <- aggregate(elecciones_cuant,
                               by = list(Cluster = km_result$cluster),
                               FUN = mean, na.rm = TRUE)
  print("--- Perfil medio de cada cluster (escala original) ---")
  print(round(perfil_clusters, 2))
  
  # También cruzamos los clusters con variables cualitativas para ver
  # composición por tipo de municipio y CCAA — útil para "etiquetar" cada cluster.
  cat("\n--- Distribución de clusters por tipo de municipio ---\n")
  print(round(prop.table(table(km_result$cluster, elecciones_final$nivel_mun), 1) * 100, 1))
  
  cat("\n--- Distribución de clusters por CCAA ---\n")
  print(round(prop.table(table(km_result$cluster, elecciones_final$nom_ccaa), 1) * 100, 1))
}

# 3.b K-means sobre las componentes principales del PCA
{
  # Es habitual hacer el clúster sobre las componentes del PCA en lugar de
  # sobre las variables originales: reducimos ruido y eliminamos la
  # multicolinealidad entre variables (que penaliza a kmeans). Usamos las
  # componentes que ya capturan ~75% de la varianza.
  n_comp <- max(componentes_75_80, 3)
  scores_pca <- pca_result$x[, 1:n_comp]
  
  km_pca <- kmeans(scores_pca, centers = k_optima, nstart = 25)
  
  cat("\n--- Tamaños de cluster (k-means sobre", n_comp, "componentes del PCA) ---\n")
  print(km_pca$size)
  
  grafico_kmeans_pca <- fviz_cluster(list(data = scores_pca[indices_muestra, ],
                                          cluster = km_pca$cluster[indices_muestra]),
                                     geom = "point",
                                     ellipse.type = "convex",
                                     main = paste("K-Means sobre componentes del PCA (K =", k_optima, ")"))
  print(grafico_kmeans_pca)
  
  # Comparamos las dos asignaciones (variables originales vs PCA)
  cat("\n--- Concordancia entre kmeans original y kmeans-PCA ---\n")
  print(table(Original = km_result$cluster, PCA = km_pca$cluster))
}

# 4. Análisis Clúster Jerárquico - EXPLORANDO VARIAS 'K'
{
  # Reducimos a 1000 observaciones para que el cálculo sea factible
  indices_jerarquico <- sample(1:nrow(datos_escalados), 1000)
  muestra_jerarquico <- datos_escalados[indices_jerarquico, ]
  
  # Matriz distancias
  matriz_distancias <- dist(muestra_jerarquico, method = "euclidean")
  
  # CLustering jerarquico
  hc_result <- hclust(matriz_distancias, method = "ward.D2")
  
  # CASO 1: EL ÓPTIMO, K = 2
  
  #Analisis para K=2
  grupos_k2 <- cutree(hc_result, k = 2)
  
  #Tamaño de cada cluster
  print(table(grupos_k2))
  
  #Centroides
  centroides_k2 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k2), FUN = mean)
  print(round(centroides_k2, 2))
  
  
  # CASO 2: K = 3
  
  #Analisis para K=3
  grupos_k3 <- cutree(hc_result, k = 3)
  
  #Tamaño de cada cluster
  print(table(grupos_k3))
  
  #Centroides
  centroides_k3 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k3), FUN = mean)
  print(round(centroides_k3, 2))
  
  
  # CASO 3: K = 4
  
  #Analisis para K=4
  grupos_k4 <- cutree(hc_result, k = 4)
  
  #Tamaño de cada cluster
  print(table(grupos_k4))
  
  #Centroides
  centroides_k4 <- aggregate(muestra_jerarquico, by = list(Cluster = grupos_k4), FUN = mean)
  print(round(centroides_k4, 2))
  
  
  # VISUALIZACIÓN DEL DENDROGRAMA
  #
  # Aunque el método del codo sugería k_optima = 2, mostramos el dendrograma
  # coloreado con K = 4 porque visualmente permite apreciar mejor la
  # estructura jerárquica intermedia (ramas que se agrupan en pasos sucesivos).
  # En la memoria justificamos K = 2 como óptimo "estadístico" y K = 4 como
  # corte alternativo "interpretativo".
  k_dendro <- 4
  dendrograma <- fviz_dend(hc_result, k = k_dendro,
                           cex = 0.3,
                           show_labels = FALSE,
                           color_labels_by_k = TRUE,
                           rect = TRUE,
                           main = paste("Clúster Jerárquico (Coloreado para K =", k_dendro, ")"))
  print(dendrograma)
}

