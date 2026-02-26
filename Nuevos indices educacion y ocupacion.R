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
      "Hmisc"
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
  library(Hmisc)
}

# Leemos la tabla.
{
  elecciones_total <- read.table(
    file = "https://raw.githubusercontent.com/Gibilloarrate/ADat/main/datos.tsv",
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
  elecciones <- elecciones_total[elecciones_total$secc_exterior != "Sí", ]
  
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
  
  elecciones$tasa_abstencion_23 <- elecciones$abs / elecciones$censo * 100
}

# Calculamos indicadores de estudios y ocupación ponderados
{
  elecciones$años_medios_de_estudios <- (
    6 * elecciones$edu_primaria + 10 * elecciones$edu_eso + 12 *
      elecciones$edu_bachiller + 16 * elecciones$edu_superior
  ) / elecciones$pob_mas_de_15
  summary(elecciones$años_medios_de_estudios)
  
  elecciones$indice_laboral <- (
    elecciones$ocupados_nivel_bajo * 2 + elecciones$ocupados_nivel_medio *
      4 + elecciones$ocupados_nivel_alto * 7
  ) / elecciones$ocupados_total
}

# Estudio normalidad algunas variables
{
  # 1. Ajustamos a 4 gráficos y reducimos los márgenes (Abajo, Izquierda, Arriba, Derecha)
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  # 2. Dibujamos los 4 gráficos
  hist(
    elecciones$años_medios_de_estudios,
    main = "Hist: Años Estudio",
    xlab = "Años",
    col = "lightblue"
  )
  qqnorm(elecciones$años_medios_de_estudios, main = "QQ: Años Estudio")
  qqline(elecciones$años_medios_de_estudios, col = "red")
  
  hist(
    elecciones$indice_laboral,
    main = "Hist: Índice Laboral",
    xlab = "Índice",
    col = "lightgreen"
  )
  qqnorm(elecciones$indice_laboral, main = "QQ: Índice Laboral")
  qqline(elecciones$indice_laboral, col = "red")
  
  # 3. Restauramos la ventana a 1 solo gráfico normal
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}

{
  # Cargamos la librería necesaria
  library(Hmisc)
  
  # 1. Calculamos los cortes ponderados ignorando los NAs (na.rm = TRUE)
  cortes_pond_est <- wtd.quantile(
    elecciones$años_medios_de_estudios,
    weights = elecciones$pob_mas_de_15,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )
  
  # 2. Creamos la variable con tus etiquetas
  elecciones$nivel_estudios <- cut(
    elecciones$años_medios_de_estudios,
    breaks = cortes_pond_est,
    include.lowest = TRUE,
    labels = c("Más bajo", "Bajo", "Medio", "Alto", "Más alto")
  )
  
  # 1. Calculamos los cortes ponderados ignorando los NAs (na.rm = TRUE)
  cortes_pond_ocu <- wtd.quantile(
    elecciones$indice_laboral,
    weights = elecciones$ocupados_total,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )
  
  # 2. Creamos la variable con tus etiquetas
  elecciones$nivel_laboral <- cut(
    elecciones$indice_laboral,
    breaks = cortes_pond_ocu,
    include.lowest = TRUE,
    labels = c("Más bajo", "Bajo", "Medio", "Alto", "Más alto")
  )
}


{
  # Cargar la librería
  library(dplyr)
  
  # Crear una tabla resumen agrupada
  resumen_por_nivel <- elecciones %>%
    filter(!is.na(nivel_estudios)) %>% # Quitamos los nulos para que la tabla quede limpia
    group_by(nivel_estudios) %>%
    summarise(
      num_secciones = n(),
      # Cuántas zonas hay en este grupo
      poblacion_total = sum(pob_mas_de_15, na.rm = TRUE),
      # Cuánta gente vive en este grupo
      abstencion_media = mean(tasa_abstencion_23, na.rm = TRUE),
      # Abstención promedio
      anos_estudio_medios = mean(años_medios_de_estudios, na.rm = TRUE) # Para comprobar que el orden tiene sentido
    )
  
  # Ver el resultado
  print(resumen_por_nivel)
}

{
  library(patchwork)
  library(ggplot2)
  library(dplyr)
  
  p1 <- ggplot(
    elecciones %>% filter(!is.na(nivel_estudios)),
    aes(x = nivel_estudios, y = tasa_abstencion_23, fill = nivel_estudios)
  ) +
    geom_boxplot(alpha = 0.7) + theme_minimal() + theme(legend.position = "none",
                                                        axis.text.x = element_text(angle = 15, hjust = 1)) +
    labs(title = "Educación: Quintiles", x = "", y = "Abstención")
  
  p2 <- ggplot(
    elecciones %>% filter(!is.na(nivel_comun_edu)),
    aes(x = nivel_comun_edu, y = tasa_abstencion_23, fill = nivel_comun_edu)
  ) +
    geom_boxplot(alpha = 0.7) + theme_minimal() + theme(legend.position = "none",
                                                        axis.text.x = element_text(angle = 15, hjust = 1)) +
    labs(title = "Educación: Nivel Común", x = "", y = "")
  
  grafico_1 <- (p1 | p2)
  print(grafico_1)
}

{
  p3 <- ggplot(
    elecciones %>% filter(!is.na(nivel_laboral)),
    aes(x = nivel_laboral, y = tasa_abstencion_23, fill = nivel_laboral)
  ) +
    geom_boxplot(alpha = 0.7) + theme_minimal() + theme(legend.position = "none",
                                                        axis.text.x = element_text(angle = 15, hjust = 1)) +
    labs(title = "Laboral: Nivel Laboral", x = "", y = "Abstención")
  
  p4 <- ggplot(
    elecciones %>% filter(!is.na(nivel_comun_ocupados)),
    aes(x = nivel_comun_ocupados, y = tasa_abstencion_23, fill = nivel_comun_ocupados)
  ) +
    geom_boxplot(alpha = 0.7) + theme_minimal() + theme(legend.position = "none",
                                                        axis.text.x = element_text(angle = 15, hjust = 1)) +
    labs(title = "Laboral: Ocupación Mayoritaria", x = "", y = "")
  
  # Forzar la visualización de los 4 paneles
  grafico_2 <- (p3 | p4)
  print(grafico_2)
}
