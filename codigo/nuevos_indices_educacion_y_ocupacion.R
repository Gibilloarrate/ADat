# Librerías que vamos a usar.
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
    colClasses = c(
      "cod_sscc" = "character",
      "cod_ccaa" = "character",
      "cod_prov" = "character",
      "cod_mun"  = "character",
      "cod_dist" = "character"
    ),
  )
  
  # Eliminamos los datos de las secciones censales en el extranjero.
  elecciones <- elecciones_total[elecciones_total$secc_exterior != "Sí", ]
  
}

# Arreglando variables:
{
  # Cualitativas nominales:
  
  # Reconstrucción de códigos tras la lectura
  elecciones$cod_ccaa <- substr(paste0("00", elecciones$cod_ccaa), nchar(paste0("00", elecciones$cod_ccaa)) - 1, nchar(paste0("00", elecciones$cod_ccaa)))
  elecciones$cod_prov <- substr(paste0("00", elecciones$cod_prov), nchar(paste0("00", elecciones$cod_prov)) - 1, nchar(paste0("00", elecciones$cod_prov)))
  elecciones$cod_mun <- substr(paste0("00000", elecciones$cod_mun), nchar(paste0("00000", elecciones$cod_mun)) - 4, nchar(paste0("00000", elecciones$cod_mun)))
  elecciones$cod_dist <- substr(paste0("0000000", elecciones$cod_dist), nchar(paste0("0000000", elecciones$cod_dist)) - 6, nchar(paste0("0000000", elecciones$cod_dist)))
  elecciones$cod_sscc <- substr(paste0("0000000000", elecciones$cod_sscc), nchar(paste0("0000000000", elecciones$cod_sscc)) - 9, nchar(paste0("0000000000", elecciones$cod_sscc)))

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
    4.5 * elecciones$edu_primaria + 9 * elecciones$edu_eso + 12 *
      elecciones$edu_bachiller + 16 * elecciones$edu_superior
  ) / elecciones$pob_mas_de_15
  summary(elecciones$años_medios_de_estudios)
  
  elecciones$indice_laboral <- (
    elecciones$ocupados_nivel_bajo * 15 + elecciones$ocupados_nivel_medio *
      35 + elecciones$ocupados_nivel_alto * 70
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

# Creación de las variables ordinales
{
  # Cargamos la librería necesaria
  library(Hmisc)
  
  # 1. Calculamos los cortes ponderados ignorando los NAs (na.rm = TRUE)
  cortes_pond_est <- wtd.quantile(
    elecciones$años_medios_de_estudios,
    weights = elecciones$pob_mas_de_15,
    probs = seq(0, 1, length.out = 5),
    na.rm = TRUE
  )
  
  # 2. Creamos la variable con tus etiquetas
  elecciones$nivel_estudios <- cut(
    elecciones$años_medios_de_estudios,
    breaks = cortes_pond_est,
    include.lowest = TRUE,
    labels = c("Bajo", "Bajo-Medio", "Medio-Alto", "Alto")
  )
  
  # 1. Calculamos los cortes ponderados ignorando los NAs (na.rm = TRUE)
  cortes_pond_ocu <- wtd.quantile(
    elecciones$indice_laboral,
    weights = elecciones$ocupados_total,
    probs = seq(0, 1, length.out = 4),
    na.rm = TRUE
  )
  
  # 2. Creamos la variable con tus etiquetas
  elecciones$nivel_laboral <- cut(
    elecciones$indice_laboral,
    breaks = cortes_pond_ocu,
    include.lowest = TRUE,
    labels = c("Bajo", "Medio", "Alto")
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
      años_estudio_min = min(años_medios_de_estudios, na.rm = TRUE),
      años_estudio_media = mean(años_medios_de_estudios, na.rm = TRUE),
      años_estudio_max = max(años_medios_de_estudios, na.rm = TRUE),
    )
  print(resumen_por_nivel)
  
  resumen_por_nivel <- elecciones %>%
    filter(!is.na(nivel_laboral)) %>% # Quitamos los nulos para que la tabla quede limpia
    group_by(nivel_laboral) %>%
    summarise(
      num_secciones = n(),
      # Cuántas zonas hay en este grupo
      poblacion_total = sum(ocupados_total, na.rm = TRUE),
      # Cuánta gente vive en este grupo
      años_estudio_min = min(indice_laboral, na.rm = TRUE),
      años_estudio_media = mean(indice_laboral, na.rm = TRUE),
      años_estudio_max = max(indice_laboral, na.rm = TRUE),
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

# 1. Creamos los modelos (asegurándonos de que R entienda que son categorías/factores)
modelo_edu_quintil  <- lm(
  tasa_abstencion_23 ~ as.factor(nivel_estudios),
  data = elecciones,
  weights = censo
)
modelo_edu_comun    <- lm(
  tasa_abstencion_23 ~ as.factor(nivel_comun_edu),
  data = elecciones,
  weights = censo
)
modelo_lab_ordinal  <- lm(tasa_abstencion_23 ~ as.factor(nivel_laboral),
                          data = elecciones,
                          weights = censo)
modelo_lab_comun  <- lm(
  tasa_abstencion_23 ~ as.factor(nivel_comun_ocupados),
  data = elecciones,
  weights = censo
)


# 2. Extraemos los R-cuadrados ajustados
r2_edu_quintil <- summary(modelo_edu_quintil)$adj.r.squared
r2_edu_comun   <- summary(modelo_edu_comun)$adj.r.squared
r2_lab_ordinal <- summary(modelo_lab_ordinal)$adj.r.squared
r2_lab_comun <- summary(modelo_lab_comun)$adj.r.squared


# 3. Los mostramos en porcentajes para compararlos fácilmente
cat("\n--- % DE ABSTENCIÓN EXPLICADO (R-cuadrado Ajustado) ---\n")
cat("Cuartiles Educativos:  ", round(r2_edu_quintil * 100, 2), "%\n")
cat("Nivel Común Educativo: ", round(r2_edu_comun * 100, 2), "%\n")
cat("Terciles Laborales:     ", round(r2_lab_ordinal * 100, 2), "%\n")
cat("Nivel Común Laboral:     ", round(r2_lab_comun * 100, 2), "%\n")