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
      "pacthwork",
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

# Calculamos algunas tasas
{
  elecciones_final$tasa_paro <- (
    elecciones_final$parados / (elecciones_final$ocupados_total + elecciones_final$parados)
  ) * 100
  elecciones_final$tasa_abs <- (elecciones_final$abstencion_23 / elecciones_final$censo) * 100
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
