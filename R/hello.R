#------------------------------------------------#
#           Definición de la función             #
#------------------------------------------------#

Fun <- function(Month, Year, City) {

#-------------------------------------------------------------------#
#    Modulo 0: Librerias, val de parámetros y Carga de datos        #
#-------------------------------------------------------------------#

  # Función para validar parámetros
  validar_parametros <- function(parametro, tipo, rango = NULL) {
    if (missing(parametro)) {
      stop("Parameter is missing", deparse(substitute(parametro)))
    }

    if (!is.null(tipo)) {
      tipo_funcion <- switch(tipo,
                             "numeric" = is.numeric,
                             "character" = is.character,
                             "list" = is.list,
                             "vector" = function(x) is.vector(x) || is.data.frame(x),
                             "default" = function(x) FALSE)

      if (!tipo_funcion(parametro)) {
        stop(paste(deparse(substitute(parametro)), " It must be of type ", tipo))
      }
    }

    if (!is.null(rango) && !is.infinite(rango[1]) && !is.infinite(rango[2])) {
      if (parametro < rango[1] || parametro > rango[2]) {
        stop(paste(deparse(substitute(parametro)), " It must be within the range ", rango[1], " - ", rango[2]))
      }
    }
  }

  # Verificación de parámetros
  validar_parametros(Month, "numeric", c(1, 12))
  validar_parametros(Year, "numeric", c(2013, 2023))
  validar_parametros(City, "character")



  # Carga de librerías
  Librerias_base <- c("here", "readxl", "tidyverse", "knitr", "moments", "maditr", "mice", "VIM", "dplyr", "finalfit", "plyr","hdd")
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = Librerias_base, character.only = TRUE)



# libreria foodprice
  if (!requireNamespace("Foodprice", quietly = TRUE)) {
    # Si no está instalado, instalarlo desde GitHub
    devtools::install_github("Foodprice/Foodprice")
  }

  # Cargar la librería Foodprice
  library(Foodprice)


  #------------------------------------------------#
  #   Sub 0.1: Crear entornos y descargar datos    #
  #------------------------------------------------#

  # Función para crear o reusar un entorno
  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }

# Crear entornos
  data_GEIH <- crear_o_reusar_entorno("data_GEIH");envr_name <- paste0("GEIH_", Month, "_", Year, "_", City)

# Validar si existe el entorno y si no, descargar datos
  if (!exists(envr_name, envir = data_GEIH)) {

    assign(envr_name, new.env(parent = emptyenv()), envir = data_GEIH)

    generate_download_url <- function(Month, Year) {
      base_url <- "https://microdatos.dane.gov.co/index.php/catalog/"
      dataset_id <- if (Year == 2022) 771 else NA
      start_month <- if (Year == 2022) 22688 else NA
      month_id <- start_month + Month - 1
      if (!is.na(dataset_id) && !is.na(start_month)) {
        url <- paste0(base_url, dataset_id, "/download/", month_id)
        return(url)
      } else {
        stop("Year not supported")
      }
    }

    # Generar la URL de descarga
    url <- generate_download_url(Month, Year)

    # Descargar y extraer el archivo ZIP
    temp_zip <- tempfile(fileext = ".zip")
    download.file(url, temp_zip, timeout = 1000)
    temp_folder <- tempdir()
    unzip(temp_zip, exdir = temp_folder)

    # encuentra la ruta
    rchivos <- list.files(temp_folder,full.names = TRUE)[2]
    csv_folder <- file.path(rchivos, "CSV")
    archivos_csv <- list.files(csv_folder, full.names = TRUE)



  # Función para detectar el delimitador
  detect_delimiter <- function(file_path) {
    first_lines <- readLines(file_path, n = 5)
    delimiter <- ifelse(sum(grepl(",", first_lines)) > sum(grepl(";", first_lines)), ",", ";")
    return(delimiter)
  }


    # Modificación en el bucle para asignar los objetos al entorno creado
    for (i in seq_along(archivos_csv)) {
      delimitador <- detect_delimiter(archivos_csv[i])
      nombres_archivos <- basename(archivos_csv[i])
      # Limpiar el nombre del archivo eliminando caracteres no ASCII
      nombre_limpiado <- iconv(nombres_archivos, "UTF-8", "ASCII", sub = " ")
      # Reemplazar los espacios por guiones bajos
      nombre_limpiado <- gsub(" ", "_", nombre_limpiado)
      assign(nombre_limpiado, read.csv(archivos_csv[i], sep = delimitador), envir = get(envr_name, envir = data_GEIH))
    }
}


  #------------------------------------------------#
  #   Sub 0.2: selecionar y nombrar datos a usar   #
  #------------------------------------------------#

  # Patrones de nombres a mantener
  patrones_a_mantener <- c("Ocupados", "No ocupados", "Otros ingresos e impuestos", "Datos del hogar y la vivienda", "Caracteristicas generales, seguridad social en salud y educacion.")

  # Nombres de los dataframes en el entorno actual
  nombres_dataframes <- names(get(envr_name, envir = data_GEIH))

  # Función para calcular la similitud entre dos cadenas de caracteres
  similarity <- function(pattern, name) {
    max_sim <- max(adist(pattern, name))
    return(1 - max_sim / max(nchar(pattern), nchar(name)))
  }

  # Encontrar los nombres más cercanos a los patrones
  nombres_mas_cercanos <- lapply(patrones_a_mantener, function(pattern) {
    similarities <- sapply(nombres_dataframes, similarity, pattern)
    closest_name <- nombres_dataframes[which.max(similarities)]
    return(closest_name)
  })

  # Eliminar los objetos que no coinciden con los patrones
  nombres_a_eliminar <- setdiff(nombres_dataframes, unlist(nombres_mas_cercanos))
  for (nombre in nombres_a_eliminar) {
    rm(list = nombre, envir = get(envr_name, envir = data_GEIH))
  }


# Nombrar datos

  Ocupados = get(envr_name, envir = data_GEIH)$Ocupados.csv

  Datos_del_hogar_y_la_vivienda = get(envr_name, envir = data_GEIH)$Datos_del_hogar_y_la_vivienda.CSV

  No_ocupados =get(envr_name, envir = data_GEIH)$No_ocupados.csv

  Otros_ingresos_e_impuestos = get(envr_name, envir = data_GEIH)$Otros_ingresos_e_impuestos.csv

  Caracteristicas_generales =get(envr_name, envir = data_GEIH)$"Caracter_sticas_generales,_seguridad_social_en_salud_y_educaci_n.csv"



  cat("Módulo 1 original ✓\n")


  #------------------------------------------------#
  #            FIN DEL MÓDULO 1 ORGINAL            # PENDIENTE: FALTA REVISAR POCAS COSAS.
  #------------------------------------------------#


  #----------------------------------------------------------------------------------#
  #    Modulo 1: Distribución de ingresos corrientes  y factores de xp               #
  #----------------------------------------------------------------------------------#

  #-----------------------------#
  # INICIO DEL MÓDULO 2 ORGINAL #
  #-----------------------------#

  # filtro para Cali, Valle del Cauca (código 76)
  OcupadosF <-filter( Ocupados, AREA == 76)
  Datos_del_hogar_y_la_viviendaF <- filter(Datos_del_hogar_y_la_vivienda,AREA == 76)
  No_ocupadosF <- filter(No_ocupados,AREA == 76)
  Otros_ingresos_e_impuestosF <- filter(Otros_ingresos_e_impuestos,AREA == 76)
  Caracteristicas_generalesF <- filter(Caracteristicas_generales,AREA == 76)

  # seleccionar variables de interés
  ocup <- OcupadosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P6800", "INGLABO")]
  Datos_vivi <- Datos_del_hogar_y_la_viviendaF[c("DIRECTORIO","SECUENCIA_P","P4030S1A1","P6008")]
  Noocup <- No_ocupadosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P7422S1")]
  Ot_ing <- Otros_ingresos_e_impuestosF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P7500S1A1","P7500S2A1","P7500S3A1")]
  Car_gen <- Caracteristicas_generalesF[c("DIRECTORIO","SECUENCIA_P","ORDEN","P3271","P6050","P6040", "P3042", "FEX_C18")]

  # merge completo
  OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OT_ING_CAR_GEN <- merge(OCUP_Noocup_OT_ING,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  dataset <-merge(OCUP_Noocup_OT_ING_CAR_GEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))

  # hogares con ingresos laborales NA
  hogares_na = ocup %>% filter(is.na(INGLABO))

  # eliminar hogares con ingresos laborales NA
  dataset_x = dataset %>% filter(DIRECTORIO %in% setdiff(levels(as.factor(dataset$DIRECTORIO)),
                                                         levels(as.factor(hogares_na$DIRECTORIO))))

  # cálculo de ingresos corrientes totales por persona
  dataset_x = mutate(dataset_x,
                     Ingresos =
                       rowSums(dataset_x[ , c("INGLABO","P7422S1","P7500S1A1","P7500S2A1","P7500S3A1")], na.rm=TRUE))

  # construccion de una variable de identificación para los hogares
  dataset_x$id = paste0(dataset_x$DIRECTORIO,"-",dataset_x$SECUENCIA_P)

  # construccion de base de datos de recepción vacía
  dataset_2 = data.frame(levels(as.factor(dataset_x$id)))
  colnames(dataset_2) = "id"
  dataset_2$ingresos = NA

  hogares_id = levels(as.factor(dataset_2$id))

  # bucle para el cálculo de los ingresos para los hogares
  for (k in 1:length(hogares_id)) {
    df = data.frame()
    df = dataset_x %>% filter(id %in% hogares_id[k])
    q = which(dataset_2$id == hogares_id[k])
    dataset_2$ingresos[q]= sum(df$Ingresos)
  }

  # incluir la variable de tamaño de los hogares
  dataset_2 = merge(dataset_2, dataset_x[c("id", "P6008", "FEX_C18")], by ="id")
  dataset_2 = dataset_2[!duplicated(dataset_2),]

  # remover bases de datos
  rm(Car_gen, Caracteristicas_generales, Caracteristicas_generalesF,
     Datos_del_hogar_y_la_vivienda, Datos_del_hogar_y_la_viviendaF,
     Datos_vivi, df, No_ocupados, No_ocupadosF, Noocup,
     ocup, OCUP_Noocup, Ocupados, OcupadosF, Ot_ing, Otros_ingresos_e_impuestos,
     Otros_ingresos_e_impuestosF, OCUP_Noocup_OT_ING, OCUP_Noocup_OT_ING_CAR_GEN,
     hogares_na, k, q, hogares_id)



  #-----------------------------#
  # INICIO DEL MÓDULO 3 ORGINAL #
  #-----------------------------#

# eliminar ingreso = 98
# (Nota: la eliminación de valores atípicos debería solucionar el problema del
# valor convencional (98))
dataset_def= dataset_2  %>% filter(!ingresos  %in% 98)

# eliminar hogares con ingresos 0
dataset_def= dataset_2  %>% filter(!ingresos  %in% 0)

# valores atipicos
prob = quantile(dataset_def$ingresos, probs = seq(0,1, by = 0.01))
p_min = prob[2]
p_max = prob[100]

dataset_def = dataset_def  %>% filter(ingresos > p_min & ingresos < p_max)



gastos_ingresos_exp <- dataset_def %>%
  slice(rep(1:n(), times = dataset_def$FEX_C18)) %>%
  na.omit()



gastos_ingresos_exp$per_capita = gastos_ingresos_exp$ingresos/gastos_ingresos_exp$P6008

dataset_def_deciles = gastos_ingresos_exp %>% mutate(deciles = ntile(per_capita, 10))

dataset_def_deciles$deciles = revalue(as.factor(dataset_def_deciles$deciles),
                                      c("1" = "Decil 1", "2" = "Decil 2",
                                        "3" = "Decil 3", "4" = "Decil 4",
                                        "5" = "Decil 5", "6" = "Decil 6",
                                        "7" = "Decil 7", "8" = "Decil 8",
                                        "9" = "Decil 9", "10" = "Decil 10"))

saveRDS(dataset_def_deciles, file="/home/juan-c/Descargas/Comparación/Resultados/geih_income_df_2022.RDS")


#-------------------------------------------------#
#               Tabla de resumen:                 #
#    ingreso (mean & max.) y gasto (mean & max.)  #
#-------------------------------------------------#

geih_ingresos = dataset_def_deciles
dataset_def_deciles = dataset_def_deciles
# Hallar el ingreso promedio por decil
deciles_grupos = c("Decil 1", "Decil 2",
                   "Decil 3", "Decil 4",
                   "Decil 5", "Decil 6",
                   "Decil 7", "Decil 8",
                   "Decil 9", "Decil 10")



cat("Módulo 2 y 3 original ✓\n")


#------------------------------------------------#
#            FIN DEL MÓDULO 2 y 3 ORGINAL        # PENDIENTE:Simplificar y generalziar código
#------------------------------------------------#


#----------------------------------------------------------------------------------#
#    Modulo 2: Proporcion del gasto en alimentación                                #
#----------------------------------------------------------------------------------#

#-----------------------------#
# INICIO DEL MÓDULO 4 ORGINAL #
#-----------------------------#


deciles_gasto = data.frame(levels(as.factor(dataset_def_deciles$deciles)))
colnames(deciles_gasto) = "deciles"
deciles_gasto$share = c( 0.47,  0.47,
                         0.35, 0.35,
                         0.30,  0.30,
                         0.27,  0.27,
                         0.22,  0.22)



# asignar la proporción a cada hogar
dataset_def_deciles$id_aux = c(1:nrow(dataset_def_deciles))

dataset_def_deciles = merge(dataset_def_deciles, deciles_gasto, by = "deciles", all.x = TRUE)

dataset_def_deciles = dataset_def_deciles[order(dataset_def_deciles$id_aux),]

dataset_def_deciles = dataset_def_deciles[setdiff(colnames(dataset_def_deciles), "id_aux")]

# calcular ingreso dedicado a alimentacion
dataset_def_deciles$ingreso_alimentos = dataset_def_deciles$share*dataset_def_deciles$ingresos
dataset_def_deciles$ingreso_alimentos_per_capita = dataset_def_deciles$ingreso_alimentos/dataset_def_deciles$P6008


#-------------------------------------------------#
#               Tabla de resumen:                 #
#    ingreso (mean & max.) y gasto (mean & max.)  #
#-------------------------------------------------#

# Hallar el ingreso promedio por decil
deciles_grupos = c("Decil 1", "Decil 2",
                   "Decil 3", "Decil 4",
                   "Decil 5", "Decil 6",
                   "Decil 7", "Decil 8",
                   "Decil 9", "Decil 10")


mean_income = data.frame(deciles_grupos)
mean_income$ingreso_prom = NA
mean_income$size_prom = NA
mean_income$n = NA
mean_income$min_ing_pc = NA
mean_income$max_ing_pc = NA
mean_income$ing_per_capita_prom = NA

mean_income$share = NA

mean_income$food = NA

mean_income$min_food_pc = NA
mean_income$max_food_pc = NA
mean_income$food_per_capita_prom = NA



# NOTA: LOS PROMEDIOS FUERON CALCULADOS CON PREVIA EXPANSIÓN
for (k in 1:length(deciles_grupos)) {
  df = data.frame()
  df = dataset_def_deciles  %>% filter(deciles  %in% deciles_grupos[k])
  y_1 = which(mean_income$deciles_grupos == deciles_grupos[k])


  mean_income$ingreso_prom[y_1] = mean(df$ingresos)

  mean_income$size_prom[y_1] = mean(df$P6008)

  mean_income$n[y_1] = nrow(df)

  mean_income$min_ing_pc[y_1] = min(df$per_capita)

  mean_income$max_ing_pc[y_1] = max(df$per_capita)

  mean_income$ing_per_capita_prom[y_1] = mean(df$per_capita)

  mean_income$share[y_1] = as.numeric(levels(as.factor(df$share)))

  mean_income$food[y_1] = mean(df$ingreso_alimentos)

  mean_income$min_food_pc[y_1] = min(df$ingreso_alimentos_per_capita)
  mean_income$max_food_pc[y_1] = max(df$ingreso_alimentos_per_capita)
  mean_income$food_per_capita_prom[y_1] = mean(df$ingreso_alimentos_per_capita)

}

mean_income_deciles = mean_income

cat("Módulo 4 original ✓\n")

#-----------------------------#
# FIN    DEL MÓDULO 4 ORGINAL #
#-----------------------------#



#----------------------------------------------------------------------------------#
#    Modulo 3: Reatroalimentación con el paquete Foodprice                        #
#----------------------------------------------------------------------------------#

#-----------------------------#
# INICIO DEL MÓDULO 5 ORGINAL #
#-----------------------------#



}







Fun(Month = 1, Year = 2022, City = "Cali")





Month = 1
Year = 2022
City = "Cali"



Data_mes_año=Foodprice::DataCol(Month = Month, Year = Year, City = City)


modelo_1=Foodprice::CoCA(data=Data_mes_año,EER = EER)$cost












