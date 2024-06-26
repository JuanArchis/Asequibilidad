#------------------------------------------------#
#           Definición de la función             #
#------------------------------------------------#

Modulos <- function(Month, Year, City) {

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
  Librerias_base <- c("here", "readxl", "tidyverse", "knitr", "moments", "maditr", "mice", "VIM", "dplyr", "finalfit", "plyr","hdd","zip","httr")
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(char = Librerias_base, character.only = TRUE)



# libreria foodprice
  if (!requireNamespace("Foodprice", quietly = TRUE)) {
    # Si no está instalado, instalarlo desde GitHub
    devtools::install_github("Foodprice/Foodprice")
  }

  # Cargar la librería Foodprice
  library(Foodprice)

  cat("Módulo 1:  Carga de librerias y Datos de GEIH \n")


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

  suppressWarnings({

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

 symbols <- c("|", "/", "-", "\\")  # Lista de símbolos para la animación
 for (i in 1:100) {  # Repetir 10 veces para simular una carga corta
      cat(paste0("     Descargando y extrayendo los datos de la GEIH... ", symbols[i %% length(symbols) + 1], "\r"))
      Sys.sleep(0.1)  # Tiempo de espera entre cada símbolo (simulando la carga)
  }



    # Generar la URL de descarga
    url <- generate_download_url(Month, Year)

    # Descargar y extraer el archivo ZIP
    temp_zip <- tempfile(fileext = ".zip")
    GET(url, write_disk(temp_zip, overwrite = TRUE), timeout(1000))
    temp_folder <- tempdir()
    #zip::unzip(temp_zip, exdir = temp_folder)



    if (.Platform$OS.type == "windows") {
      unzip_command <- sprintf('powershell -Command "Expand-Archive -LiteralPath \'%s\' -DestinationPath \'%s\' > $null 2>&1"', temp_zip, temp_folder)
      system(unzip_command, wait = TRUE)
    } else {
      # Para Linux y otros sistemas Unix-like
      system2("unzip", c(temp_zip, "-d", temp_folder), stdout = NULL, stderr = NULL)
    }


    archivos_csv <- list.files(temp_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)


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
  patrones_a_mantener <- c("Ocupados", "No ocupados", "Otros ingresos e impuestos", "Datos del hogar y la vivienda", "Caracteristicas generales, seguridad social en salud y educacion.","Fuerza_trabajo","Otras formas de trabajo")

  # Nombres de los dataframes en el entorno actual
  nombres_dataframes <- names(get(envr_name, envir = data_GEIH))
  print(nombres_dataframes)
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

  Caracteristicas_generales =get(envr_name, envir = data_GEIH)$`Caracter__sticas_generales,_seguridad_social_en_salud_y_educaci__n.csv`

  Fuerza_trabajo=get(envr_name, envir = data_GEIH)$Fuerza_de_trabajo.csv

  Otras_formas_de_trabajo=get(envr_name, envir = data_GEIH)$Otras_formas_de_trabajo.csv
  })

  cat("\n    Finalizado ✓ \n")


  #------------------------------------------------#
  #            FIN DEL MÓDULO 1 ORGINAL            # PENDIENTE: FALTA REVISAR POCAS COSAS.
  #------------------------------------------------#


  #----------------------------------------------------------------------------------#
  #    Modulo 1: Distribución de ingresos corrientes  y factores de xp               #
  #----------------------------------------------------------------------------------#

  #---------------------------------------------#
  # INICIO DEL MÓDULO 1: Algoritmo ingreso GEIH #
  #---------------------------------------------#

  Sys.sleep(1);cat("Módulo 2 y 3:   Cálculo ingresos de hogares ")

  # Crear una variable para identificar cada módulo
  Ocupados$ocu = 1
  Datos_del_hogar_y_la_vivienda$DHV = 1
  No_ocupados$no_ocu = 1
  Otros_ingresos_e_impuestos$OI = 1
  Caracteristicas_generales$CG = 1
  Fuerza_trabajo$L = 1
  Otras_formas_de_trabajo$OFT = 1

  # Omisión de variables
  ocup <- Ocupados %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS, FT))
  Datos_vivi <- Datos_del_hogar_y_la_vivienda %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Noocup <- No_ocupados %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS, FFT))
  Ot_ing <- Otros_ingresos_e_impuestos %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Fuerza <- Fuerza_trabajo %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Ot_formas <- Otras_formas_de_trabajo %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Car_gen <- Caracteristicas_generales  %>% select(-c(PERIODO, HOGAR, REGIS))


  # filtro para Cali, Valle del Cauca (código 76)

  #OcupadosF <-filter( Ocupados, AREA == 76)
  #Datos_del_hogar_y_la_viviendaF <- filter(Datos_del_hogar_y_la_vivienda,AREA == 76)
  #No_ocupadosF <- filter(No_ocupados,AREA == 76)
  #Otros_ingresos_e_impuestosF <- filter(Otros_ingresos_e_impuestos,AREA == 76)
  #Caracteristicas_generalesF <- filter(Caracteristicas_generales,AREA == 76)

  # merge completo
  OCUP_Noocup <- merge(ocup,Noocup, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OTING<- merge(OCUP_Noocup,Ot_ing, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OTING_FT <- merge(OCUP_Noocup_OTING, Fuerza, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OTING_FT_OTF <- merge(OCUP_Noocup_OTING_FT, Ot_formas, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)
  OCUP_Noocup_OTING_FT_OTF_CARGEN <- merge(OCUP_Noocup_OTING_FT_OTF,Car_gen, by = c("DIRECTORIO", "SECUENCIA_P","ORDEN"), all = TRUE)

  personas <-merge(OCUP_Noocup_OTING_FT_OTF_CARGEN,Datos_vivi, by = c("DIRECTORIO", "SECUENCIA_P"))



  # Convertir las variables a minúsculas
  colnames(personas) <- tolower(colnames(personas))

  # Quitar AREA == 88 y AREA == NA
  personas <- personas %>% filter(dpto <= 80)

  # Crear variable id para las personas
  personas$id <- paste0(personas$directorio,"-",
                        personas$secuencia_p,"-",
                        personas$orden)

  # Filtro para el dominio urbano (esta línea de código no es estrictamente necesaria
  # porque, en teorías, la variable "AREA" filta para la zona urbana [ciudades y A.M.].
  # La dejo por precaución)
  df_urbano <- personas %>% filter(clase == 1)

  # Renombrar el estrato socioeconómico (P4030S1A1)
  df_total <- df_urbano %>% mutate(estrato = p4030s1a1) %>% select(-p4030s1a1)

  # Recodificar la variable dominio
  # Se busca definir tres categorías:
  # (1) 13 ciudades principales y áreas metropolitanas
  # (2) Resto urbano
  # (3) Área rural

  df_total$dominio = as.numeric(df_total$area)

  df_total$dominio[df_total$dominio == "63"] = "ARMENIA"
  df_total$dominio[df_total$dominio == "8"] = "BARRANQUILLA"
  df_total$dominio[df_total$dominio == "11"] = "BOGOTA"
  df_total$dominio[df_total$dominio == "68"] = "BUCARAMANGA"
  df_total$dominio[df_total$dominio == "76"] = "CALI"
  df_total$dominio[df_total$dominio == "13"] = "CARTAGENA"
  df_total$dominio[df_total$dominio == "54"] = "CUCUTA"
  df_total$dominio[df_total$dominio == "18"] = "FLORENCIA"
  df_total$dominio[df_total$dominio == "73"] = "IBAGUE"
  df_total$dominio[df_total$dominio == "17"] = "MANIZALES"
  df_total$dominio[df_total$dominio == "5"] = "MEDELLIN"
  df_total$dominio[df_total$dominio == "23"] = "MONTERIA"
  df_total$dominio[df_total$dominio == "41"] = "NEIVA"
  df_total$dominio[df_total$dominio == "52"] = "PASTO"
  df_total$dominio[df_total$dominio == "66"] = "PEREIRA"
  df_total$dominio[df_total$dominio == "19"] = "POPAYAN"
  df_total$dominio[df_total$dominio == "27"] = "QUIBDO"
  df_total$dominio[df_total$dominio == "44"] = "RIOHACHA"
  df_total$dominio[df_total$dominio == "47"] = "SANTA MARTA"
  df_total$dominio[df_total$dominio == "70"] = "SINCELEJO"
  df_total$dominio[df_total$dominio == "15"] = "TUNJA"
  df_total$dominio[df_total$dominio == "20"] = "VALLEDUPAR"
  df_total$dominio[df_total$dominio == "50"] = "VILLAVICENCIO"
  df_total$dominio[is.na(df_total$dominio)] = "RESTO URBANO"

  # Ajustar NA en las variables binarias creadas para cada módulo
  # (Por ejemplo, en el módulo de ocupados: si ocu == NA, entonces ocu == 0)
  df_total$ocu[is.na(df_total$ocu)] = 0
  df_total$dhv[is.na(df_total$dhv)] = 0
  df_total$no_ocu[is.na(df_total$no_ocu)] = 0
  df_total$oi[is.na(df_total$oi)] = 0
  df_total$cg[is.na(df_total$cg)] = 0
  df_total$l[is.na(df_total$l)] = 0
  df_total$oft[is.na(df_total$oft)] = 0


#---------------------------------------------#
# INICIO DEL MÓDULO 2: Algoritmo ingreso GEIH #
#---------------------------------------------#


cat("     Finalizado ✓ \n")


#------------------------------------------------#
#            FIN DEL MÓDULO 2 y 3 ORGINAL        # PENDIENTE:Simplificar y generalziar código
#------------------------------------------------#


#----------------------------------------------------------------------------------#
#    Modulo 2: Proporcion del gasto en alimentación                                #
#----------------------------------------------------------------------------------#

#-----------------------------#
# INICIO DEL MÓDULO 4 ORGINAL #
#-----------------------------#

Sys.sleep(1);cat("Módulo 4:  Proporcion del gasto en alimentación ")


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

cat("     Finalizado ✓ \n")

#-----------------------------#
# FIN    DEL MÓDULO 4 ORGINAL #
#-----------------------------#
Sys.sleep(1);cat("Módulo 5:  Reatroalimentación con el paquete Foodprice ")

invisible(capture.output({
#----------------------------------------------------------------------------------#
#    Modulo 3: Reatroalimentación con el paquete Foodprice                        #
#----------------------------------------------------------------------------------#

#-----------------------------#
# INICIO DEL MÓDULO 5 ORGINAL #
#-----------------------------#




Data_mes_año=Foodprice::DataCol(Month = Month, Year = Year, City = City)

model_household <- data.frame(
  Household = c(1, 1, 1),
  Person = c(1, 2, 3),
  Sex = c(0, 1, 1),
  Demo_Group = c("31 a 50 años", "31 a 50 años", "9 a 13 años")
)

modelo_1=Foodprice::CoCA(data=Data_mes_año,EER = EER)$cost
modelo_2=Foodprice::CoNA(data=Data_mes_año,EER_LL=EER_LL,UL=UL)$cost
modelo_3=Foodprice::CoRD(data = Data_mes_año,diverse = diverse,serv = serv)$cost

}))

#-----------------------------#
# Modelo:COCA- costo hogar Rep#
#-----------------------------#



model_dieta_1 = merge(model_household, modelo_1[c("Demo_Group", "Sex", "cost_day")],
                      by = c("Demo_Group", "Sex"),
                      all.x = TRUE, all.y = FALSE)

model_dieta_1$hogar_total = sum(as.numeric(model_dieta_1$cost_day))
model_dieta_1$per_capita = model_dieta_1$hogar_total/nrow(model_dieta_1)



#-----------------------------#
# Modelo:CONA- costo hogar Rep#
#-----------------------------#




model_dieta_2 = merge(model_household, modelo_2[c("Demo_Group", "Sex", "cost_day")],
                      by = c("Demo_Group", "Sex"),
                      all.x = TRUE, all.y = FALSE)

model_dieta_2$hogar_total = sum(as.numeric(model_dieta_2$cost_day))
model_dieta_2$per_capita = model_dieta_2$hogar_total/nrow(model_dieta_2)



#-----------------------------#
# Modelo:CORD- costo hogar Rep#
#-----------------------------#

model_household_3 <- data.frame(
  Household = c(1, 1, 1),
  Person = c(1, 2, 3),
  Sex = c(0, 1, 1),
  Demo_Group = c("31-50 años", "31-50 años", "9-13 años")
)

model_dieta_3 = merge(model_household_3, modelo_3[c("Demo_Group", "Sex", "cost_day")],
                      by = c("Demo_Group", "Sex"),
                      all.x = TRUE, all.y = FALSE)

model_dieta_3$hogar_total = sum(as.numeric(model_dieta_3$cost_day))
model_dieta_3$per_capita = model_dieta_3$hogar_total/nrow(model_dieta_3)

#-------------------------------#
# Calcular costo anual y mensual #
#--------------------------------#



model_dieta_1$per_capita_year = model_dieta_1$per_capita*365
model_dieta_2$per_capita_year = model_dieta_2$per_capita*365
model_dieta_3$per_capita_year = model_dieta_3$per_capita*365


model_dieta_1$per_capita_month = model_dieta_1$per_capita*30
model_dieta_2$per_capita_month = model_dieta_2$per_capita*30
model_dieta_3$per_capita_month = model_dieta_3$per_capita*30

cat("     Finalizado ✓ \n")


#-----------------------------#
# FIN    DEL MÓDULO 5 ORGINAL # FALTA SIMPLIFICAR Y GENERALIZAR
#-----------------------------#

#----------------------------------------------------------------------------------#
#    Modulo 4: Cálculo de indicadores de asequibilida                              #
#----------------------------------------------------------------------------------#

#-----------------------------#
# INICIO DEL MÓDULO 6 ORGINAL #
#-----------------------------#

Sys.sleep(1);cat("Módulo 6: Cálculo de indicadores de asequibilidad")

# Calcular la proporción para cada decil
dataset_def_deciles$per_capita_year = dataset_def_deciles$ingreso_alimentos_per_capita*12


outcome_1_list = list()
length(outcome_1_list) = 10

z <- as.numeric(levels(as.factor(model_dieta_1$per_capita_year)))

outcome_1_list <- lapply(deciles_grupos, function(decile) {
  # Filtrar dataset_def_deciles una vez para el decil actual
  df_y <- dataset_def_deciles %>% filter(deciles %in% decile)

  # Crear dummy vectorizado
  df_y$dummy <- ifelse(df_y$per_capita_year < z, 1, 0)

  # Filtrar df_y para obtener solo filas donde dummy es 1
  df_z <- df_y %>% filter(dummy == 1)

  # Calcular brecha relativa y su cuadrado
  df_z$brecha_rel <- (z - df_z$per_capita_year) / z
  df_z$brecha_rel_sqr <- df_z$brecha_rel^2

  # Calcular los índices
  N <- nrow(df_y)
  rate <- (nrow(df_z) / N) * 100
  gap <- sum(df_z$brecha_rel) / N
  severity <- sum(df_z$brecha_rel_sqr) / N

  # Crear el dataframe de salida
  df_w <- data.frame(deciles = decile, rate = rate, gap = gap, severity = severity)

  return(df_w)
})

# Asignar nombres a la lista de salida
names(outcome_1_list) <- deciles_grupos

calculate_outcome <- function(dataset, model, deciles_grupos) {
  z <- as.numeric(levels(as.factor(model$per_capita_year)))
  outcome_list <- list()

  for (j in 1:length(deciles_grupos)) {
    df_y <- dataset %>% filter(deciles %in% deciles_grupos[j])

    # Crear dummy vectorizado
    df_y$dummy <- ifelse(df_y$per_capita_year < z, 1, 0)

    df_z <- df_y %>% filter(dummy == 1)

    df_z$brecha_rel <- (z - df_z$per_capita_year) / z
    df_z$brecha_rel_sqr <- df_z$brecha_rel^2

    N <- nrow(df_y)
    rate <- (nrow(df_z) / N) * 100
    gap <- sum(df_z$brecha_rel) / N
    severity <- sum(df_z$brecha_rel_sqr) / N

    df_w <- data.frame(deciles = deciles_grupos[j], rate = rate, gap = gap, severity = severity)

    outcome_list[[j]] <- df_w
  }

  names(outcome_list) <- deciles_grupos
  return(outcome_list)
}

# Calcular resultados para los tres escenarios
outcome_1_list <- calculate_outcome(dataset_def_deciles, model_dieta_1, deciles_grupos)
outcome_2_list <- calculate_outcome(dataset_def_deciles, model_dieta_2, deciles_grupos)
outcome_3_list <- calculate_outcome(dataset_def_deciles, model_dieta_3, deciles_grupos)

# Combinar resultados para cada escenario
poverty_1_outcome <- do.call(rbind, outcome_1_list)
poverty_2_outcome <- do.call(rbind, outcome_2_list)
poverty_3_outcome <- do.call(rbind, outcome_3_list)



# Agregar resultados finales en un DF
poverty_1_outcome <- poverty_1_outcome %>%
  mutate(model = "CoCA")

poverty_2_outcome <- poverty_2_outcome %>%
  mutate(model = "CoNA")

poverty_3_outcome <- poverty_3_outcome %>%
  mutate(model = "CoRD")

# Unir los dataframes en uno solo
poverty_outcome <- bind_rows(poverty_1_outcome, poverty_2_outcome, poverty_3_outcome)


#--------------------------------------------------#
# Razones costo m?nimo e ingreso en alimentación   #
#--------------------------------------------------#


umbral_1 =as.numeric(levels(as.factor(model_dieta_1$per_capita_month)))
umbral_2 =as.numeric(levels(as.factor(model_dieta_2$per_capita_month)))
umbral_3 =as.numeric(levels(as.factor(model_dieta_3$per_capita_month)))


mean_income_food = mean_income_deciles[c("deciles_grupos", "food_per_capita_prom")]

mean_income_food$umbral_1 = umbral_1
mean_income_food$umbral_2= umbral_2
mean_income_food$umbral_3= umbral_3

mean_income_food$ratio_1 = mean_income_food$umbral_1/mean_income_food$food_per_capita_prom
mean_income_food$ratio_2 = mean_income_food$umbral_2/mean_income_food$food_per_capita_prom
mean_income_food$ratio_3 = mean_income_food$umbral_3/mean_income_food$food_per_capita_prom
names(mean_income_food)= c("decile_groups", "food_per_capita_avg", "threshold_1", "threshold_2", "threshold_3", "ratio_1", "ratio_2", "ratio_3")

#-----------------------------#
# FIN    DEL MÓDULO 6 ORGINAL # FALTA SIMPLIFICAR Y GENERALIZAR
#-----------------------------#

# Guardando las salidas como lista

Resultados=list(poverty_outcome,mean_income_food);names(Resultados)=c("Poverty_outcome","Mean_income_food")

# retorno

Sys.sleep(1);cat("     Finalizado ✓ \n")

return(invisible(Resultados))

}

x=Modulos(Month=1, Year=2022,City="Cali")



