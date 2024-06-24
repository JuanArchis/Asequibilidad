#------------------------------------------------#
#           Definición de la función             #
#------------------------------------------------#

Modulo_1 <- function(Month, Year, City) {
  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#
  #    Modulo 0: Librerias, val de parámetros y Carga de datos        # ------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------#
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
  Librerias_base <- c("here", "readxl", "tidyverse", "knitr", "moments", "xgboost","maditr", "mice", "VIM", "dplyr", "finalfit", "plyr","hdd","zip","httr","caret","nnet","quantreg","gridExtra","ggpubr","cowplot")
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
  #------------------------------------------------#
  #   Sub 0.1: Crear entornos y descargar datos    # ------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------#
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
  #----------------------------------------------------------------------------------#
  #    Modulo 2: Distribución de ingresos corrientes  y factores de xp               # ------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------#
  #----------------------------------------------------------------------------------#

  #---------------------------------------------#
  # INICIO DEL MÓDULO 2.1: Algoritmo ingreso GEIH #
  #---------------------------------------------#

  Sys.sleep(1);cat("Módulo 2.1: Aplicando Algoritmo GEIH ")

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
  Datos_vivi <- Datos_del_hogar_y_la_vivienda %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, PER, REGIS,FEX_C18))
  Noocup <- No_ocupados %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS, FFT))
  Ot_ing <- Otros_ingresos_e_impuestos %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Fuerza <- Fuerza_trabajo %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Ot_formas <- Otras_formas_de_trabajo %>% select(-c(PERIODO, HOGAR, CLASE, AREA, MES, DPTO, FEX_C18, PER, REGIS))
  Car_gen <- Caracteristicas_generales  %>% select(-c(PERIODO, HOGAR, REGIS))

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
  #---------------------------------------------#
  # INICIO DEL MÓDULO 2: Algoritmo ingreso GEIH # ------------------------------------------------------------------------------------------------------------------------------------------------------
  #---------------------------------------------#
  #---------------------------------------------#

  #------------------------------------------#
  #   Módulo 2: Construcción de variables    #
  #------------------------------------------#


  #--------------------------------------#
  #  Selección de variables de interés   #
  #--------------------------------------#
  variables <- c("id",
                 "directorio",                      # Llave vivienda
                 "secuencia_p",                     # Llave hogar
                 "orden",                           # Llave persona
                 "clase",                           # Dominio (urbano o rural)
                 "dominio",                         # Dominio (ciudades y AM)
                 "estrato",                         # Estrato socioeconómico
                 "p3271",                           # Sexo
                 "p6040",                           # Edad
                 "p6800",                           # Horas de trabajo en PA
                 "p3042",                           # Nivel educativo máximo
                 "p3042s1",                         # Grado
                 "p3043",                           #Título o diploma
                 "dpto",                            # Departamento
                 "p6240",                           # Actividad en que se ocupó
                 "p6430",                           # Posición en PA
                 "p6050",                           # ¿Jefe del hogar?
                 "p6426",                           # Tiempo trabajando en la empresa
                 "p6500",                           # ¿Cuánto ganó?
                 "p6090",                           # Afiliado a salud
                 "p6920",                           # Pensiones
                 "p6100",                            # Regimen SS
                 "p6510", "p6510s1","p6510s2",        # Horas extra
                 "p6545", "p6545s1","p6545s2",        # Primas
                 "p6580","p6580s1","p6580s2",         # Bonificaciones
                 "p6585s1","p6585s1a2", "p6585s1a1",  # Alimentación
                 "p6585s2","p6585s2a2", "p6585s2a1",  # Transporte
                 "p6585s3","p6585s3a2","p6585s3a1",   # Subsidio familiar
                 "p6585s4","p6585s4a2", "p6585s4a1",  # Subsidio educativo
                 "p6630s1","p6630s1a1",            # Prima de servicios
                 "p6630s2","p6630s2a1",            # Prima de navidad
                 "p6630s3","p6630s3a1",            # Prima de vacaciones
                 "p6630s4","p6630s4a1",            # Viáticos permanentes
                 "p6630s6","p6630s6a1",            # Bonificaciones anuales
                 "p6750",                          # Ganancia neta
                 "p3073",                          #¿a cuántos meses equivale la ganancia neta?
                 "p550",                           # Ganancia neta en CD
                 "p7040",                          # ¿tiene segunda actividad?
                 "p7070",                          # ¿cuánto recibió o ganó el mes pasado en ese 2do trabajo?
                 "p7045",                          # ¿cuántas horas trabajó en el segundo trabajo
                 "p6590", "p6590s1",               # Ingreso en especie (IE): alimentos
                 "p6600", "p6600s1",               # IE: vivienda
                 "p6610", "p6610s1",               # IE: transporte
                 "p6620", "p6620s1",               # IE: electrodomésticos, ropa, etc.
                 "p7422", "p7422s1",               # Ingresos de desocupados/inactivos
                 "p7500s2", "p7500s2a1",           # Dinero por pensiones o jubilaciones
                 "p7500s3","p7500s3a1",            # Dinero recibido por pensión alimenticia
                 "p7500s1", "p7500s1a1",           # Dinero por arriendos de propiedades
                 "p7505",                          # Recibió dinero por intereses, dividendos, utilidades, etc.
                 "p7510s1", "p7510s1a1",           # Dinero recibido desde el país
                 "p7510s2", "p7510s2a1",           # Dinero recibido desde otro país
                 "p7510s3", "p7510s3a1",           # Ayuda en dinero de instituciones
                 "p7510s5", "p7510s5a1",           # Dinero por inversiones (intereses, ganancias, dividendos)
                 "p7510s6", "p7510s6a1",           # Cesantías o intereses a cesantías
                 "p7510s7", "p7510s7a1",           # Otras fuentes
                 "ocu",                            # Binaria: ocupados
                 "no_ocu",                         # Binaria: no-ocupados
                 "ft",                             # Binaria: fuerza de trabajo
                 "fft",                             # Binaria: fuerza de trabajo
                 "inglabo",
                 "p6008",
                 "fex_c18"
  )

  # Seleccionar variables en la base de datos
  df_total <- df_total[variables]

  #------------------------------------------------------#
  #  Creación de variables para preceptores de ingresos  #
  #------------------------------------------------------#

  # Variable para asalariados
  # Dummy asalariados
  df_total$asalariado <- ifelse(df_total$p6430 %in% c(1,2,3,7), 1, 0)

  # Variable para independientes
  df_total$independiente <- ifelse(df_total$p6430 %in% c(4,5,8), 1, 0)

  # Variable para trabajadores familiares sin remuneración
  df_total$trab_familiares <- ifelse(df_total$p6430 ==6, 1, 0)

  # Variable para inactivos (población fuera de la fuerza de trabajo)
  df_total$ina <- ifelse(df_total$fft == 1, 1, 0)

  # Variable para desocupados
  # Consideramos que una persona desocupada es aquella que
  # pertenece a la fuerza laboral pero no está ocupada
  df_total$des <- ifelse(df_total$ft == 1 & df_total$ocu == 0, 1, 0)

  # Variable para desocupados e inactivos
  df_total$des_ina <- ifelse(df_total$des == 1 | df_total$ina == 1, 1, 0)

  #---------------------------------------------------------------------#
  # Pre-procesamiento de la base de datos: ajuste de valores en 98 y 99 #
  #---------------------------------------------------------------------#


  # Condición 1: asalariados en ingreso principal
  df_total$p6500[(df_total$asalariado == 1) &
                   (df_total$p6500 %in% c(98,99) |
                      (is.na(df_total$p6500)))] = 0

  # Condición 2: independientes en su ingreso principal
  df_total$p6750[(df_total$independiente == 1) &
                   (df_total$p6750 %in% c(98,99))] = 0

  df_total$p550[(df_total$independiente == 1) &
                  (df_total$p550 %in% c(98,99))] = 0

  # Condición 3: asalariados en subcomponentes del ingreso principal
  df_total$p6510s1[(df_total$asalariado == 1) &
                     (df_total$p6510s1 %in% c(98,99) | is.na(df_total$p6510s1))] = 0

  df_total$p6545s1[(df_total$asalariado == 1) &
                     (df_total$p6545s1 %in% c(98,99) | is.na(df_total$p6545s1))] = 0

  df_total$p6580s1[(df_total$asalariado == 1) &
                     (df_total$p6580s1 %in% c(98,99) | is.na(df_total$p6580s1))] = 0

  df_total$p6585s2a1[(df_total$asalariado == 1) &
                       (df_total$p6585s2a1 %in% c(98,99) | is.na(df_total$p6585s2a1))] = 0

  df_total$p6585s3a1[(df_total$asalariado == 1) &
                       (df_total$p6585s3a1 %in% c(98,99) | is.na(df_total$p6585s3a1))] = 0

  df_total$p6585s4a1[(df_total$asalariado == 1) &
                       (df_total$p6585s4a1 %in% c(98,99) | is.na(df_total$p6585s4a1))] = 0

  df_total$p6510s2[(df_total$asalariado == 1) &
                     (df_total$p6510s2 %in% c(98,99) | is.na(df_total$p6510s2))] = 0

  # Condición 4: si la persona afirma que el ingreso está incluido en su
  # salario, entonces es igual a 0

  df_total$p6510s1[df_total$asalariado == 1 & (df_total$p6510s2 == 1)] = 0 #Horas extra
  df_total$p6545s1[df_total$asalariado == 1 & df_total$p6545s2== 1] = 0 #Primas
  df_total$p6580s1[df_total$asalariado == 1 & (df_total$p6580s2 == 1)] = 0 # Bonificaciones
  df_total$p6585s1a1[df_total$asalariado == 1 & (df_total$p6585s1a2 == 1)] = 0 # Aux. alimentación
  df_total$p6585s2a1[df_total$asalariado == 1 & (df_total$p6585s2a2 == 1)] = 0 # Aux. transporte
  df_total$p6585s3a1[df_total$asalariado == 1 & (df_total$p6585s3a2 == 1)] = 0 # Subsidio familiar
  df_total$p6585s4a1[df_total$asalariado == 1 & (df_total$p6585s4a2 == 1)] = 0 # Subsidio educativo

  # Condición 5: para el caso del ingreso por actividad, se considera que, si la persona acepta que
  # tiene una segunda actividad, pero 98 o 99, entonces 0. Si NA, entonces NA
  # Si no tiene segunda actividad (=2), directamente p7070 = 0

  df_total$p7070[(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1) &
                   (df_total$p7040 == 1) & (df_total$p7070 %in% c(98,99))] = 0
  df_total$p7070[(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1) &
                   (df_total$p7040 == 1) & is.na(df_total$p7070)] = NA
  df_total$p7070[(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1) &
                   (df_total$p7040 == 2)] = 0

  # Condición 6: lo mismo aplicaría para el caso de los componentes del IE
  df_total$p6590s1[(df_total$asalariado == 1) & (df_total$p6590 == 2)] = 0
  df_total$p6590s1[(df_total$asalariado == 1) & (df_total$p6590 == 1) &
                     (df_total$p6590s1 %in%c(98,99))] = 0

  df_total$p6600s1[(df_total$asalariado == 1) & (df_total$p6600 == 2)] = 0
  df_total$p6600s1[(df_total$asalariado == 1) & (df_total$p6600 == 1) &
                     (df_total$p6600s1 %in%c(98,99))] = 0

  df_total$p6610s1[(df_total$asalariado == 1) & (df_total$p6610 == 2)] = 0
  df_total$p6610s1[(df_total$asalariado == 1) & (df_total$p6610 == 1) &
                     (df_total$p6610s1 %in%c(98,99))] = 0

  df_total$p6620s1[(df_total$asalariado == 1) & (df_total$p6620 == 2)] = 0
  df_total$p6620s1[(df_total$asalariado == 1) & (df_total$p6620 == 1) &
                     (df_total$p6620s1 %in%c(98,99))] = 0

  # Condición 7: criterios aplicados sobre IMDI (p7422, p7422s1)
  df_total$p7422s1[(df_total$des_ina == 1) &
                     (df_total$p7422 == 1) & (df_total$p7422s1 %in% c(98,99))] = 0
  df_total$p7422s1[(df_total$des_ina == 1) &
                     (df_total$p7422 == 1) & is.na(df_total$p7422s1)] = NA
  df_total$p7422s1[(df_total$des_ina == 1) &
                     (df_total$p7422 == 2)] = 0

  # Condición 8: criterios aplicados para IOF1
  # Si marca que no sabe o que no recibió, directamente es 0
  # Si marca que sí recibió, pero marca 98 o 99, entonces 0
  df_total$p7510s5a1[(df_total$p7510s5 == 1) & (df_total$p7510s5a1 %in% c(98,99))] = 0
  df_total$p7510s5a1[(df_total$p7510s5 %in% c(2,9))] = 0

  # Condición 9: idénticos criterios aplicados para IOF2
  # Aquí hay una limitación en el estudio: no conocemos los registros administrativos para complementar
  # la información de las personas que no reportaron ningún valor
  df_total$p7500s2a1[(df_total$p7500s2 == 1) & (df_total$p7500s2a1 %in% c(98,99))] = 0
  df_total$p7500s2a1[(df_total$p7500s2 == 1) & is.na(df_total$p7500s2a1)] = NA
  df_total$p7500s2a1[(df_total$p7500s2 == 2)] = 0

  # Condición 10: idénticos criterios aplicados para IOF3
  # Aquí hay una limitación en el estudio: no conocemos los registros administrativos para complementar
  # la información de las personas que no reportaron ningún valor
  df_total$p7510s1a1[(df_total$p7510s1 == 1) & (df_total$p7510s1a1 %in% c(98,99))] = 0
  df_total$p7510s1a1[(df_total$p7510s1 == 1) & is.na(df_total$p7510s1a1)] = NA
  df_total$p7510s1a1[(df_total$p7510s1 == 2)] = 0

  df_total$p7510s2a1[(df_total$p7510s2 == 1) & (df_total$p7510s2a1 %in% c(98,99))] = 0
  df_total$p7510s2a1[(df_total$p7510s2 == 1) & is.na(df_total$p7510s2a1)] = NA
  df_total$p7510s2a1[(df_total$p7510s2 == 2)] = 0

  df_total$p7500s3a1[(df_total$p7500s3 == 1) & (df_total$p7500s3a1 %in% c(98,99))] = 0
  df_total$p7500s3a1[(df_total$p7500s3 == 1) & is.na(df_total$p7500s3a1)] = NA
  df_total$p7500s3a1[(df_total$p7500s3 == 2)] = 0

  df_total$p7510s3a1[(df_total$p7510s3 == 1) & (df_total$p7510s3a1 %in% c(98,99))] = 0
  df_total$p7510s3a1[(df_total$p7510s3 == 1) & is.na(df_total$p7510s3a1)] = NA
  df_total$p7510s3a1[(df_total$p7510s3 == 2)] = 0

  # Condición 11: idénticos criterios aplicados para IOF6
  # Se emplea la siguiente variable: p7500s1a1
  df_total$p7500s1a1[(df_total$p7500s1 == 1) & (df_total$p7500s1a1 %in% c(98,99))] = 0
  df_total$p7500s1a1[(df_total$p7500s1 == 1) & is.na(df_total$p7500s1a1)] = NA
  df_total$p7500s1a1[(df_total$p7500s1 == 2)] = 0



  #--------------------------------------------------------------------#
  #   Módulo 2.2.1: Definición de variables explicativas individuales    #
  #--------------------------------------------------------------------#


  # Ejecutar los módulos anteriores:


  # Variable edad y edad al cuadrado
  df_total$edad <- df_total$p6040
  df_total$edad_sqr <- df_total$edad^2

  # Horas trabajadas en la primera actividad
  df_total$horas_pa <- df_total$p6800

  # Horas trabajadas en la segunda actividad
  df_total$horas_sa <- df_total$p7045

  # Años de educación
  df_total$edu <- df_total$p3042
  df_total$grado <- df_total$p3042s1
  df_total <- df_total %>% filter(!grado %in% c(98,99)) # Eliminar a los que no conocen su grado de educación

  df_total$anios_edu <- 0
  df_total$anios_edu[which(df_total$edu == 1 & !is.na(df_total$edu))] = 0
  df_total$anios_edu[which(df_total$edu == 2 & !is.na(df_total$edu))] = df_total$grado[which(df_total$edu == 2 & !is.na(df_total$edu))]
  df_total$anios_edu[df_total$edu == 3 & !is.na(df_total$edu)] = 1 + df_total$grado[df_total$edu == 3 & !is.na(df_total$edu)]
  df_total$anios_edu[df_total$edu == 4 & !is.na(df_total$edu)] = 6 + df_total$grado[df_total$edu == 4 & !is.na(df_total$edu)]
  df_total$anios_edu[(df_total$edu == 5 | df_total$edu == 6) & !is.na(df_total$edu)] = 6 + df_total$grado[(df_total$edu == 5 | df_total$edu == 6) & !is.na(df_total$edu)]
  df_total$anios_edu[df_total$edu %in% c(7:10) & !is.na(df_total$edu)] = 12 + df_total$grado[df_total$edu %in% c(7:10) & !is.na(df_total$edu)]
  df_total$anios_edu[df_total$edu %in% c(11,12) & !is.na(df_total$edu)] = 17 + df_total$grado[df_total$edu %in% c(11,12) & !is.na(df_total$edu)]
  df_total$anios_edu[df_total$edu %in% c(13) & !is.na(df_total$edu)] = 19 + df_total$grado[df_total$edu %in% c(13) & !is.na(df_total$edu)]

  # Dummy Bogotá
  df_total$bogota <- ifelse(df_total$dpto == 11, 1, 0)

  # Dummy Sexo
  df_total$sexo <- ifelse(df_total$p3271 == 1, 0, 1)

  # One-hot encoding para la variable "posición laboral"
  # Eliminar a las observaciones con posición laboral == otros
  df_total$posicion <- factor(df_total$p6430, levels = c(1:9))
  df_total$posicion <- dplyr::recode(df_total$posicion, "1" = "obrero", "2" = "obrero",
                                     "3" = "domestico", "4" = "propia", "5" = "patrono",
                                     "6" = "domestico", "7" = "domestico",
                                     "8" = "obrero", "9" = "otros")
  df_total <- df_total %>% mutate(value = 1)  %>% spread(posicion, value,  fill = 0 )

  # Número de meses trabajando
  df_total$meses_trab <- df_total$p6426

  # Dummy jefe del hogar
  df_total$jefe <- ifelse(df_total$p6050 == 1, 1, 0)

  # Dummy menor de 5 años
  df_total$edad_5 <- ifelse(df_total$edad < 5, 1, 0)

  # Dummy adolescentes 14 - 17 años
  df_total$edad_14_17 <- ifelse(df_total$edad >= 14 &
                                  df_total$edad <= 17, 1, 0)

  # Dummy ancianos 65 años o más
  df_total$edad_65 <- ifelse(df_total$edad >= 65, 1, 0)

  # Dummy (personas de 25 años o más sin educación)
  df_total$edad_25_ne <- ifelse(df_total$edad >= 25 &
                                  df_total$edu == 1, 1, 0)

  # Dummy educación superior
  df_total$superior <- ifelse(df_total$edu %in% c(10:13), 1, 0)

  # Dummy para afiliado a salud
  df_total$salud <- ifelse(df_total$p6090 == 1, 1, 0)

  # Dummy Estudiantes
  df_total$estu <- ifelse(df_total$p6240 == 3, 1, 0)

  #---------------------------------------------------------------------#
  # Anexo 1: valores faltantes en horas trabajadas en primera actividad #
  #---------------------------------------------------------------------#

  # Si las horas trabajadas === 998, entonces se imputan 121 horas
  # Si las horas trabajadas son faltantes (=== 999), entonces se imputa el promedio en grupos de p6430

  # Calcular el promedio por grupos de acuerdo con p6430
  avg_horas_pa <- df_total %>% group_by(p6430) %>% summarize(avg = mean(horas_pa, na.rm = T))


  # Imputación de valores faltantes en horas trabajadas
  df_total$horas_pa[df_total$horas_pa == 998] = 121

  # Reemplazo condicional para horas_pa
  for (i in 1:8) {
    if (any(avg_horas_pa$p6430 == i)) {
      replacement_value <- avg_horas_pa$avg[avg_horas_pa$p6430 == i]
      df_total$horas_pa[df_total$horas_pa == 999 & df_total$p6430 == i] <- replacement_value
    }
  }


  #----------------------------------------------------------#
  # Anexo 2: valores faltantes en número de meses trabajados #
  #----------------------------------------------------------#

  # Si el número de meses trabajados es 998 o 999 o si la razón edad/años trabajados < 1.2, se imputa el promedio en grupos de p6430

  # Cálculo del promedio por grupos según p6430
  avg_meses <- df_total %>% group_by(p6430) %>% summarize(avg = mean(meses_trab, na.rm = TRUE))
  df_total$ratio_edad_tt <- df_total$edad / (df_total$meses_trab / 12)

  # Definición de la condición general (valor faltante o razón < 1.2)
  c1 <- (df_total$meses_trab %in% c(998, 999) | df_total$ratio_edad_tt < 1.2)

  # Reemplazo condicional para meses_trab
  for (i in 1:8) {
    if (any(avg_meses$p6430 == i)) {
      replacement_value <- avg_meses$avg[avg_meses$p6430 == i]
      df_total$meses_trab[c1 & df_total$p6430 == i] <- replacement_value
    }
  }

  #---------------------------------------------------------------------#
  # Anexo 3: valores faltantes en horas trabajadas en segunda actividad #
  #---------------------------------------------------------------------#

  # Si las horas trabajadas son 998, entonces se imputan 121 horas
  # Si las horas trabajadas son faltantes (=== 999), entonces se imputa el promedio en grupos de p6430

  # Calcular el promedio por grupos de acuerdo con p6430
  avg_horas_sa <- df_total %>% group_by(p6430) %>% summarize(avg = mean(horas_sa, na.rm = TRUE))

  # Imputación de valores faltantes en horas trabajadas en segunda actividad
  df_total$horas_sa[df_total$horas_sa == 998] = 121

  # Reemplazo condicional para horas_sa
  for (i in 1:8) {
    if (any(avg_horas_sa$p6430 == i)) {
      replacement_value <- avg_horas_sa$avg[avg_horas_sa$p6430 == i]
      df_total$horas_sa[df_total$horas_sa == 999 & df_total$p6430 == i] <- replacement_value
    }
  }

  #--------------------------------------------------------------------#
  #    Módulo 2.2.2: Definición de variables explicativas por hogar      #
  #--------------------------------------------------------------------#

  # Crear variable para identificar cada hogar (directorio y secuencia_p)
  df_total$id_hogar <- paste0(df_total$directorio,"-",
                              df_total$secuencia_p)

  df_hogar <- df_total %>% group_by(id_hogar) %>%
    dplyr::summarize(  n = n(),
                       n_asala = sum(asalariado, na.rm = T),
                       n_indep = sum(independiente, na.rm = T),
                       n_desoc = sum(des, na.rm = T),
                       n_edad_5 = sum(edad_5, na.rm = T),
                       n_edad_14_17 = sum(edad_14_17, na.rm =T),
                       n_edad_65 = sum(edad_65, na.rm = T),
                       n_edad_25_ne = sum(edad_25_ne, na.rm = T),
                       n_superior = sum(superior, na.rm = T),
                       n_salud = sum(salud, na.rm = T),
                       avg_edu = mean(edu, na.rm = T))

  df_total <- merge(df_total, df_hogar, by = "id_hogar", all.x = T)

  #------------------------------------------------------#
  #------------------------------------------------------#
  #   Módulo 3 ALgoritmo GEIH: Cálculo de los componentes del ingreso# ------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------#
  #------------------------------------------------------#

  #--------------------------------------------------#
  #  Ingreso Monetario por Primera Actividad (IMPA)  #
  #--------------------------------------------------#

  # Definir variables para calcular IMPA
  sum_impa <- c("p6500",
                "p6510s1",    # Horas extra
                "p6545s1",    # Primas
                "p6580s1",    # Bonificaciones
                "p6585s1a1",  # Alimentación
                "p6585s2a1",  # Transporte
                "p6585s3a1",  # Subsidio familiar
                "p6585s4a1",  # Subsidio educativo
                "p6630s1a1",  # Prima de servicios
                "p6630s2a1",  # Prima de navidad
                "p6630s3a1",  # Prima de vacaciones
                "p6630s4a1",  # Viáticos permanentes
                "p6630s6a1",  # Bonificaciones anuales
                "p6750",      # Ganancia neta
                "p550"        # Ganancia neta en CD
  )

  # Normalizar datos anuales para ingresos adicionales al salario
  # (e.g. bonificaciones, prima de servicios, prima de navidad, etc.)
  df_total$p6630s1a1 = df_total$p6630s1a1/12
  df_total$p6630s2a1 = df_total$p6630s2a1/12
  df_total$p6630s3a1 = df_total$p6630s3a1/12
  df_total$p6630s4a1 = df_total$p6630s4a1/12
  df_total$p6630s6a1 = df_total$p6630s6a1/12

  # Normalizar ganacias netas
  # Ganancia neta/número de meses que representa la ganancia
  # Ejemplo: ganancia neta de 12.000 que representa 10 meses (ganancia sumada = 1.200)
  df_total$p6750 = df_total$p6750/df_total$p3073
  df_total$p550 = df_total$p550/12

  # Determinar el ingreso para los asalariados
  df_total$IMPA <- NA
  df_total$IMPA <- ifelse(df_total$asalariado == 1,
                          rowSums(df_total[sum_impa], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_impa])),
                          ifelse(df_total$independiente == 1,
                                 rowSums(df_total[c("p6750", "p550")], na.rm = T)*NA^!rowSums(!is.na(df_total[c("p6750", "p550")])),
                                 NA))

  #-------------------------------------------------#
  #  Ingreso Monetario por Segunda Actividad (ISA)  #------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------#
  # Variables para sumar
  sum_isa <- c("p7070")

  # Determinar el ingreso para la segunda actividad
  df_total$ISA <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                         rowSums(df_total[sum_isa], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_isa])),
                         NA)

  #----------------------------#
  #  Ingreso en Especie (IE)   #
  #----------------------------#
  # Variables para sumar
  sum_ie <- c("p6590s1","p6600s1",
              "p6610s1","p6620s1")

  # Determinar el ingreso en especie
  df_total$IE <- ifelse(df_total$asalariado == 1,
                        rowSums(df_total[sum_ie], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_ie])), NA)

  #----------------------------------------------------#
  #  Ingreso Monetario Desocupados e Inactivos (IMDI)  #
  #----------------------------------------------------#
  # Variables para sumar
  sum_imdi <- c("p7422s1")

  # Determinar el ingreso para desocupados e inactivos
  df_total$IMDI <- ifelse(df_total$des_ina == 1 & df_total$p7422 == 1,
                          rowSums(df_total[sum_imdi], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_imdi])), NA)

  #-----------------------------------------------------#
  #         Ingresos de Otras Fuentes (IOF)             #
  #-----------------------------------------------------#
  # Normalizar por ingreso mensual para el cálculo general del IOF para ocupados y no-ocupados
  df_total$p7510s1a1 = df_total$p7510s1a1/12
  df_total$p7510s2a1 = df_total$p7510s2a1/12
  df_total$p7510s3a1 = df_total$p7510s3a1/12
  df_total$p7510s5a1 = df_total$p7510s5a1/12
  df_total$p7510s6a1 = df_total$p7510s6a1/12
  df_total$p7510s7a1 = df_total$p7510s7a1/12

  #--------------------------------------------------------#
  #     Ingresos de Otras Fuentes (IOF) para Ocupados (O)  #
  #--------------------------------------------------------#

  # Ingreso IOF1: Ingresos por intereses y dividendo
  sum_IOF1_o <- c("p7510s5a1")

  df_total$IOF1_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                            rowSums(df_total[sum_IOF1_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF1_o])),
                            NA)

  # Ingreso IOF2: Ingresos por jubilaciones y pensions
  sum_IOF2_o <- c("p7500s2a1")

  df_total$IOF2_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                            rowSums(df_total[sum_IOF2_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF2_o])),
                            NA)

  # Ingreso IOF3H: Ingresos por ayuda de hogares
  sum_IOF3h_o <- c("p7510s1a1", "p7510s2a1", "p7500s3a1")

  df_total$IOF3h_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                             rowSums(df_total[sum_IOF3h_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3h_o])),
                             NA)

  # Ingreso IOF3I: Ingresos por ayuda de instituciones
  sum_IOF3i_o <- c("p7510s3a1")

  df_total$IOF3i_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                             rowSums(df_total[sum_IOF3i_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3i_o])),
                             NA)

  # Ingreso IOF3: Ingresos por ayuda de instituciones u otros hogares
  sum_IOF3_o <- c("IOF3h_o", "IOF3i_o")

  df_total$IOF3_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                            rowSums(df_total[sum_IOF3_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3_o])),
                            NA)

  # Ingreso IOF6: Ingresos por rentas (arriendos)
  sum_IOF6_o <- c("p7500s1a1")

  df_total$IOF6_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                            rowSums(df_total[sum_IOF6_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF6_o])),
                            NA)

  # Ingreso IOF general para ocupados (O)
  sum_IOF_o <- c("IOF1_o", "IOF2_o", "IOF3_o", "IOF6_o")

  df_total$IOF_o <- ifelse(df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1,
                           rowSums(df_total[sum_IOF_o], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF_o])),
                           NA)

  #-------------------------------------------------------------------------#
  #     Ingresos de Otras Fuentes (IOF) para Desocupados e Inactivos (DI)   #
  #-------------------------------------------------------------------------#

  # Ingreso IOF1: Ingresos por intereses y dividendo
  sum_IOF1_no <- c("p7510s5a1")

  df_total$IOF1_no <- ifelse(df_total$des_ina == 1,
                             rowSums(df_total[sum_IOF1_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF1_no])),
                             NA)

  # Ingreso IOF2: Ingresos por jubilaciones y pensions
  sum_IOF2_no <- c("p7500s2a1")

  df_total$IOF2_no <- ifelse(df_total$des_ina == 1,
                             rowSums(df_total[sum_IOF2_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF2_no])),
                             NA)

  # Ingreso IOF3H: Ingresos por ayuda de hogares
  sum_IOF3h_no <- c("p7510s1a1", "p7510s2a1", "p7500s3a1")

  df_total$IOF3h_no <- ifelse(df_total$des_ina == 1,
                              rowSums(df_total[sum_IOF3h_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3h_no])),
                              NA)

  # Ingreso IOF3I: Ingresos por ayuda de instituciones
  sum_IOF3i_no <- c("p7510s3a1")

  df_total$IOF3i_no <- ifelse(df_total$des_ina == 1,
                              rowSums(df_total[sum_IOF3i_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3i_no])),
                              NA)

  # Ingreso IOF3: Ingresos por ayuda de instituciones u otros hogares
  sum_IOF3_no <- c("IOF3h_no", "IOF3i_no")

  df_total$IOF3_no <- ifelse(df_total$des_ina == 1,
                             rowSums(df_total[sum_IOF3_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3_no])),
                             NA)

  # Ingreso IOF6: Ingresos por rentas (arriendos)
  sum_IOF6_no <- c("p7500s1a1")

  df_total$IOF6_no <- ifelse(df_total$des_ina == 1,
                             rowSums(df_total[sum_IOF6_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF6_no])),
                             NA)

  # Ingreso IOF general para no-ocupados, i.e., desocupados e inactivods (DI)
  sum_IOF_no <- c("IOF1_no", "IOF2_no", "IOF3_no", "IOF6_no")

  df_total$IOF_no <- ifelse(df_total$des_ina == 1,
                            rowSums(df_total[sum_IOF_no], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF_no])),
                            NA)


  #-------------------------------------------------------------------------------------#
  #     Ingresos de Otras Fuentes (IOF) generales para ocupados (O) y no-ocupados (DI)  #
  #-------------------------------------------------------------------------------------#

  # Ingreso IOF1
  sum_IOF1 <- c("IOF1_o", "IOF1_no")

  df_total$IOF1 <- rowSums(df_total[sum_IOF1], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF1]))

  # Ingreso IOF2
  sum_IOF2 <- c("IOF2_o", "IOF2_no")

  df_total$IOF2 <- rowSums(df_total[sum_IOF2], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF2]))

  # Ingreso IOF3H
  sum_IOF3h <- c("IOF3h_o", "IOF3h_no")

  df_total$IOF3h <- rowSums(df_total[sum_IOF3h], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3h]))

  # Ingreso IOF3I
  sum_IOF3i <- c("IOF3i_o", "IOF3i_no")

  df_total$IOF3i <- rowSums(df_total[sum_IOF3i], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3i]))

  # Ingreso IOF3
  sum_IOF3 <- c("IOF3_o", "IOF3_no")

  df_total$IOF3 <- rowSums(df_total[sum_IOF3], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF3]))

  # Ingreso IOF6
  sum_IOF6 <- c("IOF6_o", "IOF6_no")

  df_total$IOF6 <- rowSums(df_total[sum_IOF6], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF6]))

  # Ingreso IOF general
  sum_IOF <- c("IOF1", "IOF2", "IOF3", "IOF6")

  df_total$IOF <- rowSums(df_total[sum_IOF], na.rm = T)*NA^!rowSums(!is.na(df_total[sum_IOF]))


  #------------------------------------------------------#
  #   Módulo 3.1: Output: componentes del ingreso        #
  #------------------------------------------------------#


  #-----------------------------------------------------------------------#
  #      Ingresos totales (IMPA, ISA, IE, IMDI, IOF1, IOF2, IOF3, IOF6)   #
  #-----------------------------------------------------------------------#

  # Ingresos de asalariados
  df_total$ING_ASAL <- ifelse(df_total$asalariado == 1,
                              rowSums(df_total[c("IMPA", "IE",
                                                 "ISA", "IOF1", "IOF2",
                                                 "IOF3", "IOF6")],
                                      na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMPA", "IE",
                                                                                  "ISA", "IOF1", "IOF2",
                                                                                  "IOF3", "IOF6")])) == 0),NA)

  # Ingresos de independientes
  df_total$ING_IND <- ifelse(df_total$independiente == 1,
                             rowSums(df_total[c("IMPA","ISA", "IOF1", "IOF2",
                                                "IOF3", "IOF6")],
                                     na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMPA","ISA", "IOF1", "IOF2",
                                                                                 "IOF3", "IOF6")])) == 0),NA)

  # Ingresos de trabajadores familiares
  df_total$ING_TF <- ifelse(df_total$trab_familiares == 1,
                            rowSums(df_total[c("ISA", "IOF1", "IOF2",
                                               "IOF3", "IOF6")],
                                    na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("ISA", "IOF1", "IOF2",
                                                                                "IOF3", "IOF6")])) == 0),NA)

  # Ingreso de desocupados e inactivos
  df_total$ING_NO <- ifelse(df_total$des_ina == 1,
                            rowSums(df_total[c("IMDI", "IOF1", "IOF2",
                                               "IOF3", "IOF6")],
                                    na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMDI", "IOF1", "IOF2",
                                                                                "IOF3", "IOF6")])) == 0),NA)

  # Cálculo del ingreso total
  df_total$INGTOTOB <- rowSums(df_total[c("ING_ASAL",
                                          "ING_IND", "ING_TF",
                                          "ING_NO")],
                               na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("ING_ASAL",
                                                                           "ING_IND", "ING_TF",
                                                                           "ING_NO")])) == 0)



  #------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------#
  #   Módulo 4: Identificación de valores faltantes en cada componente del ingreso     # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------------#

  # Identificamos tres tipos de valores faltantes:

  # 1. Por convención: los código 98, 99, 999, 9999. etc. denotan valores faltantes;
  # sin embargo, se considera valor faltante únicamente cuando la pregunta principal está vacía

  # 2. Por falta de conocimiento: la persona no reporta el valor de su salario o la reporta en 0
  # y tampoco sabe si recibió otros beneficios (horas extra, bonificaciones, por ejemplo)

  # 3. Por inconsistencias: la respuesta a la pregunta es un valor faltante y tampoco
  # hay datos sobre los ingresos (ejemplo: el encuestado dice que recibió horas extra,
  # dice también que no están incluidas en el valor que reportó sobre su salio, pero
  # no deja el valor explícito).

  # Para guardar los valores faltantes, creamos la variable vf (1 = valor faltante, 0 = otro caso)

  # Nótese que los tres tipos de valores faltantes no aparecen necesariamente en todos los
  # componentes del ingreso.

  #---------------------------------------------------------------------------#
  # Valores faltantes para el Ingreso Monetario por Primero Actividad (IMPA)  #
  #---------------------------------------------------------------------------#

  df_total$vf_impa <- 0

  #----------------------------------------------------------#
  # 1. Valores faltantes por convención: 98, 99, 999, 9999   #
  #----------------------------------------------------------#

  # Los condicionales establecen que el ingreso es un valor faltante cuando P6630S1A1 es faltante y,
  # además, el salario principal (P6500) es 0 o faltante.
  # Defino el siguiente condicional general para los asalariados
  c0 <- df_total$asalariado==1 & (df_total$p6500 == 0 | is.na(df_total$p6500) | df_total$p6500 %in% c(98,99))

  # ¿Cuánto ganó el mes pasado? (P6500): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[(df_total$p6500 %in% c(98,99) & !is.na(df_total$p6500))] = 1

  # Prima de servicios (P6630S1A1): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[c0 & (df_total$p6630s1a1 %in% c(98,99) & !is.na(df_total$p6630s1a1))] = 1

  # Prima de navidad (P6630S2A1): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[c0 & (df_total$p6630s2a1 %in% c(98,99) & !is.na(df_total$p6630s2a1))] = 1

  # Prima de vacaciones (P6630S3A1): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[c0 & (df_total$p6630s3a1 %in% c(98,99) & !is.na(df_total$p6630s3a1))] = 1

  # Viáticos permanentes (P6630S4A1): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[c0 & (df_total$p6630s4a1 %in% c(98,99) & !is.na(df_total$p6630s4a1))] = 1


  # Bonificaciones anuales(P6630S6A1): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[c0 & (df_total$p6630s6a1 %in% c(98,99) & !is.na(df_total$p6630s6a1))] = 1

  # En el caso de los independientes, los valores faltantes son identificados para las fuentes p6750 y p550 (ganancias netas)
  # Ganancia neta (P6750): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[df_total$independiente==1 & (df_total$p6750 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p6750)) &
                     (df_total$p550 %in% c(98,99, 999, 9999, 99999, 999999) |
                        is.na(df_total$p550) | df_total$p550==0)] = 1

  # Ganancia neta en negocio o cosecha (P550): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_impa[df_total$independiente==1 & (df_total$p550 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p550)) &
                     (df_total$p6750 %in% c(98,99, 999, 9999, 99999, 999999) |
                        is.na(df_total$p6750) | df_total$p6750==0)] = 1

  #--------------------------------------------#
  # 2. Valores faltantes por desconocimiento   #
  #--------------------------------------------#
  # En este caso, no sabe (9) para las siguientes preguntas:
  # Nótese que es valor faltante siempre y cuando no exista información sobre el salario

  # ¿Recibió horas extras? (P6510)
  df_total$vf_impa[c0 & (df_total$p6510 == 9 & !is.na(df_total$p6510))] = 1

  # ¿Recibió ingresos por primas? (P6545)
  df_total$vf_impa[c0 & (df_total$p6545 == 9 & !is.na(df_total$p6545))] = 1

  # ¿Recibió ingresos por bonificaciones? (p6580)
  df_total$vf_impa[c0 & (df_total$p6580 == 9 & !is.na(df_total$p6580))] = 1

  # ¿Recibió ingresos por auxilio de alimentación? (p6585s1)
  df_total$vf_impa[c0 & (df_total$p6585s1 == 9 & !is.na(df_total$p6585s1))] = 1

  # ¿Recibió ingresos por auxilio de transporte? (p6585s2)
  df_total$vf_impa[c0 & (df_total$p6585s2 == 9 & !is.na(df_total$p6585s2))] = 1

  # ¿Recibió ingresos por subsidio familiar? (p6585s3)
  df_total$vf_impa[c0 & (df_total$p6585s3 == 9 & !is.na(df_total$p6585s3))] = 1

  # ¿Recibió ingresos por subsidio educativo? (p6585s4)
  df_total$vf_impa[c0 & (df_total$p6585s4 == 9 & !is.na(df_total$p6585s4))] = 1

  #---------------------------------------------#
  # 3. Valores faltantes por inconsistencias    #
  #---------------------------------------------#

  # ¿Recibió horas extras? (p6510)
  df_total$vf_impa[c0 & (df_total$p6510 == 1) & (is.na(df_total$p6510s1) | df_total$p6510s1 %in% c(0,98,99)) & df_total$p6510s2 == 2] = 1

  # ¿Recibió ingresos por primas? (p6545)
  df_total$vf_impa[c0 & (df_total$p6545 == 1) & (is.na(df_total$p6545s1) | df_total$p6545s1 %in% c(0,98,99)) & df_total$p6545s2 == 2] = 1

  # ¿Recibió ingresos por bonificaciones? (p6580)
  df_total$vf_impa[c0 & (df_total$p6580 == 1) & (is.na(df_total$p6580s1) | df_total$p6580s1 %in% c(0,98,99)) & df_total$p6580s2 == 2] = 1

  # ¿Recibió ingresos por auxilio de alimentación? (p6585s1)
  df_total$vf_impa[c0 & (df_total$p6585s1 == 1) & (is.na(df_total$p6585s1a1) | df_total$p6585s1a1 %in% c(0,98,99)) & df_total$p6585s1a2 == 2] = 1

  # ¿Recibió ingresos por auxilio de transporte? (p6585s2)
  df_total$vf_impa[c0 & (df_total$p6585s2 == 1) & (is.na(df_total$p6585s2a1) | df_total$p6585s2a1 %in% c(0,98,99)) & df_total$p6585s2a2 == 2] = 1

  # ¿Recibió ingresos por subsidio familiar? (p6585s3)
  df_total$vf_impa[c0 & (df_total$p6585s3 == 1) & (is.na(df_total$p6585s3a1) | df_total$p6585s3a1 %in% c(0,98,99)) & df_total$p6585s3a2 == 2] = 1

  # ¿Recibió ingresos por subsidio educativo? (p6585s4)
  df_total$vf_impa[c0 & (df_total$p6585s4 == 1) & (is.na(df_total$p6585s4a1) | df_total$p6585s4a1 %in% c(0,98,99)) & df_total$p6585s4a2 == 2] = 1

  # Prima de servicios (P6630S1A1)
  df_total$vf_impa[c0 & (df_total$p6630s1 == 1) & (is.na(df_total$p6630s1a1) | df_total$p6630s1a1 %in% c(0, 98, 99))] = 1

  # Prima de navidad (P6630S2A1)
  df_total$vf_impa[c0 & (df_total$p6630s2 == 1) & (is.na(df_total$p6630s2a1) | df_total$p6630s2a1 %in% c(0, 98, 99))] = 1

  # Prima de vacaciones (P6630S3A1)
  df_total$vf_impa[c0 & (df_total$p6630s3 == 1) & (is.na(df_total$p6630s3a1) | df_total$p6630s3a1 %in% c(0, 98, 99))] = 1

  # Viáticos permanentes (P6630S4A1)
  df_total$vf_impa[c0 & (df_total$p6630s4 == 1) & (is.na(df_total$p6630s4a1) | df_total$p6630s4a1 %in% c(0, 98, 99))] = 1

  # Bonificaciones anuales(P6630S6A1)
  df_total$vf_impa[c0 & (df_total$p6630s6 == 1) & (is.na(df_total$p6630s6a1) | df_total$p6630s6a1 %in% c(0, 98, 99))] = 1

  #---------------------------------------------------------------------------#
  # Valores faltantes para el Ingreso Monetario por Segunda Actividad (ISA)   #
  #---------------------------------------------------------------------------#

  df_total$vf_isa <- 0

  #----------------------------------------------------------#
  # 1. Valores faltantes por convención: 98, 99, 999, 9999   #
  #----------------------------------------------------------#

  # Los condicionales establecen que el ingreso es un valor faltante cuando en P7040 responde que
  # tuvo una segunda actividad, pero no reporta el ingreso en p7070
  # ¿Cuánto ganó en esta actividad secundaria? (p7070): 98 (no sabe el monto) y 99 (no sabe si recibió)
  df_total$vf_isa[df_total$p7040 == 1 & df_total$p7070 %in% c(98,99, 999, 9999, 99999, 999999)] = 1

  #---------------------------------------------#
  # 3. Valores faltantes por inconsistencias    #
  #---------------------------------------------#

  # La persona es ocupado (asalaiado, independiente o trabajador doméstico),
  # responde que sí recibe dinero por una segunda actividad pero no reporta el valor
  df_total$vf_isa[(df_total$asalariado | df_total$independiente == 1) & df_total$p7040 == 1 & (df_total$p7070 == 0 | is.na(df_total$p7070))] = 1

  #----------------------------------------------------------#
  #    Valores faltantes para el Ingreso en Especie (IE)     #
  #----------------------------------------------------------#

  df_total$vf_ie <- 0

  #----------------------------------------------------------#
  # 1. Valores faltantes por convención: 98, 99, 999, 9999   #
  #----------------------------------------------------------#
  # Recibieron ingreso en especie pero establecen el valor faltante por convención

  # IE: Alimentos (p6590s1):
  df_total$vf_ie[df_total$p6590 == 1 & df_total$p6590s1 %in% c(98,99,999,9999,99999,999999)] = 1

  # IE: Vivienda (p6600s1):
  df_total$vf_ie[df_total$p6600 == 1 & df_total$p6600s1 %in% c(98,99,999,9999,99999,999999)] = 1

  # IE: Transporte (p6610s1):
  df_total$vf_ie[df_total$p6610 == 1 & df_total$p6610s1 %in% c(98,99,999,9999,99999,999999)] = 1

  # IE: electrodomésticos, ropa, etc. (p6620s1):
  df_total$vf_ie[df_total$p6620 == 1 & df_total$p6620s1 %in% c(98,99,999,9999,99999,999999)] = 1

  #----------------------------------------------------#
  # 2. Valores faltantes por falta de conocimiento     #
  #----------------------------------------------------#
  # No sabe si recibió (cod = 9)

  # IE: Alimentos (p6590s1):
  df_total$vf_ie[df_total$p6590 == 9] = 1

  # IE: Vivienda (p6600s1):
  df_total$vf_ie[df_total$p6600 == 9] = 1

  # IE: Transporte (p6610s1):
  df_total$vf_ie[df_total$p6610 == 9] = 1

  # IE: electrodomésticos, ropa, etc. (p6620s1):
  df_total$vf_ie[df_total$p6620 == 9] = 1

  #---------------------------------------------#
  # 3. Valores faltantes por inconsistencias    #
  #---------------------------------------------#

  # Se consideran NAs aquellos que respondieron que reciben IE pero no ofrecen ningún valor

  # IE: Alimentos (p6590s1):
  df_total$vf_ie[df_total$p6590 == 1 & (df_total$p6590s1 == 0 | is.na(df_total$p6590s1))] = 1

  # IE: Vivienda (p6600s1):
  df_total$vf_ie[df_total$p6600 == 1 & (df_total$p6600s1 == 0 | is.na(df_total$p6600s1))] = 1

  # IE: Transporte (p6610s1):
  df_total$vf_ie[df_total$p6610 == 1 & (df_total$p6610s1 == 0 | is.na(df_total$p6610s1))] = 1

  # IE: electrodomésticos, ropa, etc. (p6620s1):
  df_total$vf_ie[df_total$p6620 == 1 & (df_total$p6620s1 == 0 | is.na(df_total$p6620s1))] = 1

  #--------------------------------------------------------------------------------------#
  #    Valores faltantes para el Ingreso Monetario de Desocupados e Inactivos (IMDI)     #
  #--------------------------------------------------------------------------------------#
  df_total$vf_imdi <- 0

  #----------------------------------------------------------#
  # 1. Valores faltantes por convención: 98, 99, 999, 9999   #
  #----------------------------------------------------------#
  # Es desocupado e inactivos, recibió ingresos (p7422) pero no da información del valor (códigos por convención)
  df_total$vf_imdi[df_total$des_ina == 1 & df_total$p7422 == 1 & (df_total$p7422s1 %in% c(98,99, 999, 9999, 99999, 999999))] = 1

  #---------------------------------------------#
  # 3. Valores faltantes por inconsistencias    #
  #---------------------------------------------#
  # Es desocupado e inactivos, recibió ingresos (p7422) pero no da información del valor (códigos por convención)
  df_total$vf_imdi[df_total$des_ina == 1 & df_total$p7422 == 1 & (is.na(df_total$p7422s1)  | df_total$p7422s1 == 0)] = 1

  #--------------------------------------------------------------------------------------#
  #    Valores faltantes para el Ingreso de otras fuentes (IOF) para ocupados (IOF_O)    #
  #--------------------------------------------------------------------------------------#

  df_total$vf_iof1_o = 0
  df_total$vf_iof2_o = 0
  df_total$vf_iof3_o = 0
  df_total$vf_iof6_o = 0

  # Condición general para ocupados
  co <- df_total$asalariado == 1 | df_total$independiente == 1 | df_total$trab_familiares == 1

  #-----------------------------------#
  # Ingreso de otras fuentes 1 (IOF1) #
  #-----------------------------------#

  # 1. Valores faltantes por convención
  df_total$vf_iof1_o[co & (df_total$p7510s5a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s5a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof1_o[co & (df_total$p7510s5 == 9 & !is.na(df_total$p7510s5))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof1_o[co & (df_total$p7510s5 == 1 & (is.na(df_total$p7510s5a1) | df_total$p7510s5a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 2 (IOF2) #
  #-----------------------------------#

  # 1. Valores faltantes por convención
  df_total$vf_iof2_o[co & (df_total$p7500s2a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s2a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof2_o[co & (df_total$p7500s2 == 9 & !is.na(df_total$p7500s2))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof2_o[co & (df_total$p7500s2 == 1 & (is.na(df_total$p7500s2a1) | df_total$p7500s2a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 3 (IOF3) #
  #-----------------------------------#
  # 1. Valores faltantes por convención
  df_total$vf_iof3_o[co & (df_total$p7510s1a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s1a1))] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s2a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s2a1))] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s3a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s3a1))] = 1
  df_total$vf_iof3_o[co & (df_total$p7500s3a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s3a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof3_o[co & (df_total$p7510s1 == 9)] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s2 == 9)] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s3 == 9)] = 1
  df_total$vf_iof3_o[co & (df_total$p7500s3 == 9)] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof3_o[co & (df_total$p7510s1 == 1 & (is.na(df_total$p7510s1a1) | df_total$p7510s1a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s2 == 1 & (is.na(df_total$p7510s2a1) | df_total$p7510s2a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_o[co & (df_total$p7510s3 == 1 & (is.na(df_total$p7510s3a1) | df_total$p7510s3a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_o[co & (df_total$p7500s3 == 1 & (is.na(df_total$p7500s3a1) | df_total$p7500s3a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 6 (IOF6) #
  #-----------------------------------#
  # 1. Valores faltantes por convención
  df_total$vf_iof6_o[co & (df_total$p7500s1a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s1a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof6_o[co & (df_total$p7500s1 == 9 & !is.na(df_total$p7500s1))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof6_o[co & (df_total$p7500s1 == 1 & (is.na(df_total$p7500s1a1) | df_total$p7500s1a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #------------------------------------------------------------------------------------------#
  #    Valores faltantes para el Ingreso de otras fuentes (IOF) para no-ocupados (IOF_NO)    #
  #------------------------------------------------------------------------------------------#

  df_total$vf_iof1_no = 0
  df_total$vf_iof2_no = 0
  df_total$vf_iof3_no = 0
  df_total$vf_iof6_no = 0

  # Condición general para no-ocupados (desocupados o inactivos)
  cno <- df_total$des_ina == 1

  #-----------------------------------#
  # Ingreso de otras fuentes 1 (IOF1) #
  #-----------------------------------#

  # 1. Valores faltantes por convención
  df_total$vf_iof1_no[cno & (df_total$p7510s5a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s5a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof1_no[cno & (df_total$p7510s5 == 9 & !is.na(df_total$p7510s5))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof1_no[cno & (df_total$p7510s5 == 1 & (is.na(df_total$p7510s5a1) | df_total$p7510s5a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 2 (IOF2) #
  #-----------------------------------#

  # 1. Valores faltantes por convención
  df_total$vf_iof2_no[cno & (df_total$p7500s2a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s2a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof2_no[cno & (df_total$p7500s2 == 9 & !is.na(df_total$p7500s2))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof2_no[cno & (df_total$p7500s2 == 1 & (is.na(df_total$p7500s2a1) | df_total$p7500s2a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 3 (IOF3) #
  #-----------------------------------#
  # 1. Valores faltantes por convención
  df_total$vf_iof3_no[cno & (df_total$p7510s1a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s1a1))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s2a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s2a1))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s3a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7510s3a1))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7500s3a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s3a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof3_no[cno & (df_total$p7510s1 == 9)] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s2 == 9)] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s3 == 9)] = 1
  df_total$vf_iof3_no[cno & (df_total$p7500s3 == 9)] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof3_no[cno & (df_total$p7510s1 == 1 & (is.na(df_total$p7510s1a1) | df_total$p7510s1a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s2 == 1 & (is.na(df_total$p7510s2a1) | df_total$p7510s2a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7510s3 == 1 & (is.na(df_total$p7510s3a1) | df_total$p7510s3a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1
  df_total$vf_iof3_no[cno & (df_total$p7500s3 == 1 & (is.na(df_total$p7500s3a1) | df_total$p7500s3a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1

  #-----------------------------------#
  # Ingreso de otras fuentes 6 (IOF6) #
  #-----------------------------------#
  # 1. Valores faltantes por convención
  df_total$vf_iof6_no[cno & (df_total$p7500s1a1 %in% c(98,99, 999, 9999, 99999, 999999) & !is.na(df_total$p7500s1a1))] = 1

  # 2. Valores faltantes por desconocimiento
  df_total$vf_iof6_no[cno & (df_total$p7500s1 == 9 & !is.na(df_total$p7500s1))] = 1

  # 3. Valores faltantes por inconsistencias
  df_total$vf_iof6_no[cno & (df_total$p7500s1 == 1 & (is.na(df_total$p7500s1a1) | df_total$p7500s1a1 %in% c(0,98,99, 999, 9999, 99999, 999999)))] = 1



  #---------------------------------------------------------------------------#
  #---------------------------------------------------------------------------#
  #   Módulo 5: Quantile regression para la detección de valores extremos    # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------#
  #---------------------------------------------------------------------------#


  # Para cada componente del ingreso, se emplea una técnica de optimización
  # no-paramétrica, a saber, una Quantile regression, para detectar los valores
  # atípicos

  # Las variables incluidas en la Quantile regression cambia de acuerdo con el
  # componente de ingreso considerado (véase abajo las especificaciones en cada modelo).

  # En general, para cada fuente de ingreso, se estiman regresiones cuantílicas
  # para los siguientes cuantiles: 10, 25, 50, 75, 85 y 95. La variable dependiente
  # es el log(componente del ingreso). El criterio es el siguiente: dada una regresión
  # cuantílica, una observación es considerada un valor extremo cuando su residuo
  # cae fuera del intervalo dado por -3 y 3 desviaciones estándar. A partir de este
  # criterio, se determina que una observación es EN GENERAL atípica cuando es clasificada
  # como un valor extremo en, por lo menos, 5 de las 6 regresiones.

  # En lo sucesivo, defino una función general para la detección de valores atípicos usando las seis (6)
  # regresiones cuantílicas anteriormente definidas:

  outliers_qr <- function(input, comp){
    # definir lista de salida para los modelos
    output <- vector(mode = "list", length = 6)
    quant <- c(0.1,0.25,0.5,0.75,0.85,0.95)

    # bucle para la estimación
    for (q in 1:6) {
      set.seed(30524)
      colnames(input)[which(colnames(input) == comp)] <- "y"
      qreg <- rq(y ~ ., data =  input[, colSums(input != 0) > 0]  %>% select(-c(id)), tau=quant[q])
      z <- scale(resid(qreg))
      df.aux <- cbind(input, z)
      colnames(df.aux)[which(colnames(df.aux) == "z")] <- paste0("z",q)
      output[[q]] <- df.aux
    }

    # Detección de valores atípicos para cada Quantile regression
    output_out <- merge(input, output[[1]][c("id", "z1")], by = "id")
    output_out <- merge(output_out, output[[2]][c("id", "z2")], by = "id")
    output_out <- merge(output_out, output[[3]][c("id", "z3")], by = "id")
    output_out <- merge(output_out, output[[4]][c("id", "z4")], by = "id")
    output_out <- merge(output_out, output[[5]][c("id", "z5")], by = "id")
    output_out <- merge(output_out, output[[6]][c("id", "z6")], by = "id")

    # Crear clasificaciones (dummy) para cada regresión: el residuo cae fuera de -3 a 3 desviaciones estándar
    output_out$class_reg1 <- ifelse(abs(output_out$z1) > 3, 1, 0)
    output_out$class_reg2 <- ifelse(abs(output_out$z2) > 3, 1, 0)
    output_out$class_reg3 <- ifelse(abs(output_out$z3) > 3, 1, 0)
    output_out$class_reg4 <- ifelse(abs(output_out$z4) > 3, 1, 0)
    output_out$class_reg5 <- ifelse(abs(output_out$z5) > 3, 1, 0)
    output_out$class_reg6 <- ifelse(abs(output_out$z6) > 3, 1, 0)


    # Crear variables que determina en cuántas regresiones (de las 6) fue identificada
    # la observación como un valor extremo (suma de las columnas)
    output_out$class_total <- rowSums(output_out[c("class_reg1","class_reg2", "class_reg3", "class_reg4", "class_reg5","class_reg6")])

    # Identificación: la observación es clasificada como valor extremo en, por lo menos, 5 regresiones cuantílicas
    output_out$outlier <- ifelse(output_out$class_total >= 5, 1, 0)

    output_ret <- output_out[c("id", "outlier")]
    colnames(output_ret)[2] <- paste0("outlier",substr(comp,3, str_length(comp)))

    output <- output_ret

    invisible(return(output))
  }


  qr_impa <- select(df_total, any_of(c("id", "vf_impa", "IMPA", "edad", "edad_sqr", "horas_pa",
                                       "anios_edu", "bogota", "sexo", "obrero", "domestico", "propia",
                                       "patrono", "meses_trab","jefe", "n", "n_asala", "n_indep",
                                       "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
                                       "n_salud", "avg_edu")))


  # Para evitar valores negativos en ln(ingresos), se eliminan los valores entre 0 y 1 del ingreso
  qr_impa <- qr_impa %>% filter(IMPA > 1)

  # Omitir valores faltantes para la estimación
  qr_impa <- qr_impa %>% filter(vf_impa == 0) %>% select(-vf_impa)

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1

  # Definir las columnas que deseas convertir en factores
  columnas_a_convertir <- c("bogota", "sexo", "obrero", "domestico", "propia", "patrono", "jefe")

  # Seleccionar y convertir en factor solo las columnas que existen en qr_impa
  qr_impa <- qr_impa %>%
    mutate(across(any_of(columnas_a_convertir), factor))


  cvar_impa <- c("edad", "edad_sqr", "horas_pa", "anios_edu", "meses_trab", "n", "n_asala", "n_indep",
                 "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
                 "n_salud", "avg_edu")

  var_impa = c("id", "IMPA", colnames(qr_impa[, sapply(qr_impa, nlevels) > 1]),
               colnames(qr_impa[cvar_impa][sapply(qr_impa[cvar_impa], var) != 0]))


  # También se excluye la categoría base: doméstico
  var_impa2 <- setdiff(var_impa, c("domestico"))

  # Prueba: variables excluidas porque no son estadísticamente significativas
  var_impa2 <- setdiff(var_impa2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  # Selección de las nuevas variables  y creación de la variable log(impa)
  qr_impa <- qr_impa[var_impa2] %>% mutate(ln_impa = log(as.numeric(IMPA))) %>% select(-c(IMPA))

  #---------------------------------#
  # Modelos de Quantile regression #
  #---------------------------------#

  # Uso de la función anteriormente  definida para estimar los modelos de Quantile regression
  output_impa <- outliers_qr(input = qr_impa, comp = "ln_impa")


  # Recuperación de la base de datos general. Nueva base de datos general: df_total2
  df_total <- merge(df_total, output_impa, by = "id", all.x = TRUE)

  # Si no es outlier, entonces 0
  df_total$outlier_impa[is.na(df_total$outlier_impa)] = 0




  #--------------------------------------------------------------------#
  #--------------------------------------------------------------------#
  #   Módulo 5.2: Regresión cuantílica para valores extremos en ISA    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------#
  #--------------------------------------------------------------------#


  #---------------------------------------------------------------------------#
  # Valores extremos para el Ingreso Monetario por Segunda Actividad (ISA)    #
  #---------------------------------------------------------------------------#

  #----------------------------------------#
  # Construcción de la base de datos input #
  #----------------------------------------#

  # Selección de variables explicativas




  qr_isa=select(df_total,any_of(c("id", "vf_isa", "ISA", "edad", "edad_sqr", "horas_sa",
                                  "anios_edu", "bogota", "sexo", "obrero", "domestico", "propia",
                                  "patrono", "meses_trab","jefe", "n", "n_asala", "n_indep",
                                  "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne",
                                  "n_superior", "avg_edu")))


  # Para evitar valores negativos en ln(ingresos), se eliminan los valores entre 0 y 1 del ingreso
  qr_isa <- qr_isa %>% filter(ISA > 1)

  # Omitir valores faltantes para la estimación
  qr_isa <- qr_isa %>% filter(vf_isa == 0) %>% select(-vf_isa)

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  qr_isa[c("bogota", "sexo", "obrero", "domestico",
           "propia", "patrono", "jefe")] <- lapply(qr_isa[c("bogota", "sexo", "obrero", "domestico",
                                                            "propia", "patrono", "jefe")], factor)

  cvar_isa <- c("edad", "edad_sqr", "horas_sa", "anios_edu", "meses_trab", "n", "n_asala", "n_indep",
                "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
                "avg_edu")

  var_isa = c("id", "ISA", colnames(qr_isa[, sapply(qr_isa, nlevels) > 1]),
              colnames(qr_isa[cvar_isa][sapply(qr_isa[cvar_isa], var) != 0]))

  # También se excluye la categoría base: doméstico
  var_isa2 <- setdiff(var_isa, c("domestico"))

  # Prueba: variables excluidas porque no son estadísticamente significativas
  var_isa2 <- setdiff(var_isa2, c("bogota","obrero", "propia","patrono", "jefe", "anios_edu",
                                  "n", "n_indep","n_asala", "n_desoc", "n_edad_5", "n_edad_14_17",
                                  "n_edad_65", "n_edad_25_ne", "n_superior"))

  # Selección de las nuevas variables  y creación de la variable log(isa)
  qr_isa <- qr_isa[var_isa2] %>% mutate(ln_isa = log(as.numeric(ISA))) %>% select(-c(ISA))

  #---------------------------------#
  # Modelos de regresión cuantílica #
  #---------------------------------#

  # Uso de la función anteriormente  definida para estimar los modelos de regresión cuantílica
  output_isa <- outliers_qr(input = qr_isa, comp = "ln_isa")

  # Recuperación de la base de datos general. Nueva base de datos general: df_total2
  df_total <- merge(df_total, output_isa, by = "id", all.x = TRUE)

  # Si no es outlier, entonces 0
  df_total$outlier_isa[is.na(df_total$outlier_isa)] = 0




  #------------------------------------------------------------#
  #        Valores extremos para el Ingreso en Especie (IE)    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------#

  #----------------------------------------#
  # Construcción de la base de datos input #
  #----------------------------------------#


  qr_ie=select(df_total,any_of(c("id", "vf_ie", "IE", "edad",
                                 "edad_sqr", "horas_pa", "anios_edu","bogota", "sexo", "obrero",
                                 "domestico", "propia","patrono", "meses_trab",
                                 "jefe", "n", "n_asala", "n_indep", "n_desoc", "n_edad_5", "n_edad_14_17",
                                 "n_edad_65", "n_edad_25_ne", "n_superior", "n_salud", "avg_edu")))

  # Para evitar valores negativos en ln(ingresos), se eliminan los valores entre 0 y 1 del ingreso
  qr_ie <- qr_ie %>% filter(IE > 1)

  # Omitir valores faltantes para la estimación
  qr_ie <- qr_ie %>% filter(vf_ie == 0) %>% select(-vf_ie)

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  qr_ie[c("bogota", "sexo", "obrero", "domestico",
          "propia", "patrono", "jefe")] <- lapply(qr_ie[c("bogota", "sexo", "obrero", "domestico",
                                                          "propia", "patrono", "jefe")], factor)

  cvar_ie <- c("edad", "edad_sqr", "horas_pa", "anios_edu", "meses_trab", "n", "n_asala", "n_indep",
               "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
               "avg_edu")

  var_ie = c("id", "IE", colnames(qr_ie[, sapply(qr_ie, nlevels) > 1]),
             colnames(qr_ie[cvar_ie][sapply(qr_ie[cvar_ie], var) != 0]))

  # También se excluye la categoría base: doméstico
  var_ie2 <- setdiff(var_ie, c("domestico"))

  # Prueba: variables excluidas porque no son estadísticamente significativas
  var_ie2 <- setdiff(var_ie2, c("propia", "patrono","n_edad_14_17", "n_edad_65", "n_superior","n_edad_25_ne"))

  # Selección de las nuevas variables  y creación de la variable log(isa)
  qr_ie <- qr_ie[var_ie2] %>% mutate(ln_ie = log(as.numeric(IE))) %>% select(-c(IE))

  #---------------------------------#
  # Modelos de regresión cuantílica #
  #---------------------------------#
  # Uso de la función anteriormente  definida para estimar los modelos de regresión cuantílica
  output_ie <- outliers_qr(input = qr_ie, comp = "ln_ie")


  # Recuperación de la base de datos general. Nueva base de datos general: df_total2
  df_total <- merge(df_total, output_ie, by = "id", all.x = TRUE)

  # Si no es outlier, entonces 0
  df_total$outlier_ie[is.na(df_total$outlier_ie)] = 0




  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#
  #   Módulo 5.4: Regresión cuantílica para valores extremos en IMDI  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#


  #-----------------------------------------------------------------------------------------#
  #        Valores extremos para el Ingreso Monetarios de Desocupados e Inactivos (IMDI)    #
  #-----------------------------------------------------------------------------------------#

  #----------------------------------------#
  # Construcción de la base de datos input #
  #----------------------------------------#
  # Selección de variables explicativas


  qr_imdi=select(df_total,any_of(c("id", "vf_imdi", "IMDI", "edad",
                                   "edad_sqr", "anios_edu",
                                   "bogota", "estu")))

  # Para evitar valores negativos en ln(ingresos), se eliminan los valores entre 0 y 1 del ingreso
  qr_imdi <- qr_imdi %>% filter(IMDI > 1)

  # Omitir valores faltantes para la estimación
  qr_imdi <- qr_imdi %>% filter(vf_imdi == 0) %>% select(-vf_imdi)

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  qr_imdi[c("bogota", "estu")] <- lapply(qr_imdi[c("bogota", "estu")], factor)

  cvar_imdi <- c("edad", "edad_sqr", "anios_edu")

  var_imdi = c("id", "IMDI", colnames(qr_imdi[, sapply(qr_imdi, nlevels) > 1]),
               colnames(qr_imdi[cvar_imdi][sapply(qr_imdi[cvar_imdi], var) != 0]))

  # No hay exclusiones adicionales de variables

  # Selección de las nuevas variables  y creación de la variable log(isa)
  qr_imdi <- qr_imdi[var_imdi] %>% mutate(ln_imdi = log(as.numeric(IMDI))) %>% select(-c(IMDI))

  #---------------------------------#
  # Modelos de regresión cuantílica #
  #---------------------------------#
  # Uso de la función anteriormente  definida para estimar los modelos de regresión cuantílica
  output_imdi <- outliers_qr(input = qr_imdi, comp = "ln_imdi")

  # Recuperación de la base de datos general. Nueva base de datos general: df_total2
  df_total <- merge(df_total, output_imdi, by = "id", all.x = TRUE)

  # Si no es outlier, entonces 0
  df_total$outlier_imdi[is.na(df_total$outlier_imdi)] = 0




  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#
  #   Módulo 5.5: Regresión cuantílica para valores extremos en IOF   #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#


  #------------------------------------------------------------------------------------#
  #        Valores extremos para el Ingreso de Otras Fuentes (IOF) para Ocupados (O)   #
  #------------------------------------------------------------------------------------#

  #------------------------------------------------#
  # Construcción de la base de datos input general #
  #------------------------------------------------#

  # Selección de variables explicativas
  qr_iof_o <- df_total[c("id", "vf_iof1_o", "vf_iof2_o", "vf_iof3_o", "vf_iof6_o",
                         "IOF1_o", "IOF2_o", "IOF3_o", "IOF6_o","edad", "edad_sqr", "horas_pa",
                         "anios_edu", "bogota", "sexo", "obrero", "domestico", "propia",
                         "patrono", "meses_trab","jefe", "n", "n_asala", "n_indep",
                         "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
                         "n_salud", "avg_edu")]



  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  qr_iof_o[c("bogota", "sexo", "obrero", "domestico",
             "propia", "patrono", "jefe")] <- lapply(qr_iof_o[c("bogota", "sexo", "obrero", "domestico",
                                                                "propia", "patrono", "jefe")], factor)

  cvar_iof_o <- c("edad", "edad_sqr", "horas_pa", "anios_edu", "meses_trab", "n", "n_asala", "n_indep",
                  "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
                  "n_salud", "avg_edu")

  #------------------------------------------------------------------------#
  # Construcción de la base de datos input para cada subcomponente del IOF #
  #------------------------------------------------------------------------#

  ######## IOF 1
  qr_iof1_o <- qr_iof_o %>% filter(IOF1_o > 1)

  qr_iof1_o <- qr_iof1_o %>% filter(vf_iof1_o == 0) %>% select(-vf_iof1_o)

  var_iof1_o = c("id", "IOF1_o", colnames(qr_iof1_o[, sapply(qr_iof1_o, nlevels) > 1]),
                 colnames(qr_iof1_o[cvar_iof_o][sapply(qr_iof1_o[cvar_iof_o], var, na.rm = T) != 0]))

  var_iof1_o2 <- setdiff(var_iof1_o, c("domestico"))

  var_iof1_o2 <- setdiff(var_iof1_o2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof1_o <- qr_iof1_o[var_iof1_o2] %>% mutate(ln_iof1_o = log(as.numeric(IOF1_o))) %>% select(-c(IOF1_o))

  ########  IOF 2
  qr_iof2_o <- qr_iof_o %>% filter(IOF2_o > 1)

  qr_iof2_o <- qr_iof2_o %>% filter(vf_iof2_o == 0) %>% select(-vf_iof2_o)

  var_iof2_o = c("id", "IOF2_o", colnames(qr_iof2_o[, sapply(qr_iof2_o, nlevels) > 1]),
                 colnames(qr_iof2_o[cvar_iof_o][sapply(qr_iof2_o[cvar_iof_o], var, na.rm = T) != 0]))

  var_iof2_o2 <- setdiff(var_iof2_o, c("domestico"))

  var_iof2_o2 <- setdiff(var_iof2_o2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof2_o <- qr_iof2_o[var_iof2_o2] %>% mutate(ln_iof2_o = log(as.numeric(IOF2_o))) %>% select(-c(IOF2_o))


  ########  IOF 3
  qr_iof3_o <- qr_iof_o %>% filter(IOF3_o > 1)

  qr_iof3_o <- qr_iof3_o %>% filter(vf_iof3_o == 0) %>% select(-vf_iof3_o)

  var_iof3_o = c("id", "IOF3_o", colnames(qr_iof3_o[, sapply(qr_iof3_o, nlevels) > 1]),
                 colnames(qr_iof3_o[cvar_iof_o][sapply(qr_iof3_o[cvar_iof_o], var, na.rm = T) != 0]))

  var_iof3_o2 <- setdiff(var_iof3_o, c("domestico"))

  var_iof3_o2 <- setdiff(var_iof3_o2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof3_o <- qr_iof3_o[var_iof3_o2] %>% mutate(ln_iof3_o = log(as.numeric(IOF3_o))) %>% select(-c(IOF3_o))



  ########  IOF 6
  qr_iof6_o <- qr_iof_o %>% filter(IOF6_o > 1)

  qr_iof6_o <- qr_iof6_o %>% filter(vf_iof6_o == 0) %>% select(-vf_iof6_o)

  var_iof6_o = c("id", "IOF6_o", colnames(qr_iof6_o[, sapply(qr_iof6_o, nlevels) > 1]),
                 colnames(qr_iof6_o[cvar_iof_o][sapply(qr_iof6_o[cvar_iof_o], var, na.rm = T) != 0]))

  var_iof6_o2 <- setdiff(var_iof6_o, c("domestico"))

  var_iof6_o2 <- setdiff(var_iof6_o2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof6_o <- qr_iof6_o[var_iof6_o2] %>% mutate(ln_iof6_o = log(as.numeric(IOF6_o))) %>% select(-c(IOF6_o))



  #---------------------------------#
  # Modelos de regresión cuantílica #
  #---------------------------------#

  ######## IOF 1
  if (nrow(qr_iof1_o) > ((ncol(qr_iof1_o)-1)*10)) {
    output_iof1_o <- outliers_qr(input = qr_iof1_o, comp = "ln_iof1_o")
    df_total <- merge(df_total, output_iof1_o, by = "id", all.x = TRUE)
    df_total$outlier_iof1_o[is.na(df_total$outlier_iof1_o)] = 0

  } else {
    df_total$outlier_iof1_o = 0
  }


  ######## IOF 2
  if (nrow(qr_iof2_o) > ((ncol(qr_iof2_o)-1)*10)) {
    output_iof2_o <- outliers_qr(input = qr_iof2_o, comp = "ln_iof2_o")
    df_total <- merge(df_total, output_iof2_o, by = "id", all.x = TRUE)
    df_total$outlier_iof2_o[is.na(df_total$outlier_iof2_o)] = 0

  } else {
    df_total$outlier_iof2_o = 0
  }


  ######## IOF 3
  if (nrow(qr_iof3_o) > ((ncol(qr_iof3_o)-1)*10)) {
    output_iof3_o <- outliers_qr(input = qr_iof3_o, comp = "ln_iof3_o")
    df_total <- merge(df_total, output_iof3_o, by = "id", all.x = TRUE)
    df_total$outlier_iof3_o[is.na(df_total$outlier_iof3_o)] = 0


  } else {
    df_total$outlier_iof3_o = 0
  }


  ######## IOF 6
  if (nrow(qr_iof6_o) > ((ncol(qr_iof6_o)-1)*10)) {
    output_iof6_o <- outliers_qr(input = qr_iof6_o, comp = "ln_iof6_o")
    df_total <- merge(df_total, output_iof6_o, by = "id", all.x = TRUE)
    df_total$outlier_iof6_o[is.na(df_total$outlier_iof6_o)] = 0

  } else {
    df_total$outlier_iof6_o = 0
  }






  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#
  #   Módulo 5.5: Regresión cuantílica para valores extremos en IOF   #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------#
  #-------------------------------------------------------------------#

  #-------------------------------------------------------------------------------------------------------------#
  #        Valores extremos para el Ingreso de Otras Fuentes (IOF) para No-Ocupados (Desocupados e Inactivos)   #
  #------------------------------------------------------------------------v------------------------------------#

  #------------------------------------------------#
  # Construcción de la base de datos input general #
  #------------------------------------------------#
  # Selección de variables explicativas
  qr_iof_no <- df_total[c("id", "vf_iof1_no", "vf_iof2_no", "vf_iof3_no", "vf_iof6_no",
                          "IOF1_no", "IOF2_no", "IOF3_no", "IOF6_no","edad",
                          "edad_sqr", "anios_edu",
                          "bogota", "estu")]

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  qr_iof_no[c("bogota", "estu")] <- lapply(qr_iof_no[c("bogota", "estu")], factor)

  cvar_iof_no <- c("edad", "edad_sqr", "anios_edu")

  #------------------------------------------------------------------------#
  # Construcción de la base de datos input para cada subcomponente del IOF #
  #------------------------------------------------------------------------#

  ######## IOF 1
  qr_iof1_no <- qr_iof_no %>% filter(IOF1_no > 1)

  qr_iof1_no <- qr_iof1_no %>% filter(vf_iof1_no == 0) %>% select(-vf_iof1_no)

  var_iof1_no = c("id", "IOF1_no", colnames(qr_iof1_no[, sapply(qr_iof1_no, nlevels) > 1]),
                  colnames(qr_iof1_no[cvar_iof_no][sapply(qr_iof1_no[cvar_iof_no], var, na.rm = T) != 0]))

  var_iof1_no2 <- setdiff(var_iof1_no, c("domestico"))

  var_iof1_no2 <- setdiff(var_iof1_no2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof1_no <- qr_iof1_no[var_iof1_no2] %>% mutate(ln_iof1_no = log(as.numeric(IOF1_no))) %>% select(-c(IOF1_no))

  ########  IOF 2
  qr_iof2_no <- qr_iof_no %>% filter(IOF2_no > 1)

  qr_iof2_no <- qr_iof2_no %>% filter(vf_iof2_no == 0) %>% select(-vf_iof2_no)

  var_iof2_no = c("id", "IOF2_no", colnames(qr_iof2_no[, sapply(qr_iof2_no, nlevels) > 1]),
                  colnames(qr_iof2_no[cvar_iof_no][sapply(qr_iof2_no[cvar_iof_no], var, na.rm = T) != 0]))

  var_iof2_no2 <- setdiff(var_iof2_no, c("domestico"))

  var_iof2_no2 <- setdiff(var_iof2_no2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof2_no <- qr_iof2_no[var_iof2_no2] %>% mutate(ln_iof2_no = log(as.numeric(IOF2_no))) %>% select(-c(IOF2_no))


  ########  IOF 3
  qr_iof3_no <- qr_iof_no %>% filter(IOF3_no > 1)

  qr_iof3_no <- qr_iof3_no %>% filter(vf_iof3_no == 0) %>% select(-vf_iof3_no)

  var_iof3_no = c("id", "IOF3_no", colnames(qr_iof3_no[, sapply(qr_iof3_no, nlevels) > 1]),
                  colnames(qr_iof3_no[cvar_iof_no][sapply(qr_iof3_no[cvar_iof_no], var, na.rm = T) != 0]))

  var_iof3_no2 <- setdiff(var_iof3_no, c("domestico"))

  var_iof3_no2 <- setdiff(var_iof3_no2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof3_no <- qr_iof3_no[var_iof3_no2] %>% mutate(ln_iof3_no = log(as.numeric(IOF3_no))) %>% select(-c(IOF3_no))



  ########  IOF 6
  qr_iof6_no <- qr_iof_no %>% filter(IOF6_no > 1)

  qr_iof6_no <- qr_iof6_no %>% filter(vf_iof6_no == 0) %>% select(-vf_iof6_no)

  var_iof6_no = c("id", "IOF6_no", colnames(qr_iof6_no[, sapply(qr_iof6_no, nlevels) > 1]),
                  colnames(qr_iof6_no[cvar_iof_no][sapply(qr_iof6_no[cvar_iof_no], var, na.rm = T) != 0]))

  var_iof6_no2 <- setdiff(var_iof6_no, c("domestico"))

  var_iof6_no2 <- setdiff(var_iof6_no2, c("n_edad_14_17","n_edad_65", "n_superior","n_edad_25_ne"))

  qr_iof6_no <- qr_iof6_no[var_iof6_no2] %>% mutate(ln_iof6_no = log(as.numeric(IOF6_no))) %>% select(-c(IOF6_no))



  #---------------------------------#
  # Modelos de regresión cuantílica #
  #---------------------------------#

  ######## IOF 1
  if (nrow(qr_iof1_no) > ((ncol(qr_iof1_no)-1)*10)) {
    output_iof1_no <- outliers_qr(input = qr_iof1_no, comp = "ln_iof1_no")
    df_total <- merge(df_total, output_iof1_no, by = "id", all.x = TRUE)
    df_total$outlier_iof1_no[is.na(df_total$outlier_iof1_no)] = 0

  } else {
    df_total$outlier_iof1_no = 0
  }

  ######## IOF 2
  if (nrow(qr_iof2_no) > ((ncol(qr_iof2_no)-1)*10)) {
    output_iof2_no <- outliers_qr(input = qr_iof2_no, comp = "ln_iof2_no")
    df_total <- merge(df_total, output_iof2_no, by = "id", all.x = TRUE)
    df_total$outlier_iof2_no[is.na(df_total$outlier_iof2_no)] = 0

  } else {
    df_total$outlier_iof2_no = 0
  }

  ######## IOF 3
  if (nrow(qr_iof3_no) > ((ncol(qr_iof3_no)-1)*10)) {
    output_iof3_no <- outliers_qr(input = qr_iof3_no, comp = "ln_iof3_no")
    df_total <- merge(df_total, output_iof3_no, by = "id", all.x = TRUE)
    df_total$outlier_iof3_no[is.na(df_total$outlier_iof3_no)] = 0

  } else {
    df_total$outlier_iof3_no = 0
  }

  ######## IOF 6
  if (nrow(qr_iof6_no) > ((ncol(qr_iof6_no)-1)*10)) {
    output_iof6_no <- outliers_qr(input = qr_iof6_no, comp = "ln_iof6_no")
    df_total <- merge(df_total, output_iof6_no, by = "id", all.x = TRUE)
    df_total$outlier_iof6_no[is.na(df_total$outlier_iof6_no)] = 0

  } else {
    df_total$outlier_iof6_no = 0
  }



  #---------------------------------------------------------------------------#
  #---------------------------------------------------------------------------#
  #   Módulo 6: Modelos de clasificación para la detección de falsos ceros    # ------------------------------------------------------------------------------------------------------------------------------
  #---------------------------------------------------------------------------#
  #---------------------------------------------------------------------------#


  #----------------------------------------------------------------#
  # Preparación de base de datos para el modelo de clasificación   #
  #----------------------------------------------------------------#
  # Excluir NAs para IMPA y se excluyen los outliers identificados en las regresiones cuantílicas
  sl_data <- df_total %>% filter(!is.na(IMPA))
  sl_data <- sl_data %>% filter(outlier_impa == 0)
  sl_data <- sl_data %>% filter(vf_impa == 0)

  # Creación de la variable para clasificación (IMPA no es cero, IMPA es cero)
  sl_data$class_0 <- ifelse(sl_data$IMPA == 0, "Cero impa", "No-cero impa")

  # Recodificar la variable dependiente
  sl_data$class_0 <- recode(sl_data$class_0,"Cero impa" = '0', "No-cero impa" = '1') %>% as.numeric()

  # Selección de variables
  sl_var <- c("id", "edad", "edad_sqr", "horas_pa", "anios_edu", "bogota", "sexo", "obrero", "domestico", "propia",
              "patrono", "meses_trab","jefe", "n", "n_asala", "n_indep", "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65",
              "n_edad_25_ne", "n_superior","n_salud", "avg_edu")

  sl_input <- sl_data[c(sl_var,"class_0")]

  #-------------------------------------------------------------#
  #     Base de datos input: modelo de clasificación            #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------#

  # Nos quedamos con las variables cualitativas cuyos niveles > 1 y las variables
  # cuantitativas cuya varianza sea diferente de 1
  sl_input[c("bogota", "sexo", "obrero", "domestico",
             "propia", "patrono", "jefe")] <- lapply(sl_input[c("bogota", "sexo", "obrero", "domestico",
                                                                "propia", "patrono", "jefe")], factor)

  sl_cvar <- c("edad", "edad_sqr", "horas_pa", "anios_edu", "meses_trab", "n", "n_asala", "n_indep",
               "n_desoc", "n_edad_5", "n_edad_14_17","n_edad_65", "n_edad_25_ne", "n_superior",
               "n_salud", "avg_edu")

  sl_var2 = c("class_0",colnames(sl_input[, sapply(sl_input, nlevels) > 1]),
              colnames(sl_input[cvar_impa][sapply(sl_input[cvar_impa], var) != 0]))

  # También se excluye la categoría base: doméstico
  sl_var2 <- setdiff(sl_var2, c("domestico"))

  # Base de datos de entrada para el modelo de clasificación
  sl_input <- sl_input[sl_var2]

  #-------------------------------------------------------------#
  #     Implementación del modelo de clasificación XGBoost      #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------#

  #-------------------------------------------------------------#
  #     Guardar matriz de confusión: algoritmo XGBoost          #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------#
  folds1 <- createFolds(sl_input$class_0, k = 10)
  x <- folds1$Fold01

  train.df1 = sl_input[-x, ]
  test.df1 = sl_input[x,]

  set.seed(7443)
  class.fit1 <- train(factor(class_0) ~ .,
                      method = "xgbTree", data = train.df1,
                      trControl = trainControl(method = "cv", number = 1))

  Predicted1 = predict(class.fit1, sl_input)

  cm1 = caret::confusionMatrix(Predicted1, as.factor(sl_input$class_0))


  #-----------------------------------------------------------------#
  #     Guardar matriz de confusión: modelo logit multinomial (MNL) #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-----------------------------------------------------------------#
  folds2 <- createFolds(sl_input$class_0, k = 10)
  x <- folds2$Fold01

  train.df2 = sl_input[-x, ]
  test.df2 = sl_input[x,]

  set.seed(7445)
  class.fit2 <- multinom(factor(class_0) ~ ., data = train.df2)

  Predicted2 = predict(class.fit2, sl_input)
  cm2 = caret::confusionMatrix(Predicted1, as.factor(sl_input$class_0))


  #------------------------------------------------#
  # Ajustar el resultado en la base de datos final #
  #------------------------------------------------#
  # Mantenemos los resultados del algoritmo XGBoost
  predicted_sl_data <- cbind(sl_data, data.frame(predicted_class_0 = Predicted1))

  predicted_sl_data$falso_cero <- ifelse(predicted_sl_data$class_0 == 0 &
                                           (predicted_sl_data$class_0 != predicted_sl_data$predicted_class_0), 1, 0)


  # El resultado es incorporado a la base de datos general
  df_total <- merge(df_total, predicted_sl_data[c("id", "class_0", "predicted_class_0", "falso_cero")],
                    by = "id", all.x = TRUE)





  #------------------------------------------------#
  #------------------------------------------------#
  #   Módulo 7: Definición de valores a imputar    #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------#
  #------------------------------------------------#

  # Así, los valores a imputar son determinados según alguna de las siguiente tres condiciones:

  # 1. Fue identificado como un valor faltante en el Módulo 4
  # 2. Fue identificado como un valor extremo en el Módulo 5
  # 3. Fue identificado como un "falso cero" en IMPA según el Módulo 6

  # Ejecutar los módulos anteriores:

  #--------------------------------#
  # IMPA: valores para imputar     #
  #--------------------------------#
  df_total$imp_impa <- ifelse((df_total$vf_impa == 1 |
                                 df_total$outlier_impa == 1 |
                                 df_total$falso_cero == 1), 1, 0)
  #--------------------------------#
  # ISA: valores para imputar      #
  #--------------------------------#
  df_total$imp_isa <- ifelse((df_total$vf_isa == 1 |
                                df_total$outlier_isa == 1), 1, 0)

  #--------------------------------#
  # IE: valores para imputar       #
  #--------------------------------#
  df_total$imp_ie <- ifelse((df_total$vf_ie == 1 |
                               df_total$outlier_ie == 1), 1, 0)

  #--------------------------------#
  # IMDI: valores para imputar     #
  df_total$imp_imdi <- ifelse((df_total$vf_imdi == 1 |
                                 df_total$outlier_imdi == 1), 1, 0)

  #---------------------------------------------------#
  # IOF: valores para imputar (RESULTADO PARCIAL)     #

  # NOTA IMPORTANTE: Respecto del componente IOF, los resultados se descomponen en cuatro
  # fuentes de ingresos: IOF1, IOF2, IOF3 e IOF6

  # Ingreso de Otras Fuentes (IOF) 1 para Ocupados y No-Ocupados
  df_total$imp_iof1 <- ifelse((df_total$vf_iof1_o == 1 | df_total$vf_iof1_no == 1) |
                                (df_total$outlier_iof1_o == 1 | df_total$outlier_iof1_no == 1), 1, 0)

  # Ingreso de Otras Fuentes (IOF) 2 para Ocupados y No-Ocupados
  df_total$imp_iof2 <- ifelse((df_total$vf_iof2_o == 1 | df_total$vf_iof2_no == 1) |
                                (df_total$outlier_iof2_o == 1 | df_total$outlier_iof2_no == 1), 1, 0)

  # Ingreso de Otras Fuentes (IOF) 3 para Ocupados y No-Ocupados
  df_total$imp_iof3 <- ifelse((df_total$vf_iof3_o == 1 | df_total$vf_iof3_no == 1) |
                                (df_total$outlier_iof3_o == 1 | df_total$outlier_iof3_no == 1), 1, 0)

  # Ingreso de Otras Fuentes (IOF) 1 para Ocupados y No-Ocupados
  df_total$imp_iof6 <- ifelse((df_total$vf_iof6_o == 1 | df_total$vf_iof6_no == 1 |
                                 (df_total$outlier_iof6_o == 1 | df_total$outlier_iof6_no == 1)), 1, 0)



  #------------------------------------------------#
  #------------------------------------------------#
  #   Módulo 8: Método de imputación múltiple      #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------#
  #------------------------------------------------#

  #-----------------------------------#
  #   Creación de dominios (celdas)   #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #-----------------------------------#

  # Dominio por territorio
  df_total$dom_terr = 1
  df_total$dom_terr[df_total$dominio %in% c("BOGOTA","MEDELLIN","CALI","BARRANQUILLA",
                                            "BUCARAMANGA","MANIZALES", "PEREIRA",
                                            "CUCUTA","PASTO","IBAGUE",
                                            "MONTERIA","CARTAGENA","VILLAVICENCIO")] = 2
  df_total$dom_terr[df_total$dominio %in% c("RESTO URBANO","RURAL")] = 3

  # Dominio por estrato
  df_total$dom_estrato <- ifelse((df_total$estrato %in% c(9,1,0)) |
                                   (is.na(df_total$estrato)), 0, df_total$estrato)

  # Dominio de la edad
  df_total <- df_total %>% mutate(dom_edad = cut(edad, c(c(0, 18, 25, 46), Inf),
                                                 right = F,
                                                 labels = c(1:4)))

  # Dominio de la educación
  df_total$dom_edu <- 0
  df_total$dom_edu[as.numeric(df_total$p3042) %in% c(1,2,3,99)] = 1
  df_total$dom_edu[as.numeric(df_total$p3042) %in% c(4,5,6)] = 2
  df_total$dom_edu[as.numeric(df_total$p3042) %in% c(7,8,9,10,11,12,13)] = 3

  # Dominio de la posición ocupacional
  df_total$dom_pos <- 0
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(1,2)] = 1
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(3,6)] = 2
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(7)] = 3
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(4,8)] = 4
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(5)] = 5
  df_total$dom_pos[as.numeric(df_total$p6430) %in% c(9)] = 6

  # Dominio de las horas de primera actividad
  df_total <- df_total %>% mutate(dom_horas = cut(horas_pa, c(c(0, 24), Inf),
                                                  right = T,
                                                  labels = c(1,2)))

  #----------------------------------------------------#
  # Definición de la función para la imputación simple #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------#

  comp_f <- function(original, var, dom, y.bp, x.hist, y.hist , xy.min.box, xy.max.box,
                     logical1, logical2,
                     logical3, logical4){

    # Definiciones:
    # i = variable binaria para indicar que se trata de un valor a imputar
    # vn1 = valor no imputado nuevo
    # vi1 = valor imputado nuevo

    # Creación de dataset base
    df.base <- original %>% select(c("id", logical1, logical2, logical3, logical4, dom,
                                     toupper(var), paste0("imp_",var)))

    # Crear variable para los valores a imputar
    df.base$i <- unlist(df.base[paste0("imp_",var)])

    # Crear variables faltantes según los valores calculados por el algoritmo
    df.base$vn1 <- unlist(df.base[toupper(var)])

    # Imponer condición para cada caso
    df.base <- df.base %>% filter(eval(parse(text = logical1)) == 1 | eval(parse(text = logical2)) == 1|
                                    eval(parse(text = logical3)) == 1 |
                                    eval(parse(text = logical4)) == 1)

    # Eliminar variables para la condición lógica
    df.base <- df.base %>% select(-c(logical1, logical2, logical3, logical4))

    # Crear variables faltantes según la varible imp_impa
    df.base$vn1 <-ifelse(df.base$i == 1, NA, df.base$vn1)

    imputed.df <- VIM::hotdeck(df.base %>% select(id, vn1,
                                                  dom),
                               variable = "vn1", domain_var = dom)

    # Cambiar el nombre de la variable imputada
    colnames(imputed.df)[which(colnames(imputed.df) == "vn1")] = "vi1"

    # Definición de los gráficos en bucle
    df.aux <- merge(df.base[c("id", "vn1")],
                    imputed.df[c("id","vi1")], by = "id", all.x = T)

    df.aux <- df.aux[c("id", "vn1", "vi1")]
    colnames(df.aux) = c("id", "VN1","VI1")

    plot.aux <- melt(df.aux, id.vars = "id")

    plot.aux$value = plot.aux$value/1000

    vlines.aux <- plot.aux %>% group_by(variable) %>% summarize(Mean = round(mean(value, na.rm = T),1),
                                                                Sd = round(sd(value, na.rm = T),1),
                                                                Median = round(median(value, na.rm = T),1),
                                                                Q1 = round(quantile(value, 0.25, na.rm = T),1),
                                                                Q3 = round(quantile(value, 0.75, na.rm = T),1))



    output <- imputed.df

    return(output)

  }

  #------------------------------------------------#
  # Ingreso monetario por Primera Actividad (IMPA) #--------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------#

  # Definición del dominio
  df_total$llave_impa <- (as.numeric(df_total$dom_terr)*100000) + (as.numeric(df_total$dom_edad)*10000) +
    (as.numeric(df_total$dom_edu)*1000) + (as.numeric(df_total$dom_pos)*100) +
    (as.numeric(df_total$sexo)*10) + (as.numeric(df_total$jefe))

  # Imputación
  impa_output <- comp_f(original = df_total, var = "impa",
                        dom = "llave_impa",
                        logical1 = "asalariado", logical2 = "independiente",
                        logical3 = "trab_familiares", logical4 = "asalariado",
                        y.bp = c(0, 7500), x.hist = c(0, 7500), y.hist = c(0, 0.0009),
                        xy.min.box = c(4000, 0.0006), xy.max.box = c(4500, 0.00065))
  imputed_impa <- impa_output
  colnames(imputed_impa)[which(colnames(imputed_impa) == "vi1")] = "IMPAES"
  df_total <- merge(df_total, imputed_impa[c("id","IMPAES")], by = "id", all.x = T)



  #------------------------------------------------#
  # Ingreso monetario por Segunda Actividad (ISA)  #--------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------#

  # Definición del dominio
  df_total$llave_isa <- (as.numeric(df_total$dom_terr)*1000) + (as.numeric(df_total$dom_edad)*100) + (as.numeric(df_total$edu)*10) + (as.numeric(df_total$dom_horas))

  # Imputación
  isa_output <- comp_f(original = df_total, var = "isa",
                       dom = "llave_isa",
                       logical1 = "asalariado", logical2 = "independiente",
                       logical3 = "trab_familiares", logical4 = "asalariado",
                       y.bp = c(0, 5000), x.hist = c(0, 5000), y.hist = c(0, 0.00003),
                       xy.min.box = c(2000, 0.000020), xy.max.box = c(4000, 0.000025))


  imputed_isa <- isa_output
  colnames(imputed_isa)[which(colnames(imputed_isa) == "vi1")] = "ISAES"
  df_total <- merge(df_total, imputed_isa[c("id","ISAES")], by = "id", all.x = T)


  #---------------------------------------#
  # Ingreso en Especie (IE)               #--------------------------------------------------------------------------------------------------------------------------
  #---------------------------------------#
  # Definición del dominio
  df_total$llave_ie <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_pos)*10) + (as.numeric(df_total$sexo))

  # Imputación
  ie_output <- comp_f(original = df_total, var = "ie",
                      dom = "llave_ie", y.bp = c(0, 100),
                      x.hist = c(0, 100), y.hist = c(0, 0.2), xy.min.box = c(50, 0.1),
                      xy.max.box = c(100, 0.15),
                      logical1 = "asalariado", logical2 = "asalariado",
                      logical3 = "asalariado", logical4 = "asalariado")
  imputed_ie <- ie_output
  colnames(imputed_ie)[which(colnames(imputed_ie) == "vi1")] = "IEES"
  df_total <- merge(df_total, imputed_ie[c("id","IEES")], by = "id", all.x = T)



  #--------------------------------#
  # IMDI: valores para imputar     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------#
  # Definición de actividad: Cesante-Aspirante, Desocupado, Estudiante, Hogar
  df_total$llave_imdi <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_estrato)*10) + df_total$p6240

  test <- df_total %>% select(dom_terr, p6240, des_ina,
                              llave_imdi) %>% filter(des_ina == 1)

  imdi_output <- comp_f(original = df_total, var = "imdi",
                        dom = "llave_imdi", y.bp = c(0, 5000),
                        x.hist = c(0, 5000), y.hist = c(0,0.002),
                        xy.min.box = c(2000, 0.00068),
                        xy.max.box = c(4000, 0.00078),
                        logical1 = "des_ina", logical2 = "des_ina",
                        logical3 = "des_ina", logical4 = "des_ina")

  imputed_imdi <- imdi_output
  colnames(imputed_imdi)[which(colnames(imputed_imdi) == "vi1")] = "IMDIES"
  df_total <- merge(df_total, imputed_imdi[c("id","IMDIES")], by = "id", all.x = T)


  #--------------------------------#
  # IOF1: valores para imputar     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------#
  df_total$llave_iof1 <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_pos)*10) + (as.numeric(df_total$p6240))

  iof1_output <- comp_f(original = df_total,
                        dom = "llave_iof1",
                        var = "iof1", y.bp = c(0, 3),
                        x.hist = c(0, 3), y.hist = c(0,2.5), xy.min.box = c(1, 1.5),
                        xy.max.box = c(1, 2),
                        logical1 = "asalariado", logical2 = "independiente",
                        logical3 = "trab_familiares", logical4 = "des_ina")

  imputed_iof1 <- iof1_output

  colnames(imputed_iof1)[which(colnames(imputed_iof1) == "vi1")] = "IOF1ES"
  df_total <- merge(df_total, imputed_iof1[c("id","IOF1ES")], by = "id", all.x = T)


  #--------------------------------#
  # IOF2: valores para imputar     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------#
  df_total$llave_iof2 <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_pos)*10) + (as.numeric(df_total$p6240))

  iof2_output <- comp_f(original = df_total,
                        dom = "llave_iof2",
                        var = "iof2", y.bp = c(0, 3),
                        x.hist = c(0, 3), y.hist = c(0,2.5), xy.min.box = c(1 , 1.5),
                        xy.max.box = c(1, 2),
                        logical1 = "asalariado", logical2 = "independiente",
                        logical3 = "trab_familiares", logical4 = "des_ina")
  imputed_iof2 <- iof2_output

  colnames(imputed_iof2)[which(colnames(imputed_iof2) == "vi1")] = "IOF2ES"
  df_total <- merge(df_total, imputed_iof2[c("id","IOF2ES")], by = "id", all.x = T)


  #--------------------------------#
  # IOF3: valores para imputar     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------#
  df_total$llave_iof3 <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_pos)*10) + (as.numeric(df_total$p6240))

  iof3_output <- comp_f(original = df_total,
                        dom = "llave_iof3",
                        var = "iof3", y.bp = c(0, 500),
                        x.hist = c(0, 800), y.hist = c(0,0.02),
                        xy.min.box = c(450, 0.010),
                        xy.max.box = c(600, 0.015),
                        logical1 = "asalariado", logical2 = "independiente",
                        logical3 = "trab_familiares", logical4 = "des_ina")
  imputed_iof3 <- iof3_output
  colnames(imputed_iof3)[which(colnames(imputed_iof3) == "vi1")] = "IOF3ES"
  df_total <- merge(df_total, imputed_iof3[c("id","IOF3ES")], by = "id", all.x = T)


  #--------------------------------#
  # IOF6: valores para imputar     #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------#
  df_total$llave_iof6 <- (as.numeric(df_total$dom_terr)*100) + (as.numeric(df_total$dom_pos)*10) + (as.numeric(df_total$p6240))

  iof6_output <- comp_f(original = df_total,
                        dom = "llave_iof6",
                        var = "iof6", y.bp = c(0, 5000),
                        x.hist = c(0,5000), y.hist = c(0,0.005),
                        xy.min.box = c(2500, 0.003),
                        xy.max.box = c(3000, 0.004),
                        logical1 = "asalariado", logical2 = "independiente",
                        logical3 = "trab_familiares", logical4 = "des_ina")
  imputed_iof6 <- iof6_output
  colnames(imputed_iof6)[which(colnames(imputed_iof6) == "vi1")] = "IOF6ES"
  df_total <- merge(df_total, imputed_iof6[c("id","IOF6ES")], by = "id", all.x = T)




  #------------------------------------------------#
  #------------------------------------------------#
  #   Módulo 8.1: Comparación ingresos imputados   #
  #------------------------------------------------#
  #------------------------------------------------#

  # Las pruebas sugieren que se omite la variable IOF6ES (se usa IOF6)


  df_total <- df_total %>% filter(dominio == "CALI")

  # Ingresos de asalariados
  df_total$INGES_ASAL <- ifelse(df_total$asalariado == 1,
                                rowSums(df_total[c("IMPAES", "IEES",
                                                   "ISA", "IOF1", "IOF2",
                                                   "IOF3ES", "IOF6")],
                                        na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMPAES", "IEES",
                                                                                    "ISA", "IOF1", "IOF2",
                                                                                    "IOF3ES", "IOF6")])) == 0),
                                NA)

  # Ingresos de independientes
  df_total$INGES_IND <- ifelse(df_total$independiente == 1,
                               rowSums(df_total[c("IMPAES",
                                                  "ISA", "IOF1", "IOF2",
                                                  "IOF3ES", "IOF6")],
                                       na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMPAES",
                                                                                   "ISA", "IOF1", "IOF2",
                                                                                   "IOF3ES", "IOF6")])) == 0),
                               NA)

  # Ingresos de trabajadores familiares
  df_total$INGES_TF <- ifelse(df_total$trab_familiares == 1,
                              rowSums(df_total[c("ISA", "IOF1", "IOF2",
                                                 "IOF3ES", "IOF6")],
                                      na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("ISA", "IOF1", "IOF2",
                                                                                  "IOF3ES", "IOF6")])) == 0),
                              NA)

  # Ingreso de desocupados e inactivos
  df_total$INGES_NO <- ifelse(df_total$des_ina == 1,
                              rowSums(df_total[c("IMDI", "IOF1", "IOF2",
                                                 "IOF3ES", "IOF6")],
                                      na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("IMDI", "IOF1", "IOF2",
                                                                                  "IOF3ES", "IOF6")])) == 0),
                              NA)

  # Cálculo del ingreso total
  df_total$ingresos <- rowSums(df_total[c("INGES_ASAL",
                                          "INGES_IND", "INGES_TF",
                                          "INGES_NO")],
                               na.rm = T)* NA ^ (rowSums(!is.na(df_total[c("INGES_ASAL",
                                                                           "INGES_IND", "INGES_TF",
                                                                           "INGES_NO")])) == 0)

  # Excluir los siguientes ingresos: parentesco con el jefe de hogar como
  # empleados del servicio doméstico y sus parientes, trabajadores o pensionistas.
  df_total$ingresos[df_total$ingresos %in% c(6,7,8,9)] = 0

  cat("     Finalizado ✓ \n")


  Sys.sleep(1);cat("Módulo 2 y 3:   Cálculo ingresos de hogares ")


  dataset_x=df_total

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
    dataset_2$ingresos[q]= sum(df$ingresos)
  }

  # incluir la variable de tamaño de los hogares
  dataset_2 = merge(dataset_2, dataset_x[c("id", "p6008", "fex_c18")], by ="id")
  dataset_2 = dataset_2[!duplicated(dataset_2),]
  # remover bases de datos

  dataset_def= dataset_2  %>% filter(!ingresos  %in% 98)
  # eliminar hogares con ingresos 0
  dataset_def= dataset_2  %>% filter(!ingresos  %in% 0)

  #-----------------------------#
  # INICIO DEL MÓDULO 3 ORGINAL #
  #-----------------------------#

  gastos_ingresos_exp <- dataset_def %>%
    dplyr::slice(rep(1:n(), times = dataset_def$fex_c18)) %>%
    na.omit()

  gastos_ingresos_exp$per_capita = gastos_ingresos_exp$ingresos/gastos_ingresos_exp$p6008
  dataset_def_deciles = gastos_ingresos_exp %>% mutate(deciles = ntile(per_capita, 10))
  dataset_def_deciles$deciles = revalue(as.factor(dataset_def_deciles$deciles),
                                        c("1" = "Decil 1", "2" = "Decil 2",
                                          "3" = "Decil 3", "4" = "Decil 4",
                                          "5" = "Decil 5", "6" = "Decil 6",
                                          "7" = "Decil 7", "8" = "Decil 8",
                                          "9" = "Decil 9", "10" = "Decil 10"))
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

  cat("     Finalizado ✓ \n")

  #----------------------------------------------------------------------------------#
  #    Modulo 2: Proporcion del gasto en alimentación -ECV                           #
  #----------------------------------------------------------------------------------#

  Sys.sleep(1);cat("Módulo 4:  Proporcion del gasto en alimentación ")

  # Verificar si el entorno data_ECV existe y si Año es 2022
  if (!exists("data_ECV")) {
    data_ECV <- new.env(parent = emptyenv())
  }

  if (!exists("envr_name", envir = data_ECV)) {
    envr_name <- "data_ECV_env"
    assign(envr_name, new.env(parent = emptyenv()), envir = data_ECV)
  }

  if (Year == 2022) {
    url_gastos_hogares <- "https://microdatos.dane.gov.co/index.php/catalog/793/download/22594"
    url_caracteristica_hogar <- "https://microdatos.dane.gov.co/index.php/catalog/793/download/22586"
    url_vivienda <- "https://microdatos.dane.gov.co/index.php/catalog/793/download/22591"
    url_servicios <- "https://microdatos.dane.gov.co/index.php/catalog/793/download/22597"

    # Lista de URLs y nombres de variables
    urls <- list(
      gastos_hogares = url_gastos_hogares,
      hogares = url_caracteristica_hogar,
      vivienda = url_vivienda,
      servicios = url_servicios
    )
  }
  # Función para descargar y extraer archivos ZIP
  download_and_extract <- function(url, temp_folder) {
    temp_zip <- tempfile(fileext = ".zip")
    GET(url, write_disk(temp_zip, overwrite = TRUE), timeout(1000))

    if (.Platform$OS.type == "windows") {
      unzip_command <- sprintf('powershell -Command "Expand-Archive -LiteralPath \'%s\' -DestinationPath \'%s\' > $null 2>&1"', temp_zip, temp_folder)
      system(unzip_command, wait = TRUE)
    } else {
      system2("unzip", c(temp_zip, "-d", temp_folder), stdout = NULL, stderr = NULL)
    }

    archivos_csv <- list.files(temp_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    return(archivos_csv)
  }

  # Función para detectar el delimitador
  detect_delimiter <- function(file_path) {
    first_lines <- readLines(file_path, n = 5)
    delimiter <- ifelse(sum(grepl(",", first_lines)) > sum(grepl(";", first_lines)), ",", ";")
    return(delimiter)
  }

  # Descargar y procesar los archivos
  for (name in names(urls)) {
    url <- urls[[name]]
    temp_folder <- file.path(tempdir(), name)
    dir.create(temp_folder)
    csv_files <- download_and_extract(url, temp_folder)

    for (csv_file in csv_files) {
      delimiter <- detect_delimiter(csv_file)
      data <- read.csv(csv_file, sep = delimiter)
      assign(name, data, envir = get(envr_name, envir = data_ECV))
    }
  }

  # Acceder a las variables asignadas en el entorno data_ECV_env
  gastos_hogares <- get("gastos_hogares", envir = get(envr_name, envir = data_ECV))
  hogares <- get("hogares", envir = get(envr_name, envir = data_ECV))
  vivienda <- get("vivienda", envir = get(envr_name, envir = data_ECV))
  servicios <- get("servicios", envir = get(envr_name, envir = data_ECV))



  cat("     Finalizado ✓ \n")

  Sys.sleep(1);cat("Módulo VACIO ")

  # Restricción de la información a Cali (tamaño de muestra n = 3338)
  vivienda_cali = vivienda %>% filter(P1_DEPARTAMENTO %in% 76)
  vivienda_cali = vivienda_cali %>% filter(CLASE %in% 1)
  gastos_hogares_cali = gastos_hogares %>% filter(DIRECTORIO %in% vivienda_cali$DIRECTORIO)
  servicios_cali = servicios %>% filter(DIRECTORIO %in% vivienda_cali$DIRECTORIO)

  # Selección de las variables de interés
  vivienda_cali_1 = vivienda_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P",
                                    "ORDEN", "P1_DEPARTAMENTO", "CLASE", "FEX_C",
                                    "CANT_HOG_COMPLETOS", "CANT_HOGARES_VIVIENDA")]

  gastos_hogares_cali_1 = gastos_hogares_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P",
                                                "ORDEN", "FEX_C", "P3204", "P3204S1", "P3204S2")]

  servicios_cali_1 = servicios_cali[c("DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN",
                                      "I_HOGAR", "I_UGASTO", "PERCAPITA", "I_OU")]

  Sys.sleep(1);cat("Módulo 4:  Proporcion del gasto en alimentación ")


  deciles_gasto = data.frame(levels(as.factor(dataset_def_deciles$deciles)))
  colnames(deciles_gasto) = "deciles"
  deciles_gasto$share = c( 0.47,  0.47,
                           0.35, 0.35,
                           0.30,  0.30,
                           0.27,  0.27,
                           0.22,  0.22)



  dataset_def_deciles$id_aux = c(1:nrow(dataset_def_deciles))
  dataset_def_deciles = merge(dataset_def_deciles, deciles_gasto, by = "deciles", all.x = TRUE)
  dataset_def_deciles = dataset_def_deciles[order(dataset_def_deciles$id_aux),]
  dataset_def_deciles = dataset_def_deciles[setdiff(colnames(dataset_def_deciles), "id_aux")]
  # calcular ingreso dedicado a alimentacion
  dataset_def_deciles$ingreso_alimentos = dataset_def_deciles$share*dataset_def_deciles$ingresos
  dataset_def_deciles$ingreso_alimentos_per_capita = dataset_def_deciles$ingreso_alimentos/dataset_def_deciles$p6008


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

  dataset_def_deciles$ingreso_alimentos_per_capita=dataset_def_deciles$ingreso_alimentos/dataset_def_deciles$fex_c18
  dataset_def_deciles$per_capita_year = dataset_def_deciles$ingreso_alimentos_per_capita*12

  resultado=list(dataset_def_deciles,mean_income_deciles,gastos_ingresos_exp);names(resultado)=c("dataset_def_deciles","mean_income_deciles","gastos_ingresos_exp")
  return(resultado)
}

x_1=Modulo_1(Month=1, Year=2022,City="Cali")


rm(data_GEIH)


