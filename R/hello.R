#------------------------------------------------#
#           Definición de la función             #
#------------------------------------------------#

Fun <- function(Month, Year, City) {

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

  # Función para crear o reusar un entorno
  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }




  # Utilizar crear_o_reusar_entorno() solo para data_GEIH
  data_GEIH <- crear_o_reusar_entorno("data_GEIH")

  # Crear el nombre de la lista
  envr_name <- paste0("GEIH_", Month, "_", Year, "_", City)

  # Verificar si la lista ya existe en data_GEIH
  if (!exists(envr_name, envir = data_GEIH)) {
    # La lista no existe, descargar y cargar los datos
    assign(envr_name, new.env(parent = emptyenv()), envir = data_GEIH)

    # Función para generar la URL de descarga
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


    rchivos <- list.files(temp_folder,full.names = TRUE)[2]
    csv_folder <- file.path(rchivos, "CSV")
    archivos_csv <- list.files(csv_folder, full.names = TRUE)


  }

  # Función para detectar el delimitador
  detect_delimiter <- function(file_path) {
    # Lee las primeras líneas del archivo
    first_lines <- readLines(file_path, n = 5)
    # Busca el delimitador más probable (, o ;)
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


  # Patrones de nombres a mantener
  patrones_a_mantener <- c("Ocupados", "No ocupados", "Otros ingresos e impuestos", "Datos del hogar y la vivienda")

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



}


# Ejemplo de uso

Fun(Month = 1, Year = 2022, City = "Cali")


