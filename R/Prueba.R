Prueba=function(Month, Year,City){


  crear_o_reusar_entorno <- function(nombre_entorno) {
    if (!exists(nombre_entorno, envir = globalenv())) {
      assign(nombre_entorno, new.env(parent = emptyenv()), envir = globalenv())
    }
    return(get(nombre_entorno, envir = globalenv()))
  }

data_GEIH <- crear_o_reusar_entorno("data_GEIH");envr_name <- paste0("GEIH_", Month, "_", Year, "_", City)



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



# Obtener la lista de archivos
archivos_csv <- dir(temp_folder, full.names = TRUE)


print(archivos_csv)


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

Prueba(Month = 1,Year=2022,City="Cali")





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
url <- generate_download_url(1, 2022)

# Descargar y extraer el archivo ZIP
temp_zip <- tempfile(fileext = ".zip")
download.file(url, temp_zip, timeout = 1000)
temp_folder_1 <- tempdir()
#unzip(temp_zip, exdir = temp_folder)

# Descomprimir el archivo ZIP
system(sprintf("unzip -qq -j -o -O UTF-8 %s -d %s", temp_zip, temp_folder_1), ignore.stdout = TRUE)

# Listar los archivos en el directorio temporal
archivos <- list.files(temp_folder_1, full.names = TRUE, recursive = TRUE)


# Crear un vector vacío para almacenar los archivos CSV
archivos_csv <- c()

# Iterar sobre los nombres de los archivos
for (archivo in archivos) {
  # Comprobar si el nombre del archivo termina con .csv (ignorando mayúsculas/minúsculas)
  if (grepl("\\.csv$", archivo, ignore.case = TRUE)) {
    # Si termina con .csv, agregarlo al vector de archivos CSV
    archivos_csv <- c(archivos_csv, archivo)
  }
}

# Imprimir los archivos CSV encontrados
archivos_csv


