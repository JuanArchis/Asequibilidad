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
GET(url, write_disk(temp_zip, overwrite = TRUE), timeout(1000))
temp_folder <- tempdir()
zip::unzip(temp_zip, exdir = temp_folder)

archivos_csv <- list.files(temp_folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

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


}








