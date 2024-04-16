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

# Encuentra la ruta de la carpeta CSV
archivos_csv <- list.files(temp_folder, recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

# Imprimir la lista de archivos CSV encontrados
print(archivos_csv)

}




