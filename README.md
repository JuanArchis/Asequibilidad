# :large_blue_diamond:	 Asequibilidad :large_blue_diamond:	
 
#

## :computer: **Introducción:**

Primera versión del paquete que estima la asequibilidad a tres tipos de dietas en Colombia.


## :red_circle: **Instrucciones de instalación:** Instale el paquete alojado en el presente repositorio ejecutando en R:



```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("JuanArchis/Asequibilidad");library(Asequibilidad)

```


## :red_circle: **Instrucciones de uso:** 

1. La función "Asequibilidad" cuenta con tres parámetros, a saber, Month (Mes) ,Year (Año) ,City (Ciudad). En R utilice:


```
Salida=Asequibilidad::Modulos(Month = 1,Year=2022,City="Cali")

```
2. Debe asginar la función para poder acceder a los resutlados de la función: Poverty_outcome y Mean_income_food.

## :bangbang: Aspectos a tener en cuenta

- Flata generalziar la estructura de depuración para los años diferentes al 2022. La función sólo funciona para los 12 meses del 2022.


