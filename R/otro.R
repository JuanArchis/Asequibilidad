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

###############################
## Cálculo del ingreso para  ##
##       cada hogar          ##
###############################
# construir id
servicios_cali_1$id = paste0(servicios_cali$DIRECTORIO,
                             "-",servicios_cali$ORDEN)

# ingresos hogares
hogar_ingresos = servicios_cali_1[c("id", "I_HOGAR")]

###########################################
## Cálculo del gasto total y  gasto      ##
##   en alimentación para cada hogar     ##
###########################################

# construir id
gastos_hogares_cali_1$id = paste0(gastos_hogares_cali_1$DIRECTORIO,
                                  "-",gastos_hogares_cali_1$SECUENCIA_P)

# reemplazar NA por 0 en las variables de gasto
gastos_hogares_cali_1$P3204S1[is.na(gastos_hogares_cali_1$P3204S1)] = 0
gastos_hogares_cali_1$P3204S2[is.na(gastos_hogares_cali_1$P3204S2)] = 0

# construir base de datos de recepción
hogar_gastos = data.frame(levels(as.factor(gastos_hogares_cali_1$id)))
hogar_gastos$gasto_total = NA
hogar_gastos$gasto_alimentos = NA
colnames(hogar_gastos) = c("id", "gasto_total", "gasto_alimentos")

for (k in 1:nrow(hogar_gastos)) {
  # bucle para gasto total
  df_1 = data.frame()
  df_1 = gastos_hogares_cali_1 %>% filter(id %in% hogar_gastos$id[k])
  hogar_gastos$gasto_total[k] = sum(df_1$P3204S1) + sum(df_1$P3204S2)
  #bucle para gasto en alimentos
  df_2 = df_1 %>% filter(P3204 %in% c(1:26,32))
  hogar_gastos$gasto_alimentos[k] = sum(df_2$P3204S1) + sum(df_2$P3204S2)
}

###################################
## Cálculo de las proporciones   ##
## (desde el ingreso y el gasto) ##
###################################

# recuperar el factor de expansión
hogar_gastos_dep = merge(hogar_gastos, gastos_hogares_cali_1[c("id","FEX_C")], by = "id")
hogar_gastos_dep = hogar_gastos_dep[!duplicated(hogar_gastos_dep),]

# eliminar valores nulos en alimentación
hogar_gastos_dep = hogar_gastos_dep %>% filter(gasto_alimentos != 0)

# merge gastos-ingresos
gastos_ingresos = merge(hogar_gastos_dep, hogar_ingresos, by = "id")

# implementación del factor de expansión

gastos_ingresos_exp = as.data.frame(matrix(ncol = ncol(gastos_ingresos)))
colnames(gastos_ingresos_exp) = colnames(gastos_ingresos)



gastos_ingresos_exp <- gastos_ingresos %>%
  group_by(row_index = row_number()) %>%
  dplyr::slice(rep(row_number(), times = FEX_C)) %>%
  ungroup() %>%
  na.omit()


# proporción del gasto
gastos_ingresos_exp$share_gasto = gastos_ingresos_exp$gasto_alimentos/gastos_ingresos_exp$gasto_total

# Nota: los resultados para la proporción del ingresos no tiene resultados realistas

############
## Ad hoc ##
############

# eliminar gastos e ingresos nulos
gastos_ingresos_exp = gastos_ingresos_exp %>% filter(I_HOGAR != 0)
gastos_ingresos_exp = gastos_ingresos_exp %>% filter(gasto_total != 0)

############
## Ad hoc ##
############

deciles_ingresos = quantile(gastos_ingresos_exp$I_HOGAR,
                            probs = seq(0.1,1, by = 0.1))
# clasificación por deciles
gastos_ingresos_exp = gastos_ingresos_exp %>% mutate(deciles = cut(I_HOGAR,
                                                                   c(min(gastos_ingresos_exp$I_HOGAR)
                                                                     , as.numeric(deciles_ingresos)),
                                                                   c("decil 1", "decil 2", "decil 3",
                                                                     "decil 4", "decil 5", "decil 6",
                                                                     "decil 7", "decil 8", "decil 9",
                                                                     "decil 10")))

# calculo de proporciones medias del gasto en alimentacion

mean_share = data.frame(c("decil 1", "decil 2", "decil 3",
                          "decil 4", "decil 5", "decil 6",
                          "decil 7", "decil 8", "decil 9",
                          "decil 10"))
colnames(mean_share) = "decil"
mean_share$share = NA

for (i in 1:nrow(mean_share)) {
  df = gastos_ingresos_exp %>% filter(deciles %in% mean_share$decil[i])
  mean_share$share[i] = mean(df$share_gasto)
}

# presentación de proporción media
mean_share$share = mean_share$share*100




dataset_def_deciles=x




dataset_def_deciles$id_aux = c(1:nrow(dataset_def_deciles))
dataset_def_deciles = merge(dataset_def_deciles, deciles_gasto, by = "deciles", all.x = TRUE)
dataset_def_deciles = dataset_def_deciles[order(dataset_def_deciles$id_aux),]
dataset_def_deciles = dataset_def_deciles[setdiff(colnames(dataset_def_deciles), "id_aux")]
# calcular ingreso dedicado a alimentacion
dataset_def_deciles$ingreso_alimentos = dataset_def_deciles$share*dataset_def_deciles$ingresos








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
