
Modulo_3 <- function(dataset_def_deciles,model_dieta_1,model_dieta_2,model_dieta_3,mean_income_deciles) {
Sys.sleep(1);cat("Módulo 6: Cálculo de indicadores de asequibilidad")

  deciles_grupos = c("Decil 1", "Decil 2",
                     "Decil 3", "Decil 4",
                     "Decil 5", "Decil 6",
                     "Decil 7", "Decil 8",
                     "Decil 9", "Decil 10")
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

x_3=Modulo_3(dataset_def_deciles=x_1$dataset_def_deciles,model_dieta_1=x_2$model_dieta_1,model_dieta_2=x_2$model_dieta_2,model_dieta_3=x_2$model_dieta_3,mean_income_deciles=x_1$mean_income_deciles)

View(x_3$Poverty_outcome)
View(x_3$Mean_income_food)

