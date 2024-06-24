
Modulo_2 <- function(Month, Year, City) {
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

resultado=list(model_dieta_1,model_dieta_2,model_dieta_3);names(resultado)=c("model_dieta_1","model_dieta_2","model_dieta_3")
return(resultado)

}


x_2=Modulo_2(Month=1, Year=2022,City="Cali")
x_2$model_dieta_1
