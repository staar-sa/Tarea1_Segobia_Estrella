#TAREA 1 - PROBLEMA 3# 

# Cargar paquetes necesarios
library(ggplot2)

# Cargar los datos
cogustos <- read.csv("cogustos.csv")

#Observar y entender la estructura
str(cogustos)

###Probabilidad de que a una persona del grupo no le guste el chocolate
#1- Calcular la probabilidad empírica
p_chocolate <- mean(cogustos$Chocolate == 0)
cat("Probabilidad de que a una persona no le guste el chocolate:", p_chocolate, "\n")


#2- Probabilidad de que al menos 5 personas del grupo no sepan conducir un auto

  #Número total de participantes
  n_participantes <- nrow(cogustos)
  n_participantes

  #Proporción de personas que no saben conducir
  p_no_car <- mean(cogustos$Conducir == 0)
  p_no_car
  
 #Calcular P(X >= 5) con distribución binomial
p_5_no_car <- 1 - dbinom(4, size = n_participantes, prob = p_no_car)
cat("Probabilidad de que al menos 5 personas no sepan conducir:", p_5_no_car, "\n")



#3- Probabilidad de que, si tomo 10 personas al azar, a 3 les guste la Fórmula 1

  #Proporción de personas a las que les gusta la F1
  p_F1 <- mean(cogustos$F1 == 1)
  p_F1
  
  #Calcular P(X = 3)
 p_3_si_F1 <- dbinom(3, size = 10, prob = p_F1)
cat("Probabilidad de que a 3 personas de 10 les guste la F1:", p_3_si_F1, "\n")

#4 - Elaborar una función en R para cada actividad que calcule la probabilidad de que, al tomar 10 personas, a n < 10 les guste o no cierta actividad
# Función para calcular P(X = x) en una muestra de 10 personas
prob_bi <- function(n, actividad, x) {
  # Proporción de personas a las que les gusta la actividad
  p_actividad <- mean(cogustos[[actividad]] == 1)
  
  # Calcular P(X = x) usando la distribución binomial
  resultado <- dbinom(x, size = n, prob = p_actividad)
  
  return(resultado)
}

# Ejemplo de uso con 10 personas y la actividad "comedias romnticas"
cat("Probabilidad de que a 4 de 10 personas les guste las comedias romanticas:", prob_bi(10, "Rom.Coms", 4), "\n")


#5- Probabilidad de que a una persona del grupo le guste el K-pop y el anime
  
  #Probabilidad conjunta
  p_kpop_anime <- mean(cogustos$Kpop == 1 & cogustos$Anime == 1)
cat("Probabilidad de que a una persona del grupo le gusta el Kpop y el anime:", p_kpop_anime, "\n")


#6- Probabilidad de que, si tomo 15 personas al azar, a n = 1,2,3,4,5 les gusten las RomCom 

  #Proporción de personas a las que les gustan las Rom-Coms
  p_RomCom <- mean(cogustos$Rom.Coms == 1)
  p_RomCom

  #Calcular P(X = 1 a 5)
  p_romcoms <- dbinom(1:5, size = 15, prob = p_RomCom)
  names(p_romcoms) <- paste0("P(X=", 1:5, ")")
  print(p_romcoms)



#7 - Calcula lo siguiente:
  
  #7.1 Hallar la probabilidad de al menos un éxito en 6 intentos con p=0.5
  p_un_exito <- 1 - dbinom(0, size = 6, prob = 0.5)
 cat("P(al menos un éxito en 6 intentos, p=0.5):", p_un_exito, "\n")

  #7.2 Determinar la probabilidad de que al menos 3 de 4 personas les guste bailar
  p_dance <- mean(cogustos$Bailar == 1)
 p_3_bailar <- 1 - dbinom(2, size = 4, prob = p_dance)
cat("Probabilidad de que al menos 3 de 4 personas les guste bailar):", p_3_bailar, "\n")


  #7.3 Probabilidad de que un grupo de 6 personas tenga al menos 4 que consuman Chocolate
  p_si_chocolate <- mean(cogustos$Chocolate == 1)
  p_4_chocolate <- 1 - dbinom(3, size = 6, prob = p_si_chocolate)
  cat("Probabilidad de que al menos 4 de 6 personas consuman Chocolate):", p_4_chocolate, "\n")


  #7.4 Hallar la probabilidad de que 3 de 7 personas hagan jogging
  p_jogging <- mean(cogustos$Jogging == 1)
  p_3_jogging <- dbinom(3, size = 7, prob = p_jogging)
  cat("Probabilidad de que al menos 3 de 7 personas hagan jogging):", p_3_jogging, "\n")

  #7.5 Calcular la probabilidad de que 2 de 5 personas les gusten las Rom-Coms
p_2_romcoms <- dbinom(2, size = 5, prob = p_RomCom)
cat("Probabilidad de que al menos 2 de 5 personass les gusten las Rom-Coms):", p_2_romcoms, "\n")



#Tabla resumen de probabilidades empíricas
probabilidades <- data.frame(
  Actividad = c("Chocolate", "Conducir", "F1", "Kpop y Anime", "RomComs"),
  Probabilidad = c(p_chocolate, p_no_car, p_F1, p_kpop_anime, p_RomCom)
)
print(probabilidades)

# Gráficos de barras para visualizar las preferencias
g_pref_chocolate <- ggplot(cogustos, aes(x = as.factor(Chocolate))) +
  geom_bar(fill = "pink", alpha = 0.7) +
  labs(title = "Preferencia por el Chocolate", x = "¿Le gusta el chocolate?", y = "Frecuencia")
g_pref_chocolate

g_pref_car <- ggplot(cogustos, aes(x = as.factor(Conducir.un.auto))) +
  geom_bar(fill = "red", alpha = 0.7) +
  labs(title = "Personas que saben conducir", x = "¿Sabe conducir?", y = "Frecuencia")
g_pref_car


# Histogramas de probabilidades
g3 <- ggplot(data.frame(x = 0:10, y = dbinom(0:10, size = 10, prob = p_F1)), aes(x, y)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  labs(title = "Distribución Binomial: Gustos por la F1", x = "Personas", y = "Probabilidad")
g3

g4 <- ggplot(data.frame(x = 0:15, y = dbinom(0:15, size = 15, prob = p_RomCom)), aes(x, y)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Distribución Binomial: Gustos por RomComs", x = "Personas", y = "Probabilidad")
g4

# Mostrar gráficos
grid.arrange(g_pref_chocolate, g_pref_car, g3, g4, ncol = 2)

# Bibliografía
cat("\n\nBibliografía:cogustos.csv\n")


