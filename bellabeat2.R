# Instalar y cargar el paquete tidyverse
install.packages("tidyverse")

# Cargar librerías necesarias
library(tidyverse)    # Paquete principal que incluye varias librerías útiles
library(lubridate)    # Para manipulación de fechas y horas
library(dplyr)        # Para manipulación y transformación de datos
library(ggplot2)      # Para visualización de datos con gráficos
library(tidyr)        # Para manipulación de datos en formato tidy

# Importar datos desde archivos CSV
daily_activity <- read.csv("C:\\Users\\Mariana\\Desktop\\Proyectos\\dailyActivity_merged.csv", header = TRUE, sep = ",")
sleep_day <- read.csv("C:\\Users\\Mariana\\Desktop\\Proyectos\\sleepDay_merged.csv", header = TRUE, sep = ",")
weight <- read.csv("C:\\Users\\Mariana\\Desktop\\Proyectos\\weightLogInfo_merged.csv", header = TRUE, sep = ",")

# Visualización rápida de los datos
glimpse(daily_activity)
glimpse(sleep_day)
glimpse(weight)

#############################################limpieza ###################################################################
# Modificar tipo de datos en la columna 'ActivityDate' de daily_activity

daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")
glimpse(daily_activity)  # Verificar el resultado


  # Modificar tipo de datos en la columna 'SleepDay' de sleep_day y eliminar la hora

sleep_day$SleepDay <- as.Date(sleep_day$SleepDay, format = "%m/%d/%Y")
glimpse(sleep_day)  # Verificar el resultado


  # Modificar tipo de datos en la columna 'Date' de weight y eliminar la hora
  weight$Date <- as.Date(weight$Date, format = "%m/%d/%Y")
glimpse(weight)  # Verificar el resultado


# Cambiar el tipo de dato de 'Id' a character

daily_activity$Id <- as.character(daily_activity$Id)
sleep_day$Id <- as.character(sleep_day$Id)
weight$Id <- as.character(weight$Id)


# Verificar existencia de valores nulos en daily_activity
valores_nulos_por_columna1 <- colSums(is.na(daily_activity))
print(valores_nulos_por_columna1)

  # Verificar existencia de valores nulos en sleep_day
  valores_nulos_por_columna3 <- colSums(is.na(sleep_day))
print(valores_nulos_por_columna3)


  # Verificar existencia de valores nulos en weight
  valores_nulos_por_columna4 <- colSums(is.na(weight))
print(valores_nulos_por_columna4)

# Verificar existencia de valores duplicados en daily_activity
duplicados <- duplicated(daily_activity)
# Mostrar filas duplicadas
daily_activity[duplicados, ]

# Verificar existencia de valores duplicados en daily_activity
duplicados <- duplicated(sleep_day)
# Mostrar filas duplicadas
sleep_day[duplicados, ]

# Verificar existencia de valores duplicados en daily_activity
duplicados <- duplicated(weight)
# Mostrar filas duplicadas
weight[duplicados, ]


#############################################Analisis ###################################################################

# Contar el número de valores únicos en la columna 'Id' del data frame 'daily_activity'
n_distinct_activity <- n_distinct(daily_activity$Id)

# Contar el número de valores únicos en la columna 'Id' del data frame 'sleep_day'
n_distinct_sleep <- n_distinct(sleep_day$Id)

# Contar el número de valores únicos en la columna 'Id' del data frame 'weight'
n_distinct_weight <- n_distinct(weight$Id)


# Seleccionar las columnas TotalSteps, TotalDistance, SedentaryMinutes y Calories del data frame 'daily_activity'
# y mostrar un resumen estadístico
daily_activity %>%
  select(TotalSteps, 
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# Resumen estadístico de el dataset sleep_day
sleep_day %>%   
  select(TotalSleepRecords, 
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# Resumen estadístico del dataset weight
weight %>%   
  select(WeightKg, 
         BMI) %>%
  summary()

# Agregar una nueva columna 'ActivityCategory' al data frame 'daily_activity' basada en el número de pasos diarios
daily_activity <- daily_activity %>%
  mutate(ActivityCategory = case_when(
    TotalSteps < 7500 ~ "Actividad baja",
    TotalSteps >= 7500 & TotalSteps < 10000 ~ "Medianamente activo",
    TotalSteps >= 10000 ~ "Activo"
  ))

# Crear una nueva columna 'total_minutes_used'
daily_activity$total_minutes_used <- daily_activity$VeryActiveMinutes +
  daily_activity$FairlyActiveMinutes +
  daily_activity$LightlyActiveMinutes +
  daily_activity$SedentaryMinutes

# Agrupar por 'Id' y calcular el promedio de 'total_minutes_used'
id_avg_minutes <- aggregate(total_minutes_used ~ Id, data = daily_activity, mean)

# Ordenar los usuarios por minutos promedio utilizados
id_avg_minutes <- id_avg_minutes[order(id_avg_minutes$total_minutes_used, decreasing = TRUE), ]

# Definir condiciones y etiquetas para la clasificación
conditions <- list(
  id_avg_minutes$total_minutes_used < 720,
  id_avg_minutes$total_minutes_used >= 720 & id_avg_minutes$total_minutes_used <= 1080,
  id_avg_minutes$total_minutes_used > 1080
)
values <- c('Uso bajo', 'Uso normal', 'Alto uso')

# Crear la columna 'Nivel de uso' basada en las condiciones
id_avg_minutes$nivel_uso <- cut(id_avg_minutes$total_minutes_used, breaks = c(-Inf, 720, 1080, Inf), labels = values)

# Mostrar las primeras 10 filas
head(id_avg_minutes, 10)

#############################################Visualización ###################################################################


# Calcular porcentajes por nivel de uso
percentage_by_level <- id_avg_minutes %>%
  group_by(nivel_uso) %>%
  summarise(percentage = sum(total_minutes_used) / sum(id_avg_minutes$total_minutes_used) * 100)

# Crear un gráfico circular de pastel con etiquetas
pie_plot <- ggplot(percentage_by_level, aes(x = "", y = percentage, fill = nivel_uso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Uso bajo" = "#EA3B2C", "Uso normal" = "#EBC13C", "Alto uso" = "#99EB15")) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
  labs(title = "Proporción de uso del dispositivo",
       fill = "Nivel de Uso",
       x = NULL,
       y = NULL) +
  theme_minimal()

# Mostrar el gráfico circular con etiquetas
print(pie_plot)

######################################################


# Categorizar la actividad diaria
daily_activity <- daily_activity %>%
  mutate(ActivityCategory = case_when(
    TotalSteps < 7500 ~ "Actividad baja",
    TotalSteps >= 7500 & TotalSteps < 10000 ~ "Medianamente activo",
    TotalSteps >= 10000 ~ "Activo"
  ))

# Especificar colores
colores <- c(
  "Actividad baja" = "#EA3B2C",
  "Medianamente activo" = "#EBC13C",
  "Activo" = "#99EB15"
)

# Calcular el porcentaje por categoría
percentage_by_category <- daily_activity %>%
  group_by(ActivityCategory) %>%
  summarise(percentage = n() / nrow(daily_activity) * 100)

# Crear el diagrama circular de porcentajes
pie_plot <- ggplot(data = percentage_by_category, aes(x = "", y = percentage, fill = ActivityCategory)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Proporción por categoría de actividad",
       fill = "Categoría de Actividad",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = colores)+
  theme_minimal()

# Mostrar el gráfico circular
print(pie_plot)

######################################################


print(ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) + 
        geom_point() + 
        geom_smooth(color = "#5392EA", method = "auto") + 
        labs(title = "Total de pasos vs. Calorias quemadas"))

######################################################

# Calcular la suma de minutos de sedentarismo y actividad
total_minutes <- daily_activity %>%
  summarise(SedentaryMinutes = sum(SedentaryMinutes),
            ActiveMinutes = sum(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes))

# Crear un data frame con los minutos de sedentarismo y actividad
data <- data.frame(
  Categoria = c("Sedentarismo", "Actividad"),
  Minutos = c(total_minutes$SedentaryMinutes, total_minutes$ActiveMinutes)
)

# Especificar colores
colores <- c("Sedentarismo" = "#EB3C15", "Actividad" = "#99EB15")

# Crear el gráfico de barras
ggplot(data, aes(x = Categoria, y = Minutos, fill = Categoria)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  labs(title = "Comparación de Minutos de Sedentarismo y Actividad",
       y = "Minutos",
       x = "Categoría") +
  scale_fill_manual(values = colores) +  # Especificar colores manualmente
  theme_minimal() +
  theme(legend.position = "none")  # No mostrar la leyenda si es innecesaria



######################################################

# Crear una nueva columna con el día de la semana correspondiente a cada fecha
daily_activity$dia_semana <- weekdays(daily_activity$ActivityDate)

# Calcular la suma de minutos de sedentarismo y actividad por día de la semana
total_minutes <- daily_activity %>%
  group_by(dia_semana) %>%
  summarise(
    SedentaryMinutes = sum(SedentaryMinutes),
    ActiveMinutes = sum(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)
  )

# Convertir los datos a formato largo (long)
total_minutes_long <- total_minutes %>%
  gather(key = "variable", value = "value", SedentaryMinutes, ActiveMinutes)

# Ordenar la columna día de la semana 
total_minutes_long$dia_semana <- factor(total_minutes_long$dia_semana, levels = c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado"))

# Crear el gráfico de barras apiladas
ggplot(total_minutes_long, aes(x = dia_semana, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Minutos de Actividad y Sedentarismo por Día de la Semana",
       x = "Día de la Semana",
       y = "Total de Minutos",
       fill = "Categorías") +  # Cambiado el nombre de la leyenda
  scale_fill_manual(values = c("SedentaryMinutes" = "#EB3C15", "ActiveMinutes" = "#99EB15")) +
  theme_minimal()





