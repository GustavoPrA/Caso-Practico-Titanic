# Este analisis es sobre el Titanic, contiene datos sobre los pasageros, genero,
#clase, nombre, sexo,r#edad, familiares, padres, etc.

# Se importa la base de datos asi como tidyverse para poder manupular los datos 
#y el csv con el que trabajaremos

library(tidyverse)

getwd()

df <- read.csv("Titanicv2.csv")
str(df)
view(df)
summary(df)


#Con esta funcion se logra identificar la cantidad de datos nulos o faltantes de 
#la base de datos. Es importante hacer saber dicho resultado para mejorar la 
#eficiencia del software o herramienta para recopilacion de datos.
colSums(is.na(df))
#En este caso se cuenta con aprox 20% de datos nulos lo cual es notable
#no se recomienda ignorar de la muestra los datos nulos si son pocos (2% o 5%)

#Porcentaje de NAs por columna
colMeans(is.na(df)) * 100

# Para no eliminar el dato de edad que podria resultar dato util se dejara tal cual
#pero se hace incapie en que tiene 20% de ausencia de datos.

#por otro lado, la columna de "Fare" tiene 0.23% de datos nulos lo cual si podrian
#eliminarese 

#se elimina de la columna "Fare" los datos nulos.

df <- df %>% filter(!is.na(Fare))
view(df)

# Ahora con el dataframe un poco mas limpio se decide trabajar con promedios de 
#edad por grupo de sobreviviente o no (SOLO PARA EDAD CONOCIDA)

df %>%
  filter(!is.na(Age)) %>%
  group_by(Survived) %>%
  summarise(
    Promedio_Edad = mean(Age),
    Mediana_Edad = median(Age),
    Max_Edad = max(Age),
    Total = n()
  )
# El resultado es que
# Análisis de Edad ySupervivencia

#En promedio, la edad no parece tener una gran diferencia entre quienes 
#sobrevivieron y quienes no (ambos promedios y medianas son muy parecidos).

#Sin embargo, la persona más longeva que sobrevivió tenía 76 años, mientras 
#que en el grupo que no sobrevivió el máximo fue de 67.

#Esto no es concluyente aún, pero es un dato interesante para 
#mencionar más adelante en el resumen.*


#para poder tener mas detalle de este resultado se incluye un histograma de edad
#por sobreviviencia
df %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Distribución de edades según sobrevivencia",
       x = "Edad",
       y = "Cantidad de pasajeros") +
  theme_minimal()
#NOTA con "binwidth" se agrupa las edades por rantos, se puede modificar a gusto







#Sobrevivientes por clase
#vamos a identificar cuantos sobrevivientes por clase hay.

#Primero se decide sacar promedio de sobrevivientes por Pclass para despues
#sacar un porcentaje

df %>%
  group_by(Pclass, Survived) %>%
  summarise(
    Total = n()
  )
# lo que nos arroja dividido por Pclass 2 diviciones "yes" y "no" de casa una.


# a continuacion podria tambien hacerse la proporcion de personas que SI sobrevivieron
# en cada clase con su porcentaje

df %>%
  group_by(Pclass) %>%
  summarise(
    Total = n(),
    Sobrevivieron = sum(Survived == "Yes"),
    Proporcion = round(Sobrevivieron / Total * 100, 1)
  )

#Para visualisar esto se incluyen 2 graficos, barras que estan lado a lado
#y tambien barras proporcionales.


#Este grafico de barras lado a lado nos muestras visualmente la proporcion que 
#hay de sobrevivientes por cada clase registrada en la base de datos.

df %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Sobrevivencia por clase social",
       x = "Clase (Pclass)",
       y = "Cantidad de pasajeros") +
  theme_minimal()
#NOTA se logra identificar que en las 3 clases la mayoria pasageros no sobrevivieron  

df %>%
  group_by(Pclass, Survived) %>%
  summarise(Total = n()) %>%
  group_by(Pclass) %>%
  mutate(Proporcion = Total / sum(Total)) %>%
  ggplot(aes(x = Pclass, y = Proporcion, fill = Survived)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporción de sobrevivientes por clase",
       x = "Clase (Pclass)",
       y = "Proporción (%)") +
  theme_minimal()

#Este ultimo grafico es un poco mas facil para visualizar y comparar resultados.


#                           PRECIO DEL TICKET DE CADA CLASE
#_____________________________________________________________________________


# A continuacion obtendremos datos sobre el precio del Ticket de cada clase
#se tomara en cuenta el Min, max, prom y med.

df %>%
  group_by(Pclass) %>%
  summarise(
    Min_Fare = min(Fare, na.rm = TRUE),
    Max_Fare = max(Fare, na.rm = TRUE),
    Promedio_Fare = mean(Fare, na.rm = TRUE),
    Mediana_Fare = median(Fare, na.rm = TRUE),
    Total_Pasajeros = n()
  )

# Con los resultados logramos identificar un sesgo donde la mediana y promedio
#no son cercanos. En el caso de upper class el precio mas alto es de 512 lo cual
#aumenta el promedio pero no significa que la mayoria compraron el ticket a ese 
#precio

#Para entender mas facil esto vamos a graficar con diagrama de caja

df %>%
  ggplot(aes(x = Pclass, y = Fare, fill = Pclass)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Distribución del precio del ticket por clase",
       x = "Clase",
       y = "Fare (precio del ticket)") +
  theme_minimal()
#Este diagrama nos muestra con cajas la distribucion de cada clase.











































