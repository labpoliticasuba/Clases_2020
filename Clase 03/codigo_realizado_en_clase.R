## cargamos nuestros paquetes
library(tidyverse)

## carguemos el datasets de clase 1 
data_clase_1 <- read.csv("https://raw.githubusercontent.com/labpoliticasuba/Clases_2020/master/Clase%2003/data_clase_1.csv",
                         encoding = "Latin1")


colnames(data_clase_1)
## seleccionamos las columnas que necesitamos:
data_clase_1 <- data_clase_1 %>% select(-X., -Total)

## utilizamos gather para transformar en filas nuestras columnas

data_clase_gather <- gather(data_clase_1, tipo_cama, value, Generales:No.discriminadas)
head(data_clase_1)
tail(data_clase_gather)

## con spread las devolvemos a su estado original 
data_clase_spread <- spread(data_clase_gather, tipo_cama, value)
tail(data_clase_spread)

## con unique podemos ver los valores únicos necesarios para transformar el nombre 
## de nuestras observaciones
unique(data_clase_gather$tipo_cama)

## aplicamos case_when() dentro del mutate() cuando queremos modificar los valores/filas
## dentro de nuestro dataset
colnames(data_clase_gather)

data_clase_1_gather <- data_clase_gather %>% mutate(tipo_cama = case_when(
  tipo_cama == "Generales" ~ 'Aislamiento y casos leves',
  tipo_cama == "Pediátricas" ~ 'Aislamiento y casos leves',
  tipo_cama == "Maternidad" ~ 'Aislamiento y casos leves',
  tipo_cama == "Htal.día" ~ 'Aislamiento y casos leves',
  tipo_cama == "No.discriminadas" ~ 'Aislamiento y casos leves',
  tipo_cama == "C.Especiales" ~ 'Casos graves',
  tipo_cama == "UTI.adultos" ~ 'Casos graves',
  tipo_cama == "UTI.pediátricas" ~ 'Casos graves',
  tipo_cama == "Neonatología" ~ 'Casos graves Neo',
  tipo_cama == "Int..Prolong." ~ 'Casos graves'))



## transformamos las observaciones/filas de nuestra columna "value" para que sean numericos 
## (sino lo hacemos se complica el summarise)
unique(data_clase_1_gather$Distrito)

data_clase_1_gather$Distrito <- as.character(data_clase_1_gather$Distrito)

data_clase_1_gather$value  <- as.numeric(as.character(data_clase_1_gather$value))

class(data_clase_1_gather$value)

## generemos un dataset nuevo agrupando nuestras variables, con una suma de las mismas
data_clase_groupby <- data_clase_1_gather %>% 
                  filter(Distrito != "TOTAL") %>%
                  group_by(Distrito, tipo_cama) %>% distinct() %>%
                  summarise(cantidad_camas = sum(value)) %>% 
                  drop_na()

## conozcamos el nombre de nuestras columnas del dataset
colnames(data_clase_groupby)

## cambiemos el nombre de nuestras columnas... que esten en MAYUSCULA!
data_clase1 <- data_clase_groupby %>% rename(
  DISTRITO = Distrito,
  TIPO_CAMA = tipo_cama,
  CANTIDAD_CAMAS =cantidad_camas
)
head(data_clase1)

## traigamos nuestro dataset clase 2
data_clase_2 <- read.csv("https://raw.githubusercontent.com/labpoliticasuba/Clases_2020/master/Clase%2003/data_clase_2.csv", 
         encoding = "Latin1")
head(data_clase_2)
## realicemos un join para poder unir las tablas gracias a la columna distrito (que es la que tienen en común)!
data_clase <- left_join(data_clase1, data_clase_2, by = "DISTRITO")

## acomodemos las columnas en el orden que estaban la clase anterior:
data_clase <- data_clase %>% 
  select(REGION, DISTRITO, POBLACION, TIPO_CAMA, CANTIDAD_CAMAS, CANTIDAD_MEDICOS)

## si queremos guardar nuestro dataset:
write.csv(data_clase, "D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/vf/clase_2804.csv", 
                fileEncoding = "UTF-8")
