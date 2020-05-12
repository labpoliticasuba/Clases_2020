
#Hoy vamos a conocer un poco que es el Data Vizualization#

#Fundamentalmente podemos entender el Data Vizualization -o DataViz-#

#Como la o las herramientas con las que vamos a contar para transmitir#

#la información que hemos procesado.#



#De nada nos sirve tener un código hermosamente escrito y funcional sino#

#podemos transmitir nada de esa información de manera clara y sencilla,#

#pero a la vez de forma explicativa#



#Hoy vamos a ver algunos elementos fundamentales para iniciarnos en DataViz#



#Para trabajar hoy vamos a utilizar un df sencillo#

#Elegimos este df porque tiene información de 800 pokemones a lo largo de seis generaciones

#con la mayoría de sus estadisticas#



#Como siempre antes de arrancar, instalamos y cargamos la librerias#



install.packages("dplyr")

install.packages("tidyverse")





library(dplyr)

library(tidyverse)

#Y cargamos nuestro df#



pokemon <- read.csv("Pokemon.csv")



#si quieren buscar otras bases de datos dense una vuelta por Kaggle que es un#

#repositorio enorme. Tiene df serios y no tan serios que pueden serles útiles#

#para prácticar o investigar#



#Volviendo a la clase, veamos un poco cuales son las columnas con las que contamos#



head(pokemon)



#Vemos que contamos con el nombre, primer y segundo tipo, puntos de vida y otros datos#

#como ataque, defensa, velocidad, etc..."



#Nosotros vamos a ponernos en plan entrenador pokemon y vamos a querer ver#

#Cual de los tres tipos basicos -agua, cesped, fuego- nos conviene para iniciar nuestra#

#aventura.#



data_basicos <- pokemon%>%
  
  filter(Type.1 =="Grass"| Type.1 == "Fire"| Type.1 == "Water")%>%
  
  distinct(X., .keep_all = TRUE) #esta linea la agregamos para eliminar pokemon con #
#numero repetido#



#Con esta función lo que hicimos fue crear un df solo con los pokemon de los tipos#

#que nos interesan#



#siempre me gustaron los pokemon tipo agua, y como me quede en el tiempo solo quiero# 

#jugar con pokemon de la primera generacion#

#así que veamos cuales son los más indicados# 

#para iniciar la campaña#



data_agua <- data_basicos%>%
  
  filter(Type.1 == "Water" & Generation == "1")



#ahora hagamos un plot muy básico para ver la cantidad de puntos de vida y su ataque#



plot(data_agua$HP, data_agua$Attack, main = "Ataque y Total de puntos HP",
     
     xlab = "Total HP", ylab = "Ataque",
     
     pch = 19, frame = FALSE)



#Y nos quedo este gráfico! Eso sería todo data viz, verdad?#

#Claramente no. Este grÃ¡fico a pesar de mostrarnos información#

#No lo hace de manera ordenada y mucho menos clara#



#esto ocurre porque usamos un comando que no es tan poderoso como nosotros precisamos#

#de hecho, es de los comandos que vienen por default con R#



#para poder visualizar de manera clara y correcta vamos a usar#

#ggplot2 



install.packages("ggplot2")

library(ggplot2)






ggplot(data = data_agua) + 
  
  geom_point(mapping = aes(x = data_agua$HP, y = data_agua$Attack)) 

#De piso ya tenemos un gráfico mucho más bonito#

#Veamos que más podemos hacer#



#Antes de seguir hay que hacer una salvedad#

#todos recordamos el "%>%" que usamos como conector lógico entre funciones?#

#ggplot2 tiene su propio conector lógico y es el signo "+"#

#Vamos a usarlo bastante de acá en más#



ggplot(data = data_agua, aes(label = "Name")) + 
  
  geom_point(mapping = aes(x = data_agua$HP, y = data_agua$Attack))+
  
  geom_text(aes(x = data_agua$HP, y = data_agua$Attack, label=data_agua$Name), size =4)+
  
  ggtitle("Scatterplot sobre el ataque y puntos de vida") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque")



#Hasta acÃ¡ tenemos nuestro grÃ¡fico, pero esta todo muy amontonado#

#veamos que podemos hacer#



ggplot(data = data_agua, aes(label = "Name")) + 
  
  geom_point(mapping = aes(x = data_agua$HP, y = data_agua$Attack))+
  
  geom_text(aes(x = data_agua$HP, y = data_agua$Attack, label=data_agua$Name), size =4, position = position_nudge(x = -5, y=5))+
  
  ggtitle("Scatterplot sobre el ataque y puntos de vida") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque")





#mejora! Pero podemos hacer mas!# 

#Agreguemos color para que se distingan los objetos

ggplot(data = data_agua, aes(label = "Name")) + 
  
  geom_point(mapping = aes(x = data_agua$HP, y = data_agua$Attack, color = data_agua$X.))+
  
  geom_text(aes(x = data_agua$HP, y = data_agua$Attack, color= data_agua$X., label=data_agua$Name), size =4, position = position_nudge(y=5))+
  
  ggtitle("Scatterplot sobre el ataque y puntos de vida") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque")



#Ya agregamos color y modificamos el lugar de las etiquetas#

#¿Podemos cambiar el tamaño? Obvio!#





ggplot(data = data_agua, aes(label = "Name")) + 
  
  geom_point(mapping = aes(x = data_agua$HP, y = data_agua$Attack, color = data_agua$X., size = data_agua$X.))+
  
  geom_text(aes(x = data_agua$HP, y = data_agua$Attack, color= data_agua$X., label=data_agua$Name), size =4, position = position_nudge(y=5))+
  
  ggtitle("Scatterplot sobre el ataque y puntos de vida") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque")



#TambiÃ©n podemos cambiarle la forma a nuestros puntos#
#Ojo que acá volvemos a usar data_básicos#

p <- data_basicos %>% filter(Generation == 1) %>%
  
  ggplot() +
  
  geom_point(mapping = aes(x = HP, y = Attack, shape = Type.1, color = Type.1, size= 2.1)) +
  
  geom_text(aes(x = HP, y = Attack, label=Name), size =4, position = position_nudge(y=5))+
  
  theme_minimal() +
  
  theme(legend.position = "top") +
  
  ggtitle("Scatterplot sobre los puntos de vida y los de ataque de los pokemons basicos de la GEN 1") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque") 

p



#Pero nos quedo todo muy pegado#

#Usamos un poco de ayuda#

install.packages("ggrepel")

library(ggrepel)



p <- data_basicos %>% filter(Generation == 1) %>%
  
  ggplot() +
  
  geom_point(mapping = aes(x = HP, y = Attack, shape = Type.1, color = Type.1, size= 2.1)) +
  
  geom_text(aes(x = HP, y = Attack, label=Name), size =4, position = position_nudge(y=5))+
  
  theme_minimal() +
  
  geom_text_repel(data = data_basicos, mapping = aes(x= HP, y= Attack, label=Name))

theme(legend.position = "top") +
  
  ggtitle("Scatterplot sobre los puntos de vida y los de ataque de los pokemons basicos de la GEN 1") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque") 

p



##¿Que hermoso no?##

##Como la idea es solo mostrar un poco las capacidades de ggplot, no nos distraigamos en esto##

##Hagamos el gráfico que queremos pero limitemos la cantidad de pokemones a 25 para##

##que sea más facil graficarlo##



a <- data_basicos %>% filter(Generation == "1" & X. <= "25")

b <- a %>%
  
  ggplot() +
  
  geom_point(mapping = aes(x = HP, y = Attack, shape = Type.1, color = Type.1, size= 2.1)) +
  
  geom_text(aes(x = a$HP, y = a$Attack, label=a$Name), size =4, position = position_nudge(y=5))+
  
  theme_minimal() +
  
  theme(legend.position = "top") +
  
  ggtitle("Scatterplot sobre los puntos de vida y los de ataque de los pokemons bÃ¡sicos de la GEN 1") +
  
  xlab("Puntos de Vida") + ylab("Puntos de Ataque") 

b

###############################


#Graficos de barras. Vamos a mostrar ejemplos basicos, sin colores ni grandes dificultadres.

#Al final vamos a mostrar un ejemplo mucho más completo de lo que se puede llegar a hacer.

# Agrupamos por tipo y cantidad. Y graficamos cuantos pokemones de cada tipo hay



c <- pokemon %>%
  
  group_by(Type.1) %>%
  
  count() %>%
  
  ggplot(aes(x=reorder (Type.1, n), y=n)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip()+
  
  ylab("Cantidad")+
  
  xlab("Clase")+
  
  ggtitle("Pokemon por clase")

c   #si no lo vemos, ponemos el nombre que le dimos al gráfico y le damos run



# Ahora agrupamos por tipo y calculamos el total de puntos de ataque de cada uno



d <- pokemon %>%
  
  group_by(Type.1) %>%
  
  summarise(Ataque =sum(Attack)) %>%
  
  ggplot(aes(x=Type.1, y=Ataque)) + 
  
  geom_bar(stat = "identity") 



d



# Ahora hacemos lo mismo, pero con los puntos de defensa



e <- pokemon %>%
  
  group_by(Type.1) %>%
  
  summarise(Defensa =sum(Defense)) %>%
  
  ggplot(aes(x=Type.1, y=Defensa)) +
  
  geom_bar(stat = "identity")

e  



# Pero ¿Son correctos los datos arrojados por en esos graficos? No, por que 

# hay más pokemones de agua que de otra cosa, entonces los puntos totales van a ser mas altos

# Que tenemos que hacer entonces? sacar un promedio de los puntos y la cantidad de pokemones



f <- pokemon %>%
  
  group_by(Type.1) %>%
  
  summarise(Promedio_Ataque = mean(Attack)) %>%
  
  ggplot(aes(x= Type.1, y = Promedio_Ataque)) + 
  
  geom_bar(stat = "identity")





f



# Ahora vemos que los de fuego son los que en promedio, tiene más puntos de ataque



# Y si hacemos un promedio de los puntos totales para saber cuales son los mejores pokemones en terminos estadisticos



g <- pokemon %>%
  
  group_by(Type.1) %>%
  
  summarise(Promedio_Total = mean(Total)) %>%
  
  ggplot(aes(x= Type.1, y = Promedio_Total)) + 
  
  geom_bar(stat = "identity")



g

# Como vemos, los de fuego son los más completos.



# Ahora, instalen la siguiente aplicacion

install.packages("plotly")

library(plotly)

# Esta aplicacion les va a permitr hacer que sus graficos se vuelvan interactivos.

# EJ:

ggplotly(b)

ggplotly(c)

ggplotly(d)

ggplotly(e)

ggplotly(f)

ggplotly(g)

# Por último, vamos a mostrar un grafico mucho más logrado,
#para que vean las posibilidades que brinda R.

# Ahora vamos a hacer un graficos con "facet_wrap" 
#teniendo en cuenta el promedio de los puntos totales.

# Comparando los tipos de pokemones pero no entre si, sino entre ellos mismos por N° de generacion



h <- pokemon %>% 
  
  group_by(Type.1, Generation) %>% 
  
  summarise(Promedio =mean(Total)) %>% 
  
  ggplot(aes(x=as.factor(Generation), y = Promedio, fill=as.factor(Generation) )) + 
  
  geom_bar(stat = "identity") +
  
  scale_fill_hue(c = 40) +
  
  theme(legend.position="none") +
  
  facet_wrap(~Type.1)



h



# Por último, para comprender los alcances de R. Si usamos "scale_fill_viridis" nos transforma

# Los colores de nuestro graficos para que puedan ser visualizados por personas con daltonismo



h + scale_fill_viridis_d()
