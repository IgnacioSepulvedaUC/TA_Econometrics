
################################################################################
# Estudio sobre los precio de las viviendas y sus determinantes.               #                                                           
################################################################################

##################################################################################################
# OBJETIVOS:                                                                                     #
# 1) Establecer hipotesis sobre la magnitud y el sentido de la relación con los determinantes.   #
# 2) Realizar analisis exploratorio                                                              #
# 3) Realizar regresion para estudiar como se relacionan el precio y sus determinantes           #
# 4) Estudiar la significancia estadisticas de los determinantes del precio de las viviendas     #
# 5) Estudiar la significancia economica de los determinantes del precio de las viviendas        #
# 6) Concluir                                                                                    #
##################################################################################################

######## Paso 1: Nos aseguramos de que se encuentre vacio el entorno ###########
rm(list=ls())

########## Paso 2: Instalamos los paquetes que necesitamos ##########
## lo corremos solo 1 vez, despues basta con llamar a las librerias.
#install.packages(c('tidyverse','stargazer'))


######### Paso 3: Cargamos las librerias en nuestro entorno ###########
library(tidyverse) ## Manipular y graficas datos
library(stargazer) ## La ocupamos para hacer table bacanes

## Paso 4: Cargamos los datos
url='https://raw.githubusercontent.com/IgnacioSepulvedaUC/TA_Econometrics/main/TA3/Viviendas2.csv'
df=read.csv(url,sep = ';')


######### Paso 5: Analisis Exploratorio ############


### Paso 5.1 Cuáles son lo tipos de datos que estan en el data.frame? 
###         Cómo corregimos las variables categoricas?

str(df) 

##### Debemos corregir la Comuna y Estacionamiento para que sean factores
##### eso permitira trabajar con el tipo de variable correcto

table(df['Estacionamiento']) ## Revisamos cuantos deptos tienen estacionamiento

df['Estacionamiento']=df$Estacionamiento %>% 
  factor(levels=c(0,1),
         labels=c('No tiene Estacionamiento',
                  'Tiene Estacionamiento'))

table(df['Estacionamiento'])


##### Cuantas comunas hay?
df$Comuna %>% unique()


##### No son muchas las convertimos en factores
table(df$Comuna)
df['Comuna']=df$Comuna %>% 
  factor() ## No es necesario especificar las labels ya que ya vienen con nombre,

##### Lo mismo ocurre con la cantidad de baños y dormitorios, son pocas variables mejor tratarlas como categoricas.

table(df$Banos)
df['Banos']=df$Banos %>% 
  factor(levels=c(1,2,3),
         labels=c('1-Bano','2-Bano','3-Bano')) ## No es necesario especificar las labels ya que ya vienen con nombre,
table(df$Banos)

table(df$Dormitorios)
df['Dormitorios']=df$Dormitorios %>% 
  factor(levels=c(1,2,3,4),
         labels=c('1-Dorm','2-Dorm','3-Dorm','4-Dorm')) ## No es necesario especificar las labels ya que ya vienen con nombre,
table(df$Dormitorios)


#### Finalmente area y precio son numeric

df['precio']=as.numeric(df$precio)
df['area']=as.numeric(df$area)

######### HIPOTESIS ##################
#Variable: Signo / Magnitud
# Area:                              #
# Dormitorios:                       #
# Banos:                             #
# Estacionamiento:                   #
# Comuna:                            #
######################################



### Paso 5.2: Estadistica Descriptiva de los datos 
df %>% summary() 


### Paso 5.3: Distribución de nuestra varible de interes
hist(df$precio)

###¿Se parece a una normal?


#### Paso 5.4 Como se relaciona el precio con las otras variables un apoyo visual.

# Area
plot(x=df$area,
     y=df$precio,
     main='Precio~Area',
     ylab='Precio',
     xlab='Area') ## Relación parece ser positiva

# Dormitorios
boxplot(data=df,
        precio~Dormitorios,
     main='Precio~Dormitorios',
     ylab='Precio',
     xlab='Dormitorios') ## No parece haber relación lineal

# Banos
boxplot(data=df,
        precio~Banos,
     main='Precio~Banos',
     ylab='Precio',
     xlab='Banos') ## No parece haber relación lineal

# Estacionamiento
boxplot(data=df,
        precio~Estacionamiento,
        main='Precio~Estacionamiento',
        ylab='Precio') ## No parece haber diferencia si tiene
                       ## O no tiene estacinamiento
# Comuna
boxplot(data=df,
        precio~Comuna,
        main='Precio~Comuna',
        ylab='Precio') ## Parece existir difencia en el precio promedio por comuna



######### Paso 6: REGRESIONES ############


#### Paso 6.1: Regresion simple: Analisis Exploratorio

modelo_Area=lm(precio~area,data=df)
modelo_Comuna=lm(precio~Comuna,data=df)
modelo_Dormitorios=lm(precio~Dormitorios,data=df)
modelo_Estacionamiento=lm(precio~Estacionamiento,data=df)
modelo_Banos=lm(precio~Banos,data=df)

## Variable continua

# Area
plot(x=df$area,
     y=df$precio,
     main='Precio~Area',
     xlab='Area',
     ylab='Precio')
abline(modelo_Area,col='red')
## La regresión reafirma que la relación es positiva

## Varibles discretas 

# Estacionamiento
boxplot(data=df,
        precio~Estacionamiento,
        main='Precio~Estacionamiento',
        ylab='Precio') #
abline(modelo_Estacionamiento,col='red')

## Con estacionamiento tiene ligeramente un precio promedio mayor

## ¿Qué nos esta mostrando el beta de una variable dummy?
modelo_Estacionamiento

## Qué pasa si resto el precio promedio de los que tienen menos los que no tiene estacionamiento.


## Calculamos el precio promedio para los con estacionamiento
Media_con_Estacionamiento=df %>% 
  filter(Estacionamiento=='Tiene Estacionamiento') %>% 
  summarise(precio_medio=mean(precio))

## Calculamos el precio primedio para los sin estacionamiento
Media_sin_Estacionamiento=df %>% 
  filter(Estacionamiento=='No tiene Estacionamiento') %>% 
  summarise(precio_medio=mean(precio))

print(Media_con_Estacionamiento$precio_medio-Media_sin_Estacionamiento$precio_medio)
modelo_Estacionamiento

## El mismo analisis se puede hacer para las otras variables categoricas, pero qué pasa cuando son mas de 2 como
## las comunas?

modelo_Comuna

## Notemos que falta una comuna, entonces todos los betas nos muestra las diferencias en el promedio con respecto
## a esa comuna, la llamamos la comuna base

## Media precio comuna base
Media_Comuna_LP=df %>% 
  filter(Comuna=='La Pintana') %>% 
  summarise(precio_medio=mean(precio))

## Media precio comuna no base
Media_Comuna_SM=df %>% 
  filter(Comuna=='San Miguel') %>% 
  summarise(precio_medio=mean(precio))

## Difencias en el promedio
print(Media_Comuna_SM$precio_medio-Media_Comuna_LP$precio_medio)
modelo_Comuna

## Lo mismo es valido para las otras comunas.

#### Paso 6.2: Regresion multiple
modelo_todas_variables=lm(precio~.,data=df) # ~. significa que ocupare todos los datos en df menos el precio

modelo_Dormitorios


#### Paso 6.3: Comparación de modelos
stargazer(modelo_Area,
          modelo_Comuna,
          modelo_Estacionamiento,
          modelo_todas_variables,
          type='text')

#### Conclusión: Elaborar entre todos.
stargazer(modelo_todas_variables,type='text')



