## Ayudantía N°6
## UCSH

rm(list=ls()) ## Removera todo lo no deseado

## Aquí encontrara el codigo de la ayudantía del día de hoy, si coloca un # antes de un frase podra escribir sus
## comentarios, por ejemplo:

lista=c(1,2,3) ## Esto es una lista.

## lo ideal es ir haciendo esto con cada linea que no tengamos claro que hace...

## Tambien configura de forma correcta tu directorio para que puedas llamar a tus bases.

## Opcion 1: 

# Obtiene tu directorio y coloca las bases de datos en esa carpeta.
getwd()
 
## Opcion 2:
 
# Escoge tu cual es el directorio a ocupar, para ocupar sacar #
#directorio='C:/Users....' 
#setwd(directorio)

## Anota todo lo que puedas, para que después sea mas facil recordar, 
## a continuación el desarrollo de la ayudantía. 


## Cargamos la librerias generales

library(tidyverse)
library(haven)
library(readr)


########### RESPUESTA 1 ####################################################################

df=read_dta('BWGHT.DTA')
df %>% head() 

modelo=lm(lbwght~cigs+lfaminc+parity+male,data=df) ## Que hace esta linea?
modelo

##a)

## Conteste a partir del output de la variable modelo

## b)

cambio_cigs=10 
beta_cigs=modelo$coefficients[2]
cambio_lbwght=beta_cigs*cambio_cigs
paste0('El efecto de fumar 10 cigarrillos mas al dia es una caída estimada de un :',
       round(cambio_lbwght*100,2),
       '% en el peso del bebe')

## c)

#Forma 1
modelo_detalle=modelo %>% summary()
modelo_detalle


#Forma 2
t_calculado_male=modelo_detalle$coefficients[5,3] 
alpha=0.05
total_muestra=dim(df)[1]
total_beta=length(modelo$coefficients)
t_critico_male=qt(p = 1-alpha/2,
                  df = total_muestra-total_beta)
tvalue=paste('t calculado',
             round(t_calculado_male,2),
             ' y t critico',
             round(t_critico_male,2))

if(t_calculado_male>t_critico_male){ 
  paste('Rechazamos, la diferencia es significativa.',tvalue) 
} else{ 
  paste('No rechazamos que sea igual a 0.',tvalue) 
}

########### RESPUESTA 3 ####################################################################

df=read_delim('Viviendas2.csv',
              delim = ";",
              show_col_types = FALSE)

modelo=lm(precio~area+Dormitorios+Estacionamiento,data=df)
modelo

## a)

area=60 
dorm=2 
est=0 
df_predict=data.frame('area'=area,'Dormitorios'=dorm,'Estacionamiento'=est) 
valor_predicho=modelo %>% predict(newdata=df_predict) 
paste("El valor predicho promedio de una vivienda con esas caracteristicas es de",
      round(valor_predicho,2),
      'UF')


## b)

modelo_detalle=modelo %>% summary()
t_calculado_est=modelo_detalle$coefficients[4,3] 
alpha=0.05
total_muestra=dim(df)[1]
total_beta=length(modelo$coefficients)
t_critico_est=qt(p = 1-alpha,
                 df = total_muestra-total_beta)
tvalue=paste('t calculado',
             round(t_calculado_est,2),
             ' y t critico',
             round(t_critico_est,2))

if(t_calculado_est>t_critico_est){
  paste('Rechazamos, la diferencia es significativa.',tvalue) 
} else{ 
  paste('No rechazamos que sea igual a 0 ',tvalue) 
}