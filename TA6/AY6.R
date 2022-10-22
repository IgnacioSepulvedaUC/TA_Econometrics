# Respuestas
library('tidyverse')
library(haven)
# Pregunta 1

## a) Pizarra

## b)
datos=read_dta("WAGE2 (1).DTA")
datos[['pareduc']]=datos$meduc+datos$feduc
modelo_b=lm(lwage~educ+pareduc+pareduc*educ+exper+tenure,data=datos)
modelo_b %>% summary()

## c)


## d)
## Como b4-b5=0, si (b4-b5)=v entonces
## v+b5=b4
datos[['tenure_plus_exper']]=datos$tenure+datos$exper
modelo_d=lm(lwage~educ+pareduc+pareduc*educ+exper+tenure_plus_exper,data=datos)
modelo_d %>% summary()
se=0.005643
# calculamos...
b4=0.019557
b5=0.010308
t_calculado=(b5-b4)/se


## El tc es 1.96 no podemos rechazar la nula, por lo tanto nuestra exper y tenure estadisticamente iguales.

# Pregunta 2

# respuestas pizarra

# Pregunta 3

## a)

datos=read_dta("gpa2.dta")
View(datos)
modelo_3_a=lm(colgpa~hsize+hsizesq+hsperc+sat+female+athlete+female*athlete,data=datos)
modelo_3_a

## b)
modelo_3_a %>% summary()



