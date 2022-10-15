### Inicio 

rm(list=ls())
getwd() ## Directorio inicial
setwd("C:/Users/IgnacioSepulveda/Documents/Ayudantias/econometrics") ## Directorio nuevo
getwd() ## Check


### Librerias

## install.package("tidyverse") si no lo tengo instalado, saco #, y lo corro
## install.packages("stargazer")
## install.packages("haven")
library("tidyverse")
library("haven")
library("stargazer")

####### 1 ##############

df=read_dta('CEOSAL1.dta')
str(df)

##### a #######
reg_1a=lm(lsalary~lsales+finance+consprod+utility,data=df)


##### b #######
reg_1a %>% summary()

##### c #######
reg_1c=lm(lsalary~lsales+indus+consprod+utility,data=df)
reg_1c %>% summary()

####### 3 ##############

rm(list=ls()) ## Removemos todo lo anterior
df=read_dta('gpa2.dta')
str(df)

##### a #######
reg_3a=lm(colgpa~hsize+hsperc+sat+female+athlete,data=df)
reg_3a
reg_3a %>% summary() 

##### c #######
nueva_data=data.frame(hsize=1,hsperc=10,sat=800,female=1,athlete=0)
paste("predecimos....")
reg_3a %>% predict(newdata=nueva_data) 

##### d #######
cor(df$sat,df$athlete)
reg_3d=lm(colgpa~hsize+hsperc+female+athlete,data=df)
stargazer(reg_3a,reg_3d,type='latex',header=FALSE)

