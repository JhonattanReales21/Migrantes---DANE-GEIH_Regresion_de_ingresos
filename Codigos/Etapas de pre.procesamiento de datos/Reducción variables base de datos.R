library(funModeling)
library(tidyverse)
library(readxl)
library(readr)

# PARA 2019

#Se leen los datos 
Dane_2019 <- read.csv("Dane2019-completa.csv", header = T, sep = ",", dec = ".")

#SOLO PERSONAS MAYORES DE 12 AÑOS
Dane_2019 <- filter(Dane_2019, P6040 >=12)
Dane_2019 <- filter(Dane_2019, !(is.na(P6040)))
sum(is.na(Dane_2019$P6040))
#filtrar TODOS LOS INACTIVOS
Dane_2019 <- filter(Dane_2019, !(!(is.na(P7430) )))

#nos quedamos con 18,161 personas.

###### REALIZAR LOS CAMBIOS PERTINENTES A LA BASE DE DATOS (escoger variables generales y renombrar) ####
Dane_2019 <- mutate(Dane_2019, "Familia" = paste(Dane_2019$ï..DIRECTORIO, "-", Dane_2019$SECUENCIA_P))
Dane_2019 <- dplyr::select(Dane_2019, -1)
Dane_2019 <- dplyr::select(Dane_2019, 1, Familia, everything())
Dane_2019 <- dplyr::select(Dane_2019, id_personal, Familia, DPTO.x, AREA.x, MES.x, P6020, P6040, P6050, P6070, P6090, P6110, P6160, P6170, P6210, P6220, ESC,
                           P6240, P6280, P6290, P6310, P6260, P6320, P6340, P6350, P6430, P6440, P6450, P6460, P6424S3, P6426, P6480, P9440, P6500,
                           P6510, P6510S1, P6510S2, P6630S1, P6630S1A1, P6765, P6772, P6775, P6750, P6780, P1800, P1800S1, P1802, P1879, P6800, P6870,
                           P6880, P6920, P9450, P7020, P760, P7026, P7028, P7100,P7120, P7240, 
                           OFICIO, INGLABO, P7250, P7280, P7310, P7350, P7360 ,P7260, OFICIO2, P7430, P7458,P1884, P1807, P7495, P7505,
                           P1661S4, P1661S4A1, P1661S4A2,P753S3, P753, P756S3,P755, fex_c_2011.x, P6585S1A1, P6585S1A2, P6585S2A1, P6585S2A2,
                           P6585S3A1, P6585S3A2, P6585S4A1, P6585S4A2, P6545S1, P6545S2, P6580S1, P6580S2, P7070, P7422S1, P7510S3A1,RAMA2D, RAMA2D_D)


Dane_2019 <- dplyr::rename(Dane_2019, Identificacion=id_personal, Departamento=DPTO.x, Area=AREA.x, Mes=MES.x, "Anios.de.escolaridad" = ESC, "Genero"=P6020,
                           Edad=P6040, Parentesco=P6050, "Estado.civil" = P6070, "P6090 Cuenta con seguridad social en salud" = P6090)
Dane_2019 <- Dane_2019%>%dplyr::rename("P6110 Quien paga por la salud?" = P6110,  "P6160 Sabe leer y escribir?" = P6160, "P6170 Asiste a una entidad educativa?" =P6170,
                                       "Nivel.educativo.alcanzado" = P6210, "P6220 Titulo o diploma de mayor nivel" = P6220, "P6290 Que ha hecho para conseguir trabajo" = P6290, "P6280 Ha intentado conseguir trabajo en este mes?" = P6280,
                                       "P6240 Que fue lo que mas hizo la semana pasada?" = P6240, "P6310 Por que no hizo diligencias para trabajar?" = P6310, "P6260 Aunque no trabajó la semana pasada, tuvo ingresos?" = P6260,
                                       "P6320 El ultimo año ha trabajado por lo menos 2 semanas?" = P6320, "P6340 En el ultimo año ha buscado trabajado?" = P6340, "P6350 Cuantos meses lleva sin buscar trabajo?" = P6350, "P6440 Como -Asalariado-, cuenta con algún tipo de contrato?" = P6440, "P6500 Cuanto ganó el ultimo mes?" =P6500,
                                       "P6765 Que forma de trabajo -independiente- realizó?" =P6765, "P6750 Ganancia neta por honorarios o negocio" =P6750, "P1800 Tiene empleado o personas que lo ayuden?" =P1800, "P6800 Cuantas horas a la semana trabaja?" = P6800,
                                       "P6870 Numero de personas en la empresa o negocio donde labora" = P6870, "P6880 Donde realiza su trabajo?" =P6880, 
                                       "P6920 Cotiza a pension?" =P6920,"P7020 Tuvo otro trabajo antes del actual?" =P7020, "P760 Meses desempleado antes de conseguir trabajo?" =P760, "P7026 Tiempo en empleo anterior (meses)"=P7026,"P7028 En su empleo anterior usted era?"=P7028,
                                       "P7100 Horas adicionales que puede trabajar en la semana"=P7100,"P7120 Disponible para trabajar mas horas a la semana?"=P7120, "P7240 Si no tiene empleo de donde obtendria los recursos?"=P7240,
                                       "P6430 En este trabajo usted es:"=P6430, "Rama de actividad de la empresa donde labora(2digitos)"=RAMA2D, "Ingresos laborales"=INGLABO, "P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando"=P7250,
                                       "P7310 Buscando trabajo por primera vez?"=P7310, "P7350.En.su.ultimo.trabajo.era"=P7350, "P7260 Cuantas horas a la semana podria trabajar?"=P7260,"Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?"=RAMA2D_D, "P7430 Es -Inactivo- pero, ha trabajado alguna vez?"=P7430,
                                       "OFICIO codigo de su oficio actual"=OFICIO, "OFICIO2 Codigo del ultimo oficio que realizó"=OFICIO2, "P1884 Cuantas horas a la semana puede trabajar?"=P1884, "P1807 -inactivo-, Salario minimo que aceptaria"=P1807,
                                       "P7495 Recibio pagos por pensiones o arriendos?"=P7495, "P7505 Recibio dinero de otras persons o instituciones?"=P7505,"P1661S4 Recibio ayuda de entidades diferentes al gobierno?"=P1661S4, 
                                       "P1661S4A1 Cual entidad no gubernamental?"=P1661S4A1, "P1661S4A2 Monto recibido"=P1661S4A2, "P7360 Numero de personas en la empresa o negocio donde laboraba" = P7360, "P755 Donde vivia hace 5 años?"=P755,
                                       "P753 Donde vivia hace 12 meses?" = P753, "P753S3 En que pais vivia hace 12 meses?"=P753S3, "Factor.de.expansion"=fex_c_2011.x, "Cuanto recibio por horas extras"=P6510S1)


#CREAMOS LA VARIABLE OCUPACION ( 3 niveles )
Dane_2019$ocupacion[!(is.na(Dane_2019$P6765))] = "No asalariado"

Dane_2019$ocupacion[ Dane_2019$P6440 %in% (1:2) & (is.na(Dane_2019$P6765))] = "Asalariado"

Dane_2019$ocupacion[!(Dane_2019$P6440 %in% (1:2))] = "Desempleado"


describe(Dane_2019$ocupacion) # Aprox el 50% de la población mayor de 12 años es: No asalariada 


#CREAMOS LA VARIABLE OCUPACION ANTES - especial para los desempleados ( 2 niveles )
# Para los desempleados:

# 4. Trabajador por cuenta propia
# 5. Patrón o empleador
# 6. Trabajador familiar sin remuneracion
# 7. Trabajador para otros hogares o negocios sin remuneracion
describe(Dane_2019$P7350.En.su.ultimo.trabajo.era)
Dane_2019$ocupacion_antes[!(Dane_2019$P6440 %in% (1:2)) & (!(Dane_2019$P7350.En.su.ultimo.trabajo.era %in% (4:7)))] = "No asalariado"
Dane_2019$ocupacion_antes[!(Dane_2019$P6440 %in% (1:2)) & (!(Dane_2019$P7350.En.su.ultimo.trabajo.era %in% c(1,2,3,8,9)))] = "Asalariado"
Dane_2019$ocupacion_antes[is.na(Dane_2019$ocupacion_antes)] = "Ocupados"

describe(Dane_2019$ocupacion_antes) 
1692/(1692+1033) # Aprox el 62% de los desempleados el mes anterior fueron: No asalariadados


######  Construir variables nuevas, combinando preguntas de cada modulo  ####

# ha trabajado antes
Dane_2019$`P7310 Buscando trabajo por primera vez?`[Dane_2019$`P7310 Buscando trabajo por primera vez?` == 1] = 3
Dane_2019$`P7310 Buscando trabajo por primera vez?`[Dane_2019$`P7310 Buscando trabajo por primera vez?` == 2] = 1
Dane_2019$`P7310 Buscando trabajo por primera vez?`[Dane_2019$`P7310 Buscando trabajo por primera vez?` == 3] = 2
Dane_2019 <- dplyr::rename(Dane_2019, "P7310.Ha.trabajado.antes"=`P7310 Buscando trabajo por primera vez?`)
for (i in 1:nrow(Dane_2019)) {
  if (!(is.na(Dane_2019$`P6800 Cuantas horas a la semana trabaja?`[i])) == T) {
    Dane_2019$Empleo.antes[i] <- Dane_2019$`P7020 Tuvo otro trabajo antes del actual?`[i]
  }
  if (!(is.na(Dane_2019$`P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando`[i])) == T) {
    Dane_2019$Empleo.antes[i] <- Dane_2019$P7310.Ha.trabajado.antes[i]
  }
}


#Horas que puede trabajar en la semana o las que trabaja
Dane_2019$Horas.disponibles= 0

for (i in 1:nrow(Dane_2019)) {
  if (!(is.na(Dane_2019$`P7260 Cuantas horas a la semana podria trabajar?`[i])) == T) {
    Dane_2019$Horas.disponibles[i] <- Dane_2019$`P7260 Cuantas horas a la semana podria trabajar?`[i]
  }
  
  if (!(is.na(Dane_2019$`P6800 Cuantas horas a la semana trabaja?`[i])) == T) {
    Dane_2019$Horas.disponibles[i] <- Dane_2019$`P6800 Cuantas horas a la semana trabaja?`[i] 
  }
  if (!(is.na(Dane_2019$`P7100 Horas adicionales que puede trabajar en la semana`[i])) == T){
    Dane_2019$Horas.disponibles[i] <- Dane_2019$Horas.disponibles[i] + Dane_2019$`P7100 Horas adicionales que puede trabajar en la semana`[i]
  }
}

#Rama de la empresa donde trabaja o donde trabaj?
Dane_2019$Rama.actividad= 0

for (i in 1:nrow(Dane_2019)) {
  if (!(is.na(Dane_2019$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i])) == T) {
    Dane_2019$Rama.actividad[i] <- Dane_2019$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i]
  }
  if (!(is.na(Dane_2019$`Rama de actividad de la empresa donde labora(2digitos)`[i])) == T) {
    Dane_2019$Rama.actividad[i] <- Dane_2019$`Rama de actividad de la empresa donde labora(2digitos)`[i]
  }
}

#Variable tiempo en Colombia
Dane_2019$tiempo.en.colombia= 0

for (i in 1:nrow(Dane_2019)) {
  if(!(is.na(Dane_2019$`P753S3 En que pais vivia hace 12 meses?`[i])) == T){
    Dane_2019$tiempo.en.colombia[i] <- 1
  }
  if (Dane_2019$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
    Dane_2019$tiempo.en.colombia[i] <- 2
  }
  if(!(is.na(Dane_2019$P756S3[i])) == T & Dane_2019$`P755 Donde vivia hace 5 años?`[i] %in% (2:3) & Dane_2019$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
    Dane_2019$tiempo.en.colombia[i] <- 3
  }
  if (Dane_2019$tiempo.en.colombia[i]==0) {
    Dane_2019$tiempo.en.colombia[i] <- 1
  }
}

# Indicativo de año
Dane_2019$anio <- 2019



           ##  !!!! SE DECIDE SEPARAR EL PROCESO PARA CADA AÑO PORQUE EXISTEN !!!! ##
    ##  !!!! VARIABLES DIFERENTES ENTRE UN AÑO Y EL OTRO. ENTONCES PARA EVITAR ERRORES ##  !!!!



# PARA 2020

#Se leen los datos 
Dane_2020 <- read.csv("Dane2020_completa_(Hasta febrero).csv", header = T, sep = ",", dec = ".")

#SOLO PERSONAS MAYORES DE 12 AÑOS
Dane_2020 <- filter(Dane_2020, P6040 >=12)
Dane_2020 <- filter(Dane_2020, !(is.na(P6040)))
sum(is.na(Dane_2020$P6040))
#filtrar TODOS LOS INACTIVOS
Dane_2020 <- filter(Dane_2020, !(!(is.na(P7430) )))

#nos quedamos con 18,161 personas.

###### REALIZAR LOS CAMBIOS PERTINENTES A LA BASE DE DATOS (escoger variables generales y renombrar) ####
Dane_2020 <- mutate(Dane_2020, "Familia" = paste(Dane_2020$ï..DIRECTORIO, "-", Dane_2020$SECUENCIA_P))
Dane_2020 <- dplyr::select(Dane_2020, -1) 
Dane_2020 <- dplyr::select(Dane_2020, 1, Familia, everything())
Dane_2020 <- dplyr::select(Dane_2020, id_personal, Familia, DPTO.x, AREA.x, MES.x, P6020, P6040, P6050, P6070, P6090, P6110, P6160, P6170, P6210, P6220, ESC,
                           P6240, P6280, P6290, P6310, P6260, P6320, P6340, P6350, P6430, P6440, P6450, P6460, P6424S3, P6426, P6480, P9440, P6500,
                           P6510, P6510S1, P6510S2, P6630S1, P6630S1A1, P6765, P6772, P6775, P6750, P6780, P1800, P1800S1, P1802, P1879, P6800, P6870,
                           P6880, P6920, P9450, P7020, P760, P7026, P7028, P7100,P7120, P7240, 
                           OFICIO, INGLABO, P7250, P7280, P7310, P7350, P7360 ,P7260, OFICIO2, P7430, P7458,P1884, P1807, P7495, P7505,
                           P1661S4, P1661S4A1, P1661S4A2,P753S3, P753, P756S3,P755, fex_c_2011.x, P6585S1A1, P6585S1A2, P6585S2A1, P6585S2A2,
                           P6585S3A1, P6585S3A2, P6585S4A1, P6585S4A2, P6545S1, P6545S2, P6580S1, P6580S2, P7070, P7422S1, P7510S3A1, RAMA2D_R4, RAMA2D_D_R4)

Dane_2020 <- dplyr::rename(Dane_2020, Identificacion=id_personal, Departamento=DPTO.x, Area=AREA.x, Mes=MES.x, "Anios.de.escolaridad" = ESC, "Genero"=P6020,
                           Edad=P6040, Parentesco=P6050, "Estado.civil" = P6070, "P6090 Cuenta con seguridad social en salud" = P6090)
Dane_2020 <- Dane_2020%>%dplyr::rename("P6110 Quien paga por la salud?" = P6110,  "P6160 Sabe leer y escribir?" = P6160, "P6170 Asiste a una entidad educativa?" =P6170,
                                       "Nivel.educativo.alcanzado" = P6210, "P6220 Titulo o diploma de mayor nivel" = P6220, "P6290 Que ha hecho para conseguir trabajo" = P6290, "P6280 Ha intentado conseguir trabajo en este mes?" = P6280,
                                       "P6240 Que fue lo que mas hizo la semana pasada?" = P6240, "P6310 Por que no hizo diligencias para trabajar?" = P6310, "P6260 Aunque no trabajó la semana pasada, tuvo ingresos?" = P6260,
                                       "P6320 El ultimo año ha trabajado por lo menos 2 semanas?" = P6320, "P6340 En el ultimo año ha buscado trabajado?" = P6340, "P6350 Cuantos meses lleva sin buscar trabajo?" = P6350, "P6440 Como -Asalariado-, cuenta con algún tipo de contrato?" = P6440, "P6500 Cuanto ganó el ultimo mes?" =P6500,
                                       "P6765 Que forma de trabajo -independiente- realizó?" =P6765, "P6750 Ganancia neta por honorarios o negocio" =P6750, "P1800 Tiene empleado o personas que lo ayuden?" =P1800, "P6800 Cuantas horas a la semana trabaja?" = P6800,
                                       "P6870 Numero de personas en la empresa o negocio donde labora" = P6870, "P6880 Donde realiza su trabajo?" =P6880, 
                                       "P6920 Cotiza a pension?" =P6920,
                                       "P7020 Tuvo otro trabajo antes del actual?" =P7020, "P760 Meses desempleado antes de conseguir trabajo?" =P760, "P7026 Tiempo en empleo anterior (meses)"=P7026,"P7028 En su empleo anterior usted era?"=P7028,
                                       "P7100 Horas adicionales que puede trabajar en la semana"=P7100, 
                                       "P7120 Disponible para trabajar mas horas a la semana?"=P7120,"P7240 Si no tiene empleo de donde obtendria los recursos?"=P7240,
                                       "P6430 En este trabajo usted es:"=P6430, "Rama de actividad de la empresa donde labora(2digitos)"=RAMA2D_R4, "Ingresos laborales"=INGLABO, "P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando"=P7250,
                                       "P7310 Buscando trabajo por primera vez?"=P7310, "P7350.En.su.ultimo.trabajo.era"=P7350, "P7260 Cuantas horas a la semana podria trabajar?"=P7260,"Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?"=RAMA2D_D_R4, "P7430 Es -Inactivo- pero, ha trabajado alguna vez?"=P7430, 
                                       "OFICIO codigo de su oficio actual"=OFICIO, "OFICIO2 Codigo del ultimo oficio que realizó"=OFICIO2, "P1884 Cuantas horas a la semana puede trabajar?"=P1884, "P1807 -inactivo-, Salario minimo que aceptaria"=P1807,
                                       "P7495 Recibio pagos por pensiones o arriendos?"=P7495, "P7505 Recibio dinero de otras persons o instituciones?"=P7505,"P1661S4 Recibio ayuda de entidades diferentes al gobierno?"=P1661S4, 
                                       "P1661S4A1 Cual entidad no gubernamental?"=P1661S4A1, "P1661S4A2 Monto recibido"=P1661S4A2, "P7360 Numero de personas en la empresa o negocio donde laboraba" = P7360, "P755 Donde vivia hace 5 años?"=P755,
                                       "P753 Donde vivia hace 12 meses?" = P753, "P753S3 En que pais vivia hace 12 meses?"=P753S3, "Factor.de.expansion"=fex_c_2011.x, "Cuanto recibio por horas extras"=P6510S1)


#CREAMOS LA VARIABLE OCUPACION ( 3 niveles )
Dane_2020$ocupacion[!(is.na(Dane_2020$P6765))] = "No asalariado"

Dane_2020$ocupacion[ Dane_2020$P6440 %in% (1:2) & (is.na(Dane_2020$P6765))] = "Asalariado"

Dane_2020$ocupacion[!(Dane_2020$P6440 %in% (1:2))] = "Desempleado"


describe(Dane_2020$ocupacion) # Aprox el 48% de la población mayor de 12 años es: No asalariada 


#CREAMOS LA VARIABLE OCUPACION ANTES - especial para los desempleados ( 2 niveles )
# Para los desempleados:
Dane_2020$ocupacion_antes[!(Dane_2020$P6440 %in% (1:2)) & (!(Dane_2020$P7350.En.su.ultimo.trabajo.era %in% (4:7)))] = "No asalariado"
Dane_2020$ocupacion_antes[!(Dane_2020$P6440 %in% (1:2)) & (!(Dane_2020$P7350.En.su.ultimo.trabajo.era %in% c(1,2,3,8,9)))] = "Asalariado"
Dane_2020$ocupacion_antes[is.na(Dane_2020$ocupacion_antes)] = "Ocupados"

describe(Dane_2020$ocupacion_antes) 
282/(282+184) # Aprox el 60% de los desempleados el mes anterior fueron: No asalariadados


######  Construir variables nuevas, combinando preguntas de cada modulo  ####

# ha trabajado antes
Dane_2020$`P7310 Buscando trabajo por primera vez?`[Dane_2020$`P7310 Buscando trabajo por primera vez?` == 1] = 3
Dane_2020$`P7310 Buscando trabajo por primera vez?`[Dane_2020$`P7310 Buscando trabajo por primera vez?` == 2] = 1
Dane_2020$`P7310 Buscando trabajo por primera vez?`[Dane_2020$`P7310 Buscando trabajo por primera vez?` == 3] = 2
Dane_2020 <- dplyr::rename(Dane_2020, "P7310.Ha.trabajado.antes"=`P7310 Buscando trabajo por primera vez?`)
for (i in 1:nrow(Dane_2020)) {
  if (!(is.na(Dane_2020$`P6800 Cuantas horas a la semana trabaja?`[i])) == T) {
    Dane_2020$Empleo.antes[i] <- Dane_2020$`P7020 Tuvo otro trabajo antes del actual?`[i]
  }
  if (!(is.na(Dane_2020$`P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando`[i])) == T) {
    Dane_2020$Empleo.antes[i] <- Dane_2020$P7310.Ha.trabajado.antes[i]
  }
}


#Horas que puede trabajar en la semana o las que trabaja
Dane_2020$Horas.disponibles= 0

for (i in 1:nrow(Dane_2020)) {
  if (!(is.na(Dane_2020$`P7260 Cuantas horas a la semana podria trabajar?`[i])) == T) {
    Dane_2020$Horas.disponibles[i] <- Dane_2020$`P7260 Cuantas horas a la semana podria trabajar?`[i]
  }
  
  if (!(is.na(Dane_2020$`P6800 Cuantas horas a la semana trabaja?`[i])) == T) {
    Dane_2020$Horas.disponibles[i] <- Dane_2020$`P6800 Cuantas horas a la semana trabaja?`[i] 
  }
  if (!(is.na(Dane_2020$`P7100 Horas adicionales que puede trabajar en la semana`[i])) == T){
    Dane_2020$Horas.disponibles[i] <- Dane_2020$Horas.disponibles[i] + Dane_2020$`P7100 Horas adicionales que puede trabajar en la semana`[i]
  }
}

#Rama de la empresa donde trabaja o donde trabaj?
Dane_2020$Rama.actividad= 0

for (i in 1:nrow(Dane_2020)) {
  if (!(is.na(Dane_2020$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i])) == T) {
    Dane_2020$Rama.actividad[i] <- Dane_2020$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i]
  }
  if (!(is.na(Dane_2020$`Rama de actividad de la empresa donde labora(2digitos)`[i])) == T) {
    Dane_2020$Rama.actividad[i] <- Dane_2020$`Rama de actividad de la empresa donde labora(2digitos)`[i]
  }
}

#Variable tiempo en Colombia
Dane_2020$tiempo.en.colombia= 0

for (i in 1:nrow(Dane_2020)) {
  if(!(is.na(Dane_2020$`P753S3 En que pais vivia hace 12 meses?`[i])) == T){
    Dane_2020$tiempo.en.colombia[i] <- 1
  }
  if (Dane_2020$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
    Dane_2020$tiempo.en.colombia[i] <- 2
  }
  if(!(is.na(Dane_2020$P756S3[i])) == T & Dane_2020$`P755 Donde vivia hace 5 años?`[i] %in% (2:3) & Dane_2020$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
    Dane_2020$tiempo.en.colombia[i] <- 3
  }
  if (Dane_2020$tiempo.en.colombia[i]==0) {
    Dane_2020$tiempo.en.colombia[i] <- 1
  }
}

# Indicativo de año
Dane_2020$anio <- 2020




    #### !!!! UNION DE 2019 Y 2020 y GUARDADO ####

Dane_reducida_total <- rbind(Dane_2019, Dane_2020)
write.csv(Dane_reducida_total, "Dane_Reducida_Total.csv")




















