library(funModeling)
library(tidyverse)


Dane <- read.csv("Dane_Reducida_Total.csv", header = T, sep = ",", dec = ".")


# Eliminamos a todas las personas que no declararon ingresos, es decir, tenian N.A o 98/99
Dane <- Dane %>% filter(!((ocupacion =="Asalariado") & (is.na(P6500.Cuanto.ganó.el.ultimo.mes.)) |
                            (ocupacion =="No asalariado") & (is.na(P6750.Ganancia.neta.por.honorarios.o.negocio)) |
                            (ocupacion =="Desempleado") & (is.na(P7422S1))|
                            P7422S1 %in% c(98,99)))

#Nos quedamos con 17,950

#--------------------   Selección de variables para pre-procesamiento    -----------------------

Dane_paramodelos <- dplyr::select(Dane, Identificacion,Familia, Departamento, Genero, Edad, Parentesco, Estado.civil, Nivel.educativo.alcanzado, 
                                  Empleo.antes, Rama.actividad,tiempo.en.colombia,Horas.disponibles, P6500.Cuanto.ganó.el.ultimo.mes.,
                                  Cuanto.recibio.por.horas.extras, P6510S2, P6585S1A1, P6585S1A2, P6585S2A1, P6585S2A2,
                                  P6585S3A1, P6585S3A2, P6585S4A1, P6585S4A2, P6545S1, P6545S2, P6580S1, P6580S2,P6750.Ganancia.neta.por.honorarios.o.negocio,
                                  P7070, P7422S1, P7510S3A1, ocupacion, ocupacion_antes, Factor.de.expansion, anio)

# Filtramos aquellos con rama de actividad 0, es decir, los que nunca han trabajado antes
Dane_paramodelos <- Dane_paramodelos %>% filter(!(Rama.actividad == "0"))



                                            #-------.-----     
        #------------     Cambio de tipo de variables y modificaciones varias   ----------

glimpse(Dane_paramodelos) 
# Es necesario modificar variables, varias son factores y no enteros.

Dane_paramodelos$Departamento <-  as.factor(Dane_paramodelos$Departamento)
Dane_paramodelos$Genero <- as.factor(Dane_paramodelos$Genero)
Dane_paramodelos$Parentesco <- as.factor(Dane_paramodelos$Parentesco)
Dane_paramodelos$Estado.civil <- as.factor(Dane_paramodelos$Estado.civil)
Dane_paramodelos$Nivel.educativo.alcanzado<- as.factor(Dane_paramodelos$Nivel.educativo.alcanzado)
Dane_paramodelos$Empleo.antes <- as.factor(Dane_paramodelos$Empleo.antes)
Dane_paramodelos$Rama.actividad <- as.factor(Dane_paramodelos$Rama.actividad)
Dane_paramodelos$tiempo.en.colombia <- as.factor(Dane_paramodelos$tiempo.en.colombia)


Dane_paramodelos$Parentesco <- fct_collapse(Dane_paramodelos$Parentesco, "Jefe de hogar"="1", "Nucleo familiar"=c("2","3"), "Otros parientes"=c("4","5"), "Otros No parientes"=c("6","7","8","9"))
Dane_paramodelos$Genero <- fct_collapse(Dane_paramodelos$Genero, "Masculino"="1", "Femenino"=c("2"))
Dane_paramodelos$Empleo.antes <- fct_collapse(Dane_paramodelos$Empleo.antes, "Si"="1", "No"=c("2"))
Dane_paramodelos$tiempo.en.colombia <- fct_collapse(Dane_paramodelos$tiempo.en.colombia, "Menos de 1 año"="1", "Entre 1 y 5 años"=c("2"), "Mas de 5 años" ="3")
Dane_paramodelos$Estado.civil <- fct_collapse(Dane_paramodelos$Estado.civil, "Soltero"="6", "Casado y/o union libre"=c("1","2","3"), "Separado y/o viudo"=c("4","5"))
Dane_paramodelos$Nivel.educativo.alcanzado <- fct_collapse(Dane_paramodelos$Nivel.educativo.alcanzado, "Ninguno/No_sabe"=c("1","9"), "Preescolar"="2","Básica primaria"="3","Básica secundaria"="4","Media (10o -13o)"="5","Superior o universitaria"="6",)


# La codificación de rama de actividad es diferente para el año 2019 y 2020, por lo tanto, para codificar
# de manera correcta esta variable, se hace necesario dividir las bases de datos por anio y luego volver
# a juntarlas

D2019 <-Dane_paramodelos %>% filter(anio==2019)
D2020 <-Dane_paramodelos %>% filter(anio==2020)

#2019

D2019$Rama.actividad <-fct_collapse(D2019$Rama.actividad, "Agricultura, ganadería y caza"= "1" , "Silvicultura y extracción de madera "= "2",
                                               "Actividades de servicios relacionas con la pesca"="5", "Explotación de minas y carbón"=c("10","11","14"),"Elaboración de productos alimenticios"="15","Fabricación de productos de tabaco"="16",
                                               "Fabricación de productos textiles"="17", "Fabricación de prendas de vestir"="18", "Curtido y preparado de cueros, trabajar con cuero"="19",
                                               "Fabricación de productos de madera"="20","Fabricación de producto de papel y cartón"="21", "Actividades de edición/reproducción de grabaciones"="22",
                                               "Fabricación de sustancias y productos químicos"="24", "Fabricación de productos de caucho y de plástico"="25", "Fabricación de productos minerales no metálicos"="26",
                                               "Fabricación de productos metalúrgicos básicos"="27", "Fabricación de productos de metal"="28", "Fabricación de maquinaria y equipo ncp"="29",
                                               "Fabricación de maquinaria y aparatos eléctricos"="31", "Fabricación de equipo  de radio y televisión"="32", "Fabricación de instrumentos médicos // relojes"="33",
                                               "Fabricación de vehículos automotores"="34", "Fabricación de otros tipos de equipo de transporte"="35", "Fabricación de muebles / industrias manufactureras"="36",
                                               "Reciclaje de metalicos y no metalicos"="37", "Suministro de electricidad, gas y vapor"="40", "Captación, depuración y distribución de agua"="41",
                                               "Construcción"="45", "Comercio al por mayor y al por menor"=c("50", "51","52"),"Hoteles, restaurantes, bares y similares"="55", "Transporte por vía terrestre; transporte por tuberías"="60", "Transporte por vía acuática"="61",
                                               "Transporte por vía aérea"="62", "Actividades complementarias/auxiliares al transporte"="63", "Correo y telecomunicaciones"="64", "Intermediación financiera"="65",
                                               "Financiación de planes de seguros y pensiones"="66", "Actividades auxiliares de la intermediación financiera"="67", "Actividades inmobiliarias"="70",
                                               "Alquiler de maquinaria y equipo sin operarios"="71", "Informática y actividades conexas"="72", "Otras actividades empresariales"="74","Administración pública y defensa"="75",
                                               "Educación"="80", "Servicios sociales y de salud"="85", "Saneamiento y eliminación de desperdicios"="90", "Actividades de asociaciones ncp"="91",
                                               "Actividades culturales y deportivas"="92", "Otras actividades de servicios"="93", "Hogares privados con servicio doméstico"="95", "Organizaciones extraterritoriales"="99")

D2019$Rama.actividad <- as.factor(D2019$Rama.actividad)
D2019$Rama.actividad <- droplevels(D2019$Rama.actividad)
levels(D2019$Rama.actividad)


#2020

D2020$Rama.actividad <-fct_collapse(D2020$Rama.actividad, "Agricultura, ganadería y caza"= "1" , "Explotación de minas y carbón"= c("5","8","9"),
                                               "Elaboración de productos alimenticios"=c("10","11"),"Fabricación de productos textiles"="13", "Fabricación de prendas de vestir"="14", "Curtido y preparado de cueros, trabajar con cuero"="15",
                                               "Fabricación de productos de madera"="16","Fabricación de producto de papel y cartón"="17", "Actividades de edición/reproducción de grabaciones"="18",
                                               "Fabricación de sustancias y productos químicos"=c("19", "20","21"), "Fabricación de productos de caucho y de plástico"="22", "Fabricación de productos minerales no metálicos"="23",
                                               "Fabricación de productos metalúrgicos básicos"="24", "Fabricación de productos de metal"="25",
                                               "Fabricación de maquinaria y aparatos eléctricos"="27", "Otras industrias manufactureras"="32", "Instalación, mantenimiento y reparación de maquinaria"="33",
                                               "Fabricación de vehículos automotores"="29", "Fabricación de otros tipos de equipo de transporte"="30", "Fabricación de muebles / industrias manufactureras"="31",
                                               "Suministro de electricidad, gas y vapor"="35", "Captación, depuración y distribución de agua"="36", "Recolección, tratamiento y disposición de desechos"="38",
                                               "Construcción"=c("41","42","43"), "Comercio al por mayor y al por menor"=c("45", "46","47"),"Hoteles, restaurantes, bares y similares"=c("55", "56"), "Transporte por vía terrestre; transporte por tuberías"="49",
                                               "Actividades complementarias/auxiliares al transporte"="52", "Correo y telecomunicaciones"=c("53","59","61"), "Informática y actividades conexas"=c("62","63"),"Intermediación financiera"="64",
                                               "Financiación de planes de seguros y pensiones"="65", "Actividades auxiliares de la intermediación financiera"="66", "Actividades inmobiliarias"="68", "Actividades profesionales, cientificas y tecnicas" =c("69","71", "73", "74"),
                                               "Otras actividades empresariales"=c("77","78","79","80","81","82"),"Administración pública y defensa"="84",
                                               "Educación"="85", "Servicios sociales y de salud"=c("86","87","88"), 
                                               "Actividades culturales y deportivas"=c("90","91","92","93"), "Otras actividades de servicios"=c("94","95","96"), "Hogares privados con servicio doméstico"="97")

D2020$Rama.actividad <- as.factor(D2020$Rama.actividad)
D2020$Rama.actividad <- droplevels(D2020$Rama.actividad)
levels(D2020$Rama.actividad)

# Unimos
Dane_paramodelos2 <- rbind(D2019, D2020)
levels(Dane_paramodelos2$Rama.actividad)
freq(Dane_paramodelos2$Rama.actividad)   # Grafica sobre frecuencia de ramas de actividad

# Categorizamos la variable edad y la variable horas disponibles
Dane_paramodelos2$Edad <-  cut(Dane_paramodelos2$Edad,  breaks = c(11,14,18, 26, 59, 100))
Dane_paramodelos2$Horas.disponibles <-  cut(Dane_paramodelos2$Horas.disponibles,  breaks = c(0,39, 48, 130))


                                    #-------.-----     
  #-------    Construcción de variable respuesta ("Ingresos totales en el ultimo mes")   -------

# En las variables de ingresos los NA los convertimos en cero
Dane_paramodelos2$P6500.Cuanto.ganó.el.ultimo.mes.[is.na(Dane_paramodelos2$P6500.Cuanto.ganó.el.ultimo.mes.)]=0
Dane_paramodelos2$Cuanto.recibio.por.horas.extras[is.na(Dane_paramodelos2$Cuanto.recibio.por.horas.extras)]=0
Dane_paramodelos2$P6585S1A1[is.na(Dane_paramodelos2$P6585S1A1)]=0
Dane_paramodelos2$P6585S2A1[is.na(Dane_paramodelos2$P6585S2A1)]=0
Dane_paramodelos2$P6585S3A1[is.na(Dane_paramodelos2$P6585S3A1)]=0
Dane_paramodelos2$P6585S4A1[is.na(Dane_paramodelos2$P6585S4A1)]=0
Dane_paramodelos2$P6545S1[is.na(Dane_paramodelos2$P6545S1)]=0
Dane_paramodelos2$P6580S1[is.na(Dane_paramodelos2$P6580S1)]=0
Dane_paramodelos2$P6750.Ganancia.neta.por.honorarios.o.negocio[is.na(Dane_paramodelos2$P6750.Ganancia.neta.por.honorarios.o.negocio)]=0
Dane_paramodelos2$P7070[is.na(Dane_paramodelos2$P7070)]=0
Dane_paramodelos2$P7422S1[is.na(Dane_paramodelos2$P7422S1)]=0
Dane_paramodelos2$P7510S3A1[is.na(Dane_paramodelos2$P7510S3A1)]=0

# En las variables de tipo Si o No los NA los convertimos en "No responde"
Dane_paramodelos2$P6510S2[is.na(Dane_paramodelos2$P6510S2)]="No responde"
Dane_paramodelos2$P6585S1A2[is.na(Dane_paramodelos2$P6585S1A2)]="No responde"
Dane_paramodelos2$P6585S2A2[is.na(Dane_paramodelos2$P6585S2A2)]="No responde"
Dane_paramodelos2$P6585S3A2[is.na(Dane_paramodelos2$P6585S3A2)]="No responde"
Dane_paramodelos2$P6585S4A2[is.na(Dane_paramodelos2$P6585S4A2)]="No responde"
Dane_paramodelos2$P6545S2[is.na(Dane_paramodelos2$P6545S2)]="No responde"
Dane_paramodelos2$P6580S2[is.na(Dane_paramodelos2$P6580S2)]="No responde"


Dane_paramodelos2$Cuanto.recibio.por.horas.extras <- as.integer(Dane_paramodelos2$Cuanto.recibio.por.horas.extras)
Dane_paramodelos2$P7422S1 <- as.integer(Dane_paramodelos2$P7422S1)

glimpse(Dane_paramodelos2)
df_status(Dane_paramodelos2)
Dane_paramodelos3 <- Dane_paramodelos2


                ##### 1. ASALARIADOS #####


#inicilizamos a variable
Dane_paramodelos3$Ingresos_total=Dane_paramodelos3$P6500.Cuanto.ganó.el.ultimo.mes.

# Comprobamos si la persona incluyó o no el valor de los subsidios en el
# ingreso declarado anteriormente (P6500). Si no lo incluyó, se suman.

#!!! Opté por ciclo while porque el ciclo for no me estaba actualizando los valores 

i <- 1
while (i <= nrow(Dane_paramodelos3)) {
  
if (Dane_paramodelos3$P6510S2[i] == "2") { # Horas extras
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$Cuanto.recibio.por.horas.extras[i]
  
  } 
if (Dane_paramodelos3$P6585S1A2[i] == "2") { # Subsidio de alimentación
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6585S1A1[i]
  
  } 
if (Dane_paramodelos3$P6585S2A2[i] == "2") { # Subsidio de transporte
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6585S2A1[i]
  
  } 
if (Dane_paramodelos3$P6585S3A2[i] == "2") { # Subsidio familiar
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6585S3A1[i]
  
  } 
if (Dane_paramodelos3$P6585S4A2[i] == "2") { # Subsidio educativo
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6585S4A1[i]
  
  } 
if (Dane_paramodelos3$P6545S2[i] == "2") { # Ingresos por primas (tecnica, antiguedad)
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6545S1[i]
  
  } 
if (Dane_paramodelos3$P6580S2[i] == "2") { # Por algún tipo de bonificación mensual
  
  Dane_paramodelos3$Ingresos_total[i] <- Dane_paramodelos3$Ingresos_total[i] + Dane_paramodelos3$P6580S1[i]
  }
  i <- i+1
}

                         ##### 2. NO ASALARIADOS #####

# Todos los que no son asalariados tienen un cero en la variable p6750, por lo tanto no afecta sumar los valores.
Dane_paramodelos3$Ingresos_total <- Dane_paramodelos3$Ingresos_total + Dane_paramodelos3$P6750.Ganancia.neta.por.honorarios.o.negocio


                          ##### 3. DESEMPLEADOS ####

# Todos los que no son asalariados tienen un cero en la variable p6750, por lo tanto no afecta sumar los valores.
Dane_paramodelos3$Ingresos_total <- Dane_paramodelos3$Ingresos_total + Dane_paramodelos3$P7422S1


                      ##### 4. PARA DOS O LAS 3 OCUPACIONES  #####

# Ingresos por trabajos secundarios y por ayudas de entidades nacionales o internacionales
Dane_paramodelos3$Ingresos_total <- Dane_paramodelos3$Ingresos_total + Dane_paramodelos3$P7070
Dane_paramodelos3$Ingresos_total <- Dane_paramodelos3$Ingresos_total + Dane_paramodelos3$P7510S3A1



Dane_paramodelos3 <- filter(Dane_paramodelos3, !(Ingresos_total%in% 1:200)) #Eliminamos 2 personas que no saben sus ingresos

describe(Dane_paramodelos3$Ingresos_total)
ggplot(Dane_paramodelos3, aes(x=ocupacion, y=Ingresos_total, fill=ocupacion)) + geom_boxplot() + 
  ggtitle("Ingresos totales por sector de población")+
  xlab("")+ ylab("Monto $") + theme_bw()

# A manera de ejemplo, nos quedamos solo con los que ganen menos de 3 millones
Dane_paramodelos3 %>% filter(Ingresos_total<=1500000) %>% ggplot(aes(x=ocupacion, y=Ingresos_total, fill=ocupacion)) + geom_boxplot() + 
  ggtitle("Ingresos totales por sector de población")+
  xlab("")+ ylab("Monto $") + theme_bw()

17948 - Dane_paramodelos3 %>% filter(Ingresos_total<=1500000) %>% nrow()  #Solo 560 ganan mas de 1.5 millones


                                            #-------.-----     
#--------------------   Selección final de variables para correr modelos de regresión   -----------------------



# A los desempleados les asignamos su ocupacón anterior para solo contar con 2 niveles

Dane_paramodelos_final <-  Dane_paramodelos3
Dane_paramodelos_final$ocupacion <- if_else(Dane_paramodelos3$ocupacion=="Desempleado", Dane_paramodelos3$ocupacion_antes, Dane_paramodelos3$ocupacion ) 


# Seleccionamos las variables de interes
Dane_paramodelos_final  <- Dane_paramodelos_final %>% select(1:12, 32, 34, 35, 36)
df_status(Dane_paramodelos_final)

describe(Dane_paramodelos_final$Ingresos_total)


                                                      #-------.-----     
                        #-------   Los ingresos del 2019 lo traemos a pesos del 2020   -----------

# Los ingresos del 2019 se encuentran en pesos nominales o corrientes
# Para hacerlos comparables con los ingresos declarados del 2020
# es necesario hacer la conversión a valores reales, y de esta manera
# tener en cuenta la inflación de un año al otro


#!!!!!!!!!!!   Escogemos como año base el 2020   !!!!!!!!!!!!!!!

# referencia variacion IPC año 2020 (3,62%): https://www.dinero.com/economia/articulo/inflacion-e-colombia-enero-2020/281530
# referencia variacion IPC año 2019 (3,8%): https://www.larepublica.co/economia/dato-de-inflacion-en-colombia-durante-2019-aumento-a-380-segun-dane-2948404


Año <- c("2019", "2020")
variación_IPC <- c(0.038, 0.0362)
índice <- c(0, 100)

tabla_inf <- as_tibble(cbind(Año, variación_IPC, índice))
tabla_inf$variación_IPC <-  as.numeric(tabla_inf$variación_IPC)
tabla_inf$índice <-  as.numeric(tabla_inf$índice)

# para calcular el indice de 2019 (t) utilizamos la siguiente ecuación:
# Indice_año_t = indice_año_t+1/ ( 1 + variacion_IPC (año t)).
# Debido a que es un año anterior al año base

tabla_inf$índice[1] <- tabla_inf$índice[2]/(1+tabla_inf$variación_IPC[1])  

# ahora para pasar el precio del 2019 a precio constante, realizamos la siguiente ecuación:
# Valor en precios constantes = (Valor en precio corriente/indice de precios) * 100


i <- 1
while (i<=nrow(Dane_paramodelos_final)) {
  
  if (Dane_paramodelos_final$anio[i] == "2019") {
    
    Dane_paramodelos_final$Ingresos_total[i] <- (Dane_paramodelos_final$Ingresos_total[i]/tabla_inf$índice[1])*100
    
  }
  i <- i+1
}

describe(Dane_paramodelos_final$Ingresos_total)

Dane_paramodelos_final <- Dane_paramodelos_final %>% select(-anio)
df_status(Dane_paramodelos_final)


                                                        #-------.-----     
    #-------   Y TERMINAMOS LA PREPARACIÓN DE LA BASE DE DATOS, SOLO QUEDA CORRER LOS MODELOS Y CONTINUAR LAS ETAPAS  -----------


write.csv(Dane_paramodelos_final, "Dane_Paramodelos_regresion.csv")









