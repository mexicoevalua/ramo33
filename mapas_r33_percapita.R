## Análisis del ramo 33 desde 1998 a 2014 percapita
# Este script genera mapas interactivos por estado basados en:
# 1) http://rmaps.github.io/blog/posts/animated-choropleths/index.html y
# 2) http://bl.ocks.org/diegovalle/8967565
# 3) Trabajo de México Evalúa
# Fuente de los datos: Informes de Gobierno y CONAPO

#####
# Cargar datos de poblacion
pob  <- read.csv("pobConapo.csv")
head(pob)
table(pob$sexo)
table(pob$year)
require(reshape2)
pob  <- pob[,c(-1,-5,-7)]
tail(pob)
pop.wide  <- dcast(pob, year + ent + id_ent ~ sexo, value.var="pob", sum)
pop.wide$pob  <- with(pop.wide, Hombres + Mujeres)
head(pop.wide)
pob  <- pop.wide
with(pob, table(ent,id_ent))

######
## Cargar datos ramo 33
data  <- read.csv("r_33_long_14.csv", encoding="utf8")
head(data)
# Agregar códigos de los estados
codes  <- read.csv("state_code.csv")
head(codes)
data  <- merge(data, codes, by.x="id", by.y="state_code")
head(data)
#####
## Cambiar de formato wide a long
require(reshape2)
temp  <- data[,c(-4)]
names(temp)
temp  <- melt(temp, id=c("id", "estado","yr","name"))
head(temp)
data  <- temp
head(data)
data$value  <- gsub(" ","",data$value)
data$value  <- as.numeric(data$value)
##### 
## Transformar a valores reales con el deflactor del Pib base 2008
# Fuente: International Monetary Fund, World Economic Outlook Database, October 2014
def  <- read.csv("deflactor.csv")
def$deflactor  <- def$deflactor / 100
data  <- merge(data, def, by.x="yr", by.y="year")
head(data[data$yr == 2008,])
str(data)
data$valReal  <- with(data, value / deflactor, na.rm=T)
head(data)

##### Cifras en percapita
head(pob)
pob  <- pob[,c(1,3,6)]
names(pob)  <- c("yr","id","pob")
data  <- merge(data, pob, c("yr","id"))
head(data)
# Valor percapita en pesos de 2008
data$percap  <- with(data, (valReal * 1000000) / pob)
summary(data$percap)

#####
## Preparar datos para construir mapa interactivo
# Eliminar obs nacional
data  <- subset(data, data$id != 0)
ramos  <- unique(data$variable)
# Cambiar nombres
head(data)
names(data)  <- c("year","state_code","long_name","name","variable","deflactor","value",
                  "valReal","poblacion","percap")
head(data)
ramos

# Reemplazar NA's con ceros
data[is.na(data)] <- 0
tail(subset(data,data$variable == "r33_total"))
summary(data)

r33_total <- subset(data, data$variable == "r33_total")
summary(r33_total)
r33_fasa  <- subset(data, data$variable == "r33_fasa")
r33_fise  <- subset(data, data$variable == "r33_fise")
r33_multiple  <- subset(data, data$variable == "r33_multiple")
r33_infraeduca  <- subset(data, data$variable == "r33_infraeduca")
r33_fortamun  <- subset(data, data$variable == "r33_fortamun")
r33_segurid  <- subset(data, data$variable == "r33_segurid")
r33_faeb  <- subset(data, data$variable == "r33_faeb")
r33_fis  <- subset(data, data$variable == "r33_fis")
r33_fism  <- subset(data, data$variable == "r33_fism")
r33_alimenticio  <- subset(data, data$variable == "r33_alimenticio")
r33_edusuperior  <- subset(data, data$variable == "r33_edusuperior")
r33_edutecnologica  <- subset(data, data$variable == "r33_edutecnologica")
r33_fortaestados  <- subset(data, data$variable == "r33_fortaestados")

source("lib_mapas.R")
