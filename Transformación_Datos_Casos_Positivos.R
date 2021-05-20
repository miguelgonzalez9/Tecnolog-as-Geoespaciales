## Datos Covid Proyecto
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
file <- c("~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Casos_positivos_de_COVID-19_en_Colombia.csv")
x <- read_csv(file, col_types = c("ciccccciiffcficfccccffc"))
## Exploración básica bases de datos.
head(x)
dim(x)
str(x)
dim(x)[1]
## Variable que registre cada caso. cada muerte. 
count <- rep(1,dim(x)[1])
x <- cbind(x,count)
fallecidos <- x$Recuperado == "Fallecido" | x$Recuperado == "fallecido"
x <- cbind(x,fallecidos)
## Tenemos que convertir todos los códigos municipales en número de 5 dígitos para esto verificamos que 
## para todos tengamos 4 o 5 dígitos. 
sum(nchar(x$`Código DIVIPOLA municipio`) == 4) + sum(nchar(x$`Código DIVIPOLA municipio`) == 5) == dim(x)[1]
## Ahora tomamos los de cuatro dígitos y añadimos un 0 al principio del dígito
df <- transform(x, Cod = ifelse(nchar(`Código DIVIPOLA municipio`)==4, paste0(0,`Código DIVIPOLA municipio`),`Código DIVIPOLA municipio`))
df <- transform(df, Cod = as.character(Cod))
## Agrupar datos por código de departamento.
data <-df %>% group_by(Cod)
## Casos por cada código municipal.
casos_municipio <- data %>% summarise(casos = sum(count), muertes = sum(fallecidos))
##identificación de capitales
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Capitales Colombia.xlsx"
cap <- read_excel(file, sheet = 1)
cap <- cap[,-2]
colnames(cap) <- c("Cod", "cap_dpto")
casos_municipio_cap <- full_join(casos_municipio, cap, by = c("Cod" = "Cod")) %>% 
        replace_na(list(cap_dpto = 0)) %>% arrange(desc(muertes), desc (casos))
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/casos_muertes_municipio.csv"
write.csv(casos_municipio_cap, file)
## Importar índice de probeza multidimensional municipal. 
file <- "Datos/IPM_2018_FuenteCensal.csv"
IPM <- read_csv(file)
str(IPM)
## Joint con base de datos.
data_base <- inner_join(IPM, casos_municipio_cap, by = c("MPIO_CCNCT" = "Cod"))
## Transformación IPS por Municipio 
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Registro_Especial_de_Prestadores_de_Servicios_de_Salud.csv"
ips <- read_csv(file)
ips_mun <- ips %>% count(idmpio, nompio)
write_file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/IPS_Municipios.csv"
write.csv(ips_mun, write_file)

## Tranformación Datos Temporales Contagios
data_temp <-df %>% group_by(Fecha.de.inicio.de.síntomas, Cod) %>% 
        summarise(casos = sum(count), muertes = sum(fallecidos)) %>% 
        transform(Fecha.de.inicio.de.síntomas = as.Date(Fecha.de.inicio.de.síntomas, "%d/%m/%Y"))
data_temp <-data_temp[!is.na(data_temp$Fecha.de.inicio.de.síntomas),]
plot(data_temp$Fecha.de.inicio.de.síntomas,data_temp$casos)
View(data_temp)
ggplot() + geom_line(data = data_temp, aes(Fecha.de.inicio.de.síntomas, casos))
ggplot() + geom_line(data = data_temp, aes(Fecha.de.inicio.de.síntomas, muertes), color = "blue")
        
boxplot(data_temp$casos)
## 
## , fem = 100*sum(sexo == "F")/(sum(count)), mas = 100*sum(sexo == "M")/(sum(count))
