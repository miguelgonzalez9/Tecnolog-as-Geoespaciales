## Transformación IPS por Municipio 
file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/Registro_Especial_de_Prestadores_de_Servicios_de_Salud.csv"
ips <- read_csv(file)
ips_mun <- ips %>% count(idmpio, nompio)
write_file <- "~/OneDrive - Universidad del rosario/Tec Geo/Proyecto/Datos/IPS_Municipios.csv"
write.csv(ips_mun, write_file)