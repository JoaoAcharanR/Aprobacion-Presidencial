#####Explorar datos#####

encuestas #Permite visualizar 10 primeras observaciones de la BBDD, observando variables y clase 
encuestas %>% nrow #Reporta cantidad de observaciones
encuestas %>% ncol #Reporta cantidad de variables/columnas
encuestas %>% names #Reporta nombres de variables

#####Transformar datos#####

encuestas <- encuestas %>% group_by(empresa,encuesta,presidente,mes,año) #Nos genera 80 grupos. Para cada uno de ellos se calculará la media
encuestas

#Summarise: crea nueva BBDD con valores agregados
encuestas <- encuestas %>% summarise(apruebo = mean(apruebo), desapruebo = mean(desapruebo)) #Se genera nueva BBDD agregada con los valores promedio de Apruebo y Desapruebo para los grupos antes generados
encuestas

#Crear variable fecha
encuestas <- encuestas %>% mutate(fecha = make_date(year = año, month = mes)) #Make_date requiere paquete lubridate.

#Crear nueva columna: variable apruebo/desapruebo
encuestas <- encuestas %>% pivot_longer(cols = c(apruebo,desapruebo), names_to = "metrica", values_to = "porcentaje") #Se cambia estructura de la BBDD

#####Visualizar#####
#Primera versión
plot_encuestas <- encuestas %>% ggplot(aes(x=fecha,y=porcentaje,color = metrica))
plot_encuestas <- plot_encuestas %+% geom_point()
plot_encuestas <- plot_encuestas %+% geom_line()
plot_encuestas <- plot_encuestas %+% facet_grid(~empresa)
plot_encuestas <- plot_encuestas %+% geom_text(aes(x=fecha,y=porcentaje,label=paste(porcentaje)), color = "black", size = 2)
plot_encuestas

#Aprobación según empresa solo para Piñera
encuestas <- encuestas %>% filter(presidente == "piñera")
encuestas <- encuestas %>% filter(metrica == "apruebo")
plot_encuestas_v2 <- encuestas %>% ggplot(aes(x=fecha,y=porcentaje,group=encuesta, fill=encuesta))
plot_encuestas_v2 <- plot_encuestas_v2 %+% geom_point(aes(x=fecha,y=porcentaje,color=encuesta))
plot_encuestas_v2 <- plot_encuestas_v2 %+% geom_line(aes(x=fecha,y=porcentaje,color=encuesta))
plot_encuestas_v2 <- plot_encuestas_v2 %+% scale_y_continuous(limit = c(0,100))
plot_encuestas_v2 <- plot_encuestas_v2 %+% geom_text(aes(x=fecha,y=porcentaje, color = encuesta, label= paste(porcentaje)), vjust=-1,  size=3)
plot_encuestas_v2 <- plot_encuestas_v2 %+% theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
plot_encuestas_v2 <- plot_encuestas_v2 %+% labs(title = "Aprobación del Presidente Piñera", subtitle = "% Aprobación de diversas encuestas",
                                                caption = "Realizado por Joao Acharán")
plot_encuestas_v2 <- plot_encuestas_v2 %+% theme_bw()
plot_encuestas_v2 <- plot_encuestas_v2 %+% geom_vline(aes(xintercept=as.numeric(encuestas$fecha[15])), color = "red") 
plot_encuestas_v2 <- plot_encuestas_v2 %+% annotate(geom = "text",x=as.Date("2019-11-01"),y=76, label= "Octubre 2019\nEstallido social", size = 3)
plot_encuestas_v2 <- plot_encuestas_v2 %+% annotate(geom = "text",x=as.Date("2019-11-01"),y=83, label= sprintf("\U279E"), color = "red", size = 10)
plot_encuestas_v2 <- plot_encuestas_v2 %+% geom_vline(aes(xintercept=as.numeric(encuestas$fecha[6])), color = "red") 
plot_encuestas_v2 <- plot_encuestas_v2 %+% annotate(geom = "text",x=as.Date("2020-04-01"),y=76, label= "Marzo 2020\nCOVID-19", size = 3)
plot_encuestas_v2 <- plot_encuestas_v2 %+% annotate(geom = "text",x=as.Date("2020-04-01"),y=83, label= sprintf("\U279E"), color = "red", size = 10)
plot_encuestas_v2

#####Guardar graficos

ggsave("plot/plot_encuestas_v3.png", plot = plot_encuestas_v2, width = 13.5, height = 5)

#####Guardar los datos

encuestas %>% write_rds("data/encuestas_pivot.rds")
