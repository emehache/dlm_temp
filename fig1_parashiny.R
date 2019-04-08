
# Codigo que produce la figura 1 en el DDT, 
# Propuesta: hacer este grafico interactivo, permitiendo seleccionar
# año o periodo, y una forma elegante de "pintar de rojo" una parte de la 
# serie 

paquetes <- c("ggplot2","tidyverse","lubridate")
sapply(paquetes,require,character.only=T)

load(file = "documento/d1")
load(file = "documento/d2") # datos completos para estanzuela

# una.ola es un ejemplo fragmento para pintar de rojo
una.ola <- data.frame( serie = 'Tn', xn=156, xx=160, yn=-2, yx=20 ) %>% 
  bind_rows( data.frame( serie = 'Tx', xn=156, xx=160, yn=5, yx=28 ) )

d2 %>% mutate(dia2 = yday(fecha)) %>% 
  ggplot() + 
  geom_hex(aes(x=dia2, y=temp) ) + # historgrama de temp en todo el periodo 
  geom_line(data = mutate(d1, dia2 = yday(fecha)),  
            aes(x=dia2,y=temp,group=serie)) + # linea con UN SOLO anio que se queire esstudiar
  geom_rect( data = una.ola, 
             aes(xmin=xn, xmax=xx, ymin=yn , ymax=yx), 
             alpha=0.2, color=NA, fill="red") + # rectangulo indicando donde puede haber una ola
  facet_grid(serie~., scales = 'free_y') +
  scale_fill_gradient(name='Frequence', high = '#034e7b', low = '#f1eef6') +
  theme_bw()
