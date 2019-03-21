
paquetes <- c("tidyverse","lubridate","dlm")
sapply(paquetes,require,character.only=T)

# datos originales
minimas <- read.table("Tn_1950_2014_con_QC_v1.txt", sep = ',', na.strings = "NaN")
maximas <- read.table("Tx_1950_2014_con_QC_v1.txt", sep = ',', na.strings = "NaN")


maximas %>%  set_names(nm = c( "year", "month", "day","artigas", "carrasco", 
                 "estancuela", "melo", "mercedes", "paso_toros", 
                 "paysandu", "prado", "rivera", "rocha", "salto") ) %>% 
  select(-year, -month, -day) %>% 
  gather(estacion, temp, -estancuela) %>% 
  ggplot() + geom_hex(aes(estancuela, temp) ) + facet_wrap(~estacion) 
          
# funcion que arma los datos UNIvariados, y centrados
data_fun <- function(estacion) { 
  # estacion es un symbol: sym('estacion')
  bind_rows(list(Tn=minimas, Tx=maximas), .id = 'serie') %>%
    set_names(nm = c("serie", "year", "month", "day","artigas", "carrasco", 
                     "estancuela", "melo", "mercedes", "paso_toros", 
                     "paysandu", "prado", "rivera", "rocha", "salto")  ) %>% 
    group_by(serie) %>% mutate(temp.bar = mean(!!!estacion , na.rm=T) ) %>% 
    ungroup() %>% 
    select(c(serie, year, month, day, temp.bar, !!!estacion) ) %>% 
    mutate(fecha = make_date(year, month, day), 
           month2 = formatC(month, width=2, format="d", flag="0"),
           day2 = formatC(day, width=2,format="d",flag="0"),
           dia = paste(month,day,sep="-"),
           temp = !!estacion -temp.bar) 
}

mod_fun <- function(ys, mm, prs) {
  # ys: los datos de la serie, (es mejor que sea un ts() ??? )
  # mm: el modelo a estimar
  # prs: los valores iniciales de los parametros a estimar
  mod <- dlmMLE( yy, parm = prs, build = mm )
  filtro <- dlmFilter(yy, mm( mod$par ) )
  smooth <- dlmSmooth(yy, mm(mod$par) )
  list(modelo=mod, filtro=filtro, smooth=smooth)
}

# Modelo para estanzuela, tiene UN solo dato faltante
esta <- sym('estancuela')
datos <- data_fun(esta)

mms <-  function(par) dlmModTrig(s = 365, q=2, dV = exp(par[1]), dW = 0) +  
  dlmModPoly(1, dV = exp(par[2]), dW = exp(par[3]))

yy <- filter(datos, serie == 'Tn') %>% pull(temp) %>% ts(start = c(1950, 1), frequency = 365)
mod1 <- mod_fun(yy, mms, c(0,0,0) )

# en estancuela falta el 31/12/2008 (mucha fiesta), pero el modelo lo imputa
datos %>% filter(serie == 'Tn') %>% 
  mutate(yhat = apply(mod1$smooth$s[-1, c(1,3,5)], 1, sum) )  %>% 
  filter(fecha > make_date(2008, 12, 26), fecha < make_date(2009, 1, 10)) %>% 
  ggplot() + geom_line(aes(fecha, temp)) + geom_point(aes(fecha, yhat), color = 'red' )
  
# corroborar la fecha del faltante ... con time, parece que es en 2009
# por los bisiestos ????
time(yy)[is.na(yy)]

# otra estacion..
pay <- sym('paysandu')
datos.pay <- data_fun(pay)

yy.pay <- filter(datos.pay, serie == 'Tn') %>% pull(temp) %>% ts(start = c(1950, 1), frequency = 365)
mod1.pay <- mod_fun(yy, mms, c(0,0,0) )

datos.pay %>% filter(serie == 'Tn') %>% 
  mutate(yhat = apply(mod1.pay$smooth$s[-1, c(1,3,5)], 1, sum) )  %>% 
  filter(fecha > make_date(1962, 1, 1), fecha < make_date(1962, 7, 31)) %>% 
  ggplot() + geom_line(aes(fecha, temp)) + geom_line(aes(fecha, yhat), color = 'red' )


#---













  
#====================================================
# # mugre ....
# ggplot(datos) + geom_line( aes(fecha, temp) ) + facet_grid(serie~.)
# 
# # modelos para minimas
# mms <- list(
#   function(par)  dlmModTrig(s = 365, q=2, dV = exp(par[1]), dW = 0) +  
#     dlmModPoly(1, dV = exp(par[2]), dW = exp(par[3])),
#   function(par)  dlmModTrig(s = 365, q=2, dV = exp(par[1]), dW = exp(par[2]))
# )
# 
# Tn.mod1 <- dlmMLE( yy, parm = c(0,0,0), build = mm[[1]] )
# Tn.mod2 <- dlmMLE( yy, parm = c(0,0), build = mm[[2]] )
# Tn.mod3 <- dlmMLE( yy, parm = c(0,0), build = mm[[3]] )
# 
# Tn.mod <- lapply(mm, function(ll) dlmMLE(yy, parm = c(0,0,0,0), build = ll ))
# lapply(list(Tn.mod2, Tn.mod3), function(x) x$convergence )
# 
# Tn.smooth1 <- dlmSmooth(yy, mm[[1]](Tn.mod1$par) )
# Tn.smooth2 <- dlmSmooth(yy, mm[[2]](Tn.mod2$par) )
# Tn.smooth3 <- dlmSmooth(yy, mm[[3]](Tn.mod3$par) )
# 
# mean(abs(residuals(dlmFilter(yy, mm[[2]](Tn.mod2$par)), type = "raw", sd = FALSE)) / yy, na.rm = T)
# 
# filter(datos, serie == 'Tn') %>% 
#   mutate(yhat = apply(Tn.smooth2$s[-1, c(1,3) ],1,sum) ) %>% 
#   select(fecha, year, temp, yhat) %>% 
#   ggplot() + 
#   geom_line(aes(fecha, temp)) + geom_line(aes(fecha, yhat), color='red') 
# 
# Tn.smooth2$s[-1,] %>% ts() %>% plot()

                       