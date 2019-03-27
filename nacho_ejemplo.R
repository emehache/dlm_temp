
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

mod_fun <- function(yy, mm, prs) {
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

mms <-  function(par) dlmModTrig(s = 365, q=2, dV = exp(par[1]), dW = exp(par[2]))

dlmModPoly(1, dV = exp(par[2]), dW = exp(par[3])) + 


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

mms <-  function(par) dlmModPoly(1, dV = exp(par[1]), dW = exp(par[2])) + dlmModTrig(s = 365, q=2, dV = exp(par[3]), dW = 0)
  


pay <- sym('paysandu')
datos.pay <- data_fun(pay)
yy.pay <- filter(datos.pay, serie == 'Tn') %>% pull(temp) %>% ts(start = c(1950, 1), frequency = 365)

mod1.pay <- mod_fun(yy = yy.pay, mms, c(0,0, 0) )

mod1.pay$modelo$par
mms(mod1.pay$modelo$par)


datos.pay %>% filter(serie == 'Tn') %>%  
  mutate(yhat = apply(mod1.pay$smooth$s[-1, c(1,2,4)], 1, sum) )  %>% 
  #filter(fecha > make_date(1962, 1, 1), fecha < make_date(1962, 7, 15)) %>%  
  #filter(fecha > make_date(2014, 9, 2), fecha < make_date(2014,10,5)) %>%  
  #filter(fecha > make_date(1967, 5, 20), fecha < make_date(1967, 6, 20)) %>%
  ggplot() + geom_line(aes(fecha, temp)) + geom_point(aes(fecha, yhat), color = 'red' )

# ============================================================
# ============================================================

# modelo Bivariado para las minimas
datos.biva <- list(estancuela = select(datos,-estancuela), paysandu = select(datos.pay, -paysandu) ) %>% 
  bind_rows(.id = 'location')

yy.biva <- datos.biva %>% filter(serie == 'Tn') %>% select(location, temp, fecha) %>%
  spread(location, temp) %>% select(-fecha) %>% ts(start = c(1950, 1), frequency = 365) 
  

# the 'base' univariate model
uni <- dlmModPoly()

# to get the matrices of the correct dim
temps_mod <- uni %+% uni 

## now redefine matrices to keep levels together and slopes together
FF(temps_mod) <- FF(uni) %x% diag(2)
GG(temps_mod) <- GG(uni) %x% diag(2)
W(temps_mod)[] <- 0 # 'clean' the system variance

## define a build function for MLE
buildSUTSE <- function(psi) {
  U <- matrix(0, nrow = 2, ncol = 2)
  U[upper.tri(U)] <- psi[1:2]
  diag(U) <- exp(0.5 * psi[3:4])
  W(temps_mod)[3:4, 3:4] <- crossprod(U)
  diag(V(temps_mod)) <- exp(0.5 * psi[5:6])
  temps_mod
}

## estimation
temps_est <- dlmMLE(yy.biva[(10*365):(14*365),], rep(-2, 9), buildSUTSE,
                    control = list(maxit = 500)) ## few iters for test

## check convergence
print(temps_est$conv)

## set up fitted model
temps_mod <- buildSUTSE(temps_est$par)

## look at the estimated variance / covariance matrices
print(W(temps_mod)[2:3, 2:3])
print(cov2cor(W(temps_mod)[2:3, 2:3])) # slopes
print(sqrt(diag(V(temps_mod)))) # observation standard deviations

library(stringr)
## smooths
tempsSmooth <- dlmSmooth(yy.biva[(10*365):(14*365),], temps_mod)

 data_frame(yhat.estancuela = apply(tempsSmooth$s[-1, c(1,3)],1,sum),
            yhat.paysandu = apply(tempsSmooth$s[-1, c(2,4)],1,sum) ) %>% 
   bind_cols(data.frame(yy.biva[(10*365):(14*365),]) ) %>% 
   mutate(time = time(yy.biva)[(10*365):(14*365)]) %>% 
   gather(serie, vv, -time) %>% 
   mutate( location = factor( str_detect(serie, 'paysandu'), labels = c('est', 'pay') ), 
           tipo = factor(str_detect(serie, 'yhat'), labels=c('obs', 'yhat')  ) ) %>% 
   ggplot() + geom_line(aes(time, vv, color = tipo)) + facet_grid(location ~ .)



  
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

                       