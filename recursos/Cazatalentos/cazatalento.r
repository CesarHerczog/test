#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


GLOBAL_jugadores  <- c()

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------


#Nueva Estrategia
#En la primera ronda se tiran x(20) tiros, se elimina a los que están por debajo del promedio entre la media y el mínimo.
#En la segunda ronda se tiran x(40) tiros más, se elimina a los que están por debajo del promedio entre la media y el mínimo.
#En la segunda ronda se tiran x(80) tiros más, se elimina a los que están por debajo del promedio entre la media y el mínimo.





#En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 90000 tiros )
#Se eligen a la mejor mitad  ( se descarta a la peor mitad )
#En la segunda ronda, a la mejor mitad de la anterior ronda se los hace tirar 400 tiros a cada uno
#Se elige el mejor jugador de la segunda ronda

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )


for( ultima_ronda in c(20, 50, 100) ){
  
  #variables globales que usan las funciones gimnasio_xxxx
  tasa_de_aciertos  <- 0
  GLOBAL_tiros_total  <- 0
  for( i in 1:10000 ) { #diez mil experimentos
    
    #vaciertos  <- mapply( ftirar, jugadores, tiros_libres )
    #mejor  <- which.max( vaciertos )
    
    #Ronda 1  ------------------------------------------------------
    #tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
    ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
    
    planilla_cazatalentos[ ids_juegan1,  tiros1 := 100 ]  #registro en la planilla que tiran 90 tiros
    
    #Hago que tiren
    resultado1  <- gimnasio_tirar( ids_juegan1, 100)
    planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
    
    
    #Ronda 2 -------------------------------------------------------
    #los mejores 40 jugadores tiran x tiros cada uno
    minmed  <- planilla_cazatalentos[ ids_juegan1, (min(aciertos1)+median(aciertos1))/2 ]
    ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 > minmed, id ]
    
    planilla_cazatalentos[ ids_juegan2,  tiros2 := 20 ]  #registro en la planilla que tiran x tiros
    resultado2  <- gimnasio_tirar( ids_juegan2, 20)
    planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla lo resultados de la ronda 2
    planilla_cazatalentos[ ids_juegan2,  acumulado2 := (aciertos1+aciertos2) ] #acumulo los resultados
    
    #Ronda 3 -------------------------------------------------------
    #los mejores 40 jugadores tiran x tiros cada uno
    
    minmed  <- planilla_cazatalentos[ ids_juegan2, (min(acumulado2)+median(acumulado2))/2 ]
    ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][acumulado2 > minmed, id ]
    
    
    planilla_cazatalentos[ ids_juegan3,  tiros3 := 20 ]  #registro en la planilla que tiran x tiros
    resultado3  <- gimnasio_tirar( ids_juegan3, 20)
    planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla la suma de la ronda uno y la dos
    planilla_cazatalentos[ ids_juegan3,  acumulado3 := (acumulado2+aciertos3) ]
    
    #Ronda 4 -------------------------------------------------------
    #los mejores 40 jugadores tiran x tiros cada uno
    
    
    minmed  <- planilla_cazatalentos[ ids_juegan3, (min(acumulado3)+median(acumulado3))/2 ]
    ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][acumulado3 > minmed, id ]
    
    planilla_cazatalentos[ ids_juegan4,  tiros4 := 10 ]  #registro en la planilla que tiran x tiros
    resultado3  <- gimnasio_tirar( ids_juegan4, 10)
    planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado3 ]  #registro en la planilla la suma de la ronda uno y la dos
    planilla_cazatalentos[ ids_juegan4,  acumulado4 := (acumulado2+aciertos3) ]
    
    
    
    #El cazatalentos toma una decision, elige al que mas aciertos tuvo en el acumulado
    pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado4) ] #Selecciono el mejor del acumulado
    id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
    
    #Finalmente, la hora de la verdadero_mejor
    #Termino el juego
    veredicto  <- gimnasio_veredicto( id_mejor )
    
    veredicto[2]
    
    
    #El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
    #Le acerto al verdadero_mejor
    
    #En el siguiente script veremos de hacer una Estimacion Montecarlo
    #De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta

    if( veredicto[2] == 1 )  tasa_de_aciertos  <- tasa_de_aciertos + 1
  }
  
  cat( ultima_ronda, "\t", tasa_de_aciertos/100, "\n" )
}
