# Detecta días con una cantidad inusual de transacciones
# utilizando una ventana móvil de días anteriores.
#
# Recibe:
#   df: base de transacciones con las columnas fecha e id_tx.
#   ventana: cantidad de días anteriores.
#   k.alerta: factor multiplicador de la desviación estándar.
#
# Devuelve:
#   Un data.frame con las columnas:
#   fecha, n.tx, media, desv y alerta.

alerta.movil <- function(df, ventana, k.alerta) {
  
  # Contar las transacciones por fecha
  conteo <- aggregate(
    id_tx ~ fecha,
    data = df,
    FUN = length
  )
  
  names(conteo)[2] <- "n.tx"
  
  # Ordenar las fechas
  conteo$fecha <- as.Date(conteo$fecha)
  conteo <- conteo[order(conteo$fecha), ]
  
  # Completar los días sin transacciones
  fechas <- data.frame(
    fecha = seq(
      min(conteo$fecha),
      max(conteo$fecha),
      by = "day"
    )
  )
  
  conteo <- merge(
    fechas,
    conteo,
    by = "fecha",
    all.x = TRUE
  )
  
  conteo$n.tx[is.na(conteo$n.tx)] <- 0
  
  # Crear columnas de resultados
  n <- nrow(conteo)
  
  conteo$media <- NA_real_
  conteo$desv <- NA_real_
  conteo$alerta <- NA
  
  # Calcular utilizando únicamente los días anteriores
  if (n > ventana) {
    
    for (i in seq.int(ventana + 1, n)) {
      
      anteriores <- conteo$n.tx[
        (i - ventana):(i - 1)
      ]
      
      conteo$media[i] <- mean(anteriores)
      conteo$desv[i] <- sd(anteriores)
      
      conteo$alerta[i] <-
        conteo$n.tx[i] >
        conteo$media[i] + k.alerta * conteo$desv[i]
    }
  }
  
  # Devolver las columnas solicitadas
  resultado <- conteo[, c(
    "fecha",
    "n.tx",
    "media",
    "desv",
    "alerta"
  )]
  
  return(resultado)
}