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


# ------------------------------------------------------------------------------
# Ejercicio 2: Ley de Benford por canton
#
# Funcion: benford.mad()
# Calcula el MAD comparando los primeros digitos de los montos
# con las proporciones esperadas segun la Ley de Benford.
#
# Parametros:
# x: vector con los montos de las transacciones.
#
# Retorna:
# Un valor numerico con el MAD calculado.
# ------------------------------------------------------------------------------
# Ejercicio 2A
benford.mad <- function(x) {
  
  # Obtener el primer dígito de cada monto
  primer.digito <- as.numeric(
    substr(as.character(abs(x)), 1, 1)
  )
  
  # Calcular proporciones observadas
  proporciones <- prop.table(
    table(factor(primer.digito, levels = 1:9))
  )
  
  # Proporciones esperadas según Benford
  benford <- log10(1 + 1 / (1:9))
  
  # Calcular MAD
  mad <- mean(abs(proporciones - benford))
  
  return(mad)
}

# Ejercicio 2B
benford.canton <- function(df, canal, cantones) {
  
  # Filtrar transacciones del canal seleccionado
  transacciones.canal <- df[
    df$canal == canal,
  ]
  
  # Obtener los codigos de canton disponibles
  codigos <- unique(transacciones.canal$cod_canton)
  
  # Calcular MAD para cada canton
  mad.cantones <- do.call(
    rbind,
    lapply(codigos, function(codigo) {
      
      datos.canton <- transacciones.canal[
        transacciones.canal$cod_canton == codigo,
      ]
      
      data.frame(
        cod_canton = codigo,
        MAD = benford.mad(datos.canton$monto_crc)
      )
    })
  )
  
  # Incorporar los nombres de los cantones
  resultado <- merge(
    cantones,
    mad.cantones,
    by = "cod_canton"
  )
  
  # Ordenar de mayor a menor MAD
  resultado <- resultado[
    order(resultado$MAD, decreasing = TRUE),
  ]
  
  rownames(resultado) <- NULL
  
  return(resultado)
}