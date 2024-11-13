-- Reservas_Haskell.hs

-- Representa una reserva con el precio de la misma, la fecha y el ID del cliente.
data Reserva = Reserva {
    precio :: Float,
    fecha :: (Int, Int, Int),  -- (Año, Mes, Día)
    clienteID :: Int
} deriving (Show)

-- Representa un cliente con su nombre y una lista de sus reservas.
data Cliente = Cliente {
    nombre :: String,
    reservas :: [Reserva]
} deriving (Show)

-- Función para verificar si una fecha está dentro de un rango.
enRango :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Bool
enRango (año, mes, día) (añoInicio, mesInicio, díaInicio) (añoFin, mesFin, díaFin) =
    (año, mes, día) >= (añoInicio, mesInicio, díaInicio) && (año, mes, día) <= (añoFin, mesFin, díaFin)

-- Calcula el total de ingresos en un período determinado.
ingresosEnPeriodo :: [Reserva] -> (Int, Int, Int) -> (Int, Int, Int) -> Float
ingresosEnPeriodo reservas fechaInicio fechaFin = 
    sum [precio r | r <- reservas, enRango (fecha r) fechaInicio fechaFin]

-- Filtra los clientes que hicieron más de 5 reservas en el último año
clientesFrecuentes :: [Cliente] -> Int -> [String]
clientesFrecuentes [] _ = []
clientesFrecuentes (c:cs) añoActual = 
    let reservasAnuales = filter (\r -> let (año, _, _) = fecha r in año == añoActual) (reservas c)
    in if length reservasAnuales > 5 
       then nombre c : clientesFrecuentes cs añoActual
       else clientesFrecuentes cs añoActual

-- Aplica un descuento a todas las reservas de un cliente en particular
aplicarDescuento :: Cliente -> Float -> Cliente
aplicarDescuento cliente descuento = 
    cliente { reservas = map (\r -> r { precio = precio r * (1 - descuento) }) (reservas cliente) }

-- Calcula la ocupación promedio de cabañas en un mes específico
ocupacionPromedio :: [Reserva] -> Int -> Int -> Float
ocupacionPromedio reservas año mes = 
    let reservasMes = filter (\r -> let (a, m, _) = fecha r in a == año && m == mes) reservas
        diasOcupados = length reservasMes  -- Número de días con ocupación
    in fromIntegral diasOcupados / 30  -- Asumimos que el mes tiene 30 días para simplificar
