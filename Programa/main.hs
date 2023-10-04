import System.IO
import System.Directory (doesPathExist)
import Control.Exception
import System.Directory
import Control.Concurrent
import Text.Read (readMaybe)
import Data.IORef
import Data.Char (toUpper)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sortOn)
import Data.Function (on)
import Control.DeepSeq
import Data.List (sortBy)

--C://Users//fredd//OneDrive//Documentos//TEC/LENGUAJES//PP2_Fredd_Randall//Programa//

rutaExiste ruta = doesPathExist ruta


leerArchivo ruta = do
    existe <- rutaExiste ruta
    if existe then do 
        contenido <- readFile ruta
        let lineas = lines contenido
        return lineas
    else do
        putStrLn("\nERROR -> La ruta ingresada es invalida")
        return []

añadirEnArchivo ruta nuevoCont = do
    existe <- rutaExiste ruta
    if existe then do
        appendFile ruta nuevoCont
        putStrLn("\nAVISO -> Informacion agregada con exito")
    else 
        putStrLn("\nERROR -> La ruta ingresada es invalida")

sobreEscribirEnArchivo ruta nuevoCont = do
    existe <- rutaExiste ruta
    if existe then do
        writeFile ruta nuevoCont
        putStrLn("\nAVISO -> Informacion sobreescrita con exito")
    else 
        putStrLn("\nERROR -> La ruta ingresada es invalida")

split [] token = [token]
split (x:xs) token
  | x == ',' = token : split xs "" 
  | otherwise = split xs (token ++ [x])

getIndicesParqueos parqueos result = do
    let parqueo = split (head parqueos) ""
    
    if null parqueos then 
        return result
    else getIndicesParqueos (tail parqueos) (result ++ [(parqueo !! 0)])


esEntero str =
    case reads str :: [(Int, String)] of
        [(numero, "")] -> True  
        _              -> False 

validarParqueosAux parqueos = do
    let provincias = ["LI", "PU", "GU", "SJ", "AL", "CA", "HE"]
    let provincia = parqueos !! 3
    let id = parqueos !! 0
    let posX = parqueos !! 4
    let posY = parqueos !! 5
    contenido <- leerArchivo "parqueos.txt"
    idsActuales <- getIndicesParqueos contenido []

    if length parqueos /= 6 then do
        putStrLn("\nERROR -> Un elemento del archivo no contiene la cantidad necesaria de atributos")
        return 0
    else if notElem provincia provincias then do
        putStrLn("\nERRRO -> Un elemento del archivo contiene una provincia invalida")
        return 0
    else if not (esEntero id) && not (esEntero posX) && not (esEntero posY) then do
        putStrLn("\nERROR -> Las coordenas o id de un elemento no es un numero entero")
        return 0
    else if not (notElem id idsActuales) then do
        putStrLn("\nERROR -> Un elemento del archivo posee un id ya existente")
        return 0
    else
        return 1

cargarParqueos = do 
    putStrLn("\nIngresa la ruta de tu archivo")
    input <- getLine
    contenido <- leerArchivo input
    if length contenido /= 0 then
        return contenido
    else 
        return []

cargarParqueosAux [] = return 1
cargarParqueosAux parqueos = do
    let parqueo = split (head parqueos) ""
    estaBien <- validarParqueosAux (parqueo)

    if estaBien == 1 then
        cargarParqueosAux (tail parqueos)
    else 
        return 0

toStringParks [] = return ""
toStringParks parqueos = do
    let parqueo = split (head parqueos) ""
    let texto = "\nID: " ++ (parqueo !! 0) ++ "| Nombre: " ++ (parqueo !! 1) ++ "| Ubicacion: " ++ (parqueo !! 2) ++ "| Provincia: " ++ (parqueo !! 3) ++ "| Coordenadas: X->" ++ (parqueo !! 4) ++ " Y->" ++ (parqueo !! 5)

    resto <- toStringParks (tail parqueos)
    return (texto ++ resto)

pegarElementos [] count = return ""
pegarElementos parqueos count = do
    let parqueo = (head parqueos)

    if count == 0 then do
        resto <- pegarElementos (tail parqueos) (count + 1)
        return ("\n" ++ parqueo ++ "\n" ++ resto)
    else if (tail parqueos) /= [] then do
        resto <- pegarElementos (tail parqueos) (count + 1)
        return (parqueo ++ "\n" ++ resto)
    else do
        resto <- pegarElementos (tail parqueos) (count + 1)
        return (parqueo ++ resto)

pegarBicis [] count = return ""
pegarBicis parqueos count = do
    let parqueo = (head parqueos)

    if count == 0 then do
        resto <- pegarBicis (tail parqueos) (count + 1)
        return ("\n" ++ parqueo ++ ",1" ++ "\n" ++ resto)
    else if not (null (tail parqueos)) then do
        resto <- pegarBicis (tail parqueos) (count + 1)
        return (parqueo ++ ",1" ++ "\n" ++ resto)
    else do
        resto <- pegarBicis (tail parqueos) (count + 1)
        return (parqueo ++ ",1" ++ resto)

cargarYmostrarParqueos = do
    parqueos <- cargarParqueos
    parqueosVali <- cargarParqueosAux parqueos 

    if not (null parqueos) && parqueosVali == 1 then do
        let cadena = pegarElementos parqueos 0
        añadirEnArchivo "parqueos.txt" (cadena !! 0)
        parqueosNew <- leerArchivo "parqueos.txt"
        let cadenaNueva  = toStringParks parqueosNew
        putStrLn((cadenaNueva !! 0))
    else do
        putStrLn "\nEstos son los parqueos sin cargar valores nuevos"
        parqueosNew <- leerArchivo "parqueos.txt"
        let cadenaNueva  = toStringParks parqueosNew
        putStrLn((cadenaNueva !! 0))

removeChars :: Char -> String -> String
removeChars simbolo cadena = filter (\c -> c /= simbolo) cadena

validarUsuariosAux :: [String] -> IO Int
validarUsuariosAux usuario = do
    contenido <- leerArchivo "usuarios.txt"
    cedulasActu <- getIndicesParqueos contenido []

    if length primerElemento /= 9 then do
        putStrLn "\nLa longitud de usuario no es 9"
        return 0
    else if (primerElemento !! 1) /= '-' || (primerElemento !! 5) /= '-' then do
        putStrLn "\nEl primer elemento no cumple con el formato esperado"
        return 0
    else if length usuario /= 2 then do
        putStrLn "\nEl usuario solo debe contener cedula y nombre"
        return 0
    else if not estaBien then do
        putStrLn "\nLa cedula solo debe contener números"
        return 0
    else if (notElem (head primerElemento) digitos) then do
        putStrLn "\nEl primer digito de la cedula debe estar en el rango de 1-7"
        return 0
    else if not (notElem (primerElemento) cedulasActu) then do
        putStrLn "\nLa cedula de un usuario ya existe"
        return 0
    else do
        return 1
    where
        primerElemento = (usuario !! 0)
        digitos = ['1', '2', '3', '4', '5', '6', '7']
        cadena = removeChars '-' primerElemento
        estaBien = esEntero cadena


validarUsuarios :: [String] -> IO Int
validarUsuarios [] = return 1
validarUsuarios (primerUsuario:restoUsuarios) = do
    let usuario = split primerUsuario ""
    estaBien <- validarUsuariosAux usuario
    if estaBien == 1
        then validarUsuarios restoUsuarios
        else return 0


cargarUsuarios = do
    putStrLn("\nIngrese su archivo de usuarios: ")
    input <- getLine
    usuarios <- leerArchivo input
    estaBien <- validarUsuarios usuarios

    if not (null usuarios) && estaBien == 1 then do
        let cadena = pegarElementos usuarios 0
        añadirEnArchivo "usuarios.txt" (cadena !! 0)
        putStrLn("\nUsuarios añadidos con exito!!")
    else 
        putStrLn("\nUsuarios no añadidos, intentalo de nuevo")

validarBicisAux bici idsNuevos = do
    bicisActuales <- leerArchivo "bicicletas.txt"
    parqueosActuales <- leerArchivo "parqueos.txt"
    idsBicisAct <- getIndicesParqueos bicisActuales []
    idsParqueosAct <- getIndicesParqueos parqueosActuales []

    if length bici /= 2 then do
        putStrLn("\nERROR -> Una bici no contiene la cantidad parametros adecuada")
        return 0
    else if notElem (head bici) idsBicisAct then do
        putStrLn("\nERROR -> Una bici posee un ID inexistente")
        return 0
    else if notElem (bici !! 1) idsParqueosAct then do
        putStrLn("\nERROR -> Una bici posee un parqueo inexistente")
        return 0
    else if length (filter (== (head bici)) idsNuevos) > 1 then do 
        putStrLn("\nERROR -> Hay un ID repetido en esta lista")
        return 0
    else
        return 1


validarBicis [] idsNuevos = return 1
validarBicis bicis idsNuevos = do
    let bici = split (head bicis) ""
    estaBien <- validarBicisAux (bici) idsNuevos

    if estaBien == 1 then
        validarBicis (tail bicis) idsNuevos
    else 
        return 0


actualizarInfoBicis dato bicisActuales = do
    let biciVieja = split (head bicisActuales) ""  
    let parqueoBV = (biciVieja !! 2)
    let parqueoBN = (dato !! 1)
    let idBV = (head biciVieja)
    let idBN = (head dato)
    let biciNueva = [idBV ++ "," ++ (biciVieja !! 1) ++ "," ++ parqueoBN]

    if idBN == idBV && parqueoBV /= "transito" then do
        return (biciNueva ++ (tail bicisActuales))
    else if idBN == idBV && parqueoBV == "transito" then do
        putStrLn ("\nLa bici con el identificador " ++ idBV ++ " se encuentra en tránsito")
        return (bicisActuales)
    else do
        restante <- actualizarInfoBicis dato (tail bicisActuales)
        return ([(head bicisActuales)] ++ restante)

actualizarBicisAux [] bicisActu = return bicisActu 
actualizarBicisAux datosNuevos bicisActuales = do 
    let dato = split (head datosNuevos) ""
    bicisActu <- actualizarInfoBicis dato bicisActuales
    actualizarBicisAux (tail datosNuevos) bicisActu


actualizarBicis = do
    putStrLn("\nIngrese la ruta de su archivo con los nuevos datos:")
    ruta <- getLine
    contenido <- leerArchivo ruta
    contenido2 <- leerArchivo "bicicletas.txt"  
    idsNuevos <- getIndicesParqueos contenido []
    bicisValidadas <- validarBicis contenido idsNuevos

    if bicisValidadas == 1 then do
        nuevasBicis <- actualizarBicisAux contenido contenido2
        let cadena = pegarElementos nuevasBicis 0
        sobreEscribirEnArchivo "bicicletasAux.txt" (cadena !! 0)
        putStrLn("\nBicis actualizadas con exito")
        return 1
    else do
        putStrLn("\nDebido a incosistencias con los datos nuevos, no se actualizo ninguna bici")
        return 0


validarBicisNuevasAux bici = do
    bicisActuales <- leerArchivo "bicicletas.txt"
    idsBicisAct <- getIndicesParqueos bicisActuales []

    if length bici /= 2 then do
        putStrLn("\nERROR -> Una bici no contiene la cantidad parametros adecuada")
        return 0
    else if elem (head bici) idsBicisAct then do
        putStrLn("\nERROR -> Una bici posee un ID existen")
        return 0
    else if head (head bici) /= 'B' then do 
        putStrLn("\nERROR -> El primer elemento de un ID de una bici debe ser una ->B<-")
        return 0
    else if not (esEntero (tail (head bici))) then do 
        putStrLn("\nERROR -> Despues de la B, deben ir numeros enteros")
        return 0
    else if (bici !! 1) /= "TR" && (bici !! 1) /= "AG" && (bici !! 1) /= "AE" then do 
        putStrLn("\nERROR -> Una bici posee un tipo incorrecto de gasolina")
        return 0
    else
        return 1


validarBicisNuevas [] = return 1
validarBicisNuevas bicis = do
    let bici = split (head bicis) ""
    estaBien <- validarBicisNuevasAux (bici) 

    if estaBien == 1 then
        validarBicisNuevas (tail bicis)
    else 
        return 0


cargarBicicletas = do
    putStrLn("\nIngrese la ruta de su archivo con las bicicletas:")
    ruta <- getLine
    contenido <- leerArchivo ruta
    estaBien <- validarBicisNuevas contenido

    if estaBien == 1 then do
        let cadena = pegarBicis contenido 0
        añadirEnArchivo "bicicletas.txt" (cadena !! 0)
        putStrLn("\nBicis añadidas con exito!!")
    else 
        putStrLn("\nDebido a incosistencias con los datos nuevos, no se guardo ninguna bici")



--Lista de parqueos -> id del mas cercano -> posicionActual -> Distancia menor Asociada al Id -> cordenadax -> cordenaday
parqueoCercano :: [String] -> Int -> Int -> Double -> Int -> Int -> Int
parqueoCercano lista id pos distancia x y =
    if null lista
    then id
    else do
        --extraemos la primera posicion.
        let temp = split (head lista) ""
        --sacamos el resto.
        let rest = tail lista
        --id posicion
        let idStr = temp !! 0
        --posicion del parqueo.
        let xStr = temp !! 4
        let yStr = temp !! 5
        --string->int
        let xTemp = read xStr :: Int
        let yTemp = read yStr :: Int
        let idTemp = read idStr :: Int
        --Calcular la distancia.
        let distTemp = sqrt (fromIntegral ((xTemp - x) ^ 2 + (yTemp - y) ^ 2))
        if distancia == 0 || distTemp < distancia then do
            parqueoCercano rest idTemp (pos + 1) distTemp x y
        else do
            parqueoCercano rest id (pos + 1) distancia x y

--Bicicletas asociadas -> id del parqueo. -> 
bicicletasAsociadas :: [String] -> Int -> IO()
bicicletasAsociadas listaB id =
    if null listaB
    then putStrLn ("")
    else do
        --extraemos la primera posicion.
        let temp = split (head listaB) ""
        --sacamos el resto.
        let rest = tail listaB
        --parqueoAsociado
        let parqueoStr = temp !! 2
        --int->String
        let parqueoStrId = show id

        if parqueoStr == parqueoStrId then do
            putStrLn ("Codigo de la bicicleta: " ++ (temp!!0) ++ "  Tipo: " ++ (temp!!1) ++ " Id del parqueo actual " ++ (temp!!2))
            bicicletasAsociadas rest id
        else do
            bicicletasAsociadas rest id

--Bicicletas asociadas en forma de lista-> id del parqueo. -> 
bicicletasAsociadasList :: [String] -> Int -> [String] -> [String]
bicicletasAsociadasList listaB id res = do
    if null listaB
    then res
    else do
        --extraemos la primera posicion.
        let temp = split (head listaB) ""
        --sacamos el resto.
        let rest = tail listaB
        --parqueoAsociado
        let parqueoStr = temp !! 2
        --int->String
        let parqueoStrId = show id

        if parqueoStr == parqueoStrId then do
            bicicletasAsociadasList rest id (res ++ temp) 
        else do
            bicicletasAsociadasList rest id res


--modificarArchivo Lista a modificar -> codigo a modificar -> texto sustituir -> resultado
modificarArchivo :: [String] -> String -> String -> String -> IO()
modificarArchivo lista codigo texto res =
    if null lista then
        sobreEscribirEnArchivo "bicicletas.txt" (take (length res - 1) res)
    else do
        --extraemos la primera posicion.
        let temp = split (head lista) ""
        --sacamos el resto.
        let rest = tail lista
        --Id de la bicicleta.
        let idBici = temp !! 0

        let line = temp !! 0  ++ "," ++ temp !! 1  ++ "," ++ temp !! 2 ++ "\n"
        let lineMod = temp !! 0 ++ "," ++ temp !! 1 ++ "," ++ texto ++ "\n"

        if codigo == idBici then do
            modificarArchivo rest codigo texto (res ++ lineMod)
        else do
            modificarArchivo rest codigo texto (res ++ line)


--Se encarga de validar que un string sea un entero.
validaEnteros :: String -> Int
validaEnteros input =
    case readMaybe input :: Maybe Int of
        Just numero -> numero
        Nothing     -> -1


--ExisteCedula
existeCedula :: [String] -> String -> Int
existeCedula usuarios cedulaB =
    if null usuarios then -1
    else do
        -- Extraemos la primera posición.
        let temp = split (head usuarios) ""
        -- Sacamos el resto de la lista.
        let rest = tail usuarios
        -- Cédula del usuario actual.
        let cedula = temp !! 0
        if cedula == cedulaB then 1
        else existeCedula rest cedulaB

--ExisteParqueo
existeParqueo :: [String] -> String -> Int
existeParqueo parqueos id =
    if null parqueos then -1
    else do
        -- Extraemos la primera posición.
        let temp = split (head parqueos) ""
        -- Sacamos el resto de la lista.
        let rest = tail parqueos
        -- Cédula del usuario actual.
        let idPos = temp !! 0
        if idPos == id then 1
        else existeParqueo rest id

--ExisteBicicleta
existeBicicleta :: [String] -> String -> Int
existeBicicleta bicicletas codigo =
    if null bicicletas then -1
    else do
        -- Extraemos la primera posición.
        let temp = split (head bicicletas) ""
        -- Sacamos el resto de la lista.
        let rest = tail bicicletas
        -- Cédula del usuario actual.
        let codigoPos = temp !! 0
        if codigoPos == codigo then 1
        else existeBicicleta rest codigo

--ExisteAlquiler
existeAlquiler :: [String] -> String -> Int
existeAlquiler alquileres id =
    if null alquileres then -1
    else do
        -- Extraemos la primera posición.
        let temp = split (head alquileres) ""
        -- Sacamos el resto de la lista.
        let rest = tail alquileres
        -- Cédula del usuario actual.
        let idTemp = temp !! 0
        let activo = temp !! 4
        if idTemp == id && activo == "Activo" then 1
        else existeAlquiler rest id

--
menuConsultarBicicleta = do
    parqueos <- leerArchivo "parqueos.txt"
    bicicletas <- leerArchivo "bicicletas.txt"
    --pedir las cordenadas.
    putStrLn "Ingrese la cordenada x:"
    cordx <- getLine
    putStrLn "Ingrese la cordenada y:"
    cordy <- getLine
    --Convertir los valores a enteros.
    let x = validaEnteros cordx
    let y = validaEnteros cordy
    if x == -1 || y == -1 then do
        putStrLn "¡Error: Las cordenadas ingresadas deben ser enteros."
    else do
        --buscar con la formula al mas cercano.
        let id = parqueoCercano parqueos 0 0 0 x y
        let p1 = extraerParqueo parqueos (show id)
        
        putStrLn ("\nEl parqueo mas cercano es: " ++ (p1 !! 1))
        putStrLn "                     "
        --
        putStrLn "======================bicicletas asociadas======================"
        bicicletasAsociadas bicicletas id
        putStrLn "======================bicicletas asociadas======================"


validarCedula cedula = do 
    bracket
        (openFile "usuarios.txt" ReadMode) 
        hClose 
        (\handle -> do
            contenido <- hGetContents handle
            let lineas = lines contenido 
            let cedulaValida = existeCedula lineas cedula
            if cedulaValida == -1 then do
                putStrLn "Error, la cedula no existe.\n"
                return ""
            else do
                putStrLn ""
                return "1"
        )
        
validarParqueo parqueoId = do
    bracket
        (openFile "parqueos.txt" ReadMode) 
        hClose 
        (\handle -> do
            contenido <- hGetContents handle
            let lineas = lines contenido 
            let parqueoValido = existeParqueo lineas parqueoId
            if parqueoValido == -1 then do
                putStrLn "Error, el parqueo no existe.\n"
                return ""
            else do
                putStrLn ""
                return "1"
        )

validarBicicleta parqueo = do 
    bracket
        (openFile "bicicletas.txt" ReadMode) -- Abre el archivo en modo lectura
        hClose -- Cierra el archivo al final
        (\handle -> do
            contenido <- hGetContents handle
            let lineas = lines contenido -- Convierte el contenido en una lista de líneas
            let id = validaEnteros parqueo
            if id == -1 then do 
                putStrLn "El id del parqueo debe ser un numero\n"
                return ""
            else do
                putStrLn "======================bicicletas asociadas======================"
                putStrLn "============== En caso de no existir ingrese '-' ===============\n"
                bicicletasAsociadas lineas id
                putStrLn "======================bicicletas asociadas======================"

                let asoc = bicicletasAsociadasList lineas id []
                putStrLn "Ingrese el codigo de la bicicleta:"
                codigo <- getLine

                let esta = elem codigo asoc -- True

                if esta then do
                    if codigo /= "-" then do
                        let bicicletaValida = existeBicicleta lineas codigo
                        if bicicletaValida == -1 then do
                            return ""
                        else do
                            return codigo
                    else do
                        putStrLn "\nNo se hayaron bicicletas disponibles para este parqueo, pruebe con otro."
                        return ""
                else do
                    putStrLn "\nEl codigo ingresado no pertenece a este parqueo."
                    return ""
        )

--
validarAlquiler id = do
    bracket
        (openFile "alquileres.txt" ReadMode) 
        hClose 
        (\handle -> do
            contenido <- hGetContents handle
            let lineas = lines contenido 
            let alquilerValido = existeAlquiler lineas id
            if alquilerValido == -1 then do
                putStrLn "Error, el alquiler no existe.\n"
                return ""
            else do
                putStrLn ""
                return "1"
        )


copiarArchivo name name2 = do
    fromHandle <- getAndOpenFile name ReadMode
    toHandle <- getAndOpenFile name2 WriteMode
    contents <- hGetContents fromHandle
    hPutStr toHandle contents
    hClose toHandle
    putStrLn "Done."

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile name mode = do
    openFile name mode
  

menuAlquiler = do
    putStrLn "Ingrese la cedula:"
    cedula <- getLine
    validc <- validarCedula cedula

    if validc /= "" then do
        putStrLn "Ingrese el parqueo de inicio:"
        parqueo1 <- getLine
        validp1 <- validarParqueo parqueo1

        putStrLn "Ingrese el parqueo de llegada:"
        parqueo2 <- getLine
        validp2 <- validarParqueo parqueo2

        if validp1 /= "" && validp2 /= "" && parqueo1 /= parqueo2 then do
            codigoBicicleta <- validarBicicleta parqueo1
            ---------------------------------
            let archivo = "alquileres.txt" 
            resultado <- getFileSize archivo
            let largo = resultado
            -------------------------------
            if codigoBicicleta /= "" then do
                let cadena = "\n" ++ show largo ++ "," ++ parqueo1 ++ "," ++ parqueo2 ++ "," ++ codigoBicicleta ++ ",Activo," ++ cedula 
                añadirEnArchivo "alquileres.txt" cadena
                bicicletas<-leerArchivo "bicicletas.txt"
                modificarArchivo bicicletas codigoBicicleta "transito" ""
                putStrLn $ "Transaccion completada, Id del alquiler: " ++ show largo
            else do
                putStrLn "Codigo de bicicleta invalido."
        else do
            putStrLn "Error: ingrese parqueos validos y diferentes."
    else 
        putStrLn "Error: la cedula no existe."


extraerAlquiler :: [String] -> String -> [String]
extraerAlquiler alquileres id =
    if null alquileres then []
    else do
        -- Extraemos la primera posición.
        let temp = split (head alquileres) ""
        -- Sacamos el resto de la lista.
        let rest = tail alquileres
        let idTemp = temp !! 0
        let activo = temp !! 4
        if idTemp == id && activo == "Activo" then temp
        else extraerAlquiler rest id

extraerParqueo :: [String] -> String -> [String]
extraerParqueo parqueos id =
    if null parqueos then []
    else do
        -- Extraemos la primera posición.
        let temp = split (head parqueos) ""
        -- Sacamos el resto de la lista.
        let rest = tail parqueos
        let idTemp = temp !! 0
        if idTemp == id then temp
        else extraerParqueo rest id

extraerUsuario :: [String] -> String -> [String]
extraerUsuario usuarios id =
    if null usuarios then []
    else do
        -- Extraemos la primera posición.
        let temp = split (head usuarios) ""
        -- Sacamos el resto de la lista.
        let rest = tail usuarios
        -- Cédula del usuario actual.
        let idTemp = temp !! 0
        if idTemp == id then temp
        else extraerUsuario rest id

extraerBicicleta :: [String] -> String -> [String]
extraerBicicleta bicicletas codigo =
    if null bicicletas then []
    else do
        -- Extraemos la primera posición.
        let temp = split (head bicicletas) ""
        -- Sacamos el resto de la lista.
        let rest = tail bicicletas
        -- Cédula del usuario actual.
        let codigoTemp = temp !! 0
        if codigoTemp == codigo then temp
        else extraerBicicleta rest codigo

--modificarArchivo Lista a modificar -> id -> texto sustituir -> resultado
modificarArchivoAlq :: [String] -> String -> String -> String -> IO()
modificarArchivoAlq lista id texto res =
    if null lista then
        sobreEscribirEnArchivo "alquileres.txt" (take (length res - 1) res)
    else do
        --extraemos la primera posicion.
        let temp = split (head lista) ""
        --sacamos el resto.
        let rest = tail lista
        --Id de la bicicleta.
        let idAlq = temp !! 0
        let line = temp !! 0  ++ "," ++ temp !! 1  ++ "," ++ temp !! 2 ++ "," ++ temp !! 3 ++ "," ++ temp !! 4 ++ "," ++ temp !! 5 ++"\n"
        let lineMod = temp !! 0  ++ "," ++ temp !! 1  ++ "," ++ temp !! 2 ++ "," ++ temp !! 3 ++ "," ++ texto ++ "," ++ temp !! 5 ++"\n"

        if id == idAlq then do
            modificarArchivoAlq rest id texto (res ++ lineMod)
        else do
            modificarArchivoAlq rest id texto (res ++ line)


calcularTarifa tipo = do
    if tipo == "TR" then 35
    else if tipo == "AE" then 50
    else 75

--MenuFacturar
menuFacturar = do
    putStrLn "Ingrese el ID del alquiler:"
    idAlquiler <- getLine

    validAlq <- validarAlquiler idAlquiler
    --Validamos que exista
    if validAlq /= "" then do
        infoComer <- leerArchivo "infoComer.txt"
        alquileres <- leerArchivo "alquileres.txt"
        parqueos <- leerArchivo "parqueos.txt"
        bicicletas <- leerArchivo "bicicletas.txt"
        usuarios <- leerArchivo "usuarios.txt"

        let datos = extraerAlquiler alquileres idAlquiler
        let codigoBici = datos !! 3
        let idParqueoInit = datos !! 1
        let idParqueoEnd = datos !! 2
        let usuario = datos !! 5

        let p1 = extraerParqueo  parqueos idParqueoInit
        let p2 = extraerParqueo  parqueos idParqueoEnd 
        let user = extraerUsuario usuarios usuario
        let bicicleta = extraerBicicleta bicicletas codigoBici

        let initx = read (p1 !! 4) :: Int
        let inity = read (p1 !! 5) :: Int
        let endx = read (p2 !! 4) :: Int
        let endy = read (p2 !! 5) :: Int

        --Calcular la distancia.
        let distancia = sqrt (fromIntegral ((initx - endx) ^ 2 + (inity - endy) ^ 2))
        let tarifa = calcularTarifa (bicicleta !! 1)


        let resInfo = split (infoComer !! 0) ""

        let infoComercial = "Nombre de la empresa: " ++ (resInfo !! 0) ++ "  Pagina: " ++ (resInfo !! 1) ++ "  Correo: " ++ (resInfo !! 2) 
        let usuario = "Nombre del usuario: " ++ (user !! 1)
        let parqueoName1 = "Parqueo de salida: " ++ (p1 !! 1)
        let parqueoName2 = "Parqueo de salida: " ++ (p2 !! 1)
        let kilometros = "kilometros: " ++ show distancia
        let precio = "Total en colones: " ++ show (distancia * tarifa)

        putStrLn infoComercial
        putStrLn usuario
        putStrLn parqueoName1
        putStrLn parqueoName2
        putStrLn kilometros
        putStrLn precio

        let final = "\n"++ (datos!!0) ++ "," ++ show distancia ++ "," ++ show(distancia * tarifa) ++ "," ++ (p1 !!0) ++ "," ++ (p2 !!0) ++ "," ++ codigoBici ++ "," ++ (user!!0)

        modificarArchivoAlq alquileres idAlquiler "Facturado" ""
        modificarArchivo bicicletas codigoBici idParqueoInit ""

        añadirEnArchivo "facturas.txt" final
    else do
        putStrLn "No existe o ya esta desactivado"


menuGeneral :: IO ()
menuGeneral = do
    putStrLn "\nOpciones operativas:"
    putStrLn "Selecciona una opción:"
    putStrLn "1. Consultar"
    putStrLn "2. alquilar"
    putStrLn "3. facturar"
    putStrLn "4. Volver"

    opcion <- getLine

    case opcion of
        "1" -> do
            menuConsultarBicicleta
            menuGeneral
        "2" -> do
            menuAlquiler
            menuGeneral
        "3" -> do
                menuFacturar
                menuGeneral
            -- Agrega aquí la lógica para la Opción 2.
        "4" -> putStrLn "Menu General"
        _   -> do
            putStrLn "Opción no válida. Por favor, selecciona una opción válida."


bicisEnTr [] = return ""
bicisEnTr bicis = do
    let bici = split (head bicis) ""
    let texto = "\nID: " ++ (bici !! 0) ++ "| Tipo: " ++ (bici !! 1) ++ "| Ubicacion: " ++ (bici !! 2)

    if (bici !! 2) /= "transito" then
        putStrLn(texto)
    else 
        putStr("")

    resto <- bicisEnTr (tail bicis)
    return (resto)

bicisEnTrV2 [] = return ""
bicisEnTrV2 bicis = do
    let bici = split (head bicis) ""
    let texto = "\nID: " ++ (bici !! 0) ++ "| Tipo: " ++ (bici !! 1) ++ "| Ubicacion: " ++ (bici !! 2)

    if (bici !! 2) == "transito" then
        putStrLn(texto)
    else 
        putStr("")

    resto <- bicisEnTrV2 (tail bicis)
    return (resto)

mostrarBicis = do
    putStrLn("\nIngresa un ID de parqueo, # o la palabra ->transito<-, segun lo que quieras ver")
    input <- getLine
    contenido <- leerArchivo "parqueos.txt"
    contenido2 <- leerArchivo "bicicletas.txt"
    indices <- getIndicesParqueos contenido []
    let cadenaEnMayusculas = map toUpper input

    if input == "#" then do
        putStrLn("\nEstas son las bicis que no estan en transito de manera global")
        bicisEnTr contenido2
        putStr("")
    else if cadenaEnMayusculas == "TRANSITO" then do
        putStrLn("\nEstas son las bicis que estan en transito de manera global")
        bicisEnTrV2 contenido2
        putStr("")
    else if esEntero(input) && elem input indices then do 
        let entero = read input :: Int
        bicicletasAsociadas contenido2 entero
    else 
        putStrLn("\nOpcion no valida, intentalo de nuevo")


menuBicis :: IO ()
menuBicis = do
    putStrLn("\nMENU DE BICIS\n1. Cargar bicis\n2. Actualizar bicicletas\n3. Mostrar bicicletas\n4. Volver")
    putStrLn("\nIngrese su opcion deseada:")
    option <- getLine

    if option == "1" then do
        cargarBicicletas
        menuBicis
    else if option == "2" then do 
        x <- actualizarBicis
        if x == 1 then do
            contenido <- leerArchivo "bicicletasAux.txt"
            let cadena = pegarElementos contenido 0
            sobreEscribirEnArchivo "bicicletas.txt" (cadena !! 0)
            menuBicis
        else do 
            menuBicis
    else if option == "3" then do
        mostrarBicis
        menuBicis
    else if option == "4" then do 
        menuOperativo
    else do
        menuBicis 
        putStrLn("\nOpcion invalida, intentalo de nuevo")

menuOperativo :: IO ()
menuOperativo = do
    putStrLn("\nMENU OPERATIVO\n1. Cargar y mostrar parqueos\n2. Mostrar y asignar bicicletas\n3. Cargar usuarios\n4. Volver")
    putStrLn("\nIngrese su opcion deseada:")
    option <- getLine

    if option == "1" then do
        cargarYmostrarParqueos 
        menuOperativo
    else if option == "2" then do 
        menuBicis
    else if option == "3" then do
        cargarUsuarios
        menuOperativo
    else if option == "4" then do 
        putStrLn "Adios"
    else do 
        menuOperativo
        putStrLn("\nOpcion invalida, intentalo de nuevo")



menuEstadisticas :: IO ()
menuEstadisticas = do
    putStrLn "\nEstadisticas:"
    putStrLn "Selecciona una opción:"
    putStrLn "1. Top 5 de bicicletas con más viajes, indicar bicicleta y cantidad de viajes."
    putStrLn "2. Top 5 de parqueos con más viajes (salida + destino) indicar parqueo y cantidad de viajes"
    putStrLn "3. Top 3 de usuarios con más kilómetros recorridos (según fórmula de distancia). Indicar usuario y cantidad."
    putStrLn "4. Resumen: total de viajes, total de kilómetros y total facturado (facturas generadas)"
    putStrLn "5. Volver"

    opcion <- getLine

    case opcion of
        "1" -> do
            topBicicletas
            menuEstadisticas
        "2" -> do
            topParqueos
            menuEstadisticas
        "3" -> do
            topUsuarios
            menuEstadisticas
            -- Agrega aquí la lógica para la Opción 2.
        "4" -> putStrLn "Menu General"

        "5" -> putStrLn "Adios."
        _   -> do
            putStrLn "Opción no válida. Por favor, selecciona una opción válida."
            menuEstadisticas



ordenar :: [(String, Int)] -> [(String, Int)]
ordenar = sortOn snd

ordenarF :: [(String, Float)] -> [(String, Float)]
ordenarF = sortOn snd

imprimirTuplaBicicletas :: (String, Int) -> IO ()
imprimirTuplaBicicletas (codigo, cantidad) = putStrLn $ "El codigo de la bicicleta es: " ++ codigo ++ "  La cantidad de viajes: " ++ show cantidad

imprimirTuplaParqueos :: (String, Int) -> IO ()
imprimirTuplaParqueos (codigo, cantidad) = putStrLn $ "El id del parqueo es: " ++ codigo ++ "  La cantidad de viajes: " ++ show cantidad

imprimirTuplaUsuarios :: (String, Float) -> IO ()
imprimirTuplaUsuarios (codigo, cantidad) = putStrLn $ "Cedula del usuario: " ++ codigo ++ "  Kilometros recorridos: " ++ show cantidad


topBicicletas = do
    viajes <- leerArchivo "facturas.txt"
    bicicletas <- leerArchivo "bicicletas.txt"

    let first = bicicletas!!0
    let splitLine = split first ""
    let codigo =  splitLine !! 0


    let listaTops = topBicicletasAux bicicletas viajes 0 0 [] codigo 0

    let listaOrd = ordenar listaTops

    let listReverse = reverse listaOrd
    --Nota Debo ordenarlos.
    let lista = take 5 listReverse

    mapM_ imprimirTuplaBicicletas lista

    

topBicicletasAux :: [String] -> [String] -> Int -> Int -> [(String, Int)] -> String -> Int -> [(String, Int)]
topBicicletasAux bicicletas viajes posBici posViaje res code cant = do
    --
    let largobicis = length bicicletas
    let largoViajes = length viajes

    let lineAB = (bicicletas !! (posBici + 1))
    let listLine = split lineAB ""
    let nextCode = listLine !! 0 

    let lineAV = viajes !! posViaje
    let listV = split lineAV ""
    let temp = listV !! 5

    let sub = [(code, cant)]
    if largobicis == posBici then res
    else 
        if largoViajes == posViaje then topBicicletasAux bicicletas viajes (posBici + 1) 0 (res ++ sub) nextCode (cant - cant)
        else 
            if temp == code then topBicicletasAux bicicletas viajes posBici (posViaje + 1) res code (cant + 1)
            else topBicicletasAux bicicletas viajes posBici (posViaje + 1) res code cant       


topParqueos = do
    viajes <- leerArchivo "facturas.txt"
    parqueos <- leerArchivo "parqueos.txt"

    let first = parqueos!!0
    let splitLine = split first ""
    let id =  splitLine !! 0

    let listaTops = topParqueosAux parqueos viajes 0 0 [] id 0

    let listaOrd = ordenar listaTops

    let listReverse = reverse listaOrd
    --Nota Debo ordenarlos.
    let lista = take 5 listReverse

    mapM_ imprimirTuplaParqueos lista

    

topParqueosAux :: [String] -> [String] -> Int -> Int -> [(String, Int)] -> String -> Int -> [(String, Int)]
topParqueosAux parqueos viajes posPar posViaje res code cant = do
    --
    let largobicis = length parqueos
    let largoViajes = length viajes

    let lineAB = (parqueos !! (posPar + 1))
    let listLine = split lineAB ""
    let nextCode = listLine !! 0 

    let lineAV = viajes !! posViaje
    let listV = split lineAV ""
    let temp = listV !! 4
    let temp1 = listV !! 3

    let sub = [(code, cant)]
    if largobicis == posPar then res
    else 
        if largoViajes == posViaje then topParqueosAux parqueos viajes (posPar + 1) 0 (res ++ sub) nextCode (cant - cant)
        else 
            if temp == code || temp1 == code then topParqueosAux parqueos viajes posPar (posViaje + 1) res code (cant + 1)
            else topParqueosAux parqueos viajes posPar (posViaje + 1) res code cant       


topUsuarios = do
    viajes <- leerArchivo "facturas.txt"
    usuarios <- leerArchivo "usuarios.txt"

    let first = usuarios!!0
    let splitLine = split first ""
    let cedula =  splitLine !! 0

    let listaTops = topUsuariosAux usuarios viajes 0 0 [] cedula 0.0

    let listaOrd = ordenarF listaTops

    let listReverse = reverse listaOrd
    --Nota Debo ordenarlos.
    let lista = take 3 listReverse

    mapM_ imprimirTuplaUsuarios lista

    

topUsuariosAux :: [String] -> [String] -> Int -> Int -> [(String, Float)] -> String -> Float -> [(String, Float)]
topUsuariosAux usuarios viajes posPar posViaje res cedula cant = do
    --
    let largobicis = length usuarios
    let largoViajes = length viajes

    let lineAB = (usuarios !! (posPar + 1))
    let listLine = split lineAB ""
    let nextCode = listLine !! 0 

    let lineAV = viajes !! posViaje
    let listV = split lineAV ""
    let temp = listV !! 6
    let temp1 = listV !! 1

    let monto = read temp1 :: Float

    let sub = [(cedula, cant)]
    if largobicis == posPar then res
    else 
        if largoViajes == posViaje then topUsuariosAux usuarios viajes (posPar + 1) 0 (res ++ sub) nextCode (0)
        else
            if temp == cedula then topUsuariosAux usuarios viajes posPar (posViaje + 1) res cedula (cant + monto)
            else topUsuariosAux usuarios viajes posPar (posViaje + 1) res cedula cant       

main :: IO ()
main = do
    menu

menu :: IO ()
menu = do
    putStrLn "Bienvenido al Menú"
    putStrLn "Selecciona una opción:"
    putStrLn "1. Opciones especificas"
    putStrLn "2. Opciones generales"
    putStrLn "3. Estadisticas"
    putStrLn "4. Salir"

    opcion <- getLine

    case opcion of
        "1" -> do
            menuOperativo
            menu
        "2" -> do
            menuGeneral
            menu
        "3" -> do
            menuEstadisticas
            menu

        "4" -> putStrLn "¡Adiós!"
        _   -> do
            putStrLn "Opción no válida. Por favor, selecciona una opción válida."
            menu



