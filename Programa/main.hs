import System.IO
import System.Directory (doesPathExist)

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
    let texto = "\nID: " ++ (parqueo !! 0) ++ " Nombre: " ++ (parqueo !! 1) ++ " Ubicacion: " ++ (parqueo !! 2) ++ " Provincia: " ++ (parqueo !! 3) ++ " Coordenadas: X->" ++ (parqueo !! 4) ++ " Y->" ++ (parqueo !! 5)

    resto <- toStringParks (tail parqueos)
    return (texto ++ resto)

pegarElementos [] = return ""
pegarElementos parqueos = do
    let parqueo = (head parqueos)

    resto <- toStringParks (tail parqueos)
    return ("\n" ++ parqueo ++ resto)

cargarYmostrarParqueos = do
    parqueos <- cargarParqueos
    parqueosVali <- cargarParqueosAux parqueos 

    if not (null parqueos) && parqueosVali == 1 then do
        let cadena = pegarElementos parqueos
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
    else if notElem (head primerElemento) digitos then do
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
        let cadena = pegarElementos usuarios
        añadirEnArchivo "usuarios.txt" (cadena !! 0)
        putStrLn("\nUsuarios añadidos con exito!!")
    else 
        putStrLn("\nUsuarios no añadidos, intentalo de nuevo")


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
bicletasAsociadas :: [String] -> Int -> IO()
bicletasAsociadas listaB id =
    if null listaB
    then putStrLn ("Fin.")
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
            putStrLn ("Bicicleta asociada al parqueo: " ++ show temp)
            bicletasAsociadas rest id
        else do
            bicletasAsociadas rest id
            
--modificarArchivo Lista a modificar -> codigo a modificar -> texto sustituir -> resultado
modificarArchivo :: [String] -> String -> String -> String -> IO()
modificarArchivo lista codigo texto res =
    if null lista then
        sobreEscribirEnArchivo "bicicletas.txt" res
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

main :: IO ()
main = do
    --cargarYmostrarParqueos
    cargarUsuarios

    putStrLn ("show x")
