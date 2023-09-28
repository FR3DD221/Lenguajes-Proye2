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

main :: IO ()
main = do
    cargarYmostrarParqueos

    putStrLn ("")
