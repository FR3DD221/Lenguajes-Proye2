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

main :: IO ()
main = do
    let fuap = "a,b,c,d"
    let result = split fuap ""

    putStrLn (show result)
