module Library where
import PdePreludat

type Material = String

data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Number,
    inventario:: [Material]
} deriving Show

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Number,
    resultado :: Material
} deriving Show

--Parte Craft
--Punto 1

intentarCraftear :: Receta -> Personaje -> Personaje
intentarCraftear receta personaje
    |tieneMateriales receta personaje = craftear receta personaje
    |otherwise = modificarPuntaje (-100) personaje

craftear :: Receta -> Personaje -> Personaje
--craftear receta personaje = modificarPuntaje ( agregarMaterial ( quitarMateriales (materiales receta) personaje ) (resultado receta)) (10*tiempo receta)
craftear receta = modificarPuntaje (10*tiempo receta) . agregarMaterial (resultado receta) . quitarMateriales (materiales receta)

agregarMaterial :: Material -> Personaje -> Personaje
agregarMaterial material personaje = personaje {inventario = material:inventario personaje}

quitarMateriales :: [Material] -> Personaje -> Personaje
quitarMateriales materiales personaje = personaje {inventario = foldl (flip quitarUnMaterial) (inventario personaje) materiales}

quitarUnMaterial :: Material -> [Material] -> [Material]
quitarUnMaterial _ [] = []
quitarUnMaterial material (x:xs)
    | material == x = xs
    | otherwise = x : quitarUnMaterial material xs

tieneMateriales :: Receta -> Personaje -> Bool
tieneMateriales receta personaje = all (tieneMaterial personaje) (materiales receta)

tieneMaterial :: Personaje -> Material -> Bool
tieneMaterial personaje material = material `elem` inventario personaje

modificarPuntaje :: Number -> Personaje -> Personaje
modificarPuntaje puntos personaje = personaje {puntaje = puntaje personaje + puntos}

--Punto 2

duplicarianSuPuntaje :: Personaje -> [Receta] -> [Material]
duplicarianSuPuntaje personaje recetas = map resultado (filter (duplicaLuegoDeCraftear personaje) recetas)

duplicaLuegoDeCraftear :: Personaje -> Receta -> Bool
duplicaLuegoDeCraftear personaje receta = puntaje (intentarCraftear receta personaje) > 2 * puntaje personaje

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldl (flip intentarCraftear)

masPuntosAlReves :: Personaje -> [Receta] -> Bool
masPuntosAlReves personaje recetas = puntaje (craftearSucesivamente personaje (reverse recetas)) > puntaje (craftearSucesivamente personaje recetas)

--Parte Mine
--Punto 1

data Bioma = UnBioma {
    materialesBioma :: [Material],
    materialNecesario :: Material
} deriving Show

type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Number -> Herramienta
pico = flip (!!)

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta personaje bioma
    | puedeMinar personaje bioma = modificarPuntaje 50 (agregarMaterial (herramienta $ materialesBioma bioma) personaje)
    | otherwise = personaje

puedeMinar :: Personaje -> Bioma -> Bool
puedeMinar personaje bioma = materialNecesario bioma `elem` inventario personaje

--Punto 2

guadaña :: Herramienta
--guadaña materiales = (!!) materiales (div (length materiales) 2)
guadaña = (!!) <*> (/2) . length

--Punto 3
{-Si estamos utilizando un hacha para minar un bioma con infinitos materiales, romperia el programa ya que nunca va a alzanzar el ultimo objeto
En caso de querer minar un bioma con infinitos materias con una espada, gracias al funcionamiento de haskell con lazy evaluation, no importa que la 
lista de materiales sea infinita ya que va a encontrar el primer elemento y va a devolver el personaje con el primer material agregado en su inventario
Con el pico, la funcion va a recorrer la lista de materiales hasta el indice que le indiquemos, por lo tanto gracias al concepto de lazy evaluation
nos va a devolver el personaje con ese material en el inventario.
con la guadaña lo primero que hace es calcular el largo de la lista de materiales, al ser infinita nunca va a poder obtener un resultado por lo tanto
no va a funcionar
-}