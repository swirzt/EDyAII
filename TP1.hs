{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E
                    deriving Show

emptyTTree :: TTree k v
emptyTTree = E

-- Recibe una clave y un TTree
-- Devuelve "Nothing" si no encuentra la clave
-- Devuelve "Just y" si la encuentra, donde y es el valor asociado a la clave
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [x] (Leaf y z) = if x == y then Just z
                                  else Nothing
search (_:_) (Leaf _ _) = Nothing
search lis@[x] (Node c t l _ r) | x == c = t
                                | x < c = search lis l
                                | otherwise = search lis r
search lis@(x:xs) (Node c t l m r) | x == c = search xs m
                                   | x < c = search lis l
                                   | otherwise = search lis r

-- Recibe una clave, un valor y un TTree
-- Devuelve el TTree con la clave insertada
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [x] y E = Leaf x y
insert (x:xs) y E = Node x Nothing E (insert xs y E) E
insert [x] y (Leaf z w) | x == z = Leaf x y
                        | x < z  = Node z (Just w) (Leaf x y) E E
                        | otherwise = Node z (Just w) E E (Leaf x y)
insert p@(x:xs) y (Leaf z w) | x == z = Node z (Just w) E (insert xs y E) E
                             | x < z  = Node z (Just w) (insert p y E) E E
                             | otherwise = Node z (Just w) E E (insert p y E)
insert p@[x] y (Node c t l m r) | x == c = Node x (Just y) l m r
                                | x < c = Node c t (insert p y l) m r
                                | otherwise = Node c t l m (insert p y r)
insert p@(x:xs) y (Node c t l m r) | x == c = Node c t l (insert xs y m) r
                                   | x < c = Node c t (insert p y l) m r
                                   | otherwise = Node c t l m (insert p y r)

-- Recibe un TTree
-- Busca el hermano mas a la derecha (maximo) de la raiz dada
-- Devuelve una Tupla de 3 elementos que contiene:
-- El elemento de la clave dentro del nodo
-- El valor asociado (Puede ser Nothing)
-- Su hijo del medio (Puede ser E)
maxTT :: TTree k v -> (k, Maybe v, TTree k v)
maxTT n@(Leaf x y) = (x,Just y,E)
maxTT n@(Node c t l m r) = case r of
                                E -> (c, t, m)
                                _ -> maxTT r

-- Recibe un TTree
-- Busca el hermano mas a la derecha (maximo) de la raiz dada
-- Elimina este nodo, si tuviera un hijo a su izquierda se añade a la derecha del padre
-- del nodo borrado
deleteMax :: TTree k v -> TTree k v
deleteMax (Leaf _ _) = E
deleteMax (Node c t l m r) = case r of
                                E -> E
                                Leaf x y -> E
                                Node _ _ l' _ r' -> case r' of
                                                        E -> Node c t l m l'
                                                        _ -> Node c t l m (deleteMax r)

-- Recibe un TTree
-- Si es un Node con un valor Just x, y todos sus hijos son E
-- lo convierte en Leaf
toLeaf :: TTree k v -> TTree k v
toLeaf (Node c (Just t) E E E) = Leaf c t
toLeaf a = a

-- Recibe una clave y un TTree
-- Devuelve el TTree sin la clave y con los nodos basura eliminados
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete [x] n@(Leaf y _) = if x == y then E else n 
delete (_:_) n@(Leaf _ _) = n
delete p@[x] n@(Node c Nothing l m r) | x == c    = n
                                      | x < c     = Node c Nothing (delete p l) m r
                                      | otherwise = Node c Nothing l m (delete p r)
-- En caso de tener que reemplazar el nodo basura, hacemos uso de maxTT y deleteMax para mover
-- el nodo máximo de l al medio y lo eliminamos de l.
-- Si l == E, solo tomamos r
delete p@[x] n@(Node c t l E r) | x == c    = case l of
                                                E -> r
                                                _ -> toLeaf (Node c' t' (deleteMax l) m' r) where
                                                                    (c',t',m') = maxTT l
                                | x < c     = toLeaf (Node c t (delete p l) E r)
                                | otherwise = toLeaf (Node c t l E (delete p r))
-- En este caso sabemos que m /= E por lo tanto no es necesario usar toLeaf
delete p@[x] n@(Node c t l m r) | x == c = Node c Nothing l m r
                                | x < c = Node c t (delete p l) m r
                                | otherwise = Node c t l m (delete p r)
-- Al ser l y r == E, entonces m /= E, si al aplicar (delete xs m) obtengo vacio
-- elimino este nodo si su valor es Nothing, de lo contrario lo hago una Leaf
delete p@(x:xs) n@(Node c t E m E) | x == c = let k = delete xs m in case k of
                                                E -> case t of
                                                        Nothing -> E
                                                        Just t' -> Leaf c t'
                                                _ -> Node c t E k E
                                   | otherwise = n
-- Aplicamos toLeaf pues l o r pueden ser E, y la funcion delete puede devolver E
-- quedando (Node c t E E E) donde t tiene la forma (Just y) de lo contrario seria un nodo basura
delete p@(x:xs) n@(Node c t l E r) | x == c    = n
                                   | x < c     = toLeaf (Node c t (delete p l) E r)
                                   | otherwise = toLeaf (Node c t l E (delete p r))
-- En caso de tener que reemplazar el nodo basura, hacemos uso de maxTT y deleteMax para mover
-- el nodo máximo de l al medio y lo eliminamos de l.
-- Si l == E, solo tomamos r
delete p@(x:xs) n@(Node c t l m r) | x == c = let k = delete xs m in case k of
                                                E -> case t of
                                                        Nothing -> case l of
                                                                        E -> r
                                                                        _ -> toLeaf (Node c' t' (deleteMax l) m' r) where
                                                                                            (c',t',m') = maxTT l
                                                        _ -> Node c t l E r
                                                _ -> Node c t l k r
                                   | x < c     = Node c t (delete p l) m r
                                   | otherwise = Node c t l m (delete p r)

-- Recibe un TTree
-- Recorrre el arbol inorder y devuelve una lista de sus claves ordenadas de menor a mayor
keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf x _) = [[x]]
keys (Node x y l m r) = case y of
                        Nothing -> keys l ++ map (x:) (keys m) ++ keys r
                        Just _  -> keys l ++ [[x]] ++ map (x:) (keys m) ++ keys r

class Dic k v d | d -> k v where
    vacio    :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar   :: Ord k => k -> d -> Maybe v
    eliminar :: Ord k => k -> d -> d
    claves   :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
    vacio = emptyTTree
    insertar = insert
    buscar = search
    eliminar = delete
    claves = keys