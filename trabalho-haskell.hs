import Data.Maybe

type Nome = String
type Valor = Float
type Quantidade = Int
type Produto = (Nome, Valor)
type Item = (Produto, Quantidade)

verifica :: Eq a => [a] -> a -> Bool
verifica l n = elem n l

--questão 1
produtos :: [Produto]
produtos = [("Cerveja",3.99),("Vodka", 15.50),("Agua",2.00),("Energetico",8.00),("Salgadinho",3.50),("Conhaque",25.00),("Balalaika",8.00),("Paiero",0.70),("Refrigerante",7.00),("Bala",0.10)]

--questão 2
repete :: a -> Int -> [a]
repete x n = replicate n x

index :: Eq a => a -> [a]-> Maybe Int
index _ [] = Nothing
index x l = calcula x l 0
where
calcula :: Eq a => a-> [a] -> Int -> Maybe Int
calcula _ [] _ = Nothing
calcula x (h:t) i
|x /= h = calcula x t (i+1)
|otherwise = Just i

elemento :: [a] -> Int -> Maybe a
elemento l n
|n > length l = Nothing
|otherwise = Just (l !! n)

--questão 3
addProduto :: [Produto] -> Produto -> [Produto]
addProduto lista produto = lista ++ [produto]

remProduto :: [Produto] -> String -> [Produto]
remProduto (h:t) x
|x == fst h = t
|otherwise = remProduto t x

buscaProduto :: [Produto] -> String -> Maybe Produto
buscaProduto [] _ = Nothing
buscaProduto (h:t) x
|x == fst h = Just h
|otherwise = buscaProduto t x

--questão 4
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq lista c n = conc c (n - (length lista)) ++ lista

conc :: Char -> Int -> String
conc c n = replicate n c

alinhaDir :: String -> Char -> Int -> String
alinhaDir lista c n = lista ++ conc c (n - (length lista))

--questão 5

start :: Valor -> String
start x = show x

indice' :: String-> Int
indice' [] = 0
indice' (h:t)
|'.' /= h = 1 + indice' t
|otherwise = 1

indice :: Valor -> Int
indice x = (indice'. start) x

fst' :: Valor -> String
fst' x = (take . indice) x (start x)

snd' :: Valor -> String
snd' x = (drop . indice) x (start x)

combina0 :: Valor -> Int -> String
combina0 x n
|length(snd' x) < n = (snd' x) ++ repete '0' (n - length(snd' x))
|otherwise = take n (snd' x)

combinall :: Valor -> Int -> String
combinall x n = (fst' x) ++ (combina0 x n)

($$) :: Valor -> Int -> String
valor $$ quant = combinall valor quant
infix 5 $$

dinheiro :: Valor -> String
dinheiro valor = '$' : (valor $$ 2)

--6

subTotal :: Item -> String
subTotal ((n, v),q) = dinheiro v ++ " x " ++ show q ++ " = " ++ alinhaEsq(dinheiro (v* fromIntegral (q))) ' ' 10

formataItem :: Item -> String
formataItem ((n,v),q) = (alinhaDir n '.' 45) ++ (alinhaEsq (subTotal ((n,v),q)) '.' 35)

--7
lItens :: [Item]
lItens = [(("Cerveja",3.99),3),(("Vodka", 15.50),2),(("Agua",2.00),10),(("Energetico",8.00),20),(("Salgadinho",3.50),3),(("Copo",0.20),20),(("Presidente",9.00),2),(("Pirulito",0.30),10),(("Cigarro",7.00),10),(("Suco",1.00),25)]

pValor :: Item -> Valor
pValor ((,v),) = v

pQnt :: Item -> Quantidade
pQnt ((,),q) = q

vTotal :: [Item] -> Float
vTotal [] = 0
vTotal (h:t) = (((pValor h)*(fromIntegral (pQnt h))) + vTotal t)

total :: [Item] -> String
total l = show (vTotal l)

--8
displayTopo :: String
displayTopo = "\n" ++ conc '' 80 ++ "\n" ++ conc ' ' 34 ++ "NOTA FISCAL" ++ conc ' ' 34 ++ "\n" ++ conc '' 80 ++ "\n" ++ conc ' ' 80 ++ "\n"

printLista :: [Item] -> String
printLista [] = []
printLista (h:t) = formataItem h ++ "\n" ++ printLista t

displayBaixo :: [Item] -> String
displayBaixo lista = conc ' ' 80 ++ "\n" ++ conc '' 80 ++ "\n" ++ alinhaEsq ("TOTAL: " ++ dinheiro((vTotal lista))) ' ' 80 ++ "\n" ++ conc '' 80 ++ "\n" ++ conc ' ' 80 ++ "\n"

notafiscal :: IO ()
notafiscal = putStrLn (displayTopo ++ printLista lItens ++ displayBaixo lItens)

-- 9
fLista :: [Produto]-> Quantidade -> [Item]
fLista [] _ = []
fLista (h:t) x = [(h, x)] ++ fLista t (x+1)

proditem :: [Item]
proditem = fLista produtos 0

chamada :: [Produto]->[Quantidade] -> [Item]
chamada [] _ = []
chamada _ [] = []
chamada (h:t) (he:ta) = [(h,he)] ++ (chamada t ta)

proditemx :: [Quantidade] -> [Item]
proditemx qtde = chamada produtos qtde

--10
itensn :: [(Nome, Quantidade)] -> [Item]
itensn lista = funcao lista produtos
where
funcao :: [(Nome, Quantidade)] -> [Produto] -> [Item]
funcao [] _ = []
funcao _ [] = []
funcao (h:t) (he:ta) = funcao2 h (he:ta) ++ funcao t (he:ta)

funcao2 :: (Nome, Quantidade)-> [Produto] -> [Item]
funcao2 _ [] = []
funcao2 comp (h:t)
|(fst comp) == fst h = [(h, (snd comp))]
|otherwise = funcao2 comp t

aux :: [(Int, Quantidade)] -> [Produto] -> [Item]
aux _ [] = []
aux [] _ = []
aux (h:t) (he:ta) = aux2 0 h (he:ta) ++ aux t (he:ta)

aux2 :: Int -> (Int, Quantidade) -> [Produto]-> [Item]
aux2 _ _ [] = []
aux2 cont tupla (h:t)
|cont == fst tupla = [(h, (snd tupla))]
|otherwise = aux2 (cont + 1) tupla t

itensi :: [(Int, Quantidade)] -> [Item]
itensi lista = aux lista produtos

lQnt :: [(Int, Quantidade)]
lQnt = [(1,3),(3,2),(5,4),(7,8),(9,30) ]

--11

venda = putStrLn (displayTopo ++ printLista (itensi lQnt) ++ displayBaixo (itensi lQnt))
