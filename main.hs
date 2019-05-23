import System.IO
import Data.List
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Numeric

stringConvToFloat [] = []
stringConvToFloat (x:xs) = (percorreString (words x)): stringConvToFloat xs

percorreString [] = []
percorreString x = (read (head x) :: Double): (percorreString (tail x))

colocaPontosEmTupla [] num = []
colocaPontosEmTupla (x:xs) num = (num, x): colocaPontosEmTupla xs (num+1)

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

main = do
    ls <- fmap lines (readFile "entrada.txt")
    let pontosIO = stringConvToFloat ls
    let tuplaSaida = colocaPontosEmTupla pontosIO 1
    num <- readFile ("distancia.txt")
    let distancia = read num :: Int    
    lider <- rodaAlgoritmo pontosIO 1 [] [] distancia
    print lider
    -- centroide <- calculaCentroide (fst primeirosLideres)
    -- lideres <- selecionaUltimosLideres centroide x (snd primeirosLideres) (fst primeirosLideres)
    -- let pontosLideres = criaArrayDeTuplas (snd lideres)
    -- grupo <- distribui pontosIO (snd lideres) 0 pontosLideres []
    -- sse <- calculaSSE grupo 0
    -- writeFile "results.txt" (formatFloatN sse 4)
    -- saida <- openFile "saida.txt" WriteMode
    -- ggg <- escreveSaida (tuplaSaida) grupo saida
    -- hClose saida

---------------------------------------------------------------------
-- Funcao que seleciona os dois primeiros lideres pelos metodos a) e b)
---------------------------------------------------------------------
procuraNaTuplaSaida tuplaSaida pontoProcurado = do
  let tuplaDaSaida = head tuplaSaida
  let pegaPonto = snd tuplaDaSaida

  if (pegaPonto == pontoProcurado) then do
    return (fst tuplaDaSaida)
  else do
    procuraNaTuplaSaida (tail tuplaSaida) pontoProcurado

escreveNoArquivo tuplaSaida arrayProcurado imprimeSaida = do
  let pontoProcurado = head arrayProcurado
  pontoEncontrado <- procuraNaTuplaSaida tuplaSaida pontoProcurado


  let impressaoDeSaida = (show pontoEncontrado)

  if(imprimeSaida == "") then do
    let impressaoSaida = impressaoDeSaida
    if(length (tail (arrayProcurado)) == 0) then do
      return impressaoSaida
    else do
      escreveNoArquivo tuplaSaida (tail arrayProcurado) impressaoSaida
  else do
    let impressaoSaida = impressaoDeSaida ++ ", " ++ imprimeSaida

    if(length (tail (arrayProcurado)) == 0) then do
      return impressaoSaida
    else do
      escreveNoArquivo tuplaSaida (tail arrayProcurado) impressaoSaida

escreveSaida tuplaSaida grupo saida = do
  let pegaTupla = (head grupo)
  let pegaGrupo = (snd pegaTupla)

  impressao <- escreveNoArquivo tuplaSaida pegaGrupo ""
  hPutStrLn saida impressao
  hPutStrLn saida ""

  if(length(tail (grupo)) == 0) then do
    return grupo
  else do
    escreveSaida tuplaSaida (tail grupo) saida

---------------------------------------------------------------------
-- Selecionando o ponto cujas coordenadas somadas tem valor mínimo (em caso de
-- empate, seleciona-se aquela cujas primeiras coordenadas tem menor valor).
---------------------------------------------------------------------

-- Percorre lista de pontos somando e salvando em um array de tuplas,
-- contendo a (soma, [Ponto]). A lista de somas.
percorreSomaPontos [] = []
percorreSomaPontos (x:xs) = ((listaSoma x 0), [xss | xss <- x]): percorreSomaPontos xs

listaSoma [] soma = soma
listaSoma (x:xs) soma = listaSoma xs (x+soma)

-- Comparando Tuplas para encontrar o valor mínimo
min' [] = error "empty list"
min' (x:xs) = minhelper x xs where
  minhelper m [] = m
  minhelper m (y:ys) | y < m = minhelper y ys
                     | otherwise = minhelper m ys

-- Pegando Array de Pontos da Tupla encontrada com valor mínimo
pegaArray:: (Double, [Double]) -> [Double]
pegaArray (0, []) = []
pegaArray (x, xs) = xs

removeItem _ [] = []
removeItem x (y:ys) | x == y = ys
                    | otherwise = y : removeItem x ys
---------------------------------------------------------------------
-- Selecionando o ponto mais distante do ponto inicial (em caso de empate, selecionando
-- aquele cujas primeiras coordenadas tem menor valor)
---------------------------------------------------------------------
listaSubtrai [] [] = []
listaSubtrai (x:xs) (y:ys) = (x-y): listaSubtrai xs ys

listaAoQuadrado [] = 0
listaAoQuadrado (x:xs) = x*x + listaAoQuadrado xs

percorrePontos _ [] = []
percorrePontos x (y:ys) = (sqrt(listaAoQuadrado(listaSubtrai x y)), [yss | yss <- y]): percorrePontos x ys

max' [] = error "empty list"
max' (x:xs) = maxhelper x [xss | xss <- xs] where
    maxhelper m [] = m
    maxhelper m (y:ys) | fst y > fst m = maxhelper y ys
                       | fst y == fst m && snd y < snd m = maxhelper y ys
                       | otherwise = maxhelper m ys
---------------------------------------------------------------------
-- Selecionando sucessivamente, até completar os K pontos iniciais, o ponto mais
-- distante do centróide do grupo formado pelos pontos selecionados até então (em
-- caso de empate, seleciona-se aquele cujas primeiras coordenadas tem menor valor)
---------------------------------------------------------------------
-- calculaCentroide xs = map (/2) (map sum . transpose $ xs)

calculaCentroide xs = do
  let misturaVetores = xs
  let tamanhoVetor = (length(misturaVetores))
  let pontosNovos = transposta misturaVetores
  let novoCentroide = (percorreSomando pontosNovos)
  let centro = percorreDividindo novoCentroide (fromIntegral $ tamanhoVetor)

  return centro

---------------------------------------------------------------------
-- Distribuir os pontos em K grupos de acordo com a distância mais próxima do
-- ponto para os centróides dos grupos (em caso de empate, atribuir o ponto ao
-- grupo do centróide cujas primeiras coordenadas tem menor valor)
---------------------------------------------------------------------

criaArrayDeTuplas [] = []
criaArrayDeTuplas lideres = (head (lideres),[]): criaArrayDeTuplas (tail lideres)

rodaAlgoritmo pontos x lideres grupos distancia = do
    if (x == 1) then do 
        let lider = (head pontos)
        let grupo = [(head pontos)]
        let grupoAtualizado = [(lider, grupo)]

        let novoLider = [(head pontos)]
        
        rodaAlgoritmo (tail pontos) (x+1) novoLider grupoAtualizado distancia
    else do        
        let distanciaEntrePontos = sqrt(listaSoma(percorrePontos2 (head lideres) (head pontos)) 0)
        
        let ponto = (head pontos)
        let lider = (head lideres)
        
        if(distanciaEntrePontos <= distancia) then do
            grupo <- procuraPontoNaTuplaEInsere ponto lider grupos []            
            
        else do 
            return grupos
            

        if(length(tail pontos) == 0) then do 
            return grupos
        else do    
            rodaAlgoritmo (tail pontos) (x+1) lideres grupos distancia

        

procuraPontoNaTuplaEInsere pontoInserido ponto grupos header = do
    if (ponto == fst (head grupos)) then do
        let grupo = pontoInserido: (snd (head grupos))
        let headerAtt = (ponto, grupo)
        if (length (tail grupos) == 0) then do
            let headerAtt2 = headerAtt: header
            return headerAtt2
        else do
            let headerAtt3 = header++ (headerAtt: (tail grupos))
            return headerAtt3
    else do
        let headerAtt = (head grupos): header
        procuraPontoNaTuplaEInsere pontoInserido ponto (tail grupos) headerAtt            
                    
percorrePontos2 [] [] = [] 
percorrePontos2 (x:xs) (y:ys) = ((x-y)*(x-y)): percorrePontos2 xs ys
            

transposta ([]:_) = []
transposta m = (map head m) : transposta (map tail m)

percorreSomando [] = []
percorreSomando (x:xs) = (listaSoma x 0): percorreSomando xs

percorreDividindo [] y = []
percorreDividindo (x:xs) y = (x/y): percorreDividindo xs y

---------------------------------------------------------------------
-- Retornando a SSE da divisão em grupos e os grupos formados
---------------------------------------------------------------------
tamanho (x:xs) = length((x:xs))
calculaDistancia x [] soma = soma
calculaDistancia x (y:ys) soma = if length((y:ys)) == 0 then soma
                                 else (listaAoQuadrado(listaSubtrai x y)) + calculaDistancia x ys soma

calculaSSE grupos soma = do
    let cabeca = head grupos
    let centroide = fst cabeca
    let pontosGrupo = snd cabeca

    let somaAtt = soma + (calculaDistancia centroide pontosGrupo 0)

    if (length(tail(grupos)) == 0) then do
        return somaAtt
    else do
        calculaSSE (tail grupos) somaAtt