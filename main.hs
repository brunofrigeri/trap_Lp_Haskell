import System.IO
import Data.List
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
    let distancia = read num :: Double

    grupo <- rodaAlgoritmo pontosIO 1 [] [] distancia []
    print grupo
    sse <- calculaSSE grupo 0
    writeFile "results.txt" (formatFloatN sse 4)
    saida <- openFile "saida.txt" WriteMode
    arquivoSaida <- escreveSaida (tuplaSaida) grupo saida
    hClose saida

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
    let impressaoSaida = impressaoDeSaida ++ " " ++ imprimeSaida

    if(length (tail (arrayProcurado)) == 0) then do
      return impressaoSaida
    else do
      escreveNoArquivo tuplaSaida (tail arrayProcurado) impressaoSaida

escreveSaida tuplaSaida grupo saida = do
  let pegaTupla = (head grupo)
  let pegaGrupo = (snd pegaTupla)

  impressao <- escreveNoArquivo tuplaSaida pegaGrupo ""
  hPutStrLn saida impressao

  if(length(tail (grupo)) == 0) then do
    return grupo
  else do
    hPutStrLn saida ""
    escreveSaida tuplaSaida (tail grupo) saida

-- Soma elementos da lista
listaSoma [] soma = soma
listaSoma (x:xs) soma = listaSoma xs (x+soma)

-- Subtrai elementos da mesma posicao de duas listas
listaSubtrai [] [] = []
listaSubtrai (x:xs) (y:ys) = (x-y): listaSubtrai xs ys

-- Quadrado de elemento a elemento da lista
listaAoQuadrado [] = 0
listaAoQuadrado (x:xs) = x*x + listaAoQuadrado xs

-- Soma vetores da mesma posicao de um array de listas
somaVetores xs = (map sum . transpose $ xs)

-- Calcula o centróide de um array de listas
calculaCentroide xs = do
  let vetor = somaVetores xs
  let tamanhoVetor = (length(xs))
  let centro = percorreDividindo vetor (fromIntegral $ tamanhoVetor)

  return centro

-- Funcao responsável por rodar o algoritmo de distribuicao de pontos,
-- percorrendo a lista de pontos, quando encontrado um novo lider, concatena
-- o lider em um array de lideres, e, cria uma tupla contendo o novo lider e
-- seu ponto (mesmo ponto), concatenando com os grupos ja criados.
-- Por outro lado, quando encontrado um ponto que pertence a um grupo existente
-- o ponto é adicionado ao grupo já existente.
rodaAlgoritmo pontos x lideres grupos distancia arrayLideres = do
    if (x == 1) then do
        let lider = (head pontos)
        let grupo = [(head pontos)]
        let grupoAtualizado = [(lider, grupo)]

        let novoLider = [(head pontos)]

        rodaAlgoritmo (tail pontos) (x+1) novoLider grupoAtualizado distancia novoLider
    else do
        let distanciaEntrePontos = sqrt(listaSoma(percorrePontos2 (head lideres) (head pontos)) 0)

        let ponto = (head pontos)
        let lider = (head lideres)

        if(distanciaEntrePontos <= distancia) then do
            gruposAtualizados <- procuraPontoNaTuplaEInsere ponto lider grupos []

            if (length (tail pontos) == 0) then do
                return gruposAtualizados
            else do
              rodaAlgoritmo (tail pontos) (x+1) arrayLideres gruposAtualizados distancia arrayLideres

        else do
            if(length (tail lideres) == 0) then do
                let novoArrayLideres = insereLista ponto arrayLideres
                let gruposAtualizados = (ponto, [ponto]): grupos

                if (length (tail pontos) == 0) then do
                    return gruposAtualizados
                else do
                    rodaAlgoritmo (tail pontos) (x+1) novoArrayLideres gruposAtualizados distancia novoArrayLideres
            else do
              rodaAlgoritmo pontos x (tail lideres) grupos distancia arrayLideres


insereLista a [] = [a]
insereLista a (x:xs) = x : insereLista a xs


-- Como o nome da funcao já diz, procura um ponto na tupla (grupos) e insere,
-- a funcao abaixo é responsável por procurar o lider encontrado que se relaciona
-- ao ponto, e inserir o ponto ao grupo desse lider.
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

-- Recebe dois pontos e calcula a distancia entre ambos.
percorrePontos2 [] [] = []
percorrePontos2 (x:xs) (y:ys) = ((x-y)*(x-y)): percorrePontos2 xs ys

-- Percorre um array de listas somando cada vetor e concatenando a soma
-- com a soma do proximo array.
percorreSomando [] = []
percorreSomando (x:xs) = (listaSoma x 0): percorreSomando xs

-- Percorre uma lista dividindo-a por um valor (recebido como parametro).
percorreDividindo [] y = []
percorreDividindo (x:xs) y = (x/y): percorreDividindo xs y

-- Como o nome já diz, calcula distancia entre um ponto e um array de pontos,
-- recebe também um valor default (soma) como 0, que vai sendo somado com a
-- execucao da funcao.
calculaDistancia x [] soma = soma
calculaDistancia x (y:ys) soma = if length((y:ys)) == 0 then soma
                                 else (listaAoQuadrado(listaSubtrai x y)) + calculaDistancia x ys soma

-- Calcula o SSE dos grupos, após todos os grupos estarem formados.
calculaSSE grupos soma = do
    let cabeca = head grupos
    let pontosGrupo = snd cabeca

    centroide <- calculaCentroide (snd cabeca)

    let somaAtt = soma + (calculaDistancia centroide pontosGrupo 0)

    if (length(tail(grupos)) == 0) then do
        return somaAtt
    else do
        calculaSSE (tail grupos) somaAtt