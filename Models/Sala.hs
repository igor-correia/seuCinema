module Models.Sala where
    data Sala = Sala {
        nome:: String,
        assentos:: [String]
    }

    --CRIAÇÃO DE SALA
    create:: String -> Int -> Int -> Sala
    create nome linha coluna = Sala nome (criaSala linha coluna)

    criaSala:: Int -> Int -> [String]
    criaSala 0 coluna = []
    criaSala linha coluna = replicate coluna 'o' : criaSala (linha-1) coluna

    --EXIBIÇÃO DE SALA
    exibe:: Sala -> IO()
    exibe sala = do
        putStrLn (" " ++ nome sala)
        putStrLn (preparaCabecalho 0 (length (head (assentos sala))))
        exibeFileira (assentos sala) 1


    codifica:: Int -> String
    codifica 0 = []
    codifica x = alfabeto !! (x `div` 26) : [alfabeto !! ((x `rem` 26) - 1)]
        where alfabeto = ['a' .. 'z']

    addEspacos:: String -> String
    addEspacos [] = []
    addEspacos x = [head x] ++ "  " ++ addEspacos (tail x)

    --limite = 25 colunas
    preparaCabecalho:: Int -> Int -> String
    preparaCabecalho cont colunas 
        | colunas >= cont = codifica cont ++ " " ++ preparaCabecalho (cont + 1) colunas
        | otherwise = []

    exibeFileira:: [String] -> Int-> IO()
    exibeFileira [] cont = putStrLn []
    exibeFileira sala cont = do
        let linha = addEspacos (head sala)
        putStrLn (" " ++ linha ++ show cont)
        exibeFileira (tail sala) (cont + 1)

    --OCUPAR ASSENTOS
    edit:: Sala -> String -> Sala
    edit sala ingresso = Sala (nome sala) (ocupaAssentos (assentos sala) ingresso)

    getIndex:: Char -> [Char] -> Int
    getIndex x [] = -1
    getIndex x y 
        | x == last y = length y - 1
        | otherwise = getIndex x (init y) 

    decodificaLetras:: String -> Int
    decodificaLetras a = getIndex (head a) alfabeto * 26 + getIndex (last a) alfabeto + 1
        where alfabeto = ['a' .. 'z']

    decodifica:: String -> [Int]
    decodifica a 
        | '-' `elem` a = decodifica (take (getIndex '-' a) a) ++ decodifica (drop (getIndex '-' a + 1) a)
        | otherwise = decodificaLetras (take 2 a) : [read (drop 2 a):: Int]

    ocupaAssento:: String -> Int -> Int-> String
    ocupaAssento fileira indice quant = take (indice - 1) fileira ++ replicate quant 'x' ++ drop (indice + quant - 1) fileira

    ocupaAssentos:: [String] -> String -> [String]
    ocupaAssentos sala x 
        | length (decodifica x) == 2 = take ((decodifica x !! 1) - 1) sala ++ [ocupaAssento (sala !! (decodifica x !! 1)) (head (decodifica x)) 1] ++ drop (decodifica x !! 1) sala
        | otherwise = take ((decodifica x !! 1) - 1) sala ++ [ocupaAssento (sala !! (decodifica x !! 1)) (head (decodifica x)) (decodifica x !! 2 - head (decodifica x) + 1)] ++ drop (decodifica x !! 1) sala

    main:: IO()
    main = do
        let sala = create "sala 1" 10 10
        exibe sala
        let novaSala = edit sala "ab7-af7"
        exibe novaSala