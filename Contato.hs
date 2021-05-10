data Contato = Contato{name :: [Char], tel :: Int, ende :: [Char], relac :: [Char]}

insereContato :: [Contato] -> Contato -> [Contato]
insereContato [] c = [c]
insereContato (cab : cal) c
    |(name cab == name c) = c : cal
    |otherwise = cab : insereContato cal c
    
removeContato :: [Contato] -> [Char] -> [Contato]
removeContato [] c = []
removeContato (cab : cal) c
    |(name cab == c) = cal
    |otherwise = cab : removeContato cal c

buscaContato :: [Contato] -> [Char] -> Contato
buscaContato [] c = Contato "0" 0 "0" "0"
buscaContato (cab : cal) c
    |(name cab == c) = cab
    |otherwise = buscaContato cal c

printaUnico :: Contato -> [Char]
printaUnico c = name c ++ " " ++ show(tel c) ++ " " ++ ende c ++ " " ++ relac c 

printaContatos :: [Contato] -> [Char]
prinaContatos [] = ""
printaContatos c = printaUnico (head c) ++ "\n" ++ printaContatos (tail c)
