data Contato = Contato{name :: [Char], tel :: Int, ende :: [Char], relac :: String}

insereContato :: [Contato] -> Contato -> [Contato]
insereContato [] c = [c]
insereContato (cab : cal) c
    |(name cab == name c) = c : cal
    |otherwise = cab : insereContato c
    
removeContato :: [Contato] -> [Char] -> [Contato]
removeContato [] c = [c]
removeContato (cab : cal) c
    |(nome cab == nome c) = cal
    |otherwise = cab : removeContato cal c

buscaContato :: [Contato] -> [Char] -> Contato
buscaContato [] c = Contato "0" 0 "0" "0"
buscaContato (cab : cal) c
    |(nome cab == c)
    |otherwise = buscaContato cal c

printaUnico Contato -> [Char]
printaUnico [] = ""
printaUnico c = name c ++ show(tel c) ++ ende c ++ relac c 

printaContatos :: [Contato] -> [Char]
prinaContatos [] = ""
printaContatos (cab : cal) c
printaContatos c = printaUnico(cab) ++ "\n" ++ printaContato(cal)
