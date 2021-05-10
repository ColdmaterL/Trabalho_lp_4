data Contato = Contato{name :: String, tel :: Int, ende :: String, relac :: String}

insereContato :: Contato -> Contato
insereContato c = [c]
insereContato (n1 : n2) c
    |(name n1 == name c) = c : n2
    |otherwise = n1 : insereContato c
    
removeContato :: Contato -> String -> Contato
removeContato [] c = Contato "" 00000000 "" ""
removeContato (n1 : n2) c
    |(nome n1 == nome c) = n2
    |otherwise = removeContato n2 c

printaContato :: Contato -> String
printaContato c = name c ++ show(tel c) ++ ende c ++ relac c ++ "\n" ++ printaContato(tail c)