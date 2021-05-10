data Contato = Contato{name :: String, tel :: Int, ende :: String, relac :: String}

insereContato :: Contato -> Contato
insereContato c = [c]
insereContato (cab : cal) c
    |(name cab == name c) = c : cal
    |(null cal) = c
    |otherwise = cab : insereContato c
    
removeContato :: Contato -> String -> Contato
removeContato [] c = Contato "" 00000000 "" ""
removeContato (cab : cal) c
    |(nome cab == nome c) = cal
    |(null cal) = c
    |otherwise = cab : removeContato cal c

printaContato :: Contato -> String
printaContato c = name c ++ show(tel c) ++ ende c ++ relac c ++ "\n" ++ printaContato(tail c)
