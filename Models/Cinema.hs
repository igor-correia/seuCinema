module Models.Cinema where
    import Models.Sala ( Sala )
    import Models.Filme ( Filme )
    data Cinema = Cinema {
        login:: String,
        senha:: String,
        valores:: [(String, Double)],
        filmes:: [Filme],
        salas:: [Sala],
        cardapio:: [(String, Double)]
    }