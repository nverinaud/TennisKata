module Tennis

type Joueur = JoueurUn | JoueurDeux

type Point = Zero | Quinze | Trente

type PointsInfo = {
    pointJoueurUn: Point
    pointJoueurDeux: Point
}

type QuaranteInfo = {
    joueurAvecQuarante: Joueur
    pointAutreJoueur: Point
}

type Score = 
    | Points of PointsInfo
    | Quarante of QuaranteInfo
    | Egalite
    | Avantage of Joueur
    | Vainqueur of Joueur

let nouveauMatch = Points { pointJoueurUn = Zero ; pointJoueurDeux = Zero }

let autre joueur =
    match joueur with 
    | JoueurUn -> JoueurDeux
    | JoueurDeux -> JoueurUn

let pointPour joueur point pointsActuel =
    match joueur with
    | JoueurUn -> { pointsActuel with pointJoueurUn = point }
    | JoueurDeux -> { pointsActuel with pointJoueurDeux = point }

let pointDe joueur points =
    match joueur with
    | JoueurUn -> points.pointJoueurUn
    | JoueurDeux -> points.pointJoueurDeux

let pointSuivant point =
    match point with
    | Zero -> Some Quinze
    | Quinze -> Some Trente
    | _ -> None

let marquerQuandAvantage joueurAvecAvantage gagnant =
    if joueurAvecAvantage = gagnant
    then Vainqueur joueurAvecAvantage
    else Egalite

let marquerQuandEgalite gagnant =
    Avantage gagnant

let marquerQuandQuarante quarante gagnant =
    if quarante.joueurAvecQuarante = gagnant
    then Vainqueur gagnant
    else 
        match pointSuivant quarante.pointAutreJoueur with
        | Some p -> Quarante { quarante with pointAutreJoueur = p }
        | None -> Egalite

let marquerQuandPoints points gagnant =
    let pointJoueurQuiMarque = pointDe gagnant points
    match pointSuivant pointJoueurQuiMarque with
    | Some p -> Points <| pointPour gagnant p points
    | None -> Quarante { joueurAvecQuarante = gagnant; pointAutreJoueur = pointDe (autre gagnant) points }

let marquer score gagnant =
    match score with 
    | Points p        -> Some <| marquerQuandPoints p gagnant
    | Egalite         -> Some <| marquerQuandEgalite gagnant
    | Quarante q      -> Some <| marquerQuandQuarante q gagnant
    | Avantage joueur -> Some <| marquerQuandAvantage joueur gagnant
    | Vainqueur _     -> None
