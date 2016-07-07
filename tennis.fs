module Tennis

type Joueur = JoueurUn | JoueurDeux

type Point = Zero | Quinze | Trente

type PointInfo = {
    pointJoueurUn: Point
    pointJoueurDeux: Point
}

type QuaranteInfo = {
    joueurAvecQuarante: Joueur
    pointAutreJoueur: Point
}

type Score = 
    | Points of PointInfo
    | Quarante of QuaranteInfo
    | Egalite
    | Avantage of Joueur
    | Vainqueur of Joueur

let autre joueur =
    match joueur with 
    | JoueurUn -> JoueurDeux
    | JoueurDeux -> JoueurUn

let scoreQuandAvantage joueurAvecAvantage joueurQuiMarque =
    if joueurAvecAvantage = joueurQuiMarque
    then Vainqueur joueurAvecAvantage
    else Egalite

let scoreQuandEgalite joueurQuiMarque =
    Avantage joueurQuiMarque

let pointSuivant point =
    match point with
    | Zero -> Quinze
    | Quinze -> Trente
    | _ -> Trente

let scoreQuandQuarante quarante joueurQuiMarque =
    if quarante.joueurAvecQuarante = joueurQuiMarque
    then Vainqueur joueurQuiMarque
    else 
        if quarante.pointAutreJoueur = Trente
        then Egalite
        else Quarante { quarante with pointAutreJoueur = pointSuivant quarante.pointAutreJoueur}