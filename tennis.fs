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

let scoreQuandAvantage joueurAvecAvantage joueurQuiMarque =
    if joueurAvecAvantage = joueurQuiMarque
    then Vainqueur joueurAvecAvantage
    else Egalite

let scoreQuandEgalite joueurQuiMarque =
    Avantage joueurQuiMarque

let scoreQuandQuarante quarante joueurQuiMarque =
    if quarante.joueurAvecQuarante = joueurQuiMarque
    then Vainqueur joueurQuiMarque
    else 
        match pointSuivant quarante.pointAutreJoueur with
        | Some p -> Quarante { quarante with pointAutreJoueur = p }
        | None -> Egalite

let scoreQuandPoints points joueurQuiMarque =
    let pointJoueurQuiMarque = pointDe joueurQuiMarque points
    match pointSuivant pointJoueurQuiMarque with
    | Some p -> Points <| pointPour joueurQuiMarque p points
    | None -> Quarante { joueurAvecQuarante = joueurQuiMarque; pointAutreJoueur = pointDe (autre joueurQuiMarque) points }