#r "packages/FsCheck.2.5.0/lib/net45/FsCheck"
open FsCheck

#load "tennis.fs"
open Tennis

type Tests =
    static member ``Quand le joueur avec l'Avantage marque le point, il doit être désigné Vainqueur`` (joueurAvecAvantage : Joueur) =
        Vainqueur joueurAvecAvantage = scoreQuandAvantage joueurAvecAvantage joueurAvecAvantage
    
    static member ``Quand le joueur avec l'Avantage perd le point, le score doit être Egalite`` (joueurAvecAvantage : Joueur) =
        Egalite = scoreQuandAvantage joueurAvecAvantage (autre joueurAvecAvantage)

    static member ``Quand il y a Egalite, le joueur qui marque le point prend l'Avantage`` (joueurQuiMarque : Joueur) =
        Avantage joueurQuiMarque = scoreQuandEgalite joueurQuiMarque

    static member ``Quand un joueur a Quarante et qu'il marque, il doit être désigné Vainqueur`` (quarante : QuaranteInfo) =
        Vainqueur quarante.joueurAvecQuarante = scoreQuandQuarante quarante quarante.joueurAvecQuarante

    static member ``Quand un joueur a Quarante et que l'autre passe à Quarante, il doit y avoir Egalite`` (joueurAvecQuarante : Joueur) =
        let quarante = { joueurAvecQuarante = joueurAvecQuarante; pointAutreJoueur = Trente }
        Egalite = scoreQuandQuarante quarante (autre joueurAvecQuarante)

    static member ``Quand un joueur a Quarante et que l'autre a Zero ou Quinze et que l'autre marque, son score augmente`` (quarante : QuaranteInfo) = 
        let valeurs = Gen.elements [Zero; Quinze] |> Arb.fromGen
        Prop.forAll valeurs (fun pointAutreJoueur ->
            let quarante = { quarante with pointAutreJoueur = pointAutreJoueur }
            let attendu = scoreQuandQuarante quarante (autre quarante.joueurAvecQuarante) 
            let actuel = pointSuivant quarante.pointAutreJoueur
                         |> Option.map (fun p -> { quarante with pointAutreJoueur = p })
                         |> Option.map Quarante
            Some attendu = actuel
        )

    static member ``Quand un joueur a Trente et qu'il marque, le score passe à Quarante`` (joueurQuiMarque : Joueur) (points : PointsInfo) =
        let points = pointPour joueurQuiMarque Trente points
        let attendu = Quarante { joueurAvecQuarante = joueurQuiMarque; pointAutreJoueur = pointDe (autre joueurQuiMarque) points }
        let actuel = scoreQuandPoints points joueurQuiMarque
        attendu = actuel

    static member ``Quand les joueurs ont moins que Trente et que l'un d'eux marque, son score augmente`` (points : PointsInfo) =
        let pointsInitiaux = Gen.elements [Zero; Quinze] |> Arb.fromGen
        let joueursQuiMarquent = Gen.elements [JoueurUn; JoueurDeux] |> Arb.fromGen
        
        Prop.forAll pointsInitiaux (fun point ->
            Prop.forAll joueursQuiMarquent (fun joueurQuiMarque ->
                let points = { points with pointJoueurUn = point; pointJoueurDeux = point }
                let attendu = Points <| pointPour joueurQuiMarque (pointSuivant point |> Option.get) points
                scoreQuandPoints points joueurQuiMarque = attendu
            )
        )


Check.QuickAll<Tests>()

