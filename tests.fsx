#r "packages/FsCheck.2.5.0/lib/net45/FsCheck"
open FsCheck

#load "tennis.fs"
open Tennis

type Tests =
    static member ``Quand le joueur avec l'Avantage marque le point, il doit être désigné Vainqueur`` (joueurAvecAvantage : Joueur) =
        Vainqueur joueurAvecAvantage = marquerQuandAvantage joueurAvecAvantage joueurAvecAvantage
    
    static member ``Quand le joueur avec l'Avantage perd le point, le score doit être Egalite`` (joueurAvecAvantage : Joueur) =
        Egalite = marquerQuandAvantage joueurAvecAvantage (autre joueurAvecAvantage)

    static member ``Quand il y a Egalite, le joueur qui marque le point prend l'Avantage`` (gagnant : Joueur) =
        Avantage gagnant = marquerQuandEgalite gagnant

    static member ``Quand un joueur a Quarante et qu'il marque, il doit être désigné Vainqueur`` (quaranteInfo : QuaranteInfo) =
        Vainqueur quaranteInfo.joueurAvecQuarante = marquerQuandQuarante quaranteInfo quaranteInfo.joueurAvecQuarante

    static member ``Quand un joueur a Quarante et que l'autre passe à Quarante, il doit y avoir Egalite`` (joueurAvecQuarante : Joueur) =
        let quarante = { joueurAvecQuarante = joueurAvecQuarante; pointAutreJoueur = Trente }
        Egalite = marquerQuandQuarante quarante (autre joueurAvecQuarante)

    static member ``Quand un joueur a Quarante et que l'autre a Zero ou Quinze et qu'il marque, son score augmente`` (quaranteInfo : QuaranteInfo) = 
        Gen.elements [Zero; Quinze] 
        |> Arb.fromGen
        |> Prop.forAll 
        <| fun pointAutreJoueur ->
            let quarante = { quaranteInfo with pointAutreJoueur = pointAutreJoueur }
            let attendu = marquerQuandQuarante quarante (autre quarante.joueurAvecQuarante) 
            let actuel = pointSuivant quarante.pointAutreJoueur
                         |> Option.map (fun p -> { quarante with pointAutreJoueur = p })
                         |> Option.map Quarante
            Some attendu = actuel

    static member ``Quand un joueur a Trente et qu'il marque, le score passe à Quarante`` (gagnant : Joueur) (points : PointsInfo) =
        let points = pointPour gagnant Trente points
        let attendu = Quarante { joueurAvecQuarante = gagnant; pointAutreJoueur = pointDe (autre gagnant) points }
        let actuel = marquerQuandPoints points gagnant
        attendu = actuel

    static member ``Quand les joueurs ont moins que Trente et que l'un d'eux marque, son score augmente`` (points : PointsInfo) (gagnant : Joueur) =
        Gen.elements [Zero; Quinze] 
        |> Arb.fromGen
        |> Prop.forAll 
        <| fun point ->
            let points = { points with pointJoueurUn = point; pointJoueurDeux = point }
            let attendu = Points <| pointPour gagnant (pointSuivant point |> Option.get) points
            marquerQuandPoints points gagnant = attendu
    
    static member ``Marquer fonctionne sans edge-cases`` (score : Score) (gagnant : Joueur) =
        let actuel = marquer score gagnant
        true // nous vérifions simplement que la fonction `marquer` ne crash pas

    static member ``Marquer : quand le joueur avec l'Avantage marque le point, il doit être désigné Vainqueur`` (joueurAvecAvantage : Joueur) =
        Some <| Vainqueur joueurAvecAvantage = marquer (Avantage joueurAvecAvantage) joueurAvecAvantage
    
    static member ``Marquer : quand le joueur avec l'Avantage perd le point, le score doit être Egalite`` (joueurAvecAvantage : Joueur) =
        Some Egalite = marquer (Avantage joueurAvecAvantage) (autre joueurAvecAvantage)

    static member ``Marquer : quand il y a Egalite, le joueur qui marque le point prend l'Avantage`` (gagnant : Joueur) =
        Some <| Avantage gagnant = marquer Egalite gagnant

    static member ``Marquer : quand un joueur a Quarante et qu'il marque, il doit être désigné Vainqueur`` (quaranteInfo : QuaranteInfo) =
        Some <| Vainqueur quaranteInfo.joueurAvecQuarante = marquer (Quarante quaranteInfo) quaranteInfo.joueurAvecQuarante

    static member ``Marquer : quand un joueur a Quarante et que l'autre passe à Quarante, il doit y avoir Egalite`` (joueurAvecQuarante : Joueur) =
        let quaranteInfo = { joueurAvecQuarante = joueurAvecQuarante; pointAutreJoueur = Trente }
        Some Egalite = marquer (Quarante quaranteInfo) (autre joueurAvecQuarante)

    static member ``Marquer : quand un joueur a Quarante et que l'autre a Zero ou Quinze et qu'il marque, son score augmente`` (quaranteInfo : QuaranteInfo) = 
        Gen.elements [Zero; Quinze] 
        |> Arb.fromGen
        |> Prop.forAll 
        <| fun pointAutreJoueur ->
            let quarante = { quaranteInfo with pointAutreJoueur = pointAutreJoueur }
            let attendu = marquer (Quarante quarante) (autre quarante.joueurAvecQuarante) 
            let actuel = pointSuivant quarante.pointAutreJoueur
                         |> Option.map (fun p -> { quarante with pointAutreJoueur = p })
                         |> Option.map Quarante
            Some attendu = Some actuel

    static member ``Marquer : quand un joueur a Trente et qu'il marque, le score passe à Quarante`` (gagnant : Joueur) (points : PointsInfo) =
        let points = pointPour gagnant Trente points
        let attendu = Quarante { joueurAvecQuarante = gagnant; pointAutreJoueur = pointDe (autre gagnant) points }
        let actuel = marquer (Points points) gagnant
        Some attendu = actuel

    static member ``Marquer : quand les joueurs ont moins que Trente et que l'un d'eux marque, son score augmente`` (points : PointsInfo) (gagnant : Joueur) =
        Gen.elements [Zero; Quinze] 
        |> Arb.fromGen
        |> Prop.forAll 
        <| fun point ->
            let points = { points with pointJoueurUn = point; pointJoueurDeux = point }
            let attendu = Points <| pointPour gagnant (pointSuivant point |> Option.get) points
            marquer (Points points) gagnant = Some attendu

    static member ``Marquer : un match terminé ne peut plus continuer.`` (gagnant : Joueur) =
        let matchTermine = Vainqueur gagnant
        None = marquer matchTermine (autre gagnant)

    static member ``autre doit retourner l'autre joueur`` (joueur : Joueur) =
        joueur <> (autre joueur)

    static member ``autre autre doit retourner le même joueur`` (joueur : Joueur) =
        joueur = (autre >> autre) joueur

Check.QuickAll<Tests>()
