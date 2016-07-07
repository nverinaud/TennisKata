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

    static member ``Quand un joueur a Quarante et que l'autre passe à Quarante, il doit y avoir Egalite`` (quarante : QuaranteInfo) =
        Vainqueur quarante.joueurAvecQuarante = scoreQuandQuarante quarante quarante.joueurAvecQuarante

    static member ``Quand un joueur a Quarante et que l'autre passe à Quarante, il doit y avoir Egalite`` (joueurAvecQuarante : Joueur) =
        let quarante = { joueurAvecQuarante = joueurAvecQuarante; pointAutreJoueur = Trente }
        Egalite = scoreQuandQuarante quarante (autre joueurAvecQuarante)

Check.QuickAll<Tests>()

