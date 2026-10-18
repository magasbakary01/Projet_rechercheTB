# Projet_rechercheTB

Outils pour les projets de recherche sur la tuberculose.

## randomPlatform : plateforme de randomisation

Le dossier [`randomPlatform/`](randomPlatform/) contient le package R
[randomPlatform](https://github.com/yangpluszhu/randomPlatform) (Ming Yang, licence MIT,
voir `randomPlatform/LICENSE`), importé avec son historique git. Il génère des listes de
randomisation reproductibles (simple, par blocs, stratifiée, minimisation), des codes
d'intervention, des enveloppes de randomisation et de levée d'aveugle en PDF, ainsi qu'un
rapport Excel d'audit. Il propose aussi une interface Shiny locale.

Modification locale : `tests/testthat/test-ui.R` utilisait un chemin absolu propre à
l'auteur (`D:/tmpProject/...`) ; il est remplacé par `test_path()`.

### Installation et lancement (R >= 4.1)

```r
install.packages(c("bslib", "digest", "DT", "jsonlite", "openxlsx",
                   "shiny", "shinyFiles", "zip"))
```

```bash
git clone https://github.com/magasbakary01/Projet_rechercheTB.git
cd Projet_rechercheTB
R CMD INSTALL randomPlatform
Rscript -e "randomPlatform::rp_launch_app()"
```

L'application s'ouvre sur http://127.0.0.1:3838. Sous Windows, on peut aussi lancer
`randomPlatform/inst/launcher/start_randomPlatform_UI.bat`.

Installation directe sans cloner :

```r
pak::pak("magasbakary01/Projet_rechercheTB/randomPlatform")
```

## Licence

Dépôt : GPL-3.0 (`LICENSE`). Le code de `randomPlatform/` reste sous licence MIT.
