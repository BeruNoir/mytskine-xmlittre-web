TODO
====

Interne
-------

- [X] Utiliser [tsvpipe](http://sphinxsearch.com/docs/current.html#xsvpipe) pour charger les données.
   On évite de patcher SphinxSearch pour qu'il puisse lire directement dans la base SQLite.

- [ ] Automatiser tout le processus qui produit l'exécutable à partir du code.
   Dépendances locales, etc.

- [ ] Ne plus inclure le patch ICU dans SQLite mais ajouter une colonne normalisée à la table.
   Malheureusement, il n'y a pas de procédure simple et standard pour normaliser.
   Donc interroger Sphinx en recherche exacte (charsettable s'applique).

- [ ] Retirer le stockage des définitions de SphinxSearch, faire une requête Sqlite supplémentaire.


Court terme
-----------

* Liste des auteurs
* Liste des oeuvres
* Mentionner Lefff, Morphalou, etc.
* **Done** Dans Leff, quand une variante n'est pas trouvée dans l'index du Littré, la supprimer.
* **Done** Identifiants textuels et pérennes pour les œuvres anonymes, titre unique avec `tr/ /_/`.
* **Done** Si mot non trouvé, proposer aussi une recherche en texte intégral
* **Done** Vérifier la performance en cas de mot non trouvé (index Sqlite ?)
* **Done** Vérifier la cohérence de tous les liens dans le SQL.
* **Done** Ligatures œ et Œ <http://fr.wikipedia.org/wiki/Œ>
    * dans les vedettes du SQL,
    * dans le texte (souvent présentes dans le texte original avant numérisation)
* **Done** *Responsive design* : sur les petits écrans, afficher différemment le menu latéral.
* **Done** Passer Sphinx en 2.2.5.
* **Done** Ajouter à l'en-tête HTML des liens vers les entrées précédant et suivant.
* **Done** Afficher 20 citations d'un auteur sur sa page (tirage aléatoire, Sphinx `ORDER BY RAND()`)
* Signaler qu'une entrée vient du Supplément.


Moyen terme
-----------

* **Done** Recherche en texte intégral dans le dictionnaire
* **Done** Recherche en texte intégral dans les citations

* **Done** Page auteur : liens vers les citations de l'auteur
* **Done** Page auteur : liens vers les citations de chaque œuvre
* **Done** recherche de citations/definitions : espace vertical ++ entre ol.li

* **Done** Liens externes vers d'autres dictionnaires (TlFi, Académie, etc)
    * **Done** si le mot ne figure pas dans le Littré,
    * **Done** en regard d'une définition.
    * **Done** Lien vers le fac-simile sur le site de la BNF.

* **Done** Pages annexes :
    * **Dropped** Page de garde (inutile, surtout avec l'intégration BNF)
    * **Done** Préface
* **Done** FAQ (XML, code souce web, historique)
* **Done** page de stats (générée automatiquement par un script Perl)
* **Done** /auteur/# : remplacer liste de mots par liste d'auteurs
* **Done** /oeuvre/# : pas de liste d'œuvres voisines (titres trop longs), adapter le meta.content
* **Done** /auteur/ : tri JS pour chaque colonne
* **Done** Paginer les résultats de la recherche
* **Done** Proverbes : recherche
* Proverbes : navigation
* Détecter les requêtes qui ne sont pas encodées en UTF-8 mais en latin1
* Normalisation Unicode des textes recherchés ?
* Si le mot n'est pas trouvé en recherche simple, rechercher en texte intégral ?


### Incertain

* Complétion de l'auteur pour la recherche dans les citations
* Complétion de l'œuvre pour la recherche dans les citations
* page de florilège (perso ? accès limité ?)
* Mettre "span .src" autour du bloc aut+ref,
  puis CSS pour ajouter de l'espace (et les crochets ?).

### Tests

* **Done** Tests unitaires sur des fonctions
* **Done** Tests fonctionnels
* Avec émulation de Sphinx-search ?


Long terme
----------

* Système de proposition de correction en ligne pour le source XML. A minima de signalement.
* Saisie collective des termes grecs par une interface web.
* Chercher des lexiques de formes fléchies d'ancien français.
  Cf <http://people.ucalgary.ca/~dcwalker/Dictionary/dict.html>
* Distinguer les types de citations par un attribut :
    * définition : `Couche de sel formant la base d'une gerbe [Enquête sur les sels, 1868]`
    * commentaire : cf remarques de Vaugelas, M. Buffet, etc.
    * illustration : la plupart des citations.
* Saisie des "Additions" (pages 355 à 375 du Supplément) à partir de l'OCR de la BNF.
* Balisage séparant le nom de l'œuvre de la référence au passage.

