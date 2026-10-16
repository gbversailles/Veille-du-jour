Veille du jour — fiche du projet

Ce fichier résume le projet pour reprendre le travail sans contexte. À lire en entier avant toute modification. Le dépôt est public : ne jamais y écrire de données personnelles (adresse, nom, notes privées).

Le propriétaire
Pharmacien hospitalier, francophone : toujours répondre et commenter en français.
Pas d'ordinateur personnel : il travaille depuis son téléphone ou un poste d'hôpital. Expliquer les manipulations GitHub pas à pas, simplement.
Il ne veut aucun service payant : uniquement des sources gratuites et sans carte bancaire.
Préférences de présentation : sobre, lisible, blocs repliables, peu d'informations affichées d'emblée.
Ce que fait le projet

Une page d'accueil personnelle, régénérée automatiquement et publiée sur GitHub Pages (https://guiome93.github.io/Veille-du-jour/).

page_du_matin.R : script principal. Récupère les données, construit une liste et remplit template.html avec le paquet R whisker (syntaxe Mustache). Écrit public/index.html.
template.html : toute la page (HTML, CSS, JavaScript). Le JavaScript gère : météo selon la position, repli des blocs, réglages, avis, bibliothèque, tâches, outils, apparence.
reglages.txt : réglages au format cle = valeur, valeurs multiples séparées par ;. Modifiable depuis la page.
favoris.csv (nom,lien,categorie), avis.csv (pmid,avis,date), bibliotheque.csv (pmid,titre,source,lien,theme,ajoute) : écrits par la page via l'API GitHub.
referentiel.html et referentiel.json : référentiel STP (fiches par molécule : cibles, PK, interprétation). Page statique indépendante, publiée à l'adresse https://guiome93.github.io/Veille-du-jour/referentiel.html. Elle charge referentiel.json en relatif et enregistre les modifications dans referentiel.json via l'API GitHub, avec le même jeton que la page principale (localStorage veille-du-jour:jeton) ; un brouillon est gardé 15 minutes dans localStorage (referentiel:brouillon). Ces deux fichiers ne passent pas par whisker : ils sont copiés tels quels dans public/ par l'étape « Copier le referentiel » du workflow, juste après « Generer la page ».
.github/workflows/page-du-matin.yml : génère et publie la page toutes les 30 minutes (4 h–21 h UTC) et à la demande. Ne pas renommer ce fichier : la page et le référentiel déclenchent ce workflow par son nom après un enregistrement (réglages ou referentiel.json).
Blocs de la page et sources
Bloc	Source	Remarque
Veille scientifique	PubMed (E-utilities)	Tri par mots-clés selon les spécialités choisies, apprentissage à partir de avis.csv. Calculée une fois par jour puis mise en cache (cache/veille.rds, via actions/cache, clé = date + hash de reglages.txt).
Traduction	Google Traduction (gtx), secours MyMemory	Google renvoie souvent 429 depuis GitHub : on bascule alors définitivement sur MyMemory pour le lancement. Quota MyMemory limité : seuls les 8 premiers résumés sont traduits sans MYMEMORY_EMAIL.
Actualité	Google Actualités (RSS), Le Monde	Rubriques et thèmes choisis dans les réglages ; heure de publication affichée.
Météo, air, pollens	Open-Meteo	Recalculée côté navigateur selon la position si autorisée.
Investissement	Yahoo Finance (cours), baromètre mensuel CAFPI (taux Île-de-France, lu dans la page HTML)	
Trajet et carburant	api-adresse.data.gouv.fr, OSRM (sans trafic) ou TomTom (avec trafic, meilleure heure de départ), prix des carburants (data.economie.gouv.fr)	Adresse du domicile uniquement dans le secret TRAJET_DOMICILE.
Sport	ESPN (API publique non officielle), Jolpica-F1	Football interrogé jour par jour (les plages de dates ne fonctionnent pas).
Secrets GitHub (Settings > Secrets and variables > Actions)

TRAJET_DOMICILE, TOMTOM_KEY (facultatif), MYMEMORY_EMAIL (facultatif), MISTRAL_API_KEY / ANTHROPIC_API_KEY (facultatifs, non utilisés actuellement : pas d'offre gratuite accessible).

Pièges déjà rencontrés
whisker : une liste R nommée est traitée comme un objet et non comme une liste à parcourir → lignes vides. Toujours passer des listes non nommées (unname()).
Aucune balise {{ }} ne doit apparaître dans le JavaScript de template.html (whisker la remplacerait). Vérifier l'équilibre des sections Mustache après chaque modification.
GitHub Pages et les navigateurs gardent l'ancienne page en cache : la page compare data-genere et se recharge si une version plus récente existe.
Les fichiers envoyés via l'interface web sont parfois renommés nom (1).R : vérifier qu'il n'y a pas de doublons.
TomTom a renvoyé 401 Unauthorized : vérifier que la clé autorise la Routing API.
Vérifications avant de proposer une modification
Rscript -e 'invisible(parse("page_du_matin.R"))'
Équilibre des sections {{#…}} / {{/…}} de template.html, et node --check sur chaque script.
Le journal du job « Generer la page » contient des lignes de diagnostic (matchs trouvés, traductions, trajets, taux) : s'en servir pour déboguer.
Idées évoquées mais non retenues (ne pas reproposer sans demande)

Lecture audio du briefing, citations de ses articles, historique graphique, appli installable (peut-être plus tard), boutons Waze, partage d'articles, impression de la veille, sorties du week-end, suivi de colis, carnet d'entretien de la voiture.
