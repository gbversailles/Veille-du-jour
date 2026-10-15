# Veille du jour
# Génère public/index.html à partir de template.html et des réglages de reglages.txt

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(whisker)
})

# ---------- Utilitaires ----------

si_null <- function(x, defaut) if (is.null(x) || length(x) == 0) defaut else x

jours <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")
jours_courts <- c("dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam.")
mois  <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
           "août", "septembre", "octobre", "novembre", "décembre")
mois_courts <- c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.",
                 "août", "sept.", "oct.", "nov.", "déc.")

maintenant <- as.POSIXlt(Sys.time(), tz = "Europe/Paris")
date_fr <- sprintf("%s %d %s %d",
                   jours[maintenant$wday + 1], maintenant$mday,
                   mois[maintenant$mon + 1], maintenant$year + 1900)
date_fr <- paste0(toupper(substr(date_fr, 1, 1)), substr(date_fr, 2, nchar(date_fr)))

date_courte <- function(t) {
  lt <- as.POSIXlt(t, tz = "Europe/Paris")
  paste(jours_courts[lt$wday + 1], lt$mday, mois_courts[lt$mon + 1])
}
heure <- function(t) format(t, "%Hh%M", tz = "Europe/Paris")

nombre_fr <- function(x, decimales = 2) {
  formatC(x, format = "f", digits = decimales, big.mark = " ", decimal.mark = ",")
}

get_json <- function(url, query = list()) {
  request(url) |>
    req_url_query(!!!query) |>
    req_user_agent("Mozilla/5.0 (veille-du-jour)") |>
    req_retry(max_tries = 3) |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
}

get_texte <- function(url, query = list()) {
  request(url) |>
    req_url_query(!!!query) |>
    req_user_agent("Mozilla/5.0 (veille-du-jour)") |>
    req_retry(max_tries = 3) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_string()
}

nettoyer <- function(x) {
  x <- gsub("<!\\[CDATA\\[|\\]\\]>", "", x)
  entites <- c("&amp;" = "&", "&quot;" = "\"", "&apos;" = "'", "&lt;" = "<", "&gt;" = ">",
               "&nbsp;" = " ", "&rsquo;" = "’", "&lsquo;" = "‘", "&laquo;" = "«", "&raquo;" = "»",
               "&hellip;" = "…", "&eacute;" = "é", "&egrave;" = "è", "&agrave;" = "à", "&ccedil;" = "ç")
  for (e in names(entites)) x <- gsub(e, entites[[e]], x, fixed = TRUE)
  m <- gregexpr("&#x?[0-9a-fA-F]+;", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) vapply(v, function(z) {
    code <- gsub("[&#;]", "", z)
    n <- if (startsWith(code, "x")) strtoi(substring(code, 2), 16L) else as.integer(code)
    if (is.na(n)) "" else intToUtf8(n)
  }, ""))
  x <- gsub("<[^>]+>", " ", x)
  trimws(gsub("\\s+", " ", x))
}

raccourcir <- function(x, n = 220) {
  if (is.na(x) || nchar(x) <= n) return(x)
  coupe <- substr(x, 1, n)
  paste0(sub("\\s+\\S*$", "", coupe), "…")
}

# ---------- Réglages (fichier reglages.txt) ----------

lire_reglages <- function(fichier = "reglages.txt") {
  r <- list(ville = "Asnières-sur-Seine", latitude = "48.914", longitude = "2.285",
            equipes_foot = "Paris Saint-Germain; France", themes_actu = "",
            modele_ia = "claude-sonnet-5-5")
  if (!file.exists(fichier)) return(r)
  lignes <- readLines(fichier, encoding = "UTF-8", warn = FALSE)
  lignes <- trimws(lignes[!grepl("^\\s*(#|$)", lignes)])
  for (l in lignes) {
    pos <- regexpr("=", l, fixed = TRUE)
    if (pos < 1) next
    r[[trimws(substr(l, 1, pos - 1))]] <- trimws(substring(l, pos + 1))
  }
  r
}
reglages <- lire_reglages()
# Coordonnées de la ville (pour la météo de secours), retrouvées automatiquement
coords <- tryCatch({
  g <- get_json("https://geocoding-api.open-meteo.com/v1/search",
                list(name = sub(",.*$", "", reglages$ville), count = 1, language = "fr", countryCode = "FR"))
  if (length(g$results)) c(g$results[[1]]$latitude, g$results[[1]]$longitude) else NULL
}, error = function(e) NULL)
if (!is.null(coords)) { reglages$latitude <- coords[1]; reglages$longitude <- coords[2] }

liste_reglage <- function(x) {
  v <- trimws(strsplit(si_null(x, ""), ";", fixed = TRUE)[[1]])
  v[nzchar(v)]
}

# ---------- Météo (Open-Meteo, sans clé) ----------
# Valeur de secours (Asnières) : si le navigateur autorise la localisation,
# la page recharge elle-même la météo pour l'endroit où tu te trouves.

libelle_meteo <- function(code) {
  if (is.null(code) || is.na(code)) return("conditions inconnues")
  if (code == 0) return("ciel dégagé")
  if (code == 1) return("plutôt dégagé")
  if (code == 2) return("partiellement nuageux")
  if (code == 3) return("couvert")
  if (code %in% c(45, 48)) return("brouillard")
  if (code %in% 51:57) return("bruine")
  if (code %in% 61:67) return("pluie")
  if (code %in% 71:77) return("neige")
  if (code %in% 80:82) return("averses")
  if (code %in% 85:86) return("averses de neige")
  if (code %in% 95:99) return("orage")
  "conditions variables"
}

nuage <- '<path d="M7 18h10a4 4 0 0 0 0-8 5.5 5.5 0 0 0-10.6 1.6A3.2 3.2 0 0 0 7 18z"></path>'
icones <- list(
  soleil = '<circle cx="12" cy="12" r="4"></circle><path d="M12 2.5v2M12 19.5v2M2.5 12h2M19.5 12h2M5.3 5.3l1.4 1.4M17.3 17.3l1.4 1.4M5.3 18.7l1.4-1.4M17.3 6.7l1.4-1.4"></path>',
  eclaircies = '<circle cx="9" cy="9" r="3.5"></circle><path d="M9 2.5v1.5M2.5 9H4M4.4 4.4l1 1M13.6 4.4l-1 1"></path><path d="M10.5 20.5h7a3.5 3.5 0 0 0 0-7 4.5 4.5 0 0 0-8.6 1.6A2.7 2.7 0 0 0 10.5 20.5z"></path>',
  nuages = nuage,
  pluie = paste0(nuage, '<path d="M9 20.5l-1 2M13 20.5l-1 2M17 20.5l-1 2"></path>'),
  neige = paste0(nuage, '<path d="M9 21.5h.01M13 21.5h.01M17 21.5h.01"></path>'),
  orage = paste0(nuage, '<path d="M12.5 18.5l-2 3h3l-2 3"></path>'),
  brouillard = '<path d="M4 9h16M3 13h18M5 17h14"></path>'
)
icone_meteo <- function(code) {
  nom <- if (is.null(code) || is.na(code)) "nuages"
    else if (code <= 1) "soleil"
    else if (code == 2) "eclaircies"
    else if (code == 3) "nuages"
    else if (code %in% c(45, 48)) "brouillard"
    else if (code %in% c(71:77, 85:86)) "neige"
    else if (code %in% 95:99) "orage"
    else "pluie"
  paste0('<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" ',
         'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">', icones[[nom]], '</svg>')
}

valeurs <- function(x) unlist(lapply(x, function(v) if (is.null(v)) NA else v))
jours_courts <- c("dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam.")

meteo <- tryCatch({
  m <- get_json("https://api.open-meteo.com/v1/forecast", list(
    latitude = as.numeric(reglages$latitude), longitude = as.numeric(reglages$longitude),
    current = "temperature_2m,weather_code",
    daily = "weather_code,temperature_2m_min,temperature_2m_max,precipitation_probability_max",
    timezone = "Europe/Paris", forecast_days = 5
  ))
  dates <- as.Date(valeurs(m$daily$time))
  codes <- valeurs(m$daily$weather_code)
  tmin  <- valeurs(m$daily$temperature_2m_min)
  tmax  <- valeurs(m$daily$temperature_2m_max)
  pluie <- valeurs(m$daily$precipitation_probability_max)
  jours_prevus <- lapply(seq_along(dates), function(i) {
    lt <- as.POSIXlt(dates[i])
    list(day = if (i == 1) "Auj." else paste(jours_courts[lt$wday + 1], lt$mday),
         icon = icone_meteo(codes[i]),
         max = paste0(round(tmax[i]), "°"),
         min = paste0(round(tmin[i]), "°"),
         rain = if (is.na(pluie[i])) "–" else paste0(pluie[i], " %"))
  })
  conseil <- if (!is.na(pluie[1]) && pluie[1] >= 50) {
    sprintf("Aujourd'hui entre %d° et %d°, risque de pluie jusqu'à %d %% : prends un parapluie.",
            round(tmin[1]), round(tmax[1]), pluie[1])
  } else {
    sprintf("Aujourd'hui entre %d° et %d°, faible risque de pluie.", round(tmin[1]), round(tmax[1]))
  }
  list(now_temp = paste0(round(m$current$temperature_2m), "°"),
       now_label = paste0(reglages$ville, ", ", libelle_meteo(m$current$weather_code)),
       now_icon = icone_meteo(m$current$weather_code),
       days = jours_prevus,
       advice = conseil)
}, error = function(e) {
  message("Météo indisponible : ", conditionMessage(e))
  list(now_temp = "–", now_label = "météo indisponible", now_icon = icone_meteo(NA),
       days = list(), advice = "La météo n'a pas pu être récupérée ce matin.")
})

# ---------- Bourse (cours de clôture) ----------

cours <- function(symbole, nom, decimales = 2, suffixe = "") {
  tryCatch({
    url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/",
                  utils::URLencode(symbole, reserved = TRUE))
    j <- get_json(url, list(range = "5d", interval = "1d"))
    clotures <- j$chart$result[[1]]$indicators$quote[[1]]$close
    clotures <- unlist(Filter(Negate(is.null), clotures))
    if (length(clotures) < 2) stop("pas assez de données")
    dernier <- tail(clotures, 1)
    precedent <- clotures[length(clotures) - 1]
    variation <- (dernier / precedent - 1) * 100
    list(name = nom,
         lien = paste0("https://fr.finance.yahoo.com/quote/", utils::URLencode(symbole, reserved = TRUE)),
         value = paste0(nombre_fr(dernier, decimales), suffixe),
         change = paste0(if (variation >= 0) "+" else "−", nombre_fr(abs(variation), 2), " %"),
         dir = if (variation >= 0) "up" else "down")
  }, error = function(e) {
    message("Cours indisponible pour ", nom, " : ", conditionMessage(e))
    list(name = nom, lien = paste0("https://fr.finance.yahoo.com/quote/", utils::URLencode(symbole, reserved = TRUE)),
         value = "indisponible", change = "", dir = "flat")
  })
}

marches <- list(
  cours("^NDX", "Nasdaq-100"),
  cours("^FCHI", "CAC 40"),
  cours("^GSPC", "S&P 500"),
  cours("EURUSD=X", "EUR / USD", decimales = 4)
)

cryptos <- list(
  cours("BTC-EUR", "Bitcoin", decimales = 0, suffixe = " €"),
  cours("ETH-EUR", "Ethereum", decimales = 0, suffixe = " €"),
  cours("SOL-EUR", "Solana", decimales = 2, suffixe = " €")
)

# ---------- Actualités ----------

lire_rss <- function(url, etiquette, n = 1, query = list(), google = FALSE) {
  tryCatch({
    txt <- get_texte(url, query)
    items <- regmatches(txt, gregexpr("(?s)<item\\b.*?</item>", txt, perl = TRUE))[[1]]
    champ <- function(it, nom) {
      m <- regmatches(it, regexec(sprintf("(?s)<%s\\b[^>]*>(.*?)</%s>", nom, nom), it, perl = TRUE))[[1]]
      if (length(m) < 2) "" else nettoyer(m[2])
    }
    lapply(head(items, n), function(it) {
      titre <- champ(it, "title")
      resume <- raccourcir(champ(it, "description"))
      if (google) {
        source <- champ(it, "source")
        titre <- sub("\\s+-\\s+[^-]+$", "", titre)
        resume <- source
      }
      list(tag = etiquette, title = titre, link = champ(it, "link"), summary = resume)
    })
  }, error = function(e) {
    message("Flux indisponible (", etiquette, ") : ", conditionMessage(e))
    list()
  })
}

gn <- "https://news.google.com/rss"
gn_fr <- list(hl = "fr", gl = "FR", ceid = "FR:fr")
themes <- liste_reglage(reglages$themes_actu)

actus <- c(
  lire_rss(paste0(gn, "/headlines/section/topic/NATION"), "France", 1, gn_fr, google = TRUE),
  lire_rss(paste0(gn, "/search"), reglages$ville, 1,
           c(list(q = paste0("\"", reglages$ville, "\" when:3d")), gn_fr), google = TRUE),
  lire_rss("https://www.lemonde.fr/international/rss_full.xml", "Monde", 1),
  unlist(lapply(themes, function(th)
    lire_rss(paste0(gn, "/search"), th, 1, c(list(q = paste(th, "when:2d")), gn_fr), google = TRUE)),
    recursive = FALSE)
)
actus <- Filter(function(a) nzchar(a$title), actus)

# ---------- Football (données publiques d'ESPN) ----------

competitions <- c("fra.1" = "Ligue 1", "fra.coupe_de_france" = "Coupe de France",
                  "uefa.champions" = "Ligue des champions", "uefa.europa" = "Ligue Europa",
                  "uefa.nations" = "Ligue des nations", "fifa.worldq.uefa" = "Qualif. Coupe du monde",
                  "fifa.friendly" = "Match amical")
equipes <- liste_reglage(reglages$equipes_foot)

nom_equipe <- function(eq) si_null(eq$team$shortDisplayName, si_null(eq$team$displayName, "?"))
score_equipe <- function(eq) {
  s <- eq$score
  if (is.list(s)) s <- si_null(s$displayValue, si_null(s$value, "?"))
  as.character(si_null(s, "?"))
}

evenements <- list()
if (length(equipes)) {
  for (lg in names(competitions)) {
    for (k in -7:10) {
      jour <- format(Sys.Date() + k, "%Y%m%d")
      j <- tryCatch(get_json(sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/scoreboard", lg),
                             list(dates = jour)), error = function(e) NULL)
      if (is.null(j)) next
      for (e in si_null(j$events, list())) {
        cs <- e$competitions[[1]]$competitors
        dom <- Filter(function(x) identical(x$homeAway, "home"), cs)
        ext <- Filter(function(x) identical(x$homeAway, "away"), cs)
        if (!length(dom) || !length(ext)) next
        dom <- dom[[1]]; ext <- ext[[1]]
        noms <- tolower(c(dom$team$displayName, dom$team$shortDisplayName, dom$team$name,
                          ext$team$displayName, ext$team$shortDisplayName, ext$team$name))
        evenements[[as.character(e$id)]] <- list(
          comp = competitions[[lg]],
          t = as.POSIXct(substr(e$date, 1, 16), format = "%Y-%m-%dT%H:%M", tz = "UTC"),
          termine = isTRUE(e$status$type$completed),
          affiche = paste(nom_equipe(dom), "–", nom_equipe(ext)),
          score = paste0(score_equipe(dom), "–", score_equipe(ext)),
          noms = noms
        )
      }
    }
  }
}

foot <- lapply(equipes, function(eq) {
  ev <- Filter(function(e) tolower(eq) %in% e$noms, evenements)
  passes <- Filter(function(e) e$termine, ev)
  futurs <- Filter(function(e) !e$termine && !is.na(e$t) && e$t > Sys.time() - 3 * 3600, ev)
  if (length(passes)) passes <- passes[order(vapply(passes, function(e) as.numeric(e$t), 0), decreasing = TRUE)]
  if (length(futurs)) futurs <- futurs[order(vapply(futurs, function(e) as.numeric(e$t), 0))]
  list(
    equipe = eq,
    resultats = lapply(head(passes, 3), function(e)
      list(comp = e$comp, quand = date_courte(e$t), affiche = e$affiche, score = e$score)),
    prochain = lapply(head(futurs, 1), function(e)
      list(comp = e$comp, quand = paste(date_courte(e$t), "à", heure(e$t)), affiche = e$affiche)),
    rien = length(passes) == 0 && length(futurs) == 0
  )
})

classement <- tryCatch({
  j <- get_json("https://site.api.espn.com/apis/v2/sports/soccer/fra.1/standings")
  entrees <- j$children[[1]]$standings$entries
  lignes <- lapply(entrees, function(en) {
    stats <- si_null(en$stats, list())
    noms <- vapply(stats, function(s) si_null(s$name, ""), "")
    val <- function(n) { i <- match(n, noms); if (is.na(i)) NA else si_null(stats[[i]]$value, NA) }
    list(rank = val("rank"), team = si_null(en$team$shortDisplayName, en$team$displayName),
         pts = val("points"))
  })
  lignes <- lignes[order(vapply(lignes, function(l) as.numeric(si_null(l$rank, 99)), 0))]
  lapply(head(lignes, 5), function(l) list(rank = l$rank, team = l$team, pts = paste(l$pts, "pts")))
}, error = function(e) {
  message("Classement indisponible : ", conditionMessage(e))
  list()
})

# ---------- Formule 1 (API Jolpica, sans clé) ----------

f1_base <- "https://api.jolpi.ca/ergast/f1/current"
f1_calendrier <- tryCatch({
  courses <- get_json(paste0(f1_base, ".json"))$MRData$RaceTable$Races
  a_venir <- Filter(function(gp) as.Date(gp$date) >= Sys.Date(), courses)
  lapply(head(a_venir, 3), function(gp) {
    t <- as.POSIXct(paste(gp$date, sub("Z$", "", si_null(gp$time, "12:00:00"))),
                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    list(gp = gp$raceName,
         lieu = paste0(gp$Circuit$Location$locality, ", ", gp$Circuit$Location$country),
         quand = paste(date_courte(t), "à", heure(t)))
  })
}, error = function(e) { message("Calendrier F1 indisponible : ", conditionMessage(e)); list() })

f1_dernier <- tryCatch({
  courses <- get_json(paste0(f1_base, "/last/results.json"))$MRData$RaceTable$Races
  if (!length(courses)) NULL else {
    course <- courses[[1]]
    list(gp = course$raceName,
         podium = lapply(head(course$Results, 5), function(x)
           list(pos = x$position, pilote = paste(x$Driver$givenName, x$Driver$familyName),
                ecurie = x$Constructor$name)))
  }
}, error = function(e) { message("Résultats F1 indisponibles : ", conditionMessage(e)); NULL })

f1_pilotes <- tryCatch({
  l <- get_json(paste0(f1_base, "/driverStandings.json"))$MRData$StandingsTable$StandingsLists
  if (!length(l)) list() else lapply(head(l[[1]]$DriverStandings, 5), function(x)
    list(pos = x$position, nom = paste(x$Driver$givenName, x$Driver$familyName), pts = paste(x$points, "pts")))
}, error = function(e) { message("Classement pilotes indisponible : ", conditionMessage(e)); list() })

f1_ecuries <- tryCatch({
  l <- get_json(paste0(f1_base, "/constructorStandings.json"))$MRData$StandingsTable$StandingsLists
  if (!length(l)) list() else lapply(head(l[[1]]$ConstructorStandings, 3), function(x)
    list(pos = x$position, nom = x$Constructor$name, pts = paste(x$points, "pts")))
}, error = function(e) { message("Classement écuries indisponible : ", conditionMessage(e)); list() })

# ---------- Veille scientifique (PubMed + tri par IA) ----------

requete_pubmed <- paste(
  '("Ther Drug Monit"[ta] OR "Clin Pharmacokinet"[ta] OR "CPT Pharmacometrics Syst Pharmacol"[ta]',
  'OR "J Anal Toxicol"[ta] OR "Clin Toxicol (Phila)"[ta] OR "Toxicol Anal Clin"[ta] OR "Rev Francoph Lab"[ta])',
  'OR ("J Antimicrob Chemother"[ta] AND (pharmacokinetic*[tiab] OR "therapeutic drug monitoring"[tiab] OR dosing[tiab]))',
  'OR (("therapeutic drug monitoring"[tiab] OR "model-informed precision dosing"[tiab] OR "population pharmacokinetic*"[tiab])',
  'AND (beta-lactam*[tiab] OR vancomycin[tiab] OR antiepileptic*[tiab] OR "bone and joint infection*"[tiab]))',
  'OR "new psychoactive substance*"[tiab]'
)

extraire <- function(x, motif) {
  m <- regmatches(x, regexec(motif, x, perl = TRUE))[[1]]
  if (length(m) < 2) "" else m[2]
}

articles <- tryCatch({
  pm <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  s <- get_json(paste0(pm, "esearch.fcgi"), list(db = "pubmed", term = requete_pubmed, reldate = 7,
                datetype = "edat", retmax = 60, retmode = "json", sort = "pub_date"))
  ids <- unlist(s$esearchresult$idlist)
  if (!length(ids)) list() else {
    xml <- get_texte(paste0(pm, "efetch.fcgi"), list(db = "pubmed", id = paste(ids, collapse = ","), retmode = "xml"))
    blocs <- regmatches(xml, gregexpr("(?s)<PubmedArticle>.*?</PubmedArticle>", xml, perl = TRUE))[[1]]
    lapply(blocs, function(b) {
      resumes <- regmatches(b, gregexpr("(?s)<AbstractText\\b[^>]*>.*?</AbstractText>", b, perl = TRUE))[[1]]
      annee <- extraire(b, "(?s)<PubDate>.*?<Year>(\\d{4})</Year>")
      mois_pub <- extraire(b, "(?s)<PubDate>.*?<Month>([^<]+)</Month>")
      list(pmid = extraire(b, "<PMID[^>]*>(\\d+)</PMID>"),
           titre = nettoyer(extraire(b, "(?s)<ArticleTitle[^>]*>(.*?)</ArticleTitle>")),
           journal = nettoyer(extraire(b, "(?s)<ISOAbbreviation>(.*?)</ISOAbbreviation>")),
           date = trimws(paste(mois_pub, annee)),
           resume = nettoyer(paste(resumes, collapse = " ")))
    })
  }
}, error = function(e) { message("PubMed indisponible : ", conditionMessage(e)); list() })
articles <- Filter(function(a) nzchar(a$pmid) && nzchar(a$titre), articles)
message(length(articles), " articles PubMed trouvés")

consigne <- paste(
  "Tu prépares la veille scientifique d'un pharmacien hospitalier biologiste (laboratoire de pharmacologie-toxicologie,",
  "suivi thérapeutique pharmacologique, pharmacométrie, application MIPD en R/Shiny, staffs de réanimation et d'orthopédie).",
  "Ses centres d'intérêt : STP/MIPD des anti-infectieux (surtout bêta-lactamines et vancomycine) ; pharmacométrie",
  "(PopPK, bayésien, validation externe, modèles hybrides et apprentissage automatique) ; antiépileptiques et molécules",
  "à marge thérapeutique étroite ; toxicologie clinique ; toxicologie analytique (LC-MS/MS, criblage, NPS) ;",
  "infections ostéo-articulaires.",
  "Parmi les articles ci-dessous, sélectionne les 8 à 12 plus utiles pour lui et écarte les autres.",
  "Pour chacun : priorite (3 = à lire, 2 = intéressant, 1 = à survoler), theme (un parmi : Anti-infectieux,",
  "Pharmacométrie, Antiépileptiques et marge étroite, Toxicologie clinique, Toxicologie analytique,",
  "Infections ostéo-articulaires, Autre), en_bref (1 à 2 phrases en français sur ce que montre l'étude),",
  "pour_toi (1 à 2 phrases en français : utilité pour la validation, les staffs ou l'appli MIPD, avec un regard",
  "critique sur la méthodologie). Indique aussi dans top les PMID des 2 ou 3 articles à lire en priorité.",
  "Ne t'appuie que sur les titres et résumés fournis, n'invente rien.",
  "Réponds uniquement avec un objet JSON valide, sans texte autour, de la forme :",
  '{"articles":[{"pmid":"...","priorite":3,"theme":"...","en_bref":"...","pour_toi":"..."}],"top":["..."]}'
)

lien_pubmed <- function(pmid) paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
badges <- c("3" = "À lire ★★★", "2" = "Intéressant ★★", "1" = "À survoler ★")

veille <- list(); veille_top <- list(); veille_note <- ""
cle_api <- Sys.getenv("ANTHROPIC_API_KEY")
selection <- NULL
if (nzchar(cle_api) && length(articles)) {
  selection <- tryCatch({
    liste <- paste(vapply(articles, function(a) sprintf("PMID %s | %s | %s\nTitre : %s\nRésumé : %s",
      a$pmid, a$journal, a$date, a$titre, raccourcir(a$resume, 1500)), ""), collapse = "\n\n")
    rep <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(`x-api-key` = cle_api, `anthropic-version` = "2023-06-01") |>
      req_body_json(list(model = reglages$modele_ia, max_tokens = 6000,
                         messages = list(list(role = "user", content = paste(consigne, liste, sep = "\n\n"))))) |>
      req_timeout(300) |> req_retry(max_tries = 2) |>
      req_perform() |> resp_body_string() |> jsonlite::fromJSON(simplifyVector = FALSE)
    texte <- paste(vapply(rep$content, function(b) si_null(b$text, ""), ""), collapse = "")
    debut <- regexpr("{", texte, fixed = TRUE)
    fin <- max(gregexpr("}", texte, fixed = TRUE)[[1]])
    jsonlite::fromJSON(substr(texte, debut, fin), simplifyVector = FALSE)
  }, error = function(e) { message("Tri par IA impossible : ", conditionMessage(e)); NULL })
}

par_pmid <- setNames(articles, vapply(articles, function(a) a$pmid, ""))
if (!is.null(selection) && length(selection$articles)) {
  veille <- lapply(selection$articles, function(s) {
    a <- par_pmid[[as.character(s$pmid)]]
    if (is.null(a)) return(NULL)
    p <- as.character(si_null(s$priorite, 1))
    list(has_badge = TRUE, prio = p, badge = unname(si_null(badges[p], "")), theme = si_null(s$theme, ""),
         titre = a$titre, lien = lien_pubmed(a$pmid), source = paste(a$journal, "·", a$date),
         has_brief = TRUE, brief = si_null(s$en_bref, ""), pour_toi = si_null(s$pour_toi, ""))
  })
  veille <- Filter(Negate(is.null), veille)
  veille <- veille[order(-as.numeric(vapply(veille, function(v) v$prio, "")))]
  veille_top <- Filter(Negate(is.null), lapply(si_null(selection$top, list()), function(id) {
    a <- par_pmid[[as.character(id)]]
    if (is.null(a)) NULL else list(titre = a$titre, lien = lien_pubmed(a$pmid))
  }))
  veille_note <- sprintf("%d articles parus ces 7 derniers jours, %d retenus.", length(articles), length(veille))
} else {
  veille <- lapply(head(articles, 12), function(a)
    list(has_badge = FALSE, theme = "", titre = a$titre, lien = lien_pubmed(a$pmid),
         source = paste(a$journal, "·", a$date), has_brief = FALSE))
  veille_note <- if (nzchar(cle_api)) "Le tri par IA n'a pas fonctionné ce matin : voici les articles les plus récents."
                 else "Articles les plus récents. Ajoute ta clé d'API pour le tri, les priorités et les commentaires."
}

# ---------- Favoris (fichier favoris.csv du dépôt) ----------

favoris <- tryCatch({
  if (file.exists("favoris.csv")) {
    f <- utils::read.csv("favoris.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                         strip.white = TRUE)
    f <- f[!is.na(f$nom) & !is.na(f$lien) & nzchar(f$nom) & nzchar(f$lien), , drop = FALSE]
    lapply(seq_len(nrow(f)), function(i) list(nom = f$nom[i], lien = f$lien[i]))
  } else list()
}, error = function(e) {
  message("Favoris illisibles : ", conditionMessage(e))
  list()
})

# ---------- Taux immobiliers sur 25 ans en Île-de-France (recherche web par l'IA) ----------

taux <- NULL
if (nzchar(cle_api)) {
  taux <- tryCatch({
    consigne_taux <- paste(
      "Cherche sur le web les taux immobiliers moyens sur 25 ans en Île-de-France les plus récents,",
      "publiés par un courtier ou un baromètre reconnu (Meilleurtaux, Pretto, Empruntis, Vousfinancer, CAFPI...).",
      "Réponds uniquement avec un objet JSON valide, sans texte autour :",
      '{"source":"nom de la source","date":"date de mise à jour","lien":"adresse de la page",',
      '"bon":"x,xx %","tres_bon":"x,xx %","excellent":"x,xx %"}.',
      "N'invente aucune valeur : si une valeur est introuvable, mets null."
    )
    rep <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(`x-api-key` = cle_api, `anthropic-version` = "2023-06-01") |>
      req_body_json(list(model = reglages$modele_ia, max_tokens = 2000,
                         tools = list(list(type = "web_search_20250305", name = "web_search", max_uses = 3)),
                         messages = list(list(role = "user", content = consigne_taux)))) |>
      req_timeout(300) |> req_retry(max_tries = 2) |>
      req_perform() |> resp_body_string() |> jsonlite::fromJSON(simplifyVector = FALSE)
    textes <- Filter(function(b) identical(b$type, "text"), rep$content)
    texte <- paste(vapply(textes, function(b) si_null(b$text, ""), ""), collapse = "")
    debut <- regexpr("{", texte, fixed = TRUE)
    fin <- max(gregexpr("}", texte, fixed = TRUE)[[1]])
    tx <- jsonlite::fromJSON(substr(texte, debut, fin), simplifyVector = FALSE)
    lignes <- Filter(function(l) !is.null(l$taux), list(
      list(profil = "Bon dossier", taux = tx$bon),
      list(profil = "Très bon dossier", taux = tx$tres_bon),
      list(profil = "Excellent dossier", taux = tx$excellent)))
    if (!length(lignes)) NULL else
      list(lignes = lignes, source = si_null(tx$source, "source inconnue"), date = si_null(tx$date, ""),
           lien = si_null(tx$lien, ""))
  }, error = function(e) { message("Taux immobiliers indisponibles : ", conditionMessage(e)); NULL })
}

# ---------- Réglages actuels, transmis à la page pour l'écran de réglages ----------

config_json <- jsonlite::toJSON(list(
  ville = reglages$ville,
  equipes_foot = reglages$equipes_foot,
  themes_actu = reglages$themes_actu,
  favoris = lapply(favoris, function(f) list(nom = f$nom, lien = f$lien))
), auto_unbox = TRUE)
config_json <- gsub("</", "<\\/", as.character(config_json), fixed = TRUE)

# ---------- Assemblage ----------

donnees <- list(
  date = date_fr,
  now_temp = meteo$now_temp,
  now_label = meteo$now_label,
  now_icon = meteo$now_icon,
  days = meteo$days,
  advice = meteo$advice,
  favoris = favoris,
  markets = marches,
  cryptos = cryptos,
  news = actus,
  foot = foot,
  standings = classement,
  has_standings = length(classement) > 0,
  has_f1 = length(f1_calendrier) > 0 || !is.null(f1_dernier) || length(f1_pilotes) > 0,
  has_f1_dernier = !is.null(f1_dernier),
  f1_dernier_gp = si_null(f1_dernier$gp, ""),
  f1_podium = si_null(f1_dernier$podium, list()),
  has_f1_pilotes = length(f1_pilotes) > 0,
  f1_pilotes = f1_pilotes,
  has_f1_ecuries = length(f1_ecuries) > 0,
  f1_ecuries = f1_ecuries,
  has_f1_calendrier = length(f1_calendrier) > 0,
  f1_calendrier = f1_calendrier,
  veille = veille,
  has_veille_note = nzchar(veille_note),
  veille_note = veille_note,
  has_top = length(veille_top) > 0,
  top = veille_top,
  has_taux = !is.null(taux),
  taux = si_null(taux$lignes, list()),
  taux_source = si_null(taux$source, ""),
  taux_date = si_null(taux$date, ""),
  has_taux_date = nzchar(si_null(taux$date, "")),
  has_taux_lien = nzchar(si_null(taux$lien, "")),
  taux_lien = si_null(taux$lien, ""),
  has_cle = nzchar(cle_api),
  ville = reglages$ville,
  meteo_lien = paste0("https://www.google.com/search?q=", utils::URLencode(paste("météo", reglages$ville), reserved = TRUE)),
  config_json = config_json,
  updated = format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%H:%M", tz = "Europe/Paris")
)

modele <- paste(readLines("template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
page <- whisker.render(modele, donnees)

dir.create("public", showWarnings = FALSE)
writeLines(page, "public/index.html", useBytes = TRUE)
message("Page générée : public/index.html")
