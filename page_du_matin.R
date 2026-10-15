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

# Traduction automatique vers le français (service public de Google Traduction, sans clé)
cache_trad <- new.env()
stats_trad <- c(ok = 0, echec = 0)
trad_google <- function(x) {
  j <- request("https://translate.googleapis.com/translate_a/single") |>
    req_url_query(client = "gtx", sl = "en", tl = "fr", dt = "t", q = x) |>
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
    req_timeout(20) |>
    req_perform() |> resp_body_string() |> jsonlite::fromJSON(simplifyVector = FALSE)
  paste(vapply(j[[1]], function(seg) { v <- seg[[1]]; if (is.null(v)) "" else as.character(v) }, ""), collapse = "")
}
email_trad <- Sys.getenv("MYMEMORY_EMAIL")
trad_mymemory <- function(x) {
  q <- list(q = substr(x, 1, 480), langpair = "en|fr")
  if (nzchar(email_trad)) q$de <- email_trad
  j <- get_json("https://api.mymemory.translated.net/get", q)
  as.character(si_null(j$responseData$translatedText, ""))
}
google_bloque <- FALSE
traduire <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(x)
  if (!is.null(cache_trad[[x]])) return(cache_trad[[x]])
  Sys.sleep(0.3)
  secours <- function() tryCatch(trad_mymemory(x), error = function(e2) { message("MyMemory en échec : ", conditionMessage(e2)); "" })
  res <- if (google_bloque) secours() else tryCatch(trad_google(x), error = function(e) {
    message("Google Traduction indisponible (", conditionMessage(e), ") : passage au service de secours")
    google_bloque <<- TRUE
    secours()
  })
  if (!nzchar(res) || grepl("MYMEMORY WARNING", res, fixed = TRUE)) {
    stats_trad[["echec"]] <<- stats_trad[["echec"]] + 1
    res <- x
  } else stats_trad[["ok"]] <<- stats_trad[["ok"]] + 1
  assign(x, res, envir = cache_trad)
  res
}

# ---------- Réglages (fichier reglages.txt) ----------

lire_reglages <- function(fichier = "reglages.txt") {
  r <- list(ville = "Asnières-sur-Seine", latitude = "48.914", longitude = "2.285",
            ville_travail = "Versailles",
            equipes_foot = "Paris Saint-Germain; France", themes_actu = "",
            rubriques_actu = "france; ville; monde; sciences; economie",
            specialites = "pharmacologie; toxicologie; infectiologie; reanimation; orthopedie",
            mots_cles_veille = "", revues_veille = "",
            sports = "foot; f1",
            carburant = "E10",
            modele_ia = "mistral-large-latest")
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
if (grepl("^claude", reglages$modele_ia)) reglages$modele_ia <- "mistral-large-latest"
# Coordonnées des villes (météo de secours, air, carburant), retrouvées automatiquement
geocoder <- function(ville) tryCatch({
  g <- get_json("https://geocoding-api.open-meteo.com/v1/search",
                list(name = sub(",.*$", "", ville), count = 1, language = "fr", countryCode = "FR"))
  if (length(g$results)) c(g$results[[1]]$latitude, g$results[[1]]$longitude) else NULL
}, error = function(e) NULL)
coords <- geocoder(reglages$ville)
if (!is.null(coords)) { reglages$latitude <- coords[1]; reglages$longitude <- coords[2] }
coords_travail <- if (nzchar(si_null(reglages$ville_travail, ""))) geocoder(reglages$ville_travail) else NULL

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

rubriques_dispo <- list(
  france     = list(nom = "France",      url = paste0(gn, "/headlines/section/topic/NATION"), q = NULL),
  monde      = list(nom = "Monde",       url = paste0(gn, "/headlines/section/topic/WORLD"), q = NULL),
  ville      = list(nom = reglages$ville, url = paste0(gn, "/search"), q = paste0("\"", reglages$ville, "\" when:3d")),
  sciences   = list(nom = "Sciences",    url = paste0(gn, "/headlines/section/topic/SCIENCE"), q = NULL),
  sante      = list(nom = "Santé",       url = paste0(gn, "/headlines/section/topic/HEALTH"), q = NULL),
  tech       = list(nom = "Tech",        url = paste0(gn, "/headlines/section/topic/TECHNOLOGY"), q = NULL),
  economie   = list(nom = "Économie",    url = paste0(gn, "/headlines/section/topic/BUSINESS"), q = NULL),
  culture    = list(nom = "Culture",     url = paste0(gn, "/headlines/section/topic/ENTERTAINMENT"), q = NULL),
  sport      = list(nom = "Sport",       url = paste0(gn, "/headlines/section/topic/SPORTS"), q = NULL)
)
numeroter <- function(l) { for (i in seq_along(l)) l[[i]]$rang <- i; l }
rubriques_choisies <- intersect(liste_reglage(reglages$rubriques_actu), names(rubriques_dispo))
actus <- c(
  unlist(lapply(rubriques_choisies, function(k) {
    rb <- rubriques_dispo[[k]]
    q <- if (is.null(rb$q)) gn_fr else c(list(q = rb$q), gn_fr)
    numeroter(lire_rss(rb$url, rb$nom, 4, q, google = TRUE))
  }), recursive = FALSE),
  unlist(lapply(themes, function(th)
    numeroter(lire_rss(paste0(gn, "/search"), th, 3, c(list(q = paste(th, "when:2d")), gn_fr), google = TRUE))),
    recursive = FALSE)
)
actus <- Filter(function(a) !is.null(a$title) && nzchar(a$title), si_null(actus, list()))
actus <- actus[!duplicated(vapply(actus, function(a) a$title, ""))]
actus_principal <- Filter(function(a) identical(a$rang, 1L) || identical(a$rang, 1), actus)
actus_suite <- Filter(function(a) !(identical(a$rang, 1L) || identical(a$rang, 1)), actus)

# ---------- Football (données publiques d'ESPN) ----------

competitions <- c("fra.1" = "Ligue 1", "fra.coupe_de_france" = "Coupe de France",
                  "uefa.champions" = "Ligue des champions", "uefa.europa" = "Ligue Europa",
                  "uefa.nations" = "Ligue des nations", "fifa.worldq.uefa" = "Qualif. Coupe du monde",
                  "fifa.friendly" = "Match amical")
sports_choisis <- liste_reglage(reglages$sports)
equipes <- if ("foot" %in% sports_choisis) liste_reglage(reglages$equipes_foot) else character()

nom_equipe <- function(eq) si_null(eq$team$shortDisplayName, si_null(eq$team$displayName, "?"))
score_equipe <- function(eq) {
  s <- eq$score
  if (is.list(s)) s <- si_null(s$displayValue, si_null(s$value, "?"))
  as.character(si_null(s, "?"))
}

evenements <- list()
erreurs_espn <- character()
message("Équipes suivies : ", if (length(equipes)) paste(equipes, collapse = " | ") else "aucune")
if (length(equipes)) {
  for (lg in names(competitions)) {
    fenetre <- if (lg %in% c("uefa.nations", "fifa.worldq.uefa", "fifa.friendly")) -21:30 else -7:10
    for (k in fenetre) {
      jour <- format(Sys.Date() + k, "%Y%m%d")
      j <- tryCatch(get_json(sprintf("https://site.api.espn.com/apis/site/v2/sports/soccer/%s/scoreboard", lg),
                             list(dates = jour)), error = function(e) {
        erreurs_espn <<- c(erreurs_espn, paste(lg, ":", conditionMessage(e)))
        NULL
      })
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

message(length(evenements), " matchs trouvés au total sur ESPN")
if (length(erreurs_espn)) message("Erreurs ESPN (", length(erreurs_espn), ") : ", paste(head(unique(erreurs_espn), 3), collapse = " || "))
if (length(evenements)) message("Exemples d'affiches : ", paste(head(vapply(evenements, function(e) e$affiche, ""), 5), collapse = " | "))

foot <- lapply(equipes, function(eq) {
  ev <- Filter(function(e) tolower(eq) %in% e$noms, evenements)
  passes <- Filter(function(e) e$termine, ev)
  futurs <- Filter(function(e) !e$termine && !is.na(e$t) && e$t > Sys.time() - 3 * 3600, ev)
  if (length(passes)) passes <- passes[order(vapply(passes, function(e) as.numeric(e$t), 0), decreasing = TRUE)]
  if (length(futurs)) futurs <- futurs[order(vapply(futurs, function(e) as.numeric(e$t), 0))]
  list(
    equipe = eq,
    resultats = unname(lapply(head(passes, 3), function(e)
      list(comp = e$comp, quand = date_courte(e$t), affiche = e$affiche, score = e$score))),
    prochain = unname(lapply(head(futurs, 1), function(e)
      list(comp = e$comp, quand = paste(date_courte(e$t), "à", heure(e$t)), affiche = e$affiche))),
    rien = length(passes) == 0 && length(futurs) == 0
  )
})

classement <- if (!"foot" %in% sports_choisis) list() else tryCatch({
  j <- get_json("https://site.api.espn.com/apis/v2/sports/soccer/uefa.champions/standings")
  entrees <- unlist(lapply(si_null(j$children, list()), function(ch) si_null(ch$standings$entries, list())),
                    recursive = FALSE)
  if (!length(entrees)) entrees <- si_null(j$standings$entries, list())
  suivies <- tolower(equipes)
  lignes <- lapply(entrees, function(en) {
    stats <- si_null(en$stats, list())
    noms <- vapply(stats, function(s) si_null(s$name, ""), "")
    val <- function(n) { i <- match(n, noms); if (is.na(i)) NA else si_null(stats[[i]]$value, NA) }
    noms_eq <- tolower(c(en$team$displayName, en$team$shortDisplayName, en$team$name))
    list(rank = val("rank"), team = si_null(en$team$shortDisplayName, en$team$displayName),
         pts = val("points"), suivi = any(suivies %in% noms_eq))
  })
  lignes <- lignes[order(vapply(lignes, function(l) as.numeric(si_null(l$rank, 99)), 0))]
  idx <- which(vapply(lignes, function(l) isTRUE(l$suivi), FALSE))
  garder <- if (length(idx)) {
    sort(unique(unlist(lapply(idx, function(i) max(1, i - 2):min(length(lignes), i + 2)))))
  } else seq_len(min(8, length(lignes)))
  message("Classement Ligue des champions : ", length(lignes), " équipes, ", length(idx), " suivie(s) trouvée(s)")
  lapply(lignes[garder], function(l) list(rank = l$rank, team = l$team, pts = paste(l$pts, "pts"), suivi = l$suivi))
}, error = function(e) {
  message("Classement indisponible : ", conditionMessage(e))
  list()
})

# ---------- Formule 1 (API Jolpica, sans clé) ----------

noms_gp <- c("Australian" = "d'Australie", "Chinese" = "de Chine", "Japanese" = "du Japon", "Bahrain" = "de Bahreïn",
  "Saudi Arabian" = "d'Arabie saoudite", "Miami" = "de Miami", "Canadian" = "du Canada", "Monaco" = "de Monaco",
  "Spanish" = "d'Espagne", "Barcelona-Catalunya" = "de Barcelone", "Madrid" = "de Madrid", "Austrian" = "d'Autriche",
  "British" = "de Grande-Bretagne", "Belgian" = "de Belgique", "Hungarian" = "de Hongrie", "Dutch" = "des Pays-Bas",
  "Italian" = "d'Italie", "Emilia Romagna" = "d'Émilie-Romagne", "Azerbaijan" = "d'Azerbaïdjan",
  "Singapore" = "de Singapour", "United States" = "des États-Unis", "Mexico City" = "de Mexico",
  "São Paulo" = "de São Paulo", "Las Vegas" = "de Las Vegas", "Qatar" = "du Qatar", "Abu Dhabi" = "d'Abou Dhabi")
nom_gp <- function(x) {
  base <- trimws(sub("Grand Prix", "", x, fixed = TRUE))
  if (!is.na(noms_gp[base])) paste("Grand Prix", noms_gp[[base]]) else x
}

f1_base <- "https://api.jolpi.ca/ergast/f1/current"
f1_calendrier <- if (!"f1" %in% sports_choisis) list() else tryCatch({
  courses <- get_json(paste0(f1_base, ".json"))$MRData$RaceTable$Races
  a_venir <- Filter(function(gp) as.Date(gp$date) >= Sys.Date(), courses)
  lapply(head(a_venir, 3), function(gp) {
    t <- as.POSIXct(paste(gp$date, sub("Z$", "", si_null(gp$time, "12:00:00"))),
                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    list(gp = nom_gp(gp$raceName),
         lieu = paste0(gp$Circuit$Location$locality, ", ", gp$Circuit$Location$country),
         quand = paste(date_courte(t), "à", heure(t)))
  })
}, error = function(e) { message("Calendrier F1 indisponible : ", conditionMessage(e)); list() })

f1_dernier <- if (!"f1" %in% sports_choisis) NULL else tryCatch({
  courses <- get_json(paste0(f1_base, "/last/results.json"))$MRData$RaceTable$Races
  if (!length(courses)) NULL else {
    course <- courses[[1]]
    list(gp = nom_gp(course$raceName),
         podium = lapply(head(course$Results, 5), function(x)
           list(pos = x$position, pilote = paste(x$Driver$givenName, x$Driver$familyName),
                ecurie = x$Constructor$name)))
  }
}, error = function(e) { message("Résultats F1 indisponibles : ", conditionMessage(e)); NULL })

f1_pilotes <- if (!"f1" %in% sports_choisis) list() else tryCatch({
  l <- get_json(paste0(f1_base, "/driverStandings.json"))$MRData$StandingsTable$StandingsLists
  if (!length(l)) list() else lapply(head(l[[1]]$DriverStandings, 5), function(x)
    list(pos = x$position, nom = paste(x$Driver$givenName, x$Driver$familyName), pts = paste(x$points, "pts")))
}, error = function(e) { message("Classement pilotes indisponible : ", conditionMessage(e)); list() })

f1_ecuries <- if (!"f1" %in% sports_choisis) list() else tryCatch({
  l <- get_json(paste0(f1_base, "/constructorStandings.json"))$MRData$StandingsTable$StandingsLists
  if (!length(l)) list() else lapply(head(l[[1]]$ConstructorStandings, 3), function(x)
    list(pos = x$position, nom = x$Constructor$name, pts = paste(x$points, "pts")))
}, error = function(e) { message("Classement écuries indisponible : ", conditionMessage(e)); list() })

# ---------- Autres sports (NBA, rugby) via ESPN ----------

resultats_espn <- function(chemin, nom, jours = 5, maxi = 5) {
  sortie <- list()
  for (k in 0:jours) {
    j <- tryCatch(get_json(sprintf("https://site.api.espn.com/apis/site/v2/sports/%s/scoreboard", chemin),
                           list(dates = format(Sys.Date() - k, "%Y%m%d"))), error = function(e) NULL)
    if (is.null(j)) next
    for (e in si_null(j$events, list())) {
      if (!isTRUE(e$status$type$completed)) next
      cs <- e$competitions[[1]]$competitors
      dom <- Filter(function(x) identical(x$homeAway, "home"), cs)
      ext <- Filter(function(x) identical(x$homeAway, "away"), cs)
      if (!length(dom) || !length(ext)) next
      t <- as.POSIXct(substr(e$date, 1, 16), format = "%Y-%m-%dT%H:%M", tz = "UTC")
      sortie[[as.character(e$id)]] <- list(comp = nom, quand = date_courte(t), t = as.numeric(t),
        affiche = paste(nom_equipe(dom[[1]]), "–", nom_equipe(ext[[1]])),
        score = paste0(score_equipe(dom[[1]]), "–", score_equipe(ext[[1]])))
    }
    if (length(sortie) >= maxi) break
  }
  if (length(sortie)) sortie <- sortie[order(-vapply(sortie, function(s) s$t, 0))]
  unname(head(sortie, maxi))
}
nba <- if ("nba" %in% sports_choisis) resultats_espn("basketball/nba", "NBA") else list()
rugby <- if ("rugby" %in% sports_choisis) c(resultats_espn("rugby/270559", "Top 14", 7, 4),
                                            resultats_espn("rugby/180659", "Six Nations", 7, 3)) else list()

# ---------- Carburant le moins cher (données ouvertes du gouvernement) ----------

codes_carburant <- c(E10 = "e10", SP95 = "sp95", SP98 = "sp98", GAZOLE = "gazole", E85 = "e85", GPLC = "gplc")
code_carb <- unname(si_null(codes_carburant[toupper(si_null(reglages$carburant, "E10"))], "e10"))
if (is.na(code_carb)) code_carb <- "e10"
stations_proches <- function(lat, lon, zone) tryCatch({
  champ <- paste0(code_carb, "_prix")
  j <- get_json("https://data.economie.gouv.fr/api/explore/v2.1/catalog/datasets/prix-des-carburants-en-france-flux-instantane-v2/records",
                list(where = sprintf("within_distance(geom, geom'POINT(%.5f %.5f)', 5km) and %s is not null", lon, lat, champ),
                     order_by = paste(champ, "asc"), limit = 3))
  lapply(si_null(j$results, list()), function(s) list(
    zone = zone,
    adresse = tools::toTitleCase(tolower(paste0(si_null(s$adresse, ""), ", ", si_null(s$ville, "")))),
    prix = paste(nombre_fr(as.numeric(s[[champ]]), 3), "€"),
    lien = if (!is.null(s$geom$lat)) sprintf("https://www.google.com/maps/search/?api=1&query=%.5f,%.5f", s$geom$lat, s$geom$lon)
           else paste0("https://www.google.com/maps/search/", utils::URLencode(paste(s$adresse, s$ville)))))
}, error = function(e) { message("Carburant indisponible (", zone, ") : ", conditionMessage(e)); list() })
carburant <- c(stations_proches(as.numeric(reglages$latitude), as.numeric(reglages$longitude), reglages$ville),
               if (!is.null(coords_travail)) stations_proches(coords_travail[1], coords_travail[2], reglages$ville_travail) else list())

# ---------- Qualité de l'air et pollens (Open-Meteo) ----------

air <- tryCatch({
  q <- get_json("https://air-quality-api.open-meteo.com/v1/air-quality", list(
    latitude = as.numeric(reglages$latitude), longitude = as.numeric(reglages$longitude),
    current = "european_aqi",
    hourly = "grass_pollen,birch_pollen,ragweed_pollen,alder_pollen,mugwort_pollen,olive_pollen",
    forecast_days = 1, timezone = "Europe/Paris"))
  aqi <- si_null(q$current$european_aqi, NA)
  qualite <- if (is.na(aqi)) "inconnue" else if (aqi <= 20) "bonne" else if (aqi <= 40) "assez bonne" else
    if (aqi <= 60) "moyenne" else if (aqi <= 80) "médiocre" else if (aqi <= 100) "mauvaise" else "très mauvaise"
  noms_pollen <- c(grass_pollen = "graminées", birch_pollen = "bouleau", ragweed_pollen = "ambroisie",
                   alder_pollen = "aulne", mugwort_pollen = "armoise", olive_pollen = "olivier")
  niveaux <- vapply(names(noms_pollen), function(k) {
    v <- suppressWarnings(max(valeurs(q$hourly[[k]]), na.rm = TRUE))
    if (!is.finite(v) || v < 1) "" else paste(noms_pollen[[k]], if (v < 10) "faible" else if (v < 50) "modéré" else "élevé")
  }, "")
  niveaux <- niveaux[nzchar(niveaux)]
  sprintf("Qualité de l'air %s%s · Pollens : %s", qualite, if (is.na(aqi)) "" else sprintf(" (indice %d)", round(aqi)),
          if (length(niveaux)) paste(niveaux, collapse = ", ") else "très faibles")
}, error = function(e) { message("Qualité de l'air indisponible : ", conditionMessage(e)); "" })

# ---------- Veille scientifique (PubMed + tri par IA) ----------

# Spécialités disponibles : fragment de recherche PubMed + mots-clés pour le tri
pk_terms <- '(pharmacokinetic*[tiab] OR "drug monitoring"[tiab] OR dosing[tiab] OR "drug interaction*"[tiab] OR toxicity[ti])'
specialites_dispo <- list(
  pharmacologie = list(nom = "Pharmacologie", poids = 3,
    requete = paste('("Ther Drug Monit"[ta] OR "Clin Pharmacokinet"[ta] OR "CPT Pharmacometrics Syst Pharmacol"[ta]',
                    'OR ("model-informed precision dosing"[tiab] OR "population pharmacokinetic*"[tiab] OR "therapeutic drug monitoring"[tiab]))'),
    mots = c("population pharmacokinetic", "poppk", "nonlinear mixed", "nonmem", "monolix", "bayesian", "model-informed",
             "precision dosing", "mipd", "external validation", "machine learning", "monte carlo", "target attainment",
             "therapeutic drug monitoring", "pharmacokinetic")),
  toxicologie = list(nom = "Toxicologie", poids = 3,
    requete = '("J Anal Toxicol"[ta] OR "Clin Toxicol (Phila)"[ta] OR "Toxicol Anal Clin"[ta] OR "new psychoactive substance*"[tiab])',
    mots = c("poisoning", "intoxication", "overdose", "antidote", "poison center", "lc-ms", "mass spectrometry", "hrms",
             "screening", "new psychoactive", "nps", "dried blood spot")),
  infectiologie = list(nom = "Infectiologie", poids = 3,
    requete = '("J Antimicrob Chemother"[ta] AND (pharmacokinetic*[tiab] OR "therapeutic drug monitoring"[tiab] OR dosing[tiab]))',
    mots = c("beta-lactam", "β-lactam", "piperacillin", "meropenem", "cefepime", "cefotaxime", "ceftriaxone", "amoxicillin",
             "cefazolin", "vancomycin", "linezolid", "daptomycin", "amikacin", "gentamicin", "antibiotic", "antimicrobial",
             "antifungal", "voriconazole")),
  reanimation = list(nom = "Réanimation", poids = 2,
    requete = paste('(("critically ill"[tiab] OR "intensive care"[tiab] OR "septic shock"[tiab]) AND', pk_terms, ')'),
    mots = c("critically ill", "intensive care", "icu", "septic", "sepsis", "augmented renal clearance", "ecmo",
             "continuous renal replacement")),
  orthopedie = list(nom = "Infections ostéo-articulaires", poids = 3,
    requete = '("bone and joint infection*"[tiab] OR "prosthetic joint infection*"[tiab] OR osteomyelitis[ti])',
    mots = c("bone and joint", "prosthetic joint", "periprosthetic", "osteomyelitis", "osteoarticular", "orthopedic", "orthopaedic")),
  neurologie = list(nom = "Antiépileptiques", poids = 2,
    requete = paste('((antiepileptic*[tiab] OR antiseizure[tiab] OR levetiracetam[tiab] OR lamotrigine[tiab] OR valpro*[tiab]) AND', pk_terms, ')'),
    mots = c("antiepileptic", "antiseizure", "valpro", "levetiracetam", "lamotrigine", "carbamazepine", "phenytoin", "lacosamide")),
  transplantation = list(nom = "Transplantation", poids = 2,
    requete = paste('((tacrolimus[tiab] OR ciclosporin[tiab] OR cyclosporine[tiab] OR mycophenol*[tiab] OR everolimus[tiab]) AND', pk_terms, ')'),
    mots = c("tacrolimus", "ciclosporin", "cyclosporine", "mycophenol", "everolimus", "transplant")),
  nephrologie = list(nom = "Néphrologie", poids = 2,
    requete = paste('(("renal impairment"[tiab] OR "kidney injury"[tiab] OR hemodialysis[tiab] OR "renal replacement"[tiab]) AND', pk_terms, ')'),
    mots = c("renal impairment", "kidney injury", "hemodialysis", "haemodialysis", "renal replacement", "glomerular filtration")),
  oncologie = list(nom = "Oncologie", poids = 2,
    requete = paste('((cancer[ti] OR oncolog*[ti] OR "tyrosine kinase inhibitor*"[tiab] OR methotrexate[tiab]) AND', pk_terms, ')'),
    mots = c("cancer", "oncology", "tyrosine kinase", "methotrexate", "busulfan", "chemotherapy")),
  cardiologie = list(nom = "Cardiologie", poids = 2,
    requete = paste('((anticoagulant*[tiab] OR apixaban[tiab] OR rivaroxaban[tiab] OR digoxin[tiab] OR amiodarone[tiab]) AND', pk_terms, ')'),
    mots = c("anticoagulant", "apixaban", "rivaroxaban", "dabigatran", "digoxin", "amiodarone", "heparin")),
  psychiatrie = list(nom = "Psychiatrie", poids = 2,
    requete = paste('((lithium[tiab] OR clozapine[tiab] OR antidepressant*[tiab] OR antipsychotic*[tiab]) AND', pk_terms, ')'),
    mots = c("lithium", "clozapine", "antidepressant", "antipsychotic")),
  pediatrie = list(nom = "Pédiatrie", poids = 2,
    requete = paste('((neonat*[tiab] OR pediatric*[tiab] OR paediatric*[tiab] OR children[ti]) AND', pk_terms, ')'),
    mots = c("neonat", "pediatric", "paediatric", "children", "infant"))
)
specialites_choisies <- intersect(liste_reglage(reglages$specialites), names(specialites_dispo))
if (!length(specialites_choisies)) specialites_choisies <- c("pharmacologie", "toxicologie")
mots_cles_perso <- gsub('"', "", liste_reglage(reglages$mots_cles_veille))
revues_perso <- gsub('"', "", liste_reglage(reglages$revues_veille))

fragments <- vapply(specialites_choisies, function(k) specialites_dispo[[k]]$requete, "")
if (length(mots_cles_perso)) fragments <- c(fragments, paste0('"', mots_cles_perso, '"[tiab]'))
if (length(revues_perso)) fragments <- c(fragments, paste0('"', revues_perso, '"[ta]'))
requete_pubmed <- paste(fragments, collapse = " OR ")
message("Spécialités de la veille : ", paste(specialites_choisies, collapse = ", "))

extraire <- function(x, motif) {
  m <- regmatches(x, regexec(motif, x, perl = TRUE))[[1]]
  if (length(m) < 2) "" else m[2]
}

articles <- tryCatch({
  pm <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  s <- get_json(paste0(pm, "esearch.fcgi"), list(db = "pubmed", term = requete_pubmed, reldate = 7,
                datetype = "edat", retmax = 120, retmode = "json", sort = "pub_date"))
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
           resume = nettoyer(paste(resumes, collapse = " ")),
           conclusion = nettoyer(paste(regmatches(b, gregexpr(
             "(?si)<AbstractText[^>]*Label=\"CONCLUSIONS?[^\"]*\"[^>]*>.*?</AbstractText>", b, perl = TRUE))[[1]],
             collapse = " ")))
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

# ---------- Apprentissage à partir de tes avis (fichier avis.csv) ----------

mots_vides <- c("with", "from", "that", "this", "were", "have", "been", "their", "these", "which", "between", "after",
                "during", "patients", "patient", "study", "studies", "using", "based", "results", "methods", "conclusion",
                "conclusions", "background", "among", "than", "more", "less", "both", "into", "also", "other", "versus",
                "case", "cases", "report", "analysis", "data", "clinical", "effect", "effects", "associated", "compared")
mots_article <- function(texte) {
  m <- unique(regmatches(tolower(texte), gregexpr("[a-z][a-z-]{3,}", tolower(texte)))[[1]])
  setdiff(m, mots_vides)
}
avis <- tryCatch(if (file.exists("avis.csv")) utils::read.csv("avis.csv", stringsAsFactors = FALSE,
                 colClasses = c(pmid = "character")) else NULL, error = function(e) NULL)
avis_negatifs <- character(); poids_mots <- numeric(); poids_revues <- numeric()
if (!is.null(avis) && nrow(avis)) {
  avis <- avis[!duplicated(avis$pmid, fromLast = TRUE), , drop = FALSE]
  avis_negatifs <- avis$pmid[avis$avis < 0]
  ids_avis <- tail(avis$pmid, 300)
  appris <- tryCatch({
    xml <- get_texte("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
                     list(db = "pubmed", id = paste(ids_avis, collapse = ","), retmode = "xml"))
    blocs <- regmatches(xml, gregexpr("(?s)<PubmedArticle>.*?</PubmedArticle>", xml, perl = TRUE))[[1]]
    lapply(blocs, function(b) list(
      pmid = extraire(b, "<PMID[^>]*>(\\d+)</PMID>"),
      journal = nettoyer(extraire(b, "(?s)<ISOAbbreviation>(.*?)</ISOAbbreviation>")),
      mots = mots_article(nettoyer(paste(extraire(b, "(?s)<ArticleTitle[^>]*>(.*?)</ArticleTitle>"),
             paste(regmatches(b, gregexpr("(?s)<AbstractText\\b[^>]*>.*?</AbstractText>", b, perl = TRUE))[[1]], collapse = " "))))))
  }, error = function(e) { message("Apprentissage impossible : ", conditionMessage(e)); list() })
  if (length(appris)) {
    valeur <- setNames(avis$avis, avis$pmid)
    somme <- list(); compte <- list(); par_revue <- list()
    for (ap in appris) {
      v <- valeur[[ap$pmid]]
      if (is.null(v)) next
      for (m in ap$mots) { somme[[m]] <- si_null(somme[[m]], 0) + v; compte[[m]] <- si_null(compte[[m]], 0) + 1 }
      if (nzchar(ap$journal)) par_revue[[ap$journal]] <- si_null(par_revue[[ap$journal]], 0) + v
    }
    if (length(somme)) {
      w <- unlist(somme) / (unlist(compte) + 2)
      w <- w[unlist(compte) >= 2]
      poids_mots <- head(w[order(-abs(w))], 300)
    }
    if (length(par_revue)) poids_revues <- unlist(par_revue)
  }
  message(nrow(avis), " avis enregistrés, ", length(poids_mots), " mots appris")
}
bonus_avis <- function(a) {
  if (!length(poids_mots) && !length(poids_revues)) return(0)
  m <- intersect(mots_article(paste(a$titre, a$resume)), names(poids_mots))
  b <- if (length(m)) 4 * sum(poids_mots[m]) else 0
  r <- if (a$journal %in% names(poids_revues)) 2 * sign(poids_revues[[a$journal]]) else 0
  max(-10, min(10, b)) + r
}

veille <- list(); veille_top <- list(); veille_note <- ""
cle_mistral <- Sys.getenv("MISTRAL_API_KEY")
cle_api <- Sys.getenv("ANTHROPIC_API_KEY")
ia_disponible <- nzchar(cle_mistral) || nzchar(cle_api)

extraire_json <- function(texte) {
  debut <- regexpr("{", texte, fixed = TRUE)
  fin <- max(gregexpr("}", texte, fixed = TRUE)[[1]])
  jsonlite::fromJSON(substr(texte, debut, fin), simplifyVector = FALSE)
}

appel_mistral <- function(prompt, modele) {
  rep <- request("https://api.mistral.ai/v1/chat/completions") |>
    req_auth_bearer_token(cle_mistral) |>
    req_body_json(list(model = modele, temperature = 0.2,
                       response_format = list(type = "json_object"),
                       messages = list(list(role = "user", content = prompt)))) |>
    req_timeout(300) |> req_retry(max_tries = 4) |>
    req_perform() |> resp_body_string() |> jsonlite::fromJSON(simplifyVector = FALSE)
  rep$choices[[1]]$message$content
}

appel_claude <- function(prompt) {
  rep <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(`x-api-key` = cle_api, `anthropic-version` = "2023-06-01") |>
    req_body_json(list(model = "claude-sonnet-5-5", max_tokens = 6000,
                       messages = list(list(role = "user", content = prompt)))) |>
    req_timeout(300) |> req_retry(max_tries = 2) |>
    req_perform() |> resp_body_string() |> jsonlite::fromJSON(simplifyVector = FALSE)
  paste(vapply(rep$content, function(b) si_null(b$text, ""), ""), collapse = "")
}

demander_ia <- function(prompt) {
  if (nzchar(cle_mistral)) {
    texte <- tryCatch(appel_mistral(prompt, reglages$modele_ia), error = function(e) {
      message("Mistral (", reglages$modele_ia, ") en échec : ", conditionMessage(e), " ; essai avec mistral-small-latest")
      appel_mistral(prompt, "mistral-small-latest")
    })
  } else {
    texte <- appel_claude(prompt)
  }
  extraire_json(texte)
}

selection <- NULL
if (ia_disponible && length(articles)) {
  selection <- tryCatch({
    liste <- paste(vapply(articles, function(a) sprintf("PMID %s | %s | %s\nTitre : %s\nRésumé : %s",
      a$pmid, a$journal, a$date, a$titre, raccourcir(a$resume, 1500)), ""), collapse = "\n\n")
    demander_ia(paste(consigne, liste, sep = "\n\n"))
  }, error = function(e) { message("Tri par IA impossible : ", conditionMessage(e)); NULL })
}

par_pmid <- setNames(articles, vapply(articles, function(a) a$pmid, ""))
if (!is.null(selection) && length(selection$articles)) {
  veille <- lapply(selection$articles, function(s) {
    a <- par_pmid[[as.character(s$pmid)]]
    if (is.null(a)) return(NULL)
    p <- as.character(si_null(s$priorite, 1))
    list(pmid = a$pmid, has_badge = TRUE, prio = p, badge = unname(si_null(badges[p], "")), theme = si_null(s$theme, ""),
         titre = a$titre, lien = lien_pubmed(a$pmid), source = paste(a$journal, "·", a$date),
         has_brief = TRUE, brief_label = "En bref", brief = si_null(s$en_bref, ""),
         has_pour_toi = TRUE, pour_toi = si_null(s$pour_toi, ""))
  })
  veille <- Filter(Negate(is.null), veille)
  veille <- veille[order(-as.numeric(vapply(veille, function(v) v$prio, "")))]
  veille_top <- Filter(Negate(is.null), lapply(si_null(selection$top, list()), function(id) {
    a <- par_pmid[[as.character(id)]]
    if (is.null(a)) NULL else list(titre = a$titre, lien = lien_pubmed(a$pmid))
  }))
  veille_note <- sprintf("%d articles parus ces 7 derniers jours, %d retenus.", length(articles), length(veille))
} else {
  # Tri automatique par mots-clés (sans IA)
  themes_veille <- lapply(specialites_choisies, function(k)
    list(nom = specialites_dispo[[k]]$nom, poids = specialites_dispo[[k]]$poids, motifs = specialites_dispo[[k]]$mots))
  names(themes_veille) <- vapply(themes_veille, function(t) t$nom, "")
  if (length(mots_cles_perso)) themes_veille[["Mes mots-clés"]] <- list(poids = 4, motifs = tolower(mots_cles_perso))
  bonus_stp <- c("therapeutic drug monitoring", "tdm", "dosing", "dose adjustment", "trough", "plasma concentration")
  revues_ref <- c("Ther Drug Monit", "Clin Pharmacokinet", "CPT Pharmacometrics Syst Pharmacol", "J Antimicrob Chemother",
                  "Clin Toxicol (Phila)", "J Anal Toxicol", "Toxicol Anal Clin", "Rev Francoph Lab", revues_perso)
  compter <- function(texte, motifs) sum(vapply(motifs, function(m)
    grepl(paste0("(?<![a-z])\\Q", m, "\\E"), texte, perl = TRUE), TRUE))
  notes <- lapply(articles, function(a) {
    titre <- tolower(a$titre); tout <- tolower(paste(a$titre, a$resume))
    par_theme <- vapply(themes_veille, function(t)
      t$poids * (compter(tout, t$motifs) + compter(titre, t$motifs)), 0)
    score <- sum(par_theme) + 2 * compter(tout, bonus_stp) + (if (a$journal %in% revues_ref) 1 else 0) +
      bonus_avis(a)
    list(a = a, score = score, theme = if (max(par_theme) > 0) names(which.max(par_theme)) else "Autre")
  })
  notes <- Filter(function(n) !(n$a$pmid %in% avis_negatifs), notes)
  notes <- notes[order(-vapply(notes, function(n) n$score, 0))]
  notes <- head(Filter(function(n) n$score > 0, notes), 25)
  premieres_phrases <- function(x, n = 2) {
    if (!nzchar(x)) return("")
    ph <- regmatches(x, gregexpr("[^.!?]+[.!?]", x))[[1]]
    raccourcir(trimws(paste(head(ph, n), collapse = " ")), 320)
  }
  veille <- lapply(notes, function(n) {
    a <- n$a
    p <- if (n$score >= 14) "3" else if (n$score >= 7) "2" else "1"
    bref <- if (nzchar(a$conclusion)) raccourcir(a$conclusion, 280) else premieres_phrases(a$resume)
    list(pmid = a$pmid, has_badge = TRUE, prio = p, badge = unname(badges[p]), theme = n$theme,
         titre = a$titre, lien = lien_pubmed(a$pmid), source = paste(a$journal, "·", a$date),
         has_brief = nzchar(bref), brief_label = if (nzchar(a$conclusion)) "Conclusion des auteurs" else "En bref",
         brief = bref, has_pour_toi = FALSE)
  })
  veille <- veille[order(-as.numeric(vapply(veille, function(v) v$prio, "")))]
  veille_top <- lapply(head(Filter(function(v) v$prio == "3", veille), 3), function(v) list(titre = v$titre, lien = v$lien))
  veille_note <- sprintf("Tri automatique par mots-clés : %d articles parus ces 7 derniers jours, %d retenus.",
                         length(articles), length(veille))
}

# Traduction en français et découpage : 5 articles visibles, les suivants repliés
# Sans adresse e-mail, le service de secours limite le volume quotidien : on traduit alors
# tous les titres, mais seulement les résumés des premiers articles.
nb_resumes_traduits <- if (nzchar(email_trad)) length(veille) else 8
veille <- lapply(seq_along(veille), function(i) {
  v <- veille[[i]]
  v$titre <- traduire(v$titre)
  if (i <= nb_resumes_traduits && isTRUE(v$has_brief) && !isTRUE(v$has_pour_toi)) v$brief <- traduire(v$brief)
  v
})
veille_top <- lapply(veille_top, function(t) { t$titre <- traduire(t$titre); t })
veille_principal <- head(veille, 5)
veille_suite <- if (length(veille) > 5) veille[-(1:5)] else list()

# ---------- Favoris (fichier favoris.csv du dépôt, avec catégories) ----------

favoris <- tryCatch({
  if (file.exists("favoris.csv")) {
    f <- utils::read.csv("favoris.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                         strip.white = TRUE)
    if (!"categorie" %in% names(f)) f$categorie <- ""
    f$categorie[is.na(f$categorie) | !nzchar(f$categorie)] <- "Favoris"
    f <- f[!is.na(f$nom) & !is.na(f$lien) & nzchar(f$nom) & nzchar(f$lien), , drop = FALSE]
    lapply(seq_len(nrow(f)), function(i) list(nom = f$nom[i], lien = f$lien[i], categorie = f$categorie[i]))
  } else list()
}, error = function(e) {
  message("Favoris illisibles : ", conditionMessage(e))
  list()
})
categories_fav <- unique(vapply(favoris, function(f) f$categorie, ""))
favoris_groupes <- lapply(categories_fav, function(cn) list(
  categorie = cn,
  liens = Filter(function(f) f$categorie == cn, favoris)
))

# ---------- Taux immobiliers en Île-de-France (baromètre mensuel CAFPI) ----------

mois_slug <- c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout",
               "septembre", "octobre", "novembre", "decembre")
taux_idf <- NULL
premier_du_mois <- as.Date(format(Sys.Date(), "%Y-%m-01"))
for (k in 0:3) {
  d <- seq(premier_du_mois, by = "-1 month", length.out = k + 1)[k + 1]
  lt <- as.POSIXlt(d)
  url <- sprintf("https://www.cafpi.fr/credit-immobilier/barometre-taux/actualites-taux/analyse-taux-credit-immobilier-%s-%d",
                 mois_slug[lt$mon + 1], lt$year + 1900)
  html <- tryCatch(get_texte(url), error = function(e) NULL)
  if (is.null(html)) next
  txt <- gsub("\u00a0|\u202f", " ", nettoyer(html))
  m <- regmatches(txt, regexec("[IÎ]le-de-France\\s+((?:\\d,\\d{2}\\s*%\\s*){4})", txt, perl = TRUE))[[1]]
  if (length(m) < 2) next
  v <- gsub("\\s+", " ", regmatches(m[2], gregexpr("\\d,\\d{2}\\s*%", m[2]))[[1]])
  nat <- regmatches(txt, regexec("Taux fixe 25 ans\\s+((?:\\d,\\d{2}\\s*%\\s*)+)", txt, perl = TRUE))[[1]]
  moyenne <- if (length(nat) >= 2) tail(gsub("\\s+", " ", regmatches(nat[2], gregexpr("\\d,\\d{2}\\s*%", nat[2]))[[1]]), 1) else ""
  taux_idf <- list(
    mois = paste(mois[lt$mon + 1], lt$year + 1900), lien = url,
    lignes = list(list(profil = "Sur 25 ans", taux = v[4]), list(profil = "Sur 20 ans", taux = v[3]),
                  list(profil = "Sur 15 ans", taux = v[2])),
    moyenne = moyenne)
  message("Taux immobiliers trouvés : baromètre CAFPI de ", taux_idf$mois)
  break
}
if (is.null(taux_idf)) message("Taux immobiliers : baromètre CAFPI introuvable sur les 4 derniers mois")

# ---------- Réglages actuels, transmis à la page pour l'écran de réglages ----------

config_json <- jsonlite::toJSON(list(
  ville = reglages$ville,
  ville_travail = si_null(reglages$ville_travail, ""),
  equipes_foot = reglages$equipes_foot,
  themes_actu = reglages$themes_actu,
  rubriques_actu = liste_reglage(reglages$rubriques_actu),
  specialites = specialites_choisies,
  mots_cles_veille = si_null(reglages$mots_cles_veille, ""),
  revues_veille = si_null(reglages$revues_veille, ""),
  sports = sports_choisis,
  carburant = toupper(si_null(reglages$carburant, "E10")),
  options_specialites = lapply(names(specialites_dispo), function(k) list(code = k, nom = specialites_dispo[[k]]$nom)),
  options_rubriques = lapply(names(rubriques_dispo), function(k)
    list(code = k, nom = if (k == "ville") "Ma ville" else rubriques_dispo[[k]]$nom)),
  favoris = lapply(favoris, function(f) list(nom = f$nom, lien = f$lien, categorie = f$categorie))
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
  favoris_groupes = favoris_groupes,
  markets = marches,
  cryptos = cryptos,
  news = actus_principal,
  news_suite = actus_suite,
  has_news_suite = length(actus_suite) > 0,
  n_news_suite = length(actus_suite),
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
  veille = veille_principal,
  veille_suite = veille_suite,
  has_suite = length(veille_suite) > 0,
  n_suite = length(veille_suite),
  has_veille_note = nzchar(veille_note),
  veille_note = veille_note,
  has_top = length(veille_top) > 0,
  top = veille_top,
  has_taux = !is.null(taux_idf),
  taux = si_null(taux_idf$lignes, list()),
  taux_mois = si_null(taux_idf$mois, ""),
  taux_lien = si_null(taux_idf$lien, "https://www.cafpi.fr/credit-immobilier/barometre-taux"),
  has_moyenne = nzchar(si_null(taux_idf$moyenne, "")),
  taux_moyenne = si_null(taux_idf$moyenne, ""),
  ville = reglages$ville,
  meteo_lien = paste0("https://www.google.com/search?q=", utils::URLencode(paste("météo", reglages$ville), reserved = TRUE)),
  config_json = config_json,
  air = air, has_air = nzchar(air),
  sport_foot = "foot" %in% sports_choisis,
  sport_f1 = "f1" %in% sports_choisis,
  sport_nba = "nba" %in% sports_choisis, nba = nba, has_nba = length(nba) > 0,
  sport_rugby = "rugby" %in% sports_choisis, rugby = rugby, has_rugby = length(rugby) > 0,
  carburant = carburant, has_carburant = length(carburant) > 0,
  carburant_nom = toupper(si_null(reglages$carburant, "E10")),
  genere_ms = format(round(as.numeric(Sys.time()) * 1000), scientific = FALSE),
  updated = format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%H:%M", tz = "Europe/Paris")
)

message("Traductions : ", stats_trad[["ok"]], " réussies, ", stats_trad[["echec"]], " en échec")
modele <- paste(readLines("template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
page <- whisker.render(modele, donnees)

dir.create("public", showWarnings = FALSE)
writeLines(page, "public/index.html", useBytes = TRUE)
message("Page générée : public/index.html")
