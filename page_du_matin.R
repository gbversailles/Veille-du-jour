# Ma page du matin — étape 1 : météo + bourse
# Génère public/index.html à partir de template.html

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(whisker)
})

message("jsonlite ", as.character(packageVersion("jsonlite")), " chargé depuis ", find.package("jsonlite"))

# ---------- Utilitaires ----------

jours <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")
mois  <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
           "août", "septembre", "octobre", "novembre", "décembre")

maintenant <- as.POSIXlt(Sys.time(), tz = "Europe/Paris")
date_fr <- sprintf("%s %d %s %d",
                   jours[maintenant$wday + 1], maintenant$mday,
                   mois[maintenant$mon + 1], maintenant$year + 1900)
date_fr <- paste0(toupper(substr(date_fr, 1, 1)), substr(date_fr, 2, nchar(date_fr)))

nombre_fr <- function(x, decimales = 2) {
  formatC(x, format = "f", digits = decimales, big.mark = " ", decimal.mark = ",")
}

get_json <- function(url, query = list()) {
  request(url) |>
    req_url_query(!!!query) |>
    req_user_agent("Mozilla/5.0 (page-du-matin)") |>
    req_retry(max_tries = 3) |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
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
    latitude = 48.914, longitude = 2.285,
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
       now_label = paste0("Asnières, ", libelle_meteo(m$current$weather_code)),
       now_icon = icone_meteo(m$current$weather_code),
       days = jours_prevus,
       advice = conseil)
}, error = function(e) {
  message("Météo indisponible : ", conditionMessage(e))
  list(now_temp = "–", now_label = "météo indisponible", now_icon = icone_meteo(NA),
       days = list(), advice = "La météo n'a pas pu être récupérée ce matin.")
})

# ---------- Bourse (cours de clôture) ----------

cours <- function(symbole, nom, decimales = 2) {
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
         value = nombre_fr(dernier, decimales),
         change = paste0(if (variation >= 0) "+" else "−", nombre_fr(abs(variation), 2), " %"),
         dir = if (variation >= 0) "up" else "down")
  }, error = function(e) {
    message("Cours indisponible pour ", nom, " : ", conditionMessage(e))
    list(name = nom, value = "indisponible", change = "", dir = "flat")
  })
}

marches <- list(
  cours("^NDX", "Nasdaq-100"),
  cours("^FCHI", "CAC 40"),
  cours("^GSPC", "S&P 500"),
  cours("EURUSD=X", "EUR / USD", decimales = 4)
)

# ---------- Assemblage ----------

donnees <- list(
  date = date_fr,
  now_temp = meteo$now_temp,
  now_label = meteo$now_label,
  now_icon = meteo$now_icon,
  days = meteo$days,
  advice = meteo$advice,
  markets = marches,
  updated = format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%H:%M", tz = "Europe/Paris")
)

modele <- paste(readLines("template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
page <- whisker.render(modele, donnees)

dir.create("public", showWarnings = FALSE)
writeLines(page, "public/index.html", useBytes = TRUE)
message("Page générée : public/index.html")
