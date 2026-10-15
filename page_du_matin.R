# Ma page du matin — étape 1 : météo + bourse
# Génère public/index.html à partir de template.html

suppressPackageStartupMessages({
  library(httr2)
  library(whisker)
})

if (!requireNamespace("jsonlite", quietly = TRUE)) {
     install.packages("jsonlite", repos = "https://cloud.r-project.org")
   }

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
    resp_body_json()
}

# ---------- Météo (Open-Meteo, sans clé) ----------

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

meteo <- tryCatch({
  m <- get_json("https://api.open-meteo.com/v1/forecast", list(
    latitude = 48.914, longitude = 2.285,
    current = "temperature_2m,weather_code",
    hourly = "temperature_2m,precipitation_probability",
    daily = "temperature_2m_min,temperature_2m_max,precipitation_probability_max",
    timezone = "Europe/Paris", forecast_days = 1
  ))
  heures_voulues <- c(8, 12, 17, 21)
  heures_api <- as.integer(substr(unlist(m$hourly$time), 12, 13))
  temp_h  <- unlist(lapply(m$hourly$temperature_2m, function(v) if (is.null(v)) NA else v))
  pluie_h <- unlist(lapply(m$hourly$precipitation_probability, function(v) if (is.null(v)) NA else v))
  heures <- lapply(heures_voulues, function(h) {
    i <- match(h, heures_api)
    list(time = paste0(h, "h"),
         temp = if (is.na(i)) "–" else paste0(round(temp_h[i]), "°"),
         rain = if (is.na(i) || is.na(pluie_h[i])) "–" else paste0("pluie ", pluie_h[i], " %"))
  })
  pluie_max <- unlist(m$daily$precipitation_probability_max)[1]
  tmin <- round(unlist(m$daily$temperature_2m_min)[1])
  tmax <- round(unlist(m$daily$temperature_2m_max)[1])
  conseil <- if (!is.na(pluie_max) && pluie_max >= 50) {
    sprintf("Entre %d° et %d°. Risque de pluie jusqu'à %d %% : prends un parapluie.", tmin, tmax, pluie_max)
  } else {
    sprintf("Entre %d° et %d°. Faible risque de pluie, pas besoin de parapluie.", tmin, tmax)
  }
  list(
    now_temp = paste0(round(m$current$temperature_2m), "°"),
    now_label = libelle_meteo(m$current$weather_code),
    hours = heures,
    advice = conseil
  )
}, error = function(e) {
  message("Météo indisponible : ", conditionMessage(e))
  list(now_temp = "–", now_label = "météo indisponible", hours = list(),
       advice = "La météo n'a pas pu être récupérée ce matin.")
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
  hours = meteo$hours,
  advice = meteo$advice,
  markets = marches,
  updated = format(as.POSIXct(Sys.time(), tz = "Europe/Paris"), "%H:%M", tz = "Europe/Paris")
)

modele <- paste(readLines("template.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
page <- whisker.render(modele, donnees)

dir.create("public", showWarnings = FALSE)
writeLines(page, "public/index.html", useBytes = TRUE)
message("Page générée : public/index.html")
