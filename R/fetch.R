# fetch.R — polite HTTP against forums.castanet.net.
#
# robots.txt posture (verified for drought-sna, June 2026): viewforum.php and
# viewtopic.php are allowed for a generic crawler; only /posting.php is
# disallowed. Named AI/SEO bots (GPTBot, Ahrefs, ...) are fully blocked, so
# this crawler must NOT present as one — it uses an honest identifying UA with
# a contact address, and a conservative delay.
suppressMessages({library(httr2); library(rvest); library(xml2); library(stringr)})

UA <- Sys.getenv(
  "OKCP_UA",
  "ok-civic-pulse research crawler (njatel@limnology.ca; personal research)")
CRAWL_DELAY <- as.numeric(Sys.getenv("OKCP_DELAY", "2.0"))

fetch_html <- function(url) {
  Sys.sleep(CRAWL_DELAY)
  request(url) |>
    req_user_agent(UA) |>
    req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_html()
}

# phpBB decorates hrefs with an ephemeral session id; strip it so ids are stable.
clean_url <- function(href) {
  href <- str_replace(href, "(?i)&?sid=[0-9a-f]+", "")
  str_replace(href, "^\\./", "https://forums.castanet.net/")
}

id_from <- function(href, key) {
  m <- str_match(href, paste0(key, "=([0-9]+)"))[, 2]
  suppressWarnings(as.integer(m))
}
