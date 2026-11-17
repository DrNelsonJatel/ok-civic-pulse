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
  resp <- request(url) |>
    req_user_agent(UA) |>
    req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    req_timeout(30) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  # Since 2026-09-23 the forum sits behind a Cloudflare managed challenge (HTTP
  # 403 "Just a moment..."). For 56 days the callers below swallowed that as
  # "no threads" and the daily log read "0 threads, 0 posts, 0 errors". Raise a
  # NAMED error so callers can tell "host refuses us" from "nothing new".
  # Ported from drought-sna 0d4f025. Never try to get past the challenge.
  if (resp_status(resp) %in% c(403, 503) &&
      grepl("Just a moment|challenges\\.cloudflare\\.com", resp_body_string(resp)))
    stop("BLOCKED: Cloudflare challenge at ", url, " (HTTP ", resp_status(resp), ")", call. = FALSE)
  if (resp_status(resp) >= 400) stop("HTTP ", resp_status(resp), " at ", url, call. = FALSE)
  resp_body_html(resp)
}

is_blocked <- function(e) grepl("^BLOCKED", conditionMessage(e))

# phpBB decorates hrefs with an ephemeral session id; strip it so ids are stable.
clean_url <- function(href) {
  href <- str_replace(href, "(?i)&?sid=[0-9a-f]+", "")
  str_replace(href, "^\\./", "https://forums.castanet.net/")
}

id_from <- function(href, key) {
  m <- str_match(href, paste0(key, "=([0-9]+)"))[, 2]
  suppressWarnings(as.integer(m))
}
