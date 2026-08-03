#!/usr/bin/env Rscript
# Reddit free-tier connection check.
#
# Run this after creating the script app and setting credentials. It makes
# three cheap calls and tells you exactly which step failed, because Reddit's
# own error messages for auth problems are famously unhelpful (a wrong app
# TYPE and a wrong password both surface as the same 401).
source("R/reddit.R")
suppressMessages(library(httr2))

ok <- function(x) cat("  [ok]  ", x, "\n")
no <- function(x) cat("  [FAIL]", x, "\n")

cat("\n1. Credentials present in the environment\n")
vars <- c("REDDIT_CLIENT_ID","REDDIT_CLIENT_SECRET","REDDIT_USERNAME","REDDIT_PASSWORD")
vals <- Sys.getenv(vars)
for (v in vars) {
  if (nzchar(vals[[v]])) ok(sprintf("%s set (%d chars)", v, nchar(vals[[v]])))
  else no(sprintf("%s is EMPTY — add it to ~/.Renviron and restart R", v))
}
if (!all(nzchar(vals))) {
  cat("\nStopping: set the missing variables first.\n"); quit(status = 1)
}
# A very common mistake: pasting the app NAME instead of the client id, or
# including the surrounding quotes from the web page.
if (nchar(vals[["REDDIT_CLIENT_ID"]]) > 30)
  no("REDDIT_CLIENT_ID looks too long — it should be the ~14-char string under the app name, not the app name")
if (grepl('^["\']|["\']$', vals[["REDDIT_CLIENT_ID"]]))
  no("REDDIT_CLIENT_ID has quotes around it — ~/.Renviron takes bare values, no quotes")

cat("\n2. OAuth token\n")
tok <- tryCatch(.reddit_token(), error = function(e) e)
if (inherits(tok, "error")) {
  no(paste("token request failed:", conditionMessage(tok)))
  cat("\n  Most likely causes, in order:\n",
      "   - the app was created as 'web app' or 'installed app' instead of 'script'\n",
      "   - the Reddit account has 2FA on (password grant cannot work with 2FA)\n",
      "   - client secret copied from the wrong field\n",
      "   - the account running this is not the app's owner\n", sep = "")
  quit(status = 1)
}
ok(sprintf("access token acquired (%d chars)", nchar(tok)))

cat("\n3. Authenticated read of r/kelowna\n")
r <- tryCatch(reddit_get("/r/kelowna/about"), error = function(e) e)
if (inherits(r, "error")) { no(conditionMessage(r)); quit(status = 1) }
ok(sprintf("r/kelowna: %s subscribers, %s here now",
           format(r$data$subscribers, big.mark = ","),
           format(r$data$active_user_count %||% NA, big.mark = ",")))

cat("\n4. Volume sample (what a full-coverage crawl would cost)\n")
lst <- tryCatch(reddit_listing("kelowna", cutoff = Sys.Date() - 14, max_pages = 1L),
                error = function(e) e)
if (inherits(lst, "error")) { no(conditionMessage(lst)); quit(status = 1) }
span <- as.numeric(difftime(max(as.POSIXct(lst$created_utc, origin = "1970-01-01")),
                            min(as.POSIXct(lst$created_utc, origin = "1970-01-01")),
                            units = "days"))
ppd <- nrow(lst) / max(span, 0.1)
cpd <- sum(lst$num_comments) / max(span, 0.1)
ok(sprintf("%d posts over %.1f days -> %.0f posts/day, ~%.0f comments/day",
           nrow(lst), span, ppd, cpd))
cat(sprintf("\n  Daily full coverage needs roughly %.0f API calls/day.\n", ppd + 3))
cat(sprintf("  Free tier allows 100 queries/MINUTE (~144,000/day), so this uses about %.3f%%.\n",
            100 * (ppd + 3) / 144000))
cat("\n  Reminder: the free tier is NON-COMMERCIAL use only.\n\n")
