# election.R — the 2026 BC general local election calendar.
#
# Dates per Elections BC. Voting day for a general local election is the third
# Saturday of October every four years.
#
# The pre-campaign period is ALREADY RUNNING as of this project's start
# (2026-08-03), which is why the timeline view leads with phase context rather
# than treating the election as a future event.
ELECTION <- list(
  voting_day        = as.Date("2026-10-17"),
  pre_campaign_from = as.Date("2026-07-20"),
  pre_campaign_to   = as.Date("2026-09-18"),
  nominations_from  = as.Date("2026-09-01"),
  nominations_to    = as.Date("2026-09-11"),
  campaign_from     = as.Date("2026-09-19"),
  campaign_to       = as.Date("2026-10-17")
)

election_phase <- function(d = Sys.Date()) {
  d <- as.Date(d)
  if (d > ELECTION$voting_day)          return("post-election")
  if (d >= ELECTION$campaign_from)      return("campaign period")
  if (d >= ELECTION$pre_campaign_from)  return("pre-campaign period")
  "pre-writ"
}

days_to_vote <- function(d = Sys.Date()) as.integer(ELECTION$voting_day - as.Date(d))

# Shaded bands for the timeline chart. Kept in one place so the dashboard and
# the PDF cannot drift apart.
election_bands <- function() {
  data.frame(
    phase = c("Pre-campaign", "Nominations", "Campaign"),
    from  = c(ELECTION$pre_campaign_from, ELECTION$nominations_from, ELECTION$campaign_from),
    to    = c(ELECTION$pre_campaign_to,   ELECTION$nominations_to,   ELECTION$campaign_to),
    stringsAsFactors = FALSE
  )
}
