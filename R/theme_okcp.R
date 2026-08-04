# theme_okcp.R — shared plot theme.
#
# Hard rule carried over from the OBWB work: ALL chart text, axis tick labels
# and numbers included, is at least 12pt. The clamp is enforced here rather
# than trusted to each call site, because tick labels are exactly what silently
# drops to 8pt when a theme is set by base_size alone.
suppressMessages({library(ggplot2)})

MIN_PT <- 12

BRAND <- list(
  # One place to change the mark. This is a personal research project; the
  # report carries the Limnology Research wordmark as requested.
  org      = Sys.getenv("OKCP_BRAND", "Limnology Research"),
  product  = "Okanagan Civic Pulse",
  ink      = "#12303A",
  accent   = "#1F7A8C",
  warm     = "#C05621",
  muted    = "#6B7A82",
  paper    = "#FBFAF7"
)

# Scope colours are semantic, not decorative — they carry the product's core
# distinction, so they stay fixed everywhere.
SCOPE_COLOURS <- c(
  local      = "#1F7A8C",
  ballot     = "#2A9D8F",
  shared     = "#8AB17D",
  provincial = "#E9C46A",
  federal    = "#C05621",
  none       = "#B8C0C4"
)

theme_okcp <- function(base_size = 13) {
  if (base_size < MIN_PT)
    stop("theme_okcp: base_size ", base_size, " is below the ", MIN_PT,
         "pt minimum for chart text.", call. = FALSE)
  theme_minimal(base_size = base_size) +
    theme(
      text          = element_text(size = base_size, colour = BRAND$ink),
      # Explicit, not inherited: this is the line that actually keeps numeric
      # tick labels legible.
      axis.text     = element_text(size = max(MIN_PT, base_size - 1), colour = BRAND$ink),
      axis.title    = element_text(size = base_size),
      legend.text   = element_text(size = max(MIN_PT, base_size - 1)),
      legend.title  = element_text(size = base_size),
      strip.text    = element_text(size = base_size, face = "bold"),
      plot.title    = element_text(size = base_size + 3, face = "bold"),
      plot.subtitle = element_text(size = base_size, colour = BRAND$muted),
      plot.caption  = element_text(size = MIN_PT, colour = BRAND$muted, hjust = 0),
      panel.grid.minor = element_blank()
    )
}

# geom_text/geom_label take size in MILLIMETRES, not points — a bare size=12
# there is ~34pt. Always route through this.
pt <- function(x = MIN_PT) x / .pt

# Wrap caption text to a fixed character width.
#
# ggplot does NOT wrap captions: a long single line runs off the right edge of
# the panel and is silently truncated mid-sentence. That is invisible in the
# console and only shows up in the rendered figure, which is exactly how a
# clipped caption reaches publication. Route every caption through this.
# width = 70 is empirical, not arbitrary: at the 12pt caption size enforced
# here, a ~6.5in panel fits roughly 72 characters; 62 leaves a safe margin
# once a legend takes horizontal space. 95 and 70 both still clipped.
cap <- function(..., width = 62) {
  txt <- paste0(...)
  # Respect any explicit breaks the caller already put in, then wrap each part.
  parts <- unlist(strsplit(txt, "\n", fixed = TRUE))
  paste(vapply(parts, function(p)
    paste(strwrap(p, width = width), collapse = "\n"), character(1)),
    collapse = "\n")
}
