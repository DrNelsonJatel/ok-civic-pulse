# adjudicate.R — human coding panel for the audit sample.
#
# RUNS LOCALLY ONLY. It displays comment text and speaker names, which is
# exactly what never leaves this machine. Do not deploy this anywhere.
#
# Purpose: establish the human standard the classifier is measured against.
# Now that the bulk tier is claude-opus-5, a model-vs-model audit is
# meaningless — a reviewer wants agreement against a domain expert, and you
# are the domain expert.
#
#   shiny::runApp("app/adjudicate.R")
#
# Codes are written under coder_id 'human:<user>', which audit_report() then
# compares against the model. The model's own codes are shown as a starting
# point, because adjudicating from scratch on 100 comments is how a coding
# exercise gets abandoned halfway.
suppressMessages({library(shiny); library(DBI); library(dplyr)})
# NOTE: do NOT library(jsonlite) here — it masks shiny::validate() and silently
# blanks every validate(need()) in the app.

# shiny::runApp() sets the working directory to the APP's folder, so a bare
# "R/db.R" resolves to app/R/db.R and fails. Find the repo root explicitly so
# the app works whether launched from the root or from app/.
.root <- if (dir.exists("R") && file.exists("R/db.R")) "." else ".."
if (!file.exists(file.path(.root, "R", "db.R")))
  stop("cannot locate the repo root from ", normalizePath("."), call. = FALSE)
for (f in c("db.R", "codebook.R", "taxonomy.R", "classify.R", "audit.R"))
  source(file.path(.root, "R", f))
# The DB and codebook paths are relative too; absolutise both for the same
# reason. codebook_note() calls codebook_load(), which reads CODEBOOK_PATH.
DB_PATH <- normalizePath(file.path(.root, "db", "civic_pulse.duckdb"), mustWork = FALSE)
Sys.setenv(OKCP_CODEBOOK = normalizePath(file.path(.root, "codebook", "codebook.yml"),
                                         mustWork = FALSE))
CODEBOOK_PATH <- Sys.getenv("OKCP_CODEBOOK")

CODER <- Sys.getenv("OKCP_HUMAN_CODER", paste0("human:", Sys.getenv("USER", "nelson")))

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 15px; }
    .comment { background:#FBFAF7; border-left:4px solid #1F7A8C; padding:14px;
               margin:10px 0; white-space:pre-wrap; max-height:340px; overflow-y:auto; }
    .modelcode { color:#6B7A82; font-size:13px; }
    .prog { font-weight:600; color:#12303A; }
  "))),
  titlePanel("Okanagan Civic Pulse — adjudication"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "prog", textOutput("progress")),
      hr(),
      selectInput("scope", "Scope (the judgement that matters)",
                  choices = c("local", "ballot", "shared", "provincial", "federal", "none")),
      selectInput("issue", "Primary issue", choices = c("none", sort(ISSUES$code))),
      selectInput("stance", "Stance toward the policy",
                  choices = c("neutral", "support", "oppose", "mixed")),
      sliderInput("conf", "Your confidence", 0, 1, 0.9, step = 0.05),
      textAreaInput("note", "Codebook note (optional)", rows = 2,
                    placeholder = "e.g. definition of climate_flood is ambiguous here"),
      fluidRow(
        column(6, actionButton("agree", "Agree with model", class = "btn-success btn-block")),
        column(6, actionButton("save", "Save my code", class = "btn-primary btn-block"))),
      br(), actionButton("skip", "Skip", class = "btn-default btn-block"),
      hr(),
      helpText("Local only. Comment text and speaker names must not leave this machine.")
    ),
    mainPanel(
      width = 8,
      h4(textOutput("thread")),
      div(class = "comment", textOutput("body")),
      div(class = "modelcode", htmlOutput("model")),
      hr(), verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  con <- db_connect(DB_PATH)
  onStop(function() DBI::dbDisconnect(con, shutdown = TRUE))

  queue <- reactiveVal(NULL)
  idx   <- reactiveVal(1L)

  load_queue <- function() {
    q <- dbGetQuery(con, sprintf("
      SELECT p.post_id, p.body_local, p.source_id, t.title
        FROM audit_sample s
        JOIN posts p ON p.post_id = s.post_id
        LEFT JOIN threads t ON t.t_id = p.thread_t
       WHERE p.body_local IS NOT NULL
         AND p.post_id NOT IN (SELECT post_id FROM post_issues WHERE coder_id = '%s')
       ORDER BY p.post_id", CODER))
    queue(q); idx(1L)
  }
  load_queue()

  cur <- reactive({
    q <- queue(); req(!is.null(q))
    if (!nrow(q) || idx() > nrow(q)) return(NULL)
    q[idx(), ]
  })

  model_codes <- reactive({
    r <- cur(); req(!is.null(r))
    dbGetQuery(con, sprintf("
      SELECT issue_code, scope, stance, round(salience,2) sal, round(confidence,2) conf
        FROM post_issues WHERE post_id = %.0f AND coder_id = '%s'
       ORDER BY salience DESC", r$post_id, coder_id_for(MODEL_BULK)))
  })

  output$progress <- renderText({
    q <- queue()
    done <- dbGetQuery(con, sprintf(
      "SELECT count(DISTINCT post_id) n FROM post_issues WHERE coder_id='%s'", CODER))$n
    sprintf("%d remaining · %d already adjudicated", max(0, nrow(q) - idx() + 1), done)
  })

  output$thread <- renderText({
    r <- cur(); if (is.null(r)) return("Sample complete.")
    paste0("[", r$source_id, "] ", substr(coalesce(r$title, "(no thread title)"), 1, 110))
  })
  output$body <- renderText({
    r <- cur(); if (is.null(r)) "Nothing left to adjudicate. Run audit_report()." else r$body_local
  })
  output$model <- renderUI({
    m <- model_codes()
    if (!nrow(m)) return(HTML("<i>model produced no codes</i>"))
    HTML(paste0("<b>Model (", MODEL_BULK, "):</b><br>",
                paste(sprintf("%s [%s] stance=%s sal=%.2f conf=%.2f",
                              m$issue_code, m$scope, m$stance, m$sal, m$conf),
                      collapse = "<br>")))
  })

  # Pre-fill from the model so adjudication is a confirm-or-correct decision.
  observeEvent(cur(), {
    m <- model_codes()
    if (nrow(m)) {
      updateSelectInput(session, "scope", selected = m$scope[1])
      updateSelectInput(session, "issue", selected = m$issue_code[1])
      updateSelectInput(session, "stance", selected = m$stance[1] %||% "neutral")
    }
  })

  write_code <- function(scope, issue, stance, conf) {
    r <- cur(); req(!is.null(r))
    db_upsert(con, "post_issues", data.frame(
      post_id = r$post_id, issue_code = issue, scope = scope,
      jurisdiction = NA_character_, stance = stance,
      salience = 1, confidence = conf, coder_id = CODER,
      coded_at = Sys.time(), stringsAsFactors = FALSE),
      c("post_id", "issue_code", "coder_id"))
    if (nzchar(input$note))
      codebook_note(con, issue_code = issue, kind = "note", note = input$note,
                    author = CODER)
    updateTextAreaInput(session, "note", value = "")
    idx(idx() + 1L)
  }

  observeEvent(input$agree, {
    m <- model_codes(); req(nrow(m) > 0)
    write_code(m$scope[1], m$issue_code[1], m$stance[1] %||% "neutral", 1)
  })
  observeEvent(input$save,  write_code(input$scope, input$issue, input$stance, input$conf))
  observeEvent(input$skip,  idx(idx() + 1L))

  output$status <- renderText({
    sprintf("coder_id: %s | bulk model: %s | codebook %s", CODER, MODEL_BULK, CODEBOOK_VERSION)
  })
}

shinyApp(ui, server)
