# =============================================================================
# PRISMA 2020 Flow Diagram – Publication-Ready Shiny Application
#
# Compliant with:
#   Page MJ, et al. (2021). The PRISMA 2020 statement: an updated guideline
#   for reporting systematic reviews. BMJ, 372, n71.
#   https://doi.org/10.1136/bmj.n71
#
# Run:  shiny::runApp()
# Requires: shiny, DiagrammeR
# =============================================================================

library(shiny)
library(DiagrammeR)

APP_VERSION <- "1.0.0"
APP_DATE    <- "2025"


# =============================================================================
# SECTION 1 – UTILITY HELPERS  (unchanged from original)
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

trim_lines <- function(x) {
  x   <- x %||% ""
  out <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  out[nzchar(out)]
}

safe_int <- function(x) {
  if (is.null(x)) return(0L)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(0L)
  x <- x[[1]]
  x <- suppressWarnings(as.integer(x))
  if (is.na(x) || x < 0) 0L else x
}


# =============================================================================
# SECTION 2 – GRAPHVIZ / HTML LABEL HELPERS  (unchanged from original)
# =============================================================================

gv_esc <- function(s) {
  s <- gsub("&", "&amp;", s)
  s <- gsub("<", "&lt;",  s)
  s <- gsub(">", "&gt;",  s)
  s
}

make_filter_label_html <- function(title, criteria, mode = c("total", "per"),
                                   per_removed  = NULL,  total_removed = 0L,
                                   remaining    = NA_integer_,
                                   included     = NA_integer_,
                                   show_included = FALSE,
                                   cellpadding  = 6) {
  mode     <- match.arg(mode)
  criteria <- criteria[!is.na(criteria) & nzchar(criteria)]
  if (length(criteria) == 0) criteria <- "(no criteria specified)"

  crit_lines <- if (mode == "per") {
    per_removed <- per_removed %||% rep(0L, length(criteria))
    per_removed <- as.integer(per_removed)
    if (length(per_removed) != length(criteria)) per_removed <- rep(0L, length(criteria))
    paste0(criteria, "   (Removed: ", pmax(0L, per_removed), ")")
  } else {
    paste0(criteria)
  }

  removed_line <- paste0("Removed: ", pmax(0L, as.integer(total_removed)))
  tail_lines   <- c(
    paste0("Records identified (n): ", pmax(0L, as.integer(included))),
    removed_line,
    paste0("Remaining: ", pmax(0L, as.integer(remaining)))
  )

  rows <- c(
    paste0("<TR><TD ALIGN='CENTER'><B>", gv_esc(title), "</B></TD></TR>"),
    paste0("<TR><TD ALIGN='CENTER'>",    gv_esc(tail_lines), "</TD></TR>", collapse = ""),
    paste0("<TR><TD ALIGN='LEFT'>",      gv_esc(crit_lines), "</TD></TR>", collapse = "")
  )

  paste0("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0' CELLPADDING='", cellpadding, "'>",
         paste(rows, collapse = ""),
         "</TABLE>>")
}

make_simple_box_html <- function(title, lines,
                                 bold_title  = TRUE,
                                 title_wrap  = FALSE,
                                 cellpadding = 8,
                                 td_width    = NULL) {
  title_lines <- if (title_wrap) {
    w   <- 22
    if (nchar(title) <= w) {
      title
    } else {
      cut <- regexpr(" [^ ]*$", substr(title, 1, w))
      if (cut > 0) {
        c(trimws(substr(title, 1, cut)), trimws(substr(title, cut + 1, nchar(title))))
      } else {
        c(substr(title, 1, w), substr(title, w + 1, nchar(title)))
      }
    }
  } else {
    title
  }

  title_joined <- paste(vapply(title_lines, gv_esc, character(1)), collapse = "<BR/>")
  if (bold_title) title_joined <- paste0("<B>", title_joined, "</B>")

  td_attr <- if (!is.null(td_width)) paste0(" WIDTH='", td_width, "'") else ""

  rows <- c(
    paste0("<TR><TD ALIGN='CENTER'", td_attr, ">", title_joined, "</TD></TR>"),
    paste0("<TR><TD ALIGN='CENTER'", td_attr, ">", gv_esc(lines), "</TD></TR>", collapse = "")
  )

  paste0("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0' CELLPADDING='", cellpadding, "'>",
         paste(rows, collapse = ""),
         "</TABLE>>")
}


# =============================================================================
# SECTION 3 – USER INTERFACE (UI)
# =============================================================================

ui <- fluidPage(

  # ---------- HEAD: Google Fonts + publication-grade CSS ----------
  tags$head(
    # Google Fonts: Lora (serif headings) + Source Sans 3 (clean body)
    tags$link(rel  = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel  = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = NA),
    tags$link(rel  = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?",
                            "family=Lora:ital,wght@0,400;0,600;0,700;1,400&",
                            "family=Source+Sans+3:wght@300;400;600;700&",
                            "display=swap")),

    tags$style(HTML("

      /* ── Global ── */
      *, *::before, *::after { box-sizing: border-box; }

      body {
        font-family: 'Source Sans 3', sans-serif;
        font-size: 14px;
        background: #F5F6FA;
        color: #1a2130;
      }

      /* ── App title bar ── */
      .app-header {
        background: linear-gradient(135deg, #0D2B52 0%, #1B4F8C 100%);
        color: #fff;
        padding: 18px 28px 14px 28px;
        margin: -15px -15px 20px -15px;
        border-bottom: 3px solid #C49A2A;
      }
      .app-header h2 {
        font-family: 'Lora', serif;
        font-size: 20px;
        font-weight: 700;
        margin: 0 0 4px 0;
        letter-spacing: 0.2px;
      }
      .app-header .app-subtitle {
        font-size: 12px;
        font-weight: 300;
        opacity: 0.82;
        letter-spacing: 0.3px;
      }
      .app-header .prisma-badge {
        display: inline-block;
        background: #C49A2A;
        color: #fff;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 1px;
        padding: 2px 8px;
        border-radius: 3px;
        margin-left: 10px;
        vertical-align: middle;
      }

      /* ── Sidebar ── */
      .well {
        background: #F5F6FA !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
      }
      .sidebar-scroll {
        height: calc(100vh - 145px);
        overflow-y: auto;
        padding-right: 4px;
      }
      .sidebar-scroll::-webkit-scrollbar { width: 5px; }
      .sidebar-scroll::-webkit-scrollbar-thumb {
        background: #c5ccd9; border-radius: 4px;
      }

      /* ── Collapsible panels ── */
      details {
        border: 1px solid #dce2ed;
        border-radius: 8px;
        padding: 0;
        background: #fff;
        margin-bottom: 10px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        overflow: hidden;
      }
      summary {
        cursor: pointer;
        font-weight: 700;
        font-size: 12.5px;
        letter-spacing: 0.4px;
        text-transform: uppercase;
        color: #0D2B52;
        background: #EDF1F8;
        padding: 9px 12px;
        list-style: none;
        border-bottom: 1px solid #dce2ed;
        user-select: none;
      }
      summary::-webkit-details-marker { display: none; }
      summary::after   { content: '▸'; float: right; color: #6b7280; }
      details[open] summary::after { content: '▾'; }
      .panel-body { padding: 10px 12px 12px 12px; }

      /* ── Card / db-box ── */
      .db-box {
        padding: 10px 12px;
        border: 1px solid #e0e5f0;
        border-radius: 7px;
        margin-bottom: 9px;
        background: #FAFBFD;
      }
      .db-title {
        font-weight: 700;
        font-size: 12.5px;
        color: #0D2B52;
        margin-bottom: 6px;
        border-bottom: 1px solid #e8ecf3;
        padding-bottom: 4px;
      }

      /* ── Helper text ── */
      .small-note {
        color: #6b7a93;
        font-size: 11.5px;
        line-height: 1.5;
        margin-bottom: 6px;
      }

      /* ── Validation / warning banner ── */
      .warn-box {
        background: #FFF8E1;
        border-left: 4px solid #F9A825;
        border-radius: 0 6px 6px 0;
        padding: 8px 12px;
        margin-bottom: 10px;
        font-size: 12.5px;
        color: #7a5500;
      }
      .error-box {
        background: #FFEBEE;
        border-left: 4px solid #C62828;
        border-radius: 0 6px 6px 0;
        padding: 8px 12px;
        margin-bottom: 10px;
        font-size: 12.5px;
        color: #7a0000;
      }

      /* ── Tab panel ── */
      .nav-tabs > li > a {
        font-family: 'Source Sans 3', sans-serif;
        font-size: 13px;
        font-weight: 600;
        color: #0D2B52;
        letter-spacing: 0.2px;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: #1B4F8C;
        border-top: 3px solid #C49A2A !important;
      }
      .tab-content {
        background: #fff;
        border: 1px solid #dde2ed;
        border-top: none;
        border-radius: 0 0 8px 8px;
        padding: 16px;
      }

      /* ── Download button row ── */
      .dl-row {
        display: flex;
        gap: 8px;
        margin-bottom: 12px;
        flex-wrap: wrap;
      }
      .btn-dl {
        font-family: 'Source Sans 3', sans-serif;
        font-size: 12px;
        font-weight: 600;
        background: #0D2B52;
        color: #fff !important;
        border: none;
        border-radius: 5px;
        padding: 6px 14px;
        cursor: pointer;
        letter-spacing: 0.3px;
        transition: background 0.15s;
      }
      .btn-dl:hover { background: #1B4F8C; color: #fff !important; }
      .btn-dl.gold  { background: #C49A2A; }
      .btn-dl.gold:hover { background: #a07c1a; }

      /* ── Methods / caption text area ── */
      .methods-box {
        font-family: 'Lora', serif;
        font-size: 13.5px;
        line-height: 1.85;
        background: #FAFBFC;
        border: 1px solid #dce2ed;
        border-radius: 6px;
        padding: 16px 20px;
        white-space: pre-wrap;
        color: #1a2130;
      }
      .methods-label {
        font-family: 'Source Sans 3', sans-serif;
        font-weight: 700;
        font-size: 11px;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: #6b7a93;
        margin-bottom: 6px;
      }

      /* ── About / citation section ── */
      .cite-block {
        background: #EDF1F8;
        border-left: 4px solid #1B4F8C;
        border-radius: 0 6px 6px 0;
        padding: 12px 16px;
        font-family: 'Lora', serif;
        font-size: 13px;
        line-height: 1.7;
        color: #1a2130;
        margin-bottom: 14px;
      }
      .about-h {
        font-family: 'Lora', serif;
        font-weight: 700;
        font-size: 15px;
        color: #0D2B52;
        margin: 16px 0 6px 0;
        padding-bottom: 4px;
        border-bottom: 1px solid #dce2ed;
      }
      .about-p {
        font-size: 13.5px;
        line-height: 1.75;
        color: #2c3e55;
        margin-bottom: 10px;
      }

      /* ── Counts summary verbatim ── */
      pre.shiny-text-output {
        font-family: 'Courier New', monospace;
        font-size: 13px;
        background: #F5F6FA;
        border: 1px solid #dce2ed;
        border-radius: 6px;
        padding: 14px 16px;
        color: #1a2130;
      }

      /* ── PRISMA footer ── */
      .prisma-footer {
        margin-top: 20px;
        padding: 10px 16px;
        background: #EDF1F8;
        border-radius: 6px;
        font-size: 11.5px;
        color: #4a5568;
        line-height: 1.6;
        border: 1px solid #dce2ed;
      }
      .prisma-footer a { color: #1B4F8C; }

      /* ── Shiny input overrides ── */
      label { font-weight: 600 !important; font-size: 12.5px !important;
              color: #1a2130 !important; }
      .form-control { font-size: 13px !important; }
      .shiny-input-container { margin-bottom: 8px !important; }
      .block { margin-top: 6px; }

    "))
  ),

  # ---------- App header (replaces default titlePanel) ----------
  div(class = "app-header",
      h2(HTML('PRISMA 2020 Flow Diagram
               <span class="prisma-badge">PRISMA 2020</span>')),
      div(class = "app-subtitle",
          "Systematic review search & selection flowchart — publication ready")
  ),

  sidebarLayout(

    # ======================================================================
    # SIDEBAR
    # ======================================================================
    sidebarPanel(
      width = 4,
      div(class = "sidebar-scroll",

          # ----------------------------------------------------------------
          # Panel 0 (NEW): Study Metadata
          # Author / title / year fields used in auto-generated text outputs
          # ----------------------------------------------------------------
          tags$details(open = FALSE,
            tags$summary("0) Study Metadata"),
            div(class = "panel-body",
                div(class = "small-note",
                    "Used in the auto-generated methods text and figure caption."),
                textInput("meta_authors",  "Author(s)",
                          placeholder = "e.g. Smith J, Jones A"),
                textInput("meta_title",    "Review title (short)",
                          placeholder = "e.g. Gut microbiome and Parkinson's disease"),
                textInput("meta_year",     "Year",
                          placeholder = format(Sys.Date(), "%Y")),
                textInput("meta_journal",  "Target journal",
                          placeholder = "e.g. Systematic Reviews")
            )
          ),

          # ----------------------------------------------------------------
          # Panel 1: Databases
          # ----------------------------------------------------------------
          tags$details(open = TRUE,
            tags$summary("1) Databases"),
            div(class = "panel-body",
                sliderInput("k_db", "Number of databases searched:",
                            min = 1, max = 20, value = 4, step = 1),
                uiOutput("db_inputs"),
                uiOutput("db_validation_ui")
            )
          ),

          # ----------------------------------------------------------------
          # Panel 2: Technical filter (per database)
          # ----------------------------------------------------------------
          tags$details(open = TRUE,
            tags$summary("2) Technical filter (per database)"),
            div(class = "panel-body",
                div(class = "small-note",
                    "Technical filters are applied per database before merging."),
                uiOutput("tech_by_db_ui"),
                uiOutput("tech_validation_ui")
            )
          ),

          # ----------------------------------------------------------------
          # Panel 3: Deduplication
          # ----------------------------------------------------------------
          tags$details(open = FALSE,
            tags$summary("3) Remove duplicates (merged)"),
            div(class = "panel-body",
                numericInput("dup_removed",
                             "Duplicates removed after merging:",
                             value = 0, min = 0, step = 1),
                uiOutput("dup_validation_ui")
            )
          ),

          # ----------------------------------------------------------------
          # Panel 4: Scientific filter
          # ----------------------------------------------------------------
          tags$details(open = FALSE,
            tags$summary("4) Scientific / eligibility filter"),
            div(class = "panel-body",
                numericInput("n_threshold",
                             HTML("Minimum sample size (n \u2265)"),
                             value = 10, min = 0, step = 1),
                textAreaInput(
                  "sci_custom",
                  "Additional eligibility criteria (one per line)",
                  rows = 3,
                  placeholder = paste("e.g.",
                    "Human PD\u2013microbiome observational studies",
                    "Exclude animal models",
                    "Exclude in vitro studies", sep = "\n")
                ),
                radioButtons(
                  "sci_mode",
                  "Exclusion count entry mode:",
                  choices  = c("Total excluded (single number)" = "total",
                               "Excluded per criterion"         = "per"),
                  selected = "total"
                ),
                uiOutput("sci_removed_ui"),
                uiOutput("sci_validation_ui")
            )
          )
      )
    ), # end sidebarPanel

    # ======================================================================
    # MAIN PANEL – five tabs
    # ======================================================================
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",

        # ------------------------------------------------------------------
        # Tab 1: Flowchart
        # ------------------------------------------------------------------
        tabPanel("Flowchart",
          br(),
          div(class = "dl-row",
              downloadButton("dl_dot", "Download DOT source",
                             class = "btn-dl"),
              downloadButton("dl_csv", "Download counts (CSV)",
                             class = "btn-dl gold")
          ),
          grVizOutput("flow", height = "860px"),
          br(),
          div(class = "prisma-footer",
              HTML(paste0(
                "<b>PRISMA 2020</b> &mdash; Page MJ, McKenzie JE, Bossuyt PM, et al. ",
                "The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. ",
                "<i>BMJ</i>. 2021;372:n71. ",
                "<a href='https://doi.org/10.1136/bmj.n71' target='_blank'>",
                "https://doi.org/10.1136/bmj.n71</a>"
              ))
          )
        ),

        # ------------------------------------------------------------------
        # Tab 2: Counts summary
        # ------------------------------------------------------------------
        tabPanel("Counts summary",
          br(),
          verbatimTextOutput("counts_summary")
        ),

        # ------------------------------------------------------------------
        # Tab 3 (NEW): Methods text
        # Auto-generates a manuscript-ready methods paragraph and figure caption
        # ------------------------------------------------------------------
        tabPanel("Methods text",
          br(),
          div(class = "methods-label", "Methods paragraph (copy into manuscript)"),
          verbatimTextOutput("methods_text"),
          br(),
          div(class = "methods-label", "Figure caption (copy into manuscript)"),
          verbatimTextOutput("figure_caption")
        ),

        # ------------------------------------------------------------------
        # Tab 4 (NEW): About & Citation
        # ------------------------------------------------------------------
        tabPanel("About & Citation",
          br(),
          div(class = "about-h", "About this application"),
          div(class = "about-p",
              "This application generates PRISMA 2020-compliant search and ",
              "selection flow diagrams for systematic reviews. It supports an ",
              "arbitrary number of databases with per-database technical filters, ",
              "merged deduplication, and configurable scientific eligibility criteria."
          ),

          div(class = "about-h", "How to cite this tool"),
          div(class = "cite-block",
              HTML(paste0(
                "[Author(s)]. (", APP_DATE, "). <i>PRISMA 2020 Flow Diagram — ",
                "Shiny Application</i> (Version ", APP_VERSION, "). ",
                "Retrieved from [repository/DOI URL]."
              ))
          ),

          div(class = "about-h", "PRISMA 2020 reference"),
          div(class = "cite-block",
              HTML(paste0(
                "Page MJ, McKenzie JE, Bossuyt PM, Boutron I, Hoffmann TC, ",
                "Mulrow CD, et al. (2021). The PRISMA 2020 statement: an updated ",
                "guideline for reporting systematic reviews. <i>BMJ</i>, 372, n71. ",
                "<a href='https://doi.org/10.1136/bmj.n71' target='_blank'>",
                "https://doi.org/10.1136/bmj.n71</a>"
              ))
          ),

          div(class = "about-h", "PRISMA 2020 compliance checklist"),
          div(class = "about-p",
              "When reporting your systematic review, complete the PRISMA 2020 checklist:",
              tags$ul(
                tags$li("Report the total number of records identified per database."),
                tags$li("Report all screening stages (technical filter, deduplication, eligibility)."),
                tags$li("State the number excluded at each stage with reasons."),
                tags$li("Report the final number of studies included in the synthesis."),
                tags$li(HTML("See: <a href='https://www.prisma-statement.org/prisma-2020-checklist'
                             target='_blank'>prisma-statement.org</a>"))
              )
          ),

          div(class = "about-h", "Exporting the diagram"),
          div(class = "about-p",
              tags$b("For journal submission:"), " right-click the rendered diagram ",
              "and choose 'Save image as…' for a raster PNG. For a fully scalable ",
              "vector graphic suitable for print publication, use the ",
              tags$b("Download DOT source"), " button and render the .dot file with ",
              "Graphviz (", tags$code("dot -Tsvg diagram.dot -o diagram.svg"), ") or ",
              "import it into Inkscape / Adobe Illustrator."
          ),

          div(class = "about-h", "Session information"),
          verbatimTextOutput("session_info")
        )
      )
    )
  )
)


# =============================================================================
# SECTION 4 – SERVER
# =============================================================================

server <- function(input, output, session) {

  # ============================================================
  # 4.1  DYNAMIC UI – database name / count inputs
  # ============================================================

  output$db_inputs <- renderUI({
    k <- safe_int(input$k_db)
    defaults  <- c("PubMed","GutFinder","Scopus","Web of Science","Embase","Cochrane",
                   "PsycINFO","CINAHL","MEDLINE","Google Scholar","ProQuest","LILACS",
                   "CNKI","WanFang","VIP","bioRxiv","medRxiv","IEEE Xplore","ACM DL","SpringerLink")
    default_n <- c(700,800,800,800,800,800, 300,300,500,1000,200,150,
                   120,120,120, 90,90, 180,180,180)
    tagList(
      lapply(seq_len(k), function(i) {
        fluidRow(
          column(6, textInput(paste0("db_name_", i), paste0("DB ", i, " name"),
                              value = defaults[i] %||% paste0("DB", i))),
          column(6, numericInput(paste0("db_n_", i), "Records (n)",
                                 value = default_n[i] %||% 0, min = 0, step = 1))
        )
      })
    )
  })


  # ============================================================
  # 4.2  REACTIVES – core data
  # ============================================================

  db_data <- reactive({
    k  <- safe_int(input$k_db)
    nm <- vapply(seq_len(k), function(i) {
      val <- input[[paste0("db_name_", i)]] %||% paste0("Database ", i)
      if (!nzchar(val)) paste0("Database ", i) else val
    }, character(1))
    nn <- vapply(seq_len(k), function(i) safe_int(input[[paste0("db_n_", i)]]), integer(1))
    data.frame(id = seq_len(k), name = nm, n = as.integer(nn), stringsAsFactors = FALSE)
  })

  tech_criteria_shared <- reactive({
    c(input$tech_opts_shared %||% character(0), trim_lines(input$tech_custom_shared))
  })

  sci_criteria <- reactive({
    c(paste0("n \u2265 ", safe_int(input$n_threshold)), trim_lines(input$sci_custom))
  })


  # ============================================================
  # 4.3  DYNAMIC UI – technical filter blocks
  # ============================================================

  output$tech_by_db_ui <- renderUI({
    db <- db_data(); k <- nrow(db)

    shared_controls <- div(class = "db-box",
      div(class = "db-title", "Shared technical criteria (all databases)"),
      checkboxGroupInput(
        "tech_opts_shared", "Pre-defined filters:",
        choices  = c("Human only","English only","Exclude reviews",
                     "Full-text available","Adult only"),
        selected = c("Human only","English only")
      ),
      textAreaInput(
        "tech_custom_shared",
        "Custom criteria (one per line):",
        rows = 3,
        placeholder = "e.g.\nPublication year \u2265 2015\nExclude conference abstracts"
      ),
      div(class = "small-note",
          "Database 1 defaults to per-criterion mode; others default to total mode.")
    )

    per_db_blocks <- tagList(lapply(seq_len(k), function(i) {
      db_id <- db$id[i]; db_name <- db$name[i]
      div(class = "db-box",
          div(class = "db-title", paste0("Database: ", db_name)),
          radioButtons(
            paste0("tech_mode_db_", db_id),
            "Exclusion count entry:",
            choices  = c("Total excluded (one number)" = "total",
                         "Excluded per criterion"      = "per"),
            selected = if (db_id == 1) "per" else "total"
          ),
          uiOutput(paste0("tech_removed_ui_db_", db_id))
      )
    }))

    tagList(shared_controls, per_db_blocks)
  })

  observe({
    db <- db_data(); k <- nrow(db)
    for (i in seq_len(k)) {
      local({
        db_id <- db$id[i]
        output[[paste0("tech_removed_ui_db_", db_id)]] <- renderUI({
          mode <- input[[paste0("tech_mode_db_", db_id)]] %||%
                  if (db_id == 1) "per" else "total"
          crit <- tech_criteria_shared()
          if (mode == "total") {
            numericInput(paste0("tech_removed_total_db_", db_id),
                         "How many excluded (total) in this database?",
                         value = 0, min = 0, step = 1)
          } else {
            if (length(crit) == 0) {
              div(class = "small-note", "No criteria selected yet.")
            } else {
              tagList(
                div(class = "small-note", "Enter excluded count per criterion:"),
                lapply(seq_along(crit), function(j) {
                  numericInput(paste0("tech_removed_db_", db_id, "_", j),
                               label = crit[j], value = 0, min = 0, step = 1)
                })
              )
            }
          }
        })
      })
    }
  })


  # ============================================================
  # 4.4  DYNAMIC UI – scientific filter exclusion inputs
  # ============================================================

  output$sci_removed_ui <- renderUI({
    mode <- input$sci_mode %||% "total"
    crit <- sci_criteria()
    if (mode == "total") {
      numericInput("sci_removed_total",
                   "How many excluded (total) by eligibility filter?",
                   value = 0, min = 0, step = 1)
    } else {
      tagList(
        div(class = "small-note", "Enter excluded count per criterion:"),
        lapply(seq_along(crit), function(i) {
          numericInput(paste0("sci_removed_", i), label = crit[i],
                       value = 0, min = 0, step = 1)
        })
      )
    }
  })


  # ============================================================
  # 4.5  REACTIVES – removal info aggregation
  # ============================================================

  tech_removed_info_by_db <- reactive({
    db <- db_data(); k <- nrow(db); crit <- tech_criteria_shared()
    lapply(seq_len(k), function(i) {
      db_id <- db$id[i]
      mode  <- input[[paste0("tech_mode_db_", db_id)]] %||%
               if (db_id == 1) "per" else "total"
      if (mode == "total") {
        total <- safe_int(input[[paste0("tech_removed_total_db_", db_id)]])
        list(mode = "total", crit = crit, per = rep(0L, length(crit)), total = total)
      } else {
        per   <- vapply(seq_along(crit), function(j)
                   safe_int(input[[paste0("tech_removed_db_", db_id, "_", j)]]),
                   integer(1))
        list(mode = "per", crit = crit, per = as.integer(per),
             total = as.integer(sum(per)))
      }
    })
  })

  sci_removed_info <- reactive({
    mode <- input$sci_mode %||% "total"; crit <- sci_criteria()
    if (mode == "total") {
      total <- safe_int(input$sci_removed_total)
      list(mode = "total", crit = crit, per = rep(0L, length(crit)), total = total)
    } else {
      per   <- vapply(seq_along(crit), function(i)
                 safe_int(input[[paste0("sci_removed_", i)]]), integer(1))
      list(mode = "per", crit = crit, per = as.integer(per),
           total = as.integer(sum(per)))
    }
  })


  # ============================================================
  # 4.6  REACTIVE – central count pipeline
  # ============================================================

  counts <- reactive({
    db        <- db_data()
    tech_list <- tech_removed_info_by_db()

    total_found_all    <- sum(db$n)
    tech_removed_by_db <- vapply(seq_along(tech_list),
                                 function(i) safe_int(tech_list[[i]]$total), integer(1))
    after_tech_by_db   <- pmax(0L, db$n - tech_removed_by_db)
    merged_after_tech  <- sum(after_tech_by_db)

    dup_removed    <- safe_int(input$dup_removed)
    after_dup      <- max(0L, merged_after_tech - dup_removed)

    sci            <- sci_removed_info()
    sci_removed    <- safe_int(sci$total)
    final_included <- max(0L, after_dup - sci_removed)

    list(
      total_found_all    = total_found_all,
      merged_after_tech  = merged_after_tech,
      dup_removed        = dup_removed,
      after_dup          = after_dup,
      sci_removed        = sci_removed,
      final_included     = final_included,
      tech_removed_by_db = tech_removed_by_db,
      after_tech_by_db   = after_tech_by_db
    )
  })


  # ============================================================
  # 4.7  VALIDATION WARNING UI (NEW)
  # Emits warning/error banners if removals exceed available records
  # ============================================================

  output$db_validation_ui <- renderUI({
    db <- db_data()
    if (any(db$n == 0)) {
      div(class = "warn-box",
          "\u26A0\uFE0F One or more databases has 0 records. ",
          "Verify your search counts.")
    }
  })

  output$tech_validation_ui <- renderUI({
    db        <- db_data()
    tech_list <- tech_removed_info_by_db()
    cts       <- counts()
    over      <- which(cts$tech_removed_by_db > db$n)
    if (length(over) > 0) {
      div(class = "error-box",
          paste0("\u274C Technical filter removes more records than are available in: ",
                 paste(db$name[over], collapse = ", "), ". Please check your counts."))
    }
  })

  output$dup_validation_ui <- renderUI({
    cts <- counts()
    if (cts$dup_removed > cts$merged_after_tech) {
      div(class = "error-box",
          "\u274C Duplicates removed exceeds records available after technical filters.")
    }
  })

  output$sci_validation_ui <- renderUI({
    cts <- counts()
    if (cts$sci_removed > cts$after_dup) {
      div(class = "error-box",
          "\u274C Scientific filter excludes more records than remain after deduplication.")
    }
  })


  # ============================================================
  # 4.8  OUTPUTS – text / summary
  # ============================================================

  output$counts_summary <- renderText({
    db  <- db_data()
    cts <- counts()
    tech_list <- tech_removed_info_by_db()

    db_lines <- paste0(
      vapply(seq_len(nrow(db)), function(i) {
        sprintf("  %-30s identified: %d  |  excluded by tech filter: %d  |  remaining: %d",
                db$name[i], db$n[i],
                cts$tech_removed_by_db[i], cts$after_tech_by_db[i])
      }, character(1)),
      collapse = "\n"
    )

    paste0(
      "── PRISMA 2020 RECORD COUNTS ──────────────────────────────────────\n\n",
      "IDENTIFICATION\n",
      db_lines, "\n",
      sprintf("  %-30s %d\n", "Total records identified:", cts$total_found_all),
      "\nSCREENING\n",
      sprintf("  %-30s %d\n", "After technical filters (merged):", cts$merged_after_tech),
      sprintf("  %-30s %d  (excluded: %d)\n",
              "After deduplication:", cts$after_dup, cts$dup_removed),
      "\nELIGIBILITY & INCLUSION\n",
      sprintf("  %-30s %d  (excluded: %d)\n",
              "After eligibility filter:", cts$final_included, cts$sci_removed),
      "\n────────────────────────────────────────────────────────────────────"
    )
  })


  # ============================================================
  # 4.9  OUTPUTS – auto-generated methods text (NEW)
  # ============================================================

  output$methods_text <- renderText({
    db        <- db_data()
    cts       <- counts()
    sci       <- sci_removed_info()
    tech_crit <- tech_criteria_shared()

    authors  <- trimws(input$meta_authors  %||% "")
    title_sr <- trimws(input$meta_title    %||% "this systematic review")
    year_sr  <- trimws(input$meta_year     %||% format(Sys.Date(), "%Y"))

    # Database sentence
    k        <- nrow(db)
    db_list  <- paste0(db$name, " (n\u2009=\u2009", db$n, ")")
    if (k == 1) {
      db_sent <- paste0("One electronic database was searched: ", db_list[1], ".")
    } else {
      db_sent <- paste0(k, " electronic databases were searched: ",
                        paste(db_list[-k], collapse = ", "),
                        ", and ", db_list[k], ", yielding ",
                        format(cts$total_found_all, big.mark = ","),
                        " records in total.")
    }

    # Technical filter sentence
    if (length(tech_crit) > 0) {
      tech_sent <- paste0(
        "Technical filters were applied per database (", paste(tech_crit, collapse = "; "),
        "), retaining ", format(cts$merged_after_tech, big.mark = ","),
        " records after merging across all databases."
      )
    } else {
      tech_sent <- paste0(
        "Records from all databases were merged, yielding ",
        format(cts$merged_after_tech, big.mark = ","), " records."
      )
    }

    # Deduplication sentence
    dup_sent <- paste0(
      "Following removal of ", format(cts$dup_removed, big.mark = ","),
      " duplicate record", ifelse(cts$dup_removed == 1, "", "s"), ", ",
      format(cts$after_dup, big.mark = ","), " unique records were screened."
    )

    # Eligibility sentence
    sci_crit_str <- if (length(sci$crit) > 0) {
      paste0("eligibility criteria included: ", paste(sci$crit, collapse = "; "), ". ")
    } else ""
    sci_sent <- paste0(
      "Eligibility screening (", sci_crit_str,
      "n\u2009=\u2009", format(cts$sci_removed, big.mark = ","), " excluded) ",
      "resulted in ", format(cts$final_included, big.mark = ","),
      " stud", ifelse(cts$final_included == 1, "y", "ies"),
      " included in the final synthesis."
    )

    paste0(
      "Search strategy and study selection\n",
      "─────────────────────────────────────────────────────────────────────\n\n",
      db_sent, " ", tech_sent, " ", dup_sent, " ", sci_sent,
      "\n\nThe search and selection process was documented using a PRISMA 2020 ",
      "flow diagram (Page et al., 2021). The complete search strategy and all ",
      "eligibility criteria are reported in accordance with PRISMA 2020 guidelines."
    )
  })


  # ============================================================
  # 4.10  OUTPUTS – figure caption (NEW)
  # ============================================================

  output$figure_caption <- renderText({
    db   <- db_data()
    cts  <- counts()
    sci  <- sci_removed_info()
    k    <- nrow(db)

    db_names_str <- if (k == 1) db$name[1] else {
      paste0(paste(db$name[-k], collapse = ", "), " and ", db$name[k])
    }

    paste0(
      "Figure 1. PRISMA 2020 flow diagram of the systematic search and study ",
      "selection process. A total of ", format(cts$total_found_all, big.mark = ","),
      " records were identified across ", k, " database",
      ifelse(k == 1, "", "s"), " (", db_names_str, "). ",
      "After per-database technical filtering, ",
      format(cts$merged_after_tech, big.mark = ","),
      " records were retained and merged. Removal of ",
      format(cts$dup_removed, big.mark = ","), " duplicate",
      ifelse(cts$dup_removed == 1, "", "s"), " yielded ",
      format(cts$after_dup, big.mark = ","), " unique records for eligibility ",
      "screening. Application of eligibility criteria excluded a further ",
      format(cts$sci_removed, big.mark = ","), " record",
      ifelse(cts$sci_removed == 1, "", "s"), ", leaving ",
      format(cts$final_included, big.mark = ","), " stud",
      ifelse(cts$final_included == 1, "y", "ies"),
      " included in the review. ",
      "Diagram produced using the PRISMA 2020 Flow Diagram Shiny application ",
      "(v", APP_VERSION, "); compliant with Page et al. (2021), BMJ 372:n71."
    )
  })


  # ============================================================
  # 4.11  OUTPUTS – about tab session info (NEW)
  # ============================================================

  output$session_info <- renderText({
    si <- sessionInfo()
    paste0(
      "R version:    ", R.version$version.string, "\n",
      "shiny:        ", as.character(packageVersion("shiny")), "\n",
      "DiagrammeR:   ", as.character(packageVersion("DiagrammeR")), "\n",
      "App version:  ", APP_VERSION, "\n",
      "Platform:     ", si$platform
    )
  })


  # ============================================================
  # 4.12  OUTPUT – Graphviz flow diagram  (unchanged logic)
  # ============================================================

  build_dot <- reactive({
    db        <- db_data()
    tech_list <- tech_removed_info_by_db()
    cts       <- counts()
    k         <- nrow(db)

    db_nodes   <- paste0("db",   seq_len(k))
    tech_nodes <- paste0("tech", seq_len(k))
    db_labels  <- db$name

    tech_html <- mapply(function(i) {
      tinfo <- tech_list[[i]]
      make_filter_label_html(
        title         = "Technical filter",
        criteria      = tinfo$crit,
        mode          = tinfo$mode,
        per_removed   = tinfo$per,
        total_removed = cts$tech_removed_by_db[i],
        remaining     = cts$after_tech_by_db[i],
        included      = db$n[i],
        show_included = TRUE,
        cellpadding   = 8
      )
    }, seq_len(k), USE.NAMES = FALSE)

    merge_html <- make_simple_box_html(
      "Records after technical filters",
      lines       = paste0("Merged remaining: ", cts$merged_after_tech),
      bold_title  = TRUE, title_wrap = TRUE, cellpadding = 10, td_width = 360
    )

    dup_html <- make_simple_box_html(
      "Remove duplicates",
      lines       = c(paste0("Removed: ",   cts$dup_removed),
                      paste0("Remaining: ", cts$after_dup)),
      bold_title  = TRUE, title_wrap = FALSE, cellpadding = 10, td_width = 320
    )

    sci      <- sci_removed_info()
    sci_html <- make_filter_label_html(
      title         = "Scientific / eligibility filter",
      criteria      = sci$crit,
      mode          = sci$mode,
      per_removed   = sci$per,
      total_removed = cts$sci_removed,
      included      = cts$final_included,
      show_included = TRUE,
      cellpadding   = 8
    )

    db_decl <- paste0(
      mapply(function(node, lab)
        paste0(node, ' [label=<<B><FONT POINT-SIZE="22">',
               gv_esc(lab), '</FONT></B>>];'),
        db_nodes, db_labels, USE.NAMES = FALSE),
      collapse = "\n  "
    )
    tech_decl <- paste0(
      mapply(function(node, lab) paste0(node, " [label=", lab, "];"),
             tech_nodes, tech_html, USE.NAMES = FALSE),
      collapse = "\n  "
    )

    db_rank   <- paste0("{ rank=same; ", paste(db_nodes,   collapse = " "), " }")
    tech_rank <- paste0("{ rank=same; ", paste(tech_nodes, collapse = " "), " }")

    edges_db_to_tech    <- paste0(
      mapply(function(d, t) paste0(d, " -> ", t, ";"),
             db_nodes, tech_nodes, USE.NAMES = FALSE),
      collapse = "\n  ")
    edges_tech_to_merge <- paste0(paste0(tech_nodes, " -> merge;"), collapse = "\n  ")

    paste0(
      "digraph prisma {",
      "\n  graph [rankdir=TB, splines=ortho, bgcolor=\"#FBFCFF\",",
      " nodesep=1.1, ranksep=1.4, pad=0.35];",
      "\n  node  [shape=box, style=\"filled\", fillcolor=\"#FFFFFF\",",
      " color=\"#1F4E79\", penwidth=1.6, fontname=\"Helvetica\",",
      " fontsize=18, margin=\"0.22,0.16\"];",
      "\n  edge  [color=\"#1F4E79\", penwidth=1.6, arrowsize=0.9];",
      "\n",
      "\n  ", db_decl,
      "\n  ", tech_decl,
      "\n  merge [label=", merge_html, ", fillcolor=\"#F2F7FF\"];",
      "\n  dup   [label=", dup_html,   ", fillcolor=\"#C6E2FF\"];",
      "\n  sci   [label=", sci_html,   ", fillcolor=\"#B9D3EE\"];",
      "\n",
      "\n  ", db_rank,
      "\n  ", tech_rank,
      "\n",
      "\n  ", edges_db_to_tech,
      "\n  ", edges_tech_to_merge,
      "\n  merge -> dup;",
      "\n  dup -> sci;",
      "\n}"
    )
  })

  output$flow <- renderGrViz({ grViz(build_dot()) })


  # ============================================================
  # 4.13  DOWNLOAD HANDLERS (NEW)
  # ============================================================

  # --- Download: Graphviz DOT source file ---
  output$dl_dot <- downloadHandler(
    filename = function() {
      paste0("prisma_diagram_", format(Sys.Date(), "%Y%m%d"), ".dot")
    },
    content = function(file) {
      writeLines(build_dot(), file)
    }
  )

  # --- Download: Counts CSV ---
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("prisma_counts_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      db  <- db_data()
      cts <- counts()

      # Per-database rows
      db_rows <- data.frame(
        Stage           = paste0("Identification – ", db$name),
        Records_In      = db$n,
        Records_Removed = cts$tech_removed_by_db,
        Records_Out     = cts$after_tech_by_db,
        Notes           = "Technical filter",
        stringsAsFactors = FALSE
      )

      # Pipeline summary rows
      summary_rows <- data.frame(
        Stage           = c("Merged after technical filters",
                            "Deduplication",
                            "Eligibility filter",
                            "Final included"),
        Records_In      = c(cts$merged_after_tech, cts$merged_after_tech,
                            cts$after_dup,          cts$after_dup),
        Records_Removed = c(0L, cts$dup_removed, cts$sci_removed, 0L),
        Records_Out     = c(cts$merged_after_tech, cts$after_dup,
                            cts$final_included,     cts$final_included),
        Notes           = c("All databases merged", "Duplicate removal",
                            "Scientific / eligibility criteria", "Included in synthesis"),
        stringsAsFactors = FALSE
      )

      write.csv(rbind(db_rows, summary_rows), file, row.names = FALSE)
    }
  )

} # end server


# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui, server)
