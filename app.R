# =============================================================================
# PRISMA 2020 Flow Diagram – Multi-Subtopic Publication-Ready Shiny App
#
# Modification: Supports 1–6 independent subtopics, each with its own
#   PRISMA flow diagram, databases, technical filters, deduplication,
#   and scientific / eligibility criteria.
#
# Diagram layout v3: Publication-grade Graphviz with coloured section
#   clusters (IDENTIFICATION / SCREENING / INCLUDED), right-side exclusion
#   boxes, and PRISMA-standard palette — closely matching the style seen in
#   peer-reviewed systematic reviews.
#
# Compliant with:
#   Page MJ, et al. (2021). The PRISMA 2020 statement: an updated guideline
#   for reporting systematic reviews. BMJ, 372, n71.
#   https://doi.org/10.1136/bmj.n71
#
# Run:  shiny::runApp()
# Requires: shiny, DiagrammeR
#           DiagrammeRsvg, rsvg  (for SVG / PNG downloads — install once)
# =============================================================================

library(shiny)
library(DiagrammeR)

# SVG / PNG export – install once if missing:
if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
    !requireNamespace("rsvg",          quietly = TRUE)) {
  message(
    "\n[PRISMA app] SVG / PNG download requires two extra packages.\n",
    "Run in R:  install.packages(c('DiagrammeRsvg', 'rsvg'))\n",
    "DOT and CSV downloads still work without them.\n"
  )
}
HAS_SVG_EXPORT <- requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
  requireNamespace("rsvg",          quietly = TRUE)

APP_VERSION <- "4.2.0"
APP_DATE    <- "2025"
MAX_ST      <- 6      # Maximum number of subtopics supported


# =============================================================================
# SECTION 1 – UTILITY HELPERS
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
# SECTION 2 – GRAPHVIZ / HTML LABEL HELPERS
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

# ---------------------------------------------------------------------------
# make_excl_box_html
# Right-side exclusion box: centred bold title + LEFT-aligned bullet rows.
# Used for "Records removed before screening" and "Records excluded".
# ---------------------------------------------------------------------------
make_excl_box_html <- function(title, lines, cellpadding = 10, td_width = NULL) {
  lines   <- lines[!is.na(lines) & nzchar(lines)]
  td_attr <- if (!is.null(td_width)) paste0(" WIDTH='", td_width, "'") else ""
  title_row <- paste0("<TR><TD ALIGN='CENTER'", td_attr,
                      "><B>", gv_esc(title), "</B></TD></TR>")
  body_rows <- paste0(
    "<TR><TD ALIGN='LEFT'", td_attr, ">",
    gv_esc(lines),
    "</TD></TR>",
    collapse = ""
  )
  paste0("<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='2' CELLPADDING='",
         cellpadding, "'>",
         title_row, body_rows,
         "</TABLE>>")
}

# ---------------------------------------------------------------------------
# make_db_id_html
# Compact DB box for the IDENTIFICATION cluster: bold name + n = X.
# ---------------------------------------------------------------------------
make_db_id_html <- function(db_name, n, cellpadding = 10, td_width = 160) {
  td_attr <- paste0(" WIDTH='", td_width, "'")
  paste0(
    "<<TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0' CELLPADDING='",
    cellpadding, "'>",
    "<TR><TD ALIGN='CENTER'", td_attr, "><B>", gv_esc(db_name), "</B></TD></TR>",
    "<TR><TD ALIGN='CENTER'", td_attr, ">n\u2009=\u2009",
    format(n, big.mark = ","), "</TD></TR>",
    "</TABLE>>"
  )
}


# =============================================================================
# SECTION 3 – USER INTERFACE (UI)
# =============================================================================

ui <- fluidPage(
  
  # ---------- HEAD: Google Fonts + publication-grade CSS ----------
  tags$head(
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
      .app-header .multi-badge {
        display: inline-block;
        background: rgba(255,255,255,0.18);
        color: #fff;
        font-size: 10px;
        font-weight: 600;
        letter-spacing: 0.8px;
        padding: 2px 8px;
        border-radius: 3px;
        margin-left: 6px;
        vertical-align: middle;
        border: 1px solid rgba(255,255,255,0.35);
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

      /* ── Subtopic highlight panel (new) ── */
      details.subtopic-panel > summary {
        background: #E8F0FB;
        color: #1B3F7A;
        border-left: 3px solid #1B4F8C;
      }
      .st-active-label {
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #1B4F8C;
        background: #E8F0FB;
        border: 1px solid #b3c8e8;
        border-radius: 4px;
        padding: 4px 10px;
        margin-bottom: 10px;
        display: block;
      }

      /* ── Active-subtopic picker (radio list, always visible) ── */
      .active-st-picker {
        background: #fff;
        border: 1px solid #dce2ed;
        border-radius: 6px;
        padding: 6px 10px 4px 10px;
        margin-top: 4px;
      }
      .active-st-picker .radio,
      .active-st-picker .form-check,
      .active-st-picker .shiny-options-group > div {
        margin: 4px 0 4px 0;
      }
      .active-st-picker label,
      .active-st-picker .radio label,
      .active-st-picker .form-check-label {
        font-weight: 600 !important;
        color: #0D2B52 !important;
        font-size: 13px !important;
        cursor: pointer;
      }

      /* ── Resizable two-pane layout (replaces sidebarLayout) ── */
      .app-layout {
        display: flex;
        align-items: stretch;
        margin: 0 -15px;       /* pull out to fluidPage container edges */
        min-height: calc(100vh - 110px);
      }
      .sidebar-pane {
        flex: 0 0 460px;       /* default sidebar width (was ~33% of 12-col grid) */
        min-width: 320px;
        max-width: 900px;
        padding: 0 15px;
        background: #F5F6FA;
        position: relative;
      }
      .main-pane {
        flex: 1 1 auto;
        min-width: 0;          /* allows flex item to shrink below content size */
        padding: 0 15px;
      }
      .split-handle {
        flex: 0 0 6px;
        cursor: col-resize;
        background: #dce2ed;
        border-left: 1px solid #c5ccd9;
        border-right: 1px solid #c5ccd9;
        transition: background 0.15s;
        position: relative;
      }
      .split-handle::before {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        width: 2px;
        height: 36px;
        margin-left: -1px;
        margin-top: -18px;
        background: repeating-linear-gradient(
          to bottom,
          #8a96ad 0,
          #8a96ad 3px,
          transparent 3px,
          transparent 6px
        );
        border-radius: 1px;
      }
      .split-handle:hover,
      .split-handle.dragging {
        background: #1B4F8C;
      }
      .split-handle:hover::before,
      .split-handle.dragging::before {
        background: repeating-linear-gradient(
          to bottom,
          #fff 0,
          #fff 3px,
          transparent 3px,
          transparent 6px
        );
      }
      body.split-dragging {
        cursor: col-resize !important;
        user-select: none !important;
      }

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
      .btn-dl.green { background: #276749; }
      .btn-dl.green:hover { background: #1C4532; }
      .btn-dl.teal  { background: #2C7A7B; }
      .btn-dl.teal:hover  { background: #1D5959; }
      .btn-dl.maroon { background: #8B2331; }
      .btn-dl.maroon:hover { background: #66161F; }

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

    ")),
    
    # ----- Draggable splitter behaviour (issue 3) -----
    tags$script(HTML("
      (function() {
        function attachSplitter() {
          var handle = document.querySelector('.split-handle');
          var sidebar = document.querySelector('.sidebar-pane');
          if (!handle || !sidebar || handle.dataset.bound === '1') return;
          handle.dataset.bound = '1';

          var startX, startW;
          function onMove(ev) {
            var dx = ev.pageX - startX;
            var w  = startW + dx;
            // clamp to CSS min/max
            if (w < 320) w = 320;
            if (w > 900) w = 900;
            sidebar.style.flex = '0 0 ' + w + 'px';
          }
          function onUp() {
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
            handle.classList.remove('dragging');
            document.body.classList.remove('split-dragging');
            // notify Shiny so plots re-fit to the new main-pane width
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_width_px',
                                  sidebar.getBoundingClientRect().width,
                                  {priority: 'event'});
            }
            window.dispatchEvent(new Event('resize'));
          }
          handle.addEventListener('mousedown', function(ev) {
            ev.preventDefault();
            startX = ev.pageX;
            startW = sidebar.getBoundingClientRect().width;
            handle.classList.add('dragging');
            document.body.classList.add('split-dragging');
            document.addEventListener('mousemove', onMove);
            document.addEventListener('mouseup', onUp);
          });

          // Double-click resets to default width
          handle.addEventListener('dblclick', function() {
            sidebar.style.flex = '';
            window.dispatchEvent(new Event('resize'));
          });
        }
        // Run after Shiny has rendered the DOM
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', attachSplitter);
        } else {
          attachSplitter();
        }
        // Also retry shortly after, in case Shiny's UI is built asynchronously
        setTimeout(attachSplitter, 200);
        setTimeout(attachSplitter, 800);
      })();
    "))
  ),
  
  # ---------- App header ----------
  div(class = "app-header",
      h2(HTML('PRISMA 2020 Flow Diagram
               <span class="prisma-badge">PRISMA 2020</span>
               <span class="multi-badge">MULTI-SUBTOPIC</span>')),
      div(class = "app-subtitle",
          "Systematic review — one independent flow diagram per subtopic — publication ready &middot; v3 cluster layout")
  ),
  
  div(class = "app-layout",
      
      # ======================================================================
      # SIDEBAR PANE (resizable)
      # ======================================================================
      div(class = "sidebar-pane",
          div(class = "sidebar-scroll",
              
              # ----------------------------------------------------------------
              # NEW: Subtopics Manager
              # ----------------------------------------------------------------
              tags$details(open = TRUE,
                           tags$summary("Subtopics"),
                           div(class = "panel-body",
                               div(class = "small-note",
                                   "Each subtopic has its own databases, filters, and eligibility ",
                                   "criteria, producing an independent PRISMA flow diagram."),
                               sliderInput("k_st",
                                           "Number of subtopics (flowcharts):",
                                           min = 1, max = MAX_ST, value = 1, step = 1),
                               uiOutput("st_name_inputs"),
                               hr(style = "margin:10px 0 8px 0;"),
                               div(class = "small-note", "Select subtopic to configure below:"),
                               uiOutput("st_selector_ui")
                           )
              ),
              
              # ----------------------------------------------------------------
              # Study Metadata (shared across all subtopics)
              # ----------------------------------------------------------------
              tags$details(open = FALSE,
                           tags$summary("0) Study Metadata"),
                           div(class = "panel-body",
                               div(class = "small-note",
                                   "Shared metadata used in auto-generated text for all subtopics."),
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
              # Per-subtopic panels 1–4, shown via conditionalPanel
              # (conditionalPanel hides/shows with CSS so input values are PRESERVED
              #  when switching between subtopics)
              # ----------------------------------------------------------------
              lapply(seq_len(MAX_ST), function(s) {
                conditionalPanel(
                  condition = paste0("input.active_st == '", s,
                                     "' && input.k_st >= ", s),
                  
                  # Panel 1: Databases
                  tags$details(open = TRUE, class = "subtopic-panel",
                               tags$summary("1) Databases"),
                               div(class = "panel-body",
                                   span(class = "st-active-label",
                                        paste0("Subtopic ", s, " — Databases")),
                                   sliderInput(paste0("k_db_st", s),
                                               "Number of databases searched:",
                                               min = 1, max = 20, value = 4, step = 1),
                                   uiOutput(paste0("db_inputs_st", s)),
                                   uiOutput(paste0("db_val_st", s))
                               )
                  ),
                  
                  # Panel 2: Technical filter
                  tags$details(open = TRUE, class = "subtopic-panel",
                               tags$summary("2) Technical filter (per database)"),
                               div(class = "panel-body",
                                   span(class = "st-active-label",
                                        paste0("Subtopic ", s, " — Technical Filter")),
                                   div(class = "small-note",
                                       "Technical filters are applied per database before merging."),
                                   uiOutput(paste0("tech_ui_st", s)),
                                   uiOutput(paste0("tech_val_st", s))
                               )
                  ),
                  
                  # Panel 3: Deduplication
                  tags$details(open = FALSE, class = "subtopic-panel",
                               tags$summary("3) Remove duplicates (merged)"),
                               div(class = "panel-body",
                                   span(class = "st-active-label",
                                        paste0("Subtopic ", s, " — Deduplication")),
                                   numericInput(paste0("dup_removed_st", s),
                                                "Duplicates removed after merging:",
                                                value = 0, min = 0, step = 1),
                                   uiOutput(paste0("dup_val_st", s))
                               )
                  ),
                  
                  # Panel 4: Scientific / eligibility filter
                  tags$details(open = FALSE, class = "subtopic-panel",
                               tags$summary("4) Scientific / eligibility filter"),
                               div(class = "panel-body",
                                   span(class = "st-active-label",
                                        paste0("Subtopic ", s, " — Eligibility Criteria")),
                                   
                                   # --- Optional minimum sample-size filter ---
                                   # (Issue 2) Mirror the "selectable filter" pattern from
                                   # section 2: a checkbox gates whether this criterion is
                                   # applied at all. Some analyses don't filter by n;
                                   # others use n >= 50 per group.
                                   div(class = "db-box",
                                       div(class = "db-title", "Minimum sample size filter"),
                                       checkboxInput(
                                         paste0("n_threshold_apply_st", s),
                                         "Apply minimum sample size filter",
                                         value = FALSE
                                       ),
                                       conditionalPanel(
                                         condition = paste0("input.n_threshold_apply_st", s, " == true"),
                                         numericInput(paste0("n_threshold_st", s),
                                                      HTML("Minimum sample size (n \u2265)"),
                                                      value = 10, min = 0, step = 1)
                                       ),
                                       div(class = "small-note",
                                           "Leave unchecked when no sample-size threshold applies. ",
                                           "When checked, the criterion \u201cn \u2265 [value]\u201d is ",
                                           "added to the eligibility list automatically.")
                                   ),
                                   
                                   textAreaInput(
                                     paste0("sci_custom_st", s),
                                     "Additional eligibility criteria (one per line)",
                                     rows = 3,
                                     placeholder = paste("e.g.",
                                                         "Human observational studies",
                                                         "Exclude animal models",
                                                         "Exclude in vitro studies", sep = "\n")
                                   ),
                                   radioButtons(
                                     paste0("sci_mode_st", s),
                                     "Exclusion count entry mode:",
                                     choices  = c("Total excluded (single number)" = "total",
                                                  "Excluded per criterion"         = "per"),
                                     selected = "total"
                                   ),
                                   uiOutput(paste0("sci_removed_ui_st", s)),
                                   uiOutput(paste0("sci_val_st", s))
                               )
                  )
                  
                )  # end conditionalPanel
              })  # end lapply over MAX_ST
              
          )
      ), # end .sidebar-pane
      
      # ======================================================================
      # DRAGGABLE SPLITTER  (drag to resize sidebar; double-click to reset)
      # ======================================================================
      div(class = "split-handle",
          title = "Drag to resize sidebar (double-click to reset)"),
      
      # ======================================================================
      # MAIN PANE
      # ======================================================================
      div(class = "main-pane",
          tabsetPanel(
            id = "main_tabs",
            
            # ------------------------------------------------------------------
            # Tab 1: Flowchart  (sub-tab per subtopic)
            # ------------------------------------------------------------------
            tabPanel("Flowchart",
                     br(),
                     uiOutput("flow_panel")
            ),
            
            # ------------------------------------------------------------------
            # Tab 2: Counts summary
            # ------------------------------------------------------------------
            tabPanel("Counts summary",
                     br(),
                     uiOutput("counts_st_selector_ui"),
                     br(),
                     verbatimTextOutput("counts_summary")
            ),
            
            # ------------------------------------------------------------------
            # Tab 3: Methods text
            # ------------------------------------------------------------------
            tabPanel("Methods text",
                     br(),
                     uiOutput("methods_st_selector_ui"),
                     br(),
                     div(class = "methods-label", "Methods paragraph (copy into manuscript)"),
                     verbatimTextOutput("methods_text"),
                     br(),
                     div(class = "methods-label", "Figure caption (copy into manuscript)"),
                     verbatimTextOutput("figure_caption")
            ),
            
            # ------------------------------------------------------------------
            # Tab 4: About & Citation
            # ------------------------------------------------------------------
            tabPanel("About & Citation",
                     br(),
                     div(class = "about-h", "About this application"),
                     div(class = "about-p",
                         "This application generates PRISMA 2020-compliant search and ",
                         "selection flow diagrams for systematic reviews. It supports ",
                         "multiple independent subtopics, each with its own databases, ",
                         "per-database technical filters, merged deduplication, and ",
                         "configurable scientific eligibility criteria."
                     ),
                     div(class = "about-p",
                         tags$b("Diagram layout (v3):"), " The flowchart uses Graphviz ",
                         "subgraph clusters to render three coloured section bands — ",
                         tags$b("IDENTIFICATION"), "(light blue), ",
                         tags$b("SCREENING"), "(light amber), and ",
                         tags$b("INCLUDED"), "(light green) — consistent with the visual ",
                         "style of published systematic reviews. Exclusion boxes appear ",
                         "to the right of each stage, connected with right-angle arrows ",
                         "(splines=ortho), matching standard PRISMA 2020 flow diagrams."
                     ),
                     
                     div(class = "about-h", "How to cite this tool"),
                     div(class = "cite-block",
                         HTML(paste0(
                           "[Author(s)]. (", APP_DATE, "). <i>PRISMA 2020 Flow Diagram — ",
                           "Multi-Subtopic Shiny Application</i> (Version ", APP_VERSION, "). ",
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
                     
                     div(class = "about-h", "Exporting diagrams"),
                     div(class = "about-p",
                         tags$b("SVG (vector) — best for journal submission:"),
                         " Click ", tags$b("⬇ SVG (vector)"), " in the Flowchart tab. ",
                         "SVG is lossless and infinitely scalable — ideal for print and ",
                         "figure submissions. Open in Inkscape or Adobe Illustrator to ",
                         "tweak any element before submitting."
                     ),
                     div(class = "about-p",
                         tags$b("PNG @ 300 dpi — for Word / PowerPoint:"),
                         " Click ", tags$b("⬇ PNG 300 dpi"), ". The image is rendered at ",
                         "3 000 px wide (≈ 10 in @ 300 dpi), which meets most journal ",
                         "figure-resolution requirements."
                     ),
                     div(class = "about-p",
                         tags$b("DOT source — for manual Graphviz rendering:"),
                         " Click ", tags$b("⬇ DOT source"), " and render with Graphviz locally:",
                         tags$pre(style = "font-size:12px;margin-top:4px;",
                                  "dot -Tsvg  prisma.dot -o prisma.svg\ndot -Tpng  prisma.dot -Gdpi=300 -o prisma.png\ndot -Tpdf  prisma.dot -o prisma.pdf")
                     ),
                     div(class = "about-p",
                         tags$b("Quick screenshot (no extra package needed):"),
                         " right-click the diagram → \"Save image as…\" for a screen-resolution PNG."
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
  # 4.0  SUBTOPIC NAME INPUTS & SELECTOR
  # ============================================================
  
  output$st_name_inputs <- renderUI({
    k <- safe_int(input$k_st)
    tagList(lapply(seq_len(k), function(s) {
      textInput(paste0("st_name_", s),
                paste0("Subtopic ", s, " name"),
                value = paste0("Subtopic ", s),
                placeholder = "e.g. Diagnosis, Intervention, Prognosis")
    }))
  })
  
  output$st_selector_ui <- renderUI({
    k <- max(1L, safe_int(input$k_st))
    
    # Build current display labels for each subtopic. Reading the name
    # inputs DOES make this UI re-render when the user types a new name,
    # which is intentional (so the displayed labels stay in sync).
    choices_labels <- vapply(seq_len(k), function(s) {
      nm <- input[[paste0("st_name_", s)]] %||% paste0("Subtopic ", s)
      if (!nzchar(trimws(nm))) paste0("Subtopic ", s) else trimws(nm)
    }, character(1))
    
    # CRITICAL: preserve the current selection across re-renders.
    # Without isolate() here, every keystroke in a subtopic-name field
    # would re-create this input and snap it back to "1", which is why
    # users couldn't stay on Subtopic 2+.
    prior <- isolate(input$active_st)
    if (is.null(prior) || !as.character(prior) %in% as.character(seq_len(k))) {
      prior <- "1"
    }
    
    # Always render a properly Shiny-bound input (radioButtons), regardless
    # of k. This ensures input$active_st is always reactive and the
    # conditionalPanel for each subtopic evaluates correctly.
    div(class = "active-st-picker",
        radioButtons("active_st",
                     label    = NULL,
                     choices  = setNames(as.character(seq_len(k)), choices_labels),
                     selected = prior,
                     inline   = FALSE)
    )
  })
  
  # Helper: get display name for subtopic s
  get_st_name <- function(s) {
    nm <- input[[paste0("st_name_", s)]] %||% paste0("Subtopic ", s)
    if (!nzchar(trimws(nm))) paste0("Subtopic ", s) else trimws(nm)
  }
  
  
  # ============================================================
  # 4.1  PER-SUBTOPIC HELPER FUNCTIONS (not reactives—plain fns)
  # These read current input values for subtopic s on demand.
  # ============================================================
  
  db_data_st <- function(s) {
    k  <- safe_int(input[[paste0("k_db_st", s)]])
    defaults  <- c("PubMed","Scopus","Web of Science","Embase","Cochrane",
                   "PsycINFO","CINAHL","MEDLINE","Google Scholar","ProQuest",
                   "LILACS","CNKI","WanFang","VIP","bioRxiv","medRxiv",
                   "IEEE Xplore","ACM DL","SpringerLink","GutFinder")
    nm <- vapply(seq_len(k), function(i) {
      val <- input[[paste0("db_name_st", s, "_", i)]] %||%
        (if (i <= length(defaults)) defaults[i] else paste0("Database ", i))
      if (!nzchar(val)) paste0("Database ", i) else val
    }, character(1))
    nn <- vapply(seq_len(k), function(i)
      safe_int(input[[paste0("db_n_st", s, "_", i)]]), integer(1))
    data.frame(id = seq_len(k), name = nm, n = as.integer(nn), stringsAsFactors = FALSE)
  }
  
  tech_criteria_st <- function(s) {
    c(input[[paste0("tech_opts_st", s)]]    %||% character(0),
      trim_lines(input[[paste0("tech_custom_st", s)]]))
  }
  
  sci_criteria_st <- function(s) {
    # (Issue 2) The n-threshold criterion is now optional. It is included
    # in the eligibility-criteria list only when the user has checked the
    # corresponding gate; otherwise the list contains only the user's
    # custom criteria.
    apply_n  <- isTRUE(input[[paste0("n_threshold_apply_st", s)]])
    custom   <- trim_lines(input[[paste0("sci_custom_st", s)]])
    if (apply_n) {
      n_val <- safe_int(input[[paste0("n_threshold_st", s)]])
      c(paste0("n \u2265 ", n_val), custom)
    } else {
      custom
    }
  }
  
  tech_removed_info_by_db_st <- function(s) {
    db   <- db_data_st(s)
    k    <- nrow(db)
    crit <- tech_criteria_st(s)
    lapply(seq_len(k), function(i) {
      mode <- input[[paste0("tech_mode_st", s, "_", i)]] %||% "per"
      if (mode == "total") {
        total <- safe_int(input[[paste0("tech_total_st", s, "_", i)]])
        list(mode = "total", crit = crit, per = rep(0L, length(crit)), total = total)
      } else {
        per <- vapply(seq_along(crit), function(j)
          safe_int(input[[paste0("tech_per_st", s, "_", i, "_", j)]]),
          integer(1))
        list(mode = "per", crit = crit, per = as.integer(per),
             total = as.integer(sum(per)))
      }
    })
  }
  
  sci_removed_info_st <- function(s) {
    mode <- input[[paste0("sci_mode_st", s)]] %||% "total"
    crit <- sci_criteria_st(s)
    if (mode == "total") {
      total <- safe_int(input[[paste0("sci_removed_total_st", s)]])
      list(mode = "total", crit = crit, per = rep(0L, length(crit)), total = total)
    } else {
      per <- vapply(seq_along(crit), function(j)
        safe_int(input[[paste0("sci_removed_st", s, "_", j)]]),
        integer(1))
      list(mode = "per", crit = crit, per = as.integer(per),
           total = as.integer(sum(per)))
    }
  }
  
  counts_st <- function(s) {
    db        <- db_data_st(s)
    tech_list <- tech_removed_info_by_db_st(s)
    
    total_found_all    <- sum(db$n)
    tech_removed_by_db <- vapply(seq_along(tech_list),
                                 function(i) safe_int(tech_list[[i]]$total), integer(1))
    after_tech_by_db   <- pmax(0L, db$n - tech_removed_by_db)
    merged_after_tech  <- sum(after_tech_by_db)
    
    dup_removed    <- safe_int(input[[paste0("dup_removed_st", s)]])
    after_dup      <- max(0L, merged_after_tech - dup_removed)
    
    sci            <- sci_removed_info_st(s)
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
  }
  
  
  # ============================================================
  # 4.2  DOT BUILDER – publication-grade PRISMA 2020 layout
  #
  # Layout mirrors the standard published PRISMA 2020 flowchart:
  #   • Graphviz clusters supply coloured section bands:
  #       cluster_id  (light blue)  = IDENTIFICATION
  #       cluster_sc  (light amber) = SCREENING
  #       cluster_inc (light green) = INCLUDED
  #   • Right-side exclusion boxes share the same rank as the
  #     main-flow node they branch from, producing the standard
  #     horizontal "→ excluded" arrows.
  #   • splines=ortho gives clean right-angle connectors.
  # ============================================================
  
  build_dot_st <- function(s) {
    db        <- db_data_st(s)
    tech_list <- tech_removed_info_by_db_st(s)
    cts       <- counts_st(s)
    sci       <- sci_removed_info_st(s)
    k         <- nrow(db)
    st_nm     <- get_st_name(s)
    fmt       <- function(n) format(n, big.mark = ",")   # number formatter
    
    # Node-name vectors (prefix avoids DOT keyword clashes)
    db_nodes   <- paste0("xdb",   seq_len(k))
    tech_nodes <- paste0("xtech", seq_len(k))
    
    # ── DB boxes ─────────────────────────────────────────────
    db_decl <- paste(
      vapply(seq_len(k), function(i) {
        paste0('    ', db_nodes[i],
               ' [label=', make_db_id_html(db$name[i], db$n[i],
                                           cellpadding = 10, td_width = 160),
               ', fillcolor="#FFFFFF", color="#2C5282", penwidth=1.6];')
      }, character(1)), collapse = "\n")
    
    # ── Tech-filter boxes ─────────────────────────────────────
    tech_decl <- paste(
      vapply(seq_len(k), function(i) {
        tinfo <- tech_list[[i]]
        lab   <- make_filter_label_html(
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
        paste0('    ', tech_nodes[i],
               ' [label=', lab,
               ', fillcolor="#D6E8F7", color="#2B6CB0", penwidth=1.4];')
      }, character(1)), collapse = "\n")
    
    # ── Merge box (bottom of IDENTIFICATION cluster) ──────────
    xmerge_lab <- make_simple_box_html(
      title      = "Records after technical filters",
      lines      = c(
        paste0("Combined: n\u2009=\u2009", fmt(cts$merged_after_tech)),
        paste0("(Technical filter exclusions: ",
               fmt(sum(cts$tech_removed_by_db)), ")")
      ),
      bold_title = TRUE, title_wrap = TRUE,
      cellpadding = 12, td_width = 400
    )
    
    # ── EXCLUSION BOX 1 – right of merge ─────────────────────
    # "Records removed before screening" (tech removals + duplicates)
    xexcl_id_lab <- make_excl_box_html(
      title = "Records removed before screening",
      lines = c(
        paste0("\u2022 Technical filter exclusions: n\u2009=\u2009",
               fmt(sum(cts$tech_removed_by_db))),
        paste0("\u2022 Duplicate records removed: n\u2009=\u2009",
               fmt(cts$dup_removed))
      ),
      cellpadding = 10, td_width = 310
    )
    
    # ── Screened box (inside SCREENING cluster) ───────────────
    xscreened_lab <- make_simple_box_html(
      title      = "Records screened",
      lines      = paste0("n\u2009=\u2009", fmt(cts$after_dup)),
      bold_title = TRUE, cellpadding = 14, td_width = 400
    )
    
    # ── EXCLUSION BOX 2 – right of screened ──────────────────
    # "Records excluded" with eligibility criteria breakdown
    excl_lines <- {
      hdr <- paste0("\u2022 Records excluded: n\u2009=\u2009", fmt(cts$sci_removed))
      if (sci$mode == "per" && length(sci$crit) > 0) {
        crit_rows <- paste0("   \u2013 ", sci$crit,
                            " (n\u2009=\u2009",
                            format(pmax(0L, sci$per), big.mark = ","), ")")
        c(hdr, crit_rows[nzchar(sci$crit)])
      } else if (length(sci$crit) > 0 && any(nzchar(sci$crit))) {
        crit_str <- paste(sci$crit[nzchar(sci$crit)], collapse = "; ")
        c(hdr, paste0("   Criteria: ", crit_str))
      } else {
        hdr
      }
    }
    xexcl_sc_lab <- make_excl_box_html(
      title = "Records excluded",
      lines = excl_lines,
      cellpadding = 10, td_width = 310
    )
    
    # ── Included box (inside INCLUDED cluster) ────────────────
    xincluded_lab <- make_simple_box_html(
      title      = "Studies included in review",
      lines      = paste0("n\u2009=\u2009", fmt(cts$final_included)),
      bold_title = TRUE, cellpadding = 16, td_width = 400
    )
    
    # ── Rank / edge strings ───────────────────────────────────
    db_rank   <- paste0("    { rank=same; ", paste(db_nodes,   collapse = " "), " }")
    tech_rank <- paste0("    { rank=same; ", paste(tech_nodes, collapse = " "), " }")
    
    edges_db_tech    <- paste(
      vapply(seq_len(k), function(i)
        paste0("  ", db_nodes[i], " -> ", tech_nodes[i], ";"), character(1)),
      collapse = "\n")
    edges_tech_merge <- paste(
      paste0("  ", tech_nodes, " -> xmerge;"), collapse = "\n")
    
    # ── Assemble DOT string ────────────────────────────────────
    paste0(
      'digraph prisma {\n\n',
      
      # ── Graph-level settings ───────────────────────────────────────────────
      '  graph [\n',
      '    rankdir  = TB,\n',
      '    splines  = ortho,\n',
      '    bgcolor  = "#FFFFFF",\n',
      '    nodesep  = 1.1,\n',
      '    ranksep  = 0.85,\n',
      '    pad      = 0.55,\n',
      '    compound = true,\n',
      '    label    = "', gv_esc(st_nm), '\\nPRISMA 2020 Flow Diagram",\n',
      '    labelloc = t,\n',
      '    fontname = "Helvetica-Bold",\n',
      '    fontsize = 14,\n',
      '    fontcolor= "#1A365D"\n',
      '  ];\n\n',
      
      # ── Default node / edge styles ─────────────────────────────────────────
      '  node [\n',
      '    fontname  = "Helvetica",\n',
      '    fontsize  = 11,\n',
      '    margin    = "0.20,0.12",\n',
      '    shape     = box,\n',
      '    style     = "filled",\n',
      '    fillcolor = "#FFFFFF",\n',
      '    color     = "#2C5282",\n',
      '    penwidth  = 1.6\n',
      '  ];\n',
      '  edge [\n',
      '    color    = "#2C5282",\n',
      '    penwidth = 1.6,\n',
      '    arrowsize= 0.88\n',
      '  ];\n\n',
      
      # ══════════════════════════════════════════════════════════════════════
      # IDENTIFICATION cluster  (light-blue band)
      # Contains: DB boxes, tech-filter boxes, merge box
      # ══════════════════════════════════════════════════════════════════════
      '  subgraph cluster_id {\n',
      '    label     = <<B><FONT POINT-SIZE="13" COLOR="#1A365D">\u00A0IDENTIFICATION\u00A0</FONT></B>>;\n',
      '    labeljust = l;\n',
      '    labelloc  = t;\n',
      '    style     = filled;\n',
      '    fillcolor = "#EBF3FB";\n',
      '    color     = "#2B6CB0";\n',
      '    penwidth  = 2.2;\n\n',
      db_decl,   '\n\n',
      tech_decl, '\n\n',
      '    xmerge [label=', xmerge_lab,
      ', fillcolor="#BFD7F0", color="#1A5E96", penwidth=1.8];\n',
      db_rank,   '\n',
      tech_rank, '\n',
      '  }\n\n',
      
      # Exclusion box 1 (outside cluster, forced to same rank as xmerge)
      '  xexcl_id [\n',
      '    label    = ', xexcl_id_lab, ',\n',
      '    fillcolor= "#FEF2F2",\n',
      '    color    = "#C53030",\n',
      '    penwidth = 1.5\n',
      '  ];\n',
      '  { rank=same; xmerge; xexcl_id }\n\n',
      
      # ══════════════════════════════════════════════════════════════════════
      # SCREENING cluster  (light-amber band)
      # Contains: screened box only
      # ══════════════════════════════════════════════════════════════════════
      '  subgraph cluster_sc {\n',
      '    label     = <<B><FONT POINT-SIZE="13" COLOR="#7B341E">\u00A0SCREENING\u00A0</FONT></B>>;\n',
      '    labeljust = l;\n',
      '    labelloc  = t;\n',
      '    style     = filled;\n',
      '    fillcolor = "#FFFAF0";\n',
      '    color     = "#C05621";\n',
      '    penwidth  = 2.2;\n\n',
      '    xscreened [label=', xscreened_lab,
      ', fillcolor="#FFFFFF", color="#2C5282"];\n',
      '  }\n\n',
      
      # Exclusion box 2 (outside cluster, same rank as xscreened)
      '  xexcl_sc [\n',
      '    label    = ', xexcl_sc_lab, ',\n',
      '    fillcolor= "#FEF2F2",\n',
      '    color    = "#C53030",\n',
      '    penwidth = 1.5\n',
      '  ];\n',
      '  { rank=same; xscreened; xexcl_sc }\n\n',
      
      # ══════════════════════════════════════════════════════════════════════
      # INCLUDED cluster  (light-green band)
      # ══════════════════════════════════════════════════════════════════════
      '  subgraph cluster_inc {\n',
      '    label     = <<B><FONT POINT-SIZE="13" COLOR="#1C4532">\u00A0INCLUDED\u00A0</FONT></B>>;\n',
      '    labeljust = l;\n',
      '    labelloc  = t;\n',
      '    style     = filled;\n',
      '    fillcolor = "#E8F5E9";\n',
      '    color     = "#276749";\n',
      '    penwidth  = 2.2;\n\n',
      '    xincluded [label=', xincluded_lab,
      ', fillcolor="#C6F6D5", color="#276749", penwidth=1.8];\n',
      '  }\n\n',
      
      # ══════════════════════════════════════════════════════════════════════
      # EDGES
      # ══════════════════════════════════════════════════════════════════════
      '  // DB -> Tech filter (one per database)\n',
      edges_db_tech, '\n\n',
      '  // Tech filter -> Merge\n',
      edges_tech_merge, '\n\n',
      '  // Merge -> Exclusion box (tech removals + duplicates)\n',
      '  xmerge -> xexcl_id;\n\n',
      '  // Merge -> Screened (crosses into SCREENING cluster)\n',
      '  xmerge -> xscreened;\n\n',
      '  // Screened -> Exclusion box (eligibility criteria)\n',
      '  xscreened -> xexcl_sc;\n\n',
      '  // Screened -> Included (crosses into INCLUDED cluster)\n',
      '  xscreened -> xincluded;\n',
      '}'
    )
  }
  
  
  # ============================================================
  # 4.3  REGISTER ALL PER-SUBTOPIC DYNAMIC OUTPUTS
  # Use local({}) to capture s correctly in each iteration
  # ============================================================
  
  for (s_outer in seq_len(MAX_ST)) {
    local({
      s <- s_outer
      
      # -- DB inputs UI --
      output[[paste0("db_inputs_st", s)]] <- renderUI({
        k <- safe_int(input[[paste0("k_db_st", s)]])
        defaults  <- c("PubMed","Scopus","Web of Science","Embase","Cochrane",
                       "PsycINFO","CINAHL","MEDLINE","Google Scholar","ProQuest",
                       "LILACS","CNKI","WanFang","VIP","bioRxiv","medRxiv",
                       "IEEE Xplore","ACM DL","SpringerLink","GutFinder")
        default_n <- c(700,800,800,800,800,300,300,500,1000,200,
                       150,120,120,120,90,90,180,180,180,800)
        tagList(lapply(seq_len(k), function(i) {
          fluidRow(
            column(6, textInput(paste0("db_name_st", s, "_", i),
                                paste0("DB ", i, " name"),
                                value = if (i <= length(defaults)) defaults[i]
                                else paste0("DB", i))),
            column(6, numericInput(paste0("db_n_st", s, "_", i),
                                   "Records (n)",
                                   value = if (i <= length(default_n)) default_n[i] else 0,
                                   min = 0, step = 1))
          )
        }))
      })
      
      # -- DB validation --
      output[[paste0("db_val_st", s)]] <- renderUI({
        db <- db_data_st(s)
        if (any(db$n == 0))
          div(class = "warn-box",
              "\u26A0\uFE0F One or more databases has 0 records. Verify your search counts.")
      })
      
      # -- Technical filter UI (shared criteria + per-database mode/counts) --
      output[[paste0("tech_ui_st", s)]] <- renderUI({
        db <- db_data_st(s); k <- nrow(db)
        
        shared_controls <- div(class = "db-box",
                               div(class = "db-title", "Shared technical criteria (all databases)"),
                               checkboxGroupInput(
                                 paste0("tech_opts_st", s), "Pre-defined filters:",
                                 choices  = c("Human only","English only","Exclude reviews",
                                              "Full-text available","Adult only"),
                                 selected = c("Human only","English only")
                               ),
                               textAreaInput(
                                 paste0("tech_custom_st", s),
                                 "Custom criteria (one per line):",
                                 rows = 3,
                                 placeholder = paste0("e.g.\nPublication year \u2265 2015\n",
                                                      "Exclude conference abstracts")
                               ),
                               div(class = "small-note", "All databases default to per-criterion mode.")
        )
        
        per_db_blocks <- tagList(lapply(seq_len(k), function(i) {
          div(class = "db-box",
              div(class = "db-title", paste0("Database: ", db$name[i])),
              radioButtons(
                paste0("tech_mode_st", s, "_", i),
                "Exclusion count entry:",
                choices  = c("Total excluded (one number)" = "total",
                             "Excluded per criterion"      = "per"),
                selected = "per"
              ),
              uiOutput(paste0("tech_removed_ui_st", s, "_", i))
          )
        }))
        
        tagList(shared_controls, per_db_blocks)
      })
      
      # -- Tech validation --
      output[[paste0("tech_val_st", s)]] <- renderUI({
        db  <- db_data_st(s)
        cts <- counts_st(s)
        over <- which(cts$tech_removed_by_db > db$n)
        if (length(over) > 0)
          div(class = "error-box",
              paste0("\u274C Technical filter removes more records than available in: ",
                     paste(db$name[over], collapse = ", "), ". Please check your counts."))
      })
      
      # -- Dup validation --
      output[[paste0("dup_val_st", s)]] <- renderUI({
        cts <- counts_st(s)
        if (cts$dup_removed > cts$merged_after_tech)
          div(class = "error-box",
              "\u274C Duplicates removed exceed records available after technical filters.")
      })
      
      # -- Sci removed UI --
      output[[paste0("sci_removed_ui_st", s)]] <- renderUI({
        mode <- input[[paste0("sci_mode_st", s)]] %||% "total"
        crit <- sci_criteria_st(s)
        if (mode == "total") {
          numericInput(paste0("sci_removed_total_st", s),
                       "How many excluded (total) by eligibility filter?",
                       value = 0, min = 0, step = 1)
        } else {
          tagList(
            div(class = "small-note", "Enter excluded count per criterion:"),
            lapply(seq_along(crit), function(j) {
              numericInput(paste0("sci_removed_st", s, "_", j),
                           label = crit[j], value = 0, min = 0, step = 1)
            })
          )
        }
      })
      
      # -- Sci validation --
      output[[paste0("sci_val_st", s)]] <- renderUI({
        cts <- counts_st(s)
        if (cts$sci_removed > cts$after_dup)
          div(class = "error-box",
              "\u274C Eligibility filter excludes more records than remain after deduplication.")
      })
      
      # -- Dynamic tech_removed_ui per database (up to 20 databases per subtopic) --
      observe({
        db   <- db_data_st(s)
        k_db <- nrow(db)
        for (i_outer in seq_len(20)) {
          local({
            i <- i_outer
            output[[paste0("tech_removed_ui_st", s, "_", i)]] <- renderUI({
              if (i > safe_int(input[[paste0("k_db_st", s)]])) return(NULL)
              mode <- input[[paste0("tech_mode_st", s, "_", i)]] %||% "per"
              crit <- tech_criteria_st(s)
              if (mode == "total") {
                numericInput(paste0("tech_total_st", s, "_", i),
                             "How many excluded (total) in this database?",
                             value = 0, min = 0, step = 1)
              } else {
                if (length(crit) == 0) {
                  div(class = "small-note", "No criteria selected yet.")
                } else {
                  tagList(
                    div(class = "small-note", "Enter excluded count per criterion:"),
                    lapply(seq_along(crit), function(j) {
                      numericInput(paste0("tech_per_st", s, "_", i, "_", j),
                                   label = crit[j], value = 0, min = 0, step = 1)
                    })
                  )
                }
              }
            })
          })
        }
      })
      
      # -- Flow diagram --
      output[[paste0("flow_st", s)]] <- renderGrViz({
        grViz(build_dot_st(s))
      })
      
      # -- Download: DOT source --
      output[[paste0("dl_dot_st", s)]] <- downloadHandler(
        filename = function() {
          nm  <- gsub("[^A-Za-z0-9_]", "_",
                      input[[paste0("st_name_", s)]] %||% paste0("st", s))
          paste0("prisma_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".dot")
        },
        content = function(file) writeLines(build_dot_st(s), file)
      )
      
      # -- Download: Counts CSV --
      output[[paste0("dl_csv_st", s)]] <- downloadHandler(
        filename = function() {
          nm  <- gsub("[^A-Za-z0-9_]", "_",
                      input[[paste0("st_name_", s)]] %||% paste0("st", s))
          paste0("prisma_counts_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
        },
        content = function(file) {
          db  <- db_data_st(s)
          cts <- counts_st(s)
          db_rows <- data.frame(
            Stage           = paste0("Identification – ", db$name),
            Records_In      = db$n,
            Records_Removed = cts$tech_removed_by_db,
            Records_Out     = cts$after_tech_by_db,
            Notes           = "Technical filter",
            stringsAsFactors = FALSE
          )
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
                                "Scientific / eligibility criteria",
                                "Included in synthesis"),
            stringsAsFactors = FALSE
          )
          write.csv(rbind(db_rows, summary_rows), file, row.names = FALSE)
        }
      )
      
      # -- Download: SVG vector graphic --
      output[[paste0("dl_svg_st", s)]] <- downloadHandler(
        filename = function() {
          nm <- gsub("[^A-Za-z0-9_]", "_",
                     input[[paste0("st_name_", s)]] %||% paste0("st", s))
          paste0("prisma_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".svg")
        },
        content = function(file) {
          req(HAS_SVG_EXPORT)
          svg_str <- DiagrammeRsvg::export_svg(grViz(build_dot_st(s)))
          writeLines(svg_str, file)
        }
      )
      
      # -- Download: PNG @ 300 dpi (≈ 3 000 px wide) --
      output[[paste0("dl_png_st", s)]] <- downloadHandler(
        filename = function() {
          nm <- gsub("[^A-Za-z0-9_]", "_",
                     input[[paste0("st_name_", s)]] %||% paste0("st", s))
          paste0("prisma_", nm, "_300dpi_", format(Sys.Date(), "%Y%m%d"), ".png")
        },
        content = function(file) {
          req(HAS_SVG_EXPORT)
          svg_str <- DiagrammeRsvg::export_svg(grViz(build_dot_st(s)))
          tmp_svg <- tempfile(fileext = ".svg")
          on.exit(unlink(tmp_svg), add = TRUE)
          writeLines(svg_str, tmp_svg)
          # width = 3000 px → ~10 in @ 300 dpi; height scales proportionally
          rsvg::rsvg_png(tmp_svg, file = file, width = 3000)
        }
      )
      
      # -- Download: PDF (vector, publication-ready) --
      output[[paste0("dl_pdf_st", s)]] <- downloadHandler(
        filename = function() {
          nm <- gsub("[^A-Za-z0-9_]", "_",
                     input[[paste0("st_name_", s)]] %||% paste0("st", s))
          paste0("prisma_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
        },
        content = function(file) {
          req(HAS_SVG_EXPORT)
          svg_str <- DiagrammeRsvg::export_svg(grViz(build_dot_st(s)))
          tmp_svg <- tempfile(fileext = ".svg")
          on.exit(unlink(tmp_svg), add = TRUE)
          writeLines(svg_str, tmp_svg)
          # rsvg_pdf produces a true vector PDF (no rasterization), so the
          # diagram remains crisp at any zoom level — ideal for journals.
          rsvg::rsvg_pdf(tmp_svg, file = file)
        }
      )
      
    })  # end local
  }  # end for s_outer
  
  
  # ============================================================
  # 4.4  MAIN OUTPUT: Flowchart panel (sub-tabs per subtopic)
  # ============================================================
  
  output$flow_panel <- renderUI({
    k <- safe_int(input$k_st)
    
    prisma_footer <- div(class = "prisma-footer",
                         HTML(paste0(
                           "<b>PRISMA 2020</b> &mdash; Page MJ, McKenzie JE, Bossuyt PM, et al. ",
                           "The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. ",
                           "<i>BMJ</i>. 2021;372:n71. ",
                           "<a href='https://doi.org/10.1136/bmj.n71' target='_blank'>",
                           "https://doi.org/10.1136/bmj.n71</a>"
                         ))
    )
    
    if (k == 1) {
      tagList(
        div(class = "dl-row",
            downloadButton("dl_dot_st1", "⬇ DOT source",    class = "btn-dl"),
            downloadButton("dl_csv_st1", "⬇ Counts CSV",    class = "btn-dl gold"),
            if (HAS_SVG_EXPORT)
              downloadButton("dl_svg_st1", "⬇ SVG (vector)", class = "btn-dl green"),
            if (HAS_SVG_EXPORT)
              downloadButton("dl_png_st1", "⬇ PNG 300 dpi",  class = "btn-dl teal"),
            if (HAS_SVG_EXPORT)
              downloadButton("dl_pdf_st1", "⬇ PDF (vector)", class = "btn-dl maroon")
        ),
        grVizOutput("flow_st1", height = "960px"),
        br(), prisma_footer
      )
    } else {
      st_tabs <- lapply(seq_len(k), function(s) {
        nm <- input[[paste0("st_name_", s)]] %||% paste0("Subtopic ", s)
        if (!nzchar(trimws(nm))) nm <- paste0("Subtopic ", s)
        tabPanel(
          nm,
          br(),
          div(class = "dl-row",
              downloadButton(paste0("dl_dot_st", s),
                             "⬇ DOT source",    class = "btn-dl"),
              downloadButton(paste0("dl_csv_st", s),
                             "⬇ Counts CSV",    class = "btn-dl gold"),
              if (HAS_SVG_EXPORT)
                downloadButton(paste0("dl_svg_st", s),
                               "⬇ SVG (vector)", class = "btn-dl green"),
              if (HAS_SVG_EXPORT)
                downloadButton(paste0("dl_png_st", s),
                               "⬇ PNG 300 dpi",  class = "btn-dl teal"),
              if (HAS_SVG_EXPORT)
                downloadButton(paste0("dl_pdf_st", s),
                               "⬇ PDF (vector)", class = "btn-dl maroon")
          ),
          grVizOutput(paste0("flow_st", s), height = "960px"),
          br(), prisma_footer
        )
      })
      do.call(tabsetPanel, c(list(id = "flow_st_inner"), st_tabs))
    }
  })
  
  
  # ============================================================
  # 4.5  SUBTOPIC SELECTORS for Counts / Methods tabs
  # ============================================================
  
  output$counts_st_selector_ui <- renderUI({
    k <- safe_int(input$k_st)
    if (k > 1) {
      choices_labels <- vapply(seq_len(k), function(s) {
        nm <- input[[paste0("st_name_", s)]] %||% paste0("Subtopic ", s)
        if (!nzchar(trimws(nm))) paste0("Subtopic ", s) else trimws(nm)
      }, character(1))
      selectInput("counts_st_sel", "Show counts for subtopic:",
                  choices  = setNames(as.character(seq_len(k)), choices_labels),
                  selected = "1")
    }
  })
  
  output$methods_st_selector_ui <- renderUI({
    k <- safe_int(input$k_st)
    if (k > 1) {
      choices_labels <- vapply(seq_len(k), function(s) {
        nm <- input[[paste0("st_name_", s)]] %||% paste0("Subtopic ", s)
        if (!nzchar(trimws(nm))) paste0("Subtopic ", s) else trimws(nm)
      }, character(1))
      selectInput("methods_st_sel", "Generate methods text for subtopic:",
                  choices  = setNames(as.character(seq_len(k)), choices_labels),
                  selected = "1")
    }
  })
  
  # Active subtopic for counts/methods outputs
  active_counts_s  <- reactive({ safe_int(input$counts_st_sel  %||% "1") })
  active_methods_s <- reactive({ safe_int(input$methods_st_sel %||% "1") })
  
  
  # ============================================================
  # 4.6  OUTPUTS – Counts summary
  # ============================================================
  
  output$counts_summary <- renderText({
    s   <- active_counts_s(); if (s < 1) s <- 1L
    db  <- db_data_st(s)
    cts <- counts_st(s)
    st_nm <- get_st_name(s)
    
    db_lines <- paste0(
      vapply(seq_len(nrow(db)), function(i) {
        sprintf("  %-30s identified: %d  |  excluded by tech filter: %d  |  remaining: %d",
                db$name[i], db$n[i],
                cts$tech_removed_by_db[i], cts$after_tech_by_db[i])
      }, character(1)),
      collapse = "\n"
    )
    
    paste0(
      "── PRISMA 2020 RECORD COUNTS  [", st_nm, "] ────────────────────────\n\n",
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
  # 4.7  OUTPUTS – Auto-generated methods text & figure caption
  # ============================================================
  
  output$methods_text <- renderText({
    s         <- active_methods_s(); if (s < 1) s <- 1L
    db        <- db_data_st(s)
    cts       <- counts_st(s)
    sci       <- sci_removed_info_st(s)
    tech_crit <- tech_criteria_st(s)
    st_nm     <- get_st_name(s)
    
    authors  <- trimws(input$meta_authors  %||% "")
    title_sr <- trimws(input$meta_title    %||% "this systematic review")
    year_sr  <- trimws(input$meta_year     %||% format(Sys.Date(), "%Y"))
    
    k       <- nrow(db)
    db_list <- paste0(db$name, " (n\u2009=\u2009", db$n, ")")
    if (k == 1) {
      db_sent <- paste0("One electronic database was searched: ", db_list[1], ".")
    } else {
      db_sent <- paste0(k, " electronic databases were searched: ",
                        paste(db_list[-k], collapse = ", "),
                        ", and ", db_list[k], ", yielding ",
                        format(cts$total_found_all, big.mark = ","),
                        " records in total.")
    }
    
    if (length(tech_crit) > 0) {
      tech_sent <- paste0(
        "Technical filters were applied per database (",
        paste(tech_crit, collapse = "; "),
        "), retaining ", format(cts$merged_after_tech, big.mark = ","),
        " records after merging across all databases."
      )
    } else {
      tech_sent <- paste0(
        "Records from all databases were merged, yielding ",
        format(cts$merged_after_tech, big.mark = ","), " records."
      )
    }
    
    dup_sent <- paste0(
      "Following removal of ", format(cts$dup_removed, big.mark = ","),
      " duplicate record", ifelse(cts$dup_removed == 1, "", "s"), ", ",
      format(cts$after_dup, big.mark = ","), " unique records were screened."
    )
    
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
      "Search strategy and study selection  [Subtopic: ", st_nm, "]\n",
      "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
      "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500",
      "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n\n",
      db_sent, " ", tech_sent, " ", dup_sent, " ", sci_sent,
      "\n\nThe search and selection process was documented using a PRISMA 2020 ",
      "flow diagram (Page et al., 2021). The complete search strategy and all ",
      "eligibility criteria are reported in accordance with PRISMA 2020 guidelines."
    )
  })
  
  
  output$figure_caption <- renderText({
    s     <- active_methods_s(); if (s < 1) s <- 1L
    db    <- db_data_st(s)
    cts   <- counts_st(s)
    sci   <- sci_removed_info_st(s)
    k     <- nrow(db)
    st_nm <- get_st_name(s)
    
    db_names_str <- if (k == 1) db$name[1] else {
      paste0(paste(db$name[-k], collapse = ", "), " and ", db$name[k])
    }
    
    paste0(
      "Figure. PRISMA 2020 flow diagram — ", st_nm, ". ",
      "A total of ", format(cts$total_found_all, big.mark = ","),
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
  # 4.8  OUTPUT – Session info
  # ============================================================
  
  output$session_info <- renderText({
    si <- sessionInfo()
    paste0(
      "R version:    ", R.version$version.string, "\n",
      "shiny:        ", as.character(packageVersion("shiny")), "\n",
      "DiagrammeR:   ", as.character(packageVersion("DiagrammeR")), "\n",
      "App version:  ", APP_VERSION, " (multi-subtopic)\n",
      "Platform:     ", si$platform
    )
  })
  
  
} # end server


# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui, server)
