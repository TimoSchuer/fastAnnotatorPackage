# Fast Annotator - UI Builder
# Returns the UI object for the Shiny app

#' @keywords internal
build_ui <- function() {
  # CSS for loading spinner
  loadingCSS <- "
.loading-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(255, 255, 255, 0.9);
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  z-index: 9999;
}
.spinner {
  width: 50px;
  height: 50px;
  border: 5px solid #f3f3f3;
  border-top: 5px solid #2c3e50;
  border-radius: 50%;
  animation: spin 1s linear infinite;
}
@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
.loading-text {
  margin-top: 15px;
  font-size: 16px;
  color: #2c3e50;
}
.preload-indicator {
  position: fixed;
  bottom: 10px;
  right: 10px;
  background: rgba(44, 62, 80, 0.8);
  color: white;
  padding: 5px 10px;
  border-radius: 5px;
  font-size: 12px;
  z-index: 1000;
  display: none;
}
.preload-indicator.active {
  display: block;
}
"

  # JavaScript for keyboard shortcuts
  keyBindingsJS <- "
document.addEventListener('keydown', function(e) {
  var targetTag = e.target.tagName.toLowerCase();
  var isInput = (targetTag === 'input' || targetTag === 'textarea');

  // Arrow keys - Save and navigate
  if (e.key === 'ArrowDown' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('annotate-saveAndNextItem', Math.random());
    return;
  }

  if (e.key === 'ArrowUp' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('annotate-saveAndPrevItem', Math.random());
    return;
  }

  if (e.key === 'ArrowRight' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('annotate-saveAndNextCat', Math.random());
    return;
  }

  if (e.key === 'ArrowLeft' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('annotate-saveAndPrevCat', Math.random());
    return;
  }

  // Enter key - Just save
  if (e.key === 'Enter' && !e.altKey && !e.shiftKey && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('annotate-saveAnn', Math.random());
    return;
  }

  // Ctrl + Space: Play IP audio
  if (e.key === ' ' && e.ctrlKey && !e.altKey && !e.shiftKey) {
    e.preventDefault();
    Shiny.setInputValue('playIP', Math.random());
    return;
  }

  // Space: Play token audio (only when not in input)
  if (e.key === ' ' && !isInput && !e.ctrlKey) {
    e.preventDefault();
    Shiny.setInputValue('playToken', Math.random());
    return;
  }
});
"

  bslib::page_sidebar(
    title = "Fast Annotator",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    fillable = TRUE,

    # Add custom CSS and JS
    tags$head(
      tags$style(HTML(loadingCSS)),
      tags$script(HTML(keyBindingsJS))
    ),

    # Loading overlay
    div(
      id = "loading-overlay",
      class = "loading-overlay",
      div(class = "spinner"),
      div(class = "loading-text", "Lade Daten und bereite Audio vor...")
    ),

    # Preload indicator
    div(
      id = "preload-indicator",
      class = "preload-indicator",
      bsicons::bs_icon("hourglass-split"),
      " Audio wird vorbereitet..."
    ),

    # Sidebar with controls
    sidebar = bslib::sidebar(
      title = "Einstellungen",
      width = 280,
      shiny::selectInput("sample", "Sample", choices = NULL, selected = NULL),
      shiny::selectInput(
        "categoryOrder",
        "Ordnen nach",
        choices = NULL,
        selected = NULL
      ),
      hr(),
      categorySetUI("categorySet"),
      hr(),
      div(
        class = "shortcut-help",
        tags$strong("Tastenkuerzel:"),
        tags$br(),
        tags$kbd("\u2193"),
        " Speichern + Naechstes Item",
        tags$br(),
        tags$kbd("\u2191"),
        " Speichern + Vorheriges Item",
        tags$br(),
        tags$kbd("\u2192"),
        " Speichern + Naechste Kat.",
        tags$br(),
        tags$kbd("\u2190"),
        " Speichern + Vorherige Kat.",
        tags$br(),
        tags$kbd("Enter"),
        " Speichern",
        tags$br(),
        tags$kbd("Space"),
        " Token abspielen",
        tags$br(),
        tags$kbd("Strg"),
        "+",
        tags$kbd("Space"),
        " IP abspielen"
      ),
      hr(),
      shiny::actionButton(
        "Quit",
        "Beenden",
        icon = shiny::icon("power-off"),
        class = "btn-danger btn-sm"
      )
    ),

    # Main content
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      text = "shinyjs.playAudio = function(path){ var audio = new Audio(path); audio.play(); }",
      functions = c("playAudio")
    ),

    # Progress and Audio row
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      bslib::value_box(
        title = "Aktuelles Item",
        value = shiny::textOutput("itemValue"),
        showcase = bsicons::bs_icon("hash"),
        theme = "primary",
        height = "140px",
        fill = FALSE
      ),
      bslib::value_box(
        title = "Fortschritt",
        value = shiny::textOutput("progressValue"),
        showcase = bsicons::bs_icon("graph-up"),
        theme = "success",
        height = "140px",
        fill = FALSE
      ),
      bslib::card(
        bslib::card_header("Audio"),
        bslib::card_body(
          class = "d-flex align-items-center gap-2 flex-wrap",
          shiny::actionButton(
            "playToken",
            "Token",
            icon = shiny::icon("play"),
            class = "btn-primary btn-sm"
          ),
          shiny::actionButton(
            "playIP",
            "IP",
            icon = shiny::icon("play-circle"),
            class = "btn-secondary btn-sm"
          ),
          shiny::actionButton(
            "openPraat",
            "Praat",
            icon = shiny::icon("waveform-lines"),
            class = "btn-outline-info btn-sm"
          )
        ),
        height = "140px",
        fill = FALSE
      )
    ),

    # Token info card
    bslib::card(
      bslib::card_header(class = "bg-light", "Aktueller Beleg"),
      bslib::card_body(
        class = "overflow-auto",
        DT::dataTableOutput("Beleg")
      ),
      min_height = "150px",
      max_height = "200px",
      fill = FALSE
    ),

    # Annotation card
    annotateUI("annotate"),

    # Transcript section
    transcriptUI("transcript")
  )
}
