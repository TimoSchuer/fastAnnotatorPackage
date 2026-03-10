#' Launch the Fast Annotator Shiny App
#'
#' Starts an interactive Shiny application for annotating linguistic data
#' stored in a MariaDB database. Provides audio playback, Praat integration,
#' and transcript display.
#'
#' @param con A DBI database connection object (e.g., created with
#'   [DBI::dbConnect()] using [RMariaDB::MariaDB()]).
#' @param pathPraat Character. Path to the directory containing `Praat.exe`.
#' @param pathAudio Character. Path to the directory where audio files are stored.
#' @param sample Character. Name of the sample (subsample) to load on start.
#'   If `NULL`, the first available sample is selected.
#' @param sampleOrder Character. Name of the annotation category to order
#'   the sample by on start. If `NULL`, the first available category is selected.
#'
#' @return Runs the Shiny app (does not return a value).
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(
#'   RMariaDB::MariaDB(),
#'   dbname = "mydb",
#'   host = "localhost",
#'   user = "user",
#'   password = "pass"
#' )
#'
#' fastAnnotator(
#'   con = con,
#'   pathPraat = "C:/Praat",
#'   pathAudio = "D:/audio/",
#'   sample = "mySample",
#'   sampleOrder = "category1"
#' )
#' }
#'
#' @export
fastAnnotator <- function(
  con,
  pathPraat,
  pathAudio,
  sample = NULL,
  sampleOrder = NULL
) {
  stopifnot(DBI::dbIsValid(con))
  stopifnot(dir.exists(pathAudio))

  # Normalize paths
  pathPraat <- normalizePath(pathPraat, winslash = "/", mustWork = FALSE)
  pathAudio <- normalizePath(pathAudio, winslash = "/", mustWork = FALSE)

  # Ensure pathAudio ends with "/"
  if (!grepl("/$", pathAudio)) {
    pathAudio <- paste0(pathAudio, "/")
  }

  # Create temp directory for audio cache
  audioTempDir <- tempfile("fastAnnotator_audio_")
  dir.create(audioTempDir, recursive = TRUE)

  # Build UI and server
  ui <- build_ui()
  server <- build_server(
    con = con,
    pathPraat = pathPraat,
    pathAudio = pathAudio,
    audioTempDir = audioTempDir,
    initialSample = sample,
    initialSampleOrder = sampleOrder
  )

  # Run the app
  shiny::shinyApp(
    ui = ui,
    server = server,
    onStart = function() {
      shiny::onStop(function() {
        # Clean up temp audio directory
        unlink(audioTempDir, recursive = TRUE)
      })
    },
    options = list(launch.browser = TRUE)
  )
}

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
