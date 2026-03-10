# Fast Annotator - Server
# Builds the Shiny server function for the package

#' @keywords internal
build_server <- function(
  con,
  pathPraat,
  pathAudio,
  audioTempDir,
  initialSample = NULL,
  initialSampleOrder = NULL,
  snippets = FALSE
) {
  function(input, output, session) {
    # Snippets directory and index handling
    snippetDirName <- paste0(
      "snippets_",
      ifelse(!is.null(initialSample), initialSample, "sampleNew")
    )
    snippetDir <- file.path(pathAudio, snippetDirName)

    # If snippets are requested, ensure snippet dir exists and generate snippets
    if (isTRUE(snippets)) {
      if (!dir.exists(snippetDir)) {
        dir.create(snippetDir, recursive = TRUE)
      }

      # Generate snippets if none exist
      wavFiles <- list.files(snippetDir, pattern = "\\.wav$", full.names = TRUE)
      if (length(wavFiles) == 0) {
        try({
          sampleName <- ifelse(
            !is.null(initialSample),
            initialSample,
            "sampleNew"
          )

          tokenRows <- DBI::dbGetQuery(
            con,
            glue::glue_sql(
              "SELECT t.id AS token_id, t.IP_id, f.audio_file,
                      ts.timevalue AS start_time, te.timevalue AS end_time
               FROM token t
               JOIN files f ON t.file_id = f.id
               JOIN timeline ts ON t.start_timestamp_id = ts.id
               JOIN timeline te ON t.end_timestamp_id = te.id
               JOIN subsample_token st ON t.id = st.token_id
               JOIN subsample s ON st.subsample_id = s.id
               WHERE s.name = {sampleName}",
              .con = con
            )
          )

          if (nrow(tokenRows) > 0) {
            # IP snippets: one per IP (query full IP extent from DB, not just sample tokens)
            uniqueIPIds <- unique(tokenRows$IP_id)
            ipInfo <- DBI::dbGetQuery(
              con,
              glue::glue_sql(
                "SELECT t.IP_id, f.audio_file,
                        MIN(ts.timevalue) AS start_time,
                        MAX(te.timevalue) AS end_time
                 FROM token t
                 JOIN files f ON t.file_id = f.id
                 JOIN timeline ts ON t.start_timestamp_id = ts.id
                 JOIN timeline te ON t.end_timestamp_id = te.id
                 WHERE t.IP_id IN ({uniqueIPIds*})
                 GROUP BY t.IP_id, f.audio_file",
                .con = con
              )
            )

            index <- list()

            # write ip snippets
            for (i in seq_len(nrow(ipInfo))) {
              row <- ipInfo[i, ]
              start <- max(0, row$start_time - 0.3)
              end <- row$end_time + 0.3
              src <- paste0(pathAudio, row$audio_file)
              if (file.exists(src)) {
                try(
                  {
                    snd <- tuneR::readWave(
                      src,
                      from = start,
                      to = end,
                      units = "seconds"
                    )
                    outName <- paste0("ip_", row$IP_id, ".wav")
                    outPath <- file.path(snippetDir, outName)
                    tuneR::writeWave(snd, outPath)
                    index[[length(index) + 1]] <- list(
                      filename = outName,
                      type = "ip",
                      token_id = NA_integer_,
                      ip_id = row$IP_id,
                      snippet_start = start
                    )
                  },
                  silent = TRUE
                )
              }
            }

            # write token and transcript snippets
            defaultRange <- 10
            for (i in seq_len(nrow(tokenRows))) {
              tr <- tokenRows[i, ]
              # token snippet
              tstart <- max(0, tr$start_time - 0.2)
              tend <- tr$end_time + 0.2
              src <- paste0(pathAudio, tr$audio_file)
              if (file.exists(src)) {
                try(
                  {
                    snd <- tuneR::readWave(
                      src,
                      from = tstart,
                      to = tend,
                      units = "seconds"
                    )
                    outName <- paste0("token_", tr$token_id, ".wav")
                    outPath <- file.path(snippetDir, outName)
                    tuneR::writeWave(snd, outPath)
                    index[[length(index) + 1]] <- list(
                      filename = outName,
                      type = "token",
                      token_id = tr$token_id,
                      ip_id = tr$IP_id,
                      snippet_start = tstart
                    )
                  },
                  silent = TRUE
                )
              }

              # transcript snippet (around entire IP range)
              iprows <- tokenRows[tokenRows$IP_id == tr$IP_id, , drop = FALSE]
              if (nrow(iprows) > 0) {
                ipStart <- min(iprows$start_time)
                ipEnd <- max(iprows$end_time)
                tts <- max(0, ipStart - defaultRange)
                tte <- ipEnd + defaultRange
                src <- paste0(pathAudio, tr$audio_file)
                if (file.exists(src)) {
                  try(
                    {
                      snd <- tuneR::readWave(
                        src,
                        from = tts,
                        to = tte,
                        units = "seconds"
                      )
                      outName <- paste0("transcript_", tr$token_id, ".wav")
                      outPath <- file.path(snippetDir, outName)
                      tuneR::writeWave(snd, outPath)
                      index[[length(index) + 1]] <- list(
                        filename = outName,
                        type = "transcript",
                        token_id = tr$token_id,
                        ip_id = tr$IP_id,
                        snippet_start = tts
                      )
                    },
                    silent = TRUE
                  )
                }
              }
            }

            # save index
            if (length(index) > 0) {
              idxdf <- do.call(
                rbind,
                lapply(index, function(x) {
                  as.data.frame(x, stringsAsFactors = FALSE)
                })
              )
              idxdf$token_id <- as.integer(idxdf$token_id)
              idxdf$ip_id <- as.integer(idxdf$ip_id)
              idxdf$snippet_start <- as.numeric(idxdf$snippet_start)
              saveRDS(idxdf, file = file.path(snippetDir, "snippet_index.rds"))
            }
          }
        })
        wavFiles <- list.files(
          snippetDir,
          pattern = "\\.wav$",
          full.names = TRUE
        )
      }
    }
    # Ensure `js` is available in server environment (shinyjs exposes it)
    try(
      js <- shinyjs::js,
      silent = TRUE
    )
    # Resource paths for audio
    shiny::addResourcePath("audio", audioTempDir)
    if (isTRUE(snippets) && dir.exists(snippetDir)) {
      shiny::addResourcePath("snippets", snippetDir)
    }

    # Cleanup on stop
    shiny::onStop(function() {
      # Clean up temporary audio files
      if (dir.exists(audioTempDir)) {
        tmpFiles <- list.files(
          audioTempDir,
          pattern = "^tmp",
          full.names = TRUE
        )
        for (f in tmpFiles) {
          try(file.remove(f), silent = TRUE)
        }
      }

      # Clean up praat temp files
      if (dir.exists(pathPraat)) {
        praatTmpFiles <- list.files(
          pathPraat,
          pattern = "^(tmpPraat|openFile)\\.(wav|TextGrid|praat)$",
          full.names = TRUE
        )
        for (f in praatTmpFiles) {
          try(file.remove(f), silent = TRUE)
        }
      }
    })

    # Quit button
    observeEvent(input$Quit, {
      if (dir.exists(audioTempDir)) {
        tmpFiles <- list.files(
          audioTempDir,
          pattern = "^tmp",
          full.names = TRUE
        )
        for (f in tmpFiles) {
          try(file.remove(f), silent = TRUE)
        }
      }

      if (dir.exists(pathPraat)) {
        praatTmpFiles <- list.files(
          pathPraat,
          pattern = "^(tmpPraat|openFile)\\.(wav|TextGrid|praat)$",
          full.names = TRUE
        )
        for (f in praatTmpFiles) {
          try(file.remove(f), silent = TRUE)
        }
      }

      shiny::stopApp()
    })

    # Loading state management ------------------------------------------------
    appReady <- reactiveVal(FALSE)

    observe({
      req(annotated$item())
      req(annotated$sampleData())

      if (!appReady()) {
        appReady(TRUE)
        shinyjs::hide("loading-overlay")
      }
    })

    # Show/hide preload indicator based on loading state
    observe({
      if (isTRUE(audioCache$loading)) {
        shinyjs::addClass("preload-indicator", "active")
      } else {
        shinyjs::removeClass("preload-indicator", "active")
      }
    })

    # Sample selection --------------------------------------------------------
    samples <- reactivePoll(
      intervalMillis = 5000,
      session = session,
      checkFunc = function() {
        DBI::dbGetQuery(con, "SELECT name FROM subsample")
      },
      valueFunc = function() {
        DBI::dbGetQuery(con, "SELECT name FROM subsample") |>
          dplyr::pull(name)
      }
    )

    observe({
      sel <- if (!is.null(initialSample) && initialSample %in% samples()) {
        initialSample
      } else if (length(samples()) >= 5) {
        samples()[5]
      } else {
        samples()[1]
      }

      updateSelectInput(
        session,
        "sample",
        choices = samples(),
        selected = sel
      )
    }) |>
      bindEvent(samples())

    observe({
      categories <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT annotation_category.name
           FROM annotation_category, subsample, subsample_category
           WHERE subsample.name={input$sample}
           AND subsample.id=subsample_category.subsample_id
           AND subsample_category.category_id=annotation_category.id",
          .con = con
        )
      ) |>
        dplyr::pull(name)

      sel <- if (
        !is.null(initialSampleOrder) &&
          initialSampleOrder %in% categories
      ) {
        initialSampleOrder
      } else if (length(categories) >= 2) {
        categories[2]
      } else {
        categories[1]
      }

      updateSelectInput(
        session,
        "categoryOrder",
        choices = categories,
        selected = sel
      )
    }) |>
      bindEvent(input$sample)

    # Category set module -----------------------------------------------------
    categorySet <- categorySetServer(
      id = "categorySet",
      con = con,
      sample = reactive(input$sample)
    )

    # Annotation module -------------------------------------------------------
    annotated <- annotateServer(
      id = "annotate",
      con = con,
      sample = reactive(input$sample),
      categoryOrder = reactive(input$categoryOrder),
      categorySet = categorySet
    )

    # Token display -----------------------------------------------------------
    allTokenData <- reactive({
      req(input$sample)
      req(annotated$sampleData())

      tokenIds <- annotated$sampleData()$id
      if (length(tokenIds) == 0) {
        return(data.frame())
      }

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT token.id, token.IP_id, speaker.name, token.event_text,
                  intonationphrases.ip_text
           FROM token
           JOIN speaker ON speaker.id = token.speaker_id
           JOIN intonationphrases ON token.IP_id = intonationphrases.id
           WHERE token.id IN ({tokenIds*})",
          .con = con
        )
      )
    }) |>
      bindCache(input$sample) |>
      bindEvent(annotated$sampleData())

    itemData <- reactive({
      req(annotated$item())
      req(allTokenData())

      allTokenData() |>
        dplyr::filter(id == annotated$item())
    })

    # Cache category data for current sample
    sampleCategories <- reactive({
      req(input$sample)

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT annotation_category.name AS category_name, annotation_category.id
           FROM annotation_category, subsample, subsample_category
           WHERE subsample.name={input$sample}
           AND subsample.id=subsample_category.subsample_id
           AND subsample_category.category_id=annotation_category.id",
          .con = con
        )
      )
    }) |>
      bindCache(input$sample)

    # Cache for annotations
    annotationCache <- reactiveValues(
      data = NULL,
      sample = NULL,
      changedItems = c()
    )

    # Initialize/refresh annotation cache when sample changes
    observe({
      req(input$sample)
      req(annotated$sampleData())

      if (
        is.null(annotationCache$sample) ||
          annotationCache$sample != input$sample
      ) {
        tokenIds <- annotated$sampleData()$id
        if (length(tokenIds) > 0) {
          annotationCache$data <- DBI::dbGetQuery(
            con,
            glue::glue_sql(
              "SELECT token_id, category_id, value
               FROM annotation
               WHERE token_id IN ({tokenIds*})",
              .con = con
            )
          )
        } else {
          annotationCache$data <- data.frame()
        }
        annotationCache$sample <- input$sample
        annotationCache$changedItems <- c()
      }
    }) |>
      bindEvent(c(input$sample, annotated$sampleData()))

    # Update cache when an item is saved
    observe({
      req(annotated$annotationVersion() > 0)
      req(annotated$item())

      itemAnnots <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT token_id, category_id, value
           FROM annotation
           WHERE token_id = {annotated$item()}",
          .con = con
        )
      )

      if (!is.null(annotationCache$data)) {
        annotationCache$data <- annotationCache$data |>
          dplyr::filter(token_id != annotated$item()) |>
          dplyr::bind_rows(itemAnnots)
      }

      annotationCache$changedItems <- unique(c(
        annotationCache$changedItems,
        annotated$item()
      ))
    }) |>
      bindEvent(annotated$annotationVersion(), ignoreInit = TRUE)

    allItemAnnotations <- reactive({
      req(annotationCache$data)
      annotationCache$data
    })

    itemAnnotations <- reactive({
      req(annotated$item())
      req(allItemAnnotations())

      allItemAnnotations() |>
        dplyr::filter(token_id == annotated$item())
    })

    # Effective categories for Beleg
    belegCategories <- reactive({
      req(sampleCategories())

      activeCats <- categorySet$activeCategoryIds()
      if (!is.null(activeCats) && nrow(activeCats) > 0) {
        sampleCategories() |>
          dplyr::filter(id %in% activeCats$id) |>
          dplyr::rename(category_id = id) |>
          dplyr::mutate(sort_order = match(category_id, activeCats$id)) |>
          dplyr::arrange(sort_order) |>
          dplyr::select(-sort_order)
      } else {
        sampleCategories() |>
          dplyr::rename(category_id = id)
      }
    })

    # Pre-compute Beleg data
    belegData <- reactive({
      req(itemData())
      req(belegCategories())
      req(nrow(itemData()) > 0)

      cats <- belegCategories()
      catOrder <- cats$category_name

      itemData() |>
        tidyr::crossing(cats) |>
        dplyr::left_join(
          itemAnnotations(),
          by = c("id" = "token_id", "category_id")
        ) |>
        dplyr::select(!category_id) |>
        tidyr::pivot_wider(
          names_from = category_name,
          values_from = value,
          values_fn = ~ dplyr::first(.x)
        ) |>
        dplyr::select(
          id,
          IP_id,
          name,
          event_text,
          ip_text,
          dplyr::any_of(catOrder)
        )
    })

    output$Beleg <- DT::renderDataTable(
      belegData(),
      server = FALSE,
      options = list(scrollX = TRUE, dom = "t", ordering = FALSE)
    )

    # Progress displays ------------------------------------------------------
    output$itemValue <- renderText({
      req(annotated$item())
      paste0("ID: ", annotated$item(), " (Item ", annotated$itemNumber(), ")")
    })

    output$progressValue <- renderText({
      req(annotated$annotationProgress())
      progress <- annotated$annotationProgress()
      pct <- if (progress$total > 0) {
        round(progress$annotated / progress$total * 100, 1)
      } else {
        0
      }
      paste0(progress$annotated, " / ", progress$total, " (", pct, "%)")
    })

    # Audio preloading system -------------------------------------------------
    audioCache <- reactiveValues(
      files = list(),
      loading = FALSE,
      currentBatch = NULL
    )

    # Function to create audio file for a token
    createAudioFile <- function(tokenId, prefix = "tmpToken") {
      # Prefer pre-generated token snippet if present in snippet dir
      if (isTRUE(snippets)) {
        snipName <- paste0("token_", tokenId, ".wav")
        snipPath <- file.path(snippetDir, snipName)
        if (file.exists(snipPath)) {
          return(paste0("snippets/", snipName))
        }
      }
      audioInfo <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT files.audio_file,
                  t_start.timevalue AS start_time,
                  t_end.timevalue AS end_time
           FROM token
           JOIN files ON token.file_id = files.id
           JOIN timeline t_start ON token.start_timestamp_id = t_start.id
           JOIN timeline t_end ON token.end_timestamp_id = t_end.id
           WHERE token.id = {tokenId}",
          .con = con
        )
      )

      if (nrow(audioInfo) == 0) {
        return(NULL)
      }

      tryCatch(
        {
          sound <- paste0(pathAudio, audioInfo$audio_file) |>
            tuneR::readWave(
              from = max(0, audioInfo$start_time - 0.2),
              to = audioInfo$end_time + 0.2,
              units = "seconds"
            )

          s <- phonTools::makesound(
            sound@left,
            filename = prefix,
            fs = sound@samp.rate
          )

          fileName <- paste0(prefix, "_", tokenId, ".wav")
          absolutePath <- file.path(audioTempDir, fileName)
          relativeName <- paste0("audio/", fileName)

          phonTools::writesound(s, filename = absolutePath)
          relativeName
        },
        error = function(e) {
          NULL
        }
      )
    }

    # Get upcoming token IDs
    upcomingTokens <- reactive({
      req(annotated$itemNumber())
      req(annotated$sampleData())

      currentIdx <- annotated$itemNumber()
      maxIdx <- nrow(annotated$sampleData())

      indices <- seq(currentIdx, min(currentIdx + 5, maxIdx))
      annotated$sampleData()$id[indices]
    })

    # Preload audio for current item immediately
    currentAudioName <- reactive({
      req(annotated$item())

      tokenId <- annotated$item()

      if (!is.null(audioCache$files[[as.character(tokenId)]])) {
        return(audioCache$files[[as.character(tokenId)]])
      }

      name <- createAudioFile(tokenId)
      if (!is.null(name)) {
        audioCache$files[[as.character(tokenId)]] <- name
      }
      name
    })

    # Background preloading
    observe({
      req(upcomingTokens())

      if (audioCache$loading) {
        return()
      }

      tokensToLoad <- upcomingTokens()

      tokensToLoad <- tokensToLoad[
        !sapply(as.character(tokensToLoad), function(id) {
          !is.null(audioCache$files[[id]])
        })
      ]

      if (length(tokensToLoad) == 0) {
        return()
      }

      audioCache$loading <- TRUE
      audioCache$currentBatch <- tokensToLoad

      later::later(
        function() {
          for (tokenId in tokensToLoad) {
            alreadyCached <- isolate(
              !is.null(audioCache$files[[as.character(tokenId)]])
            )
            if (!alreadyCached) {
              name <- createAudioFile(tokenId)
              if (!is.null(name)) {
                isolate(audioCache$files[[as.character(tokenId)]] <- name)
              }
            }
          }
          isolate({
            audioCache$loading <- FALSE
            audioCache$currentBatch <- NULL
          })
        },
        delay = 0.1
      )
    }) |>
      bindEvent(upcomingTokens(), ignoreInit = FALSE)

    # Clean up old cache entries
    observe({
      req(annotated$itemNumber())
      req(annotated$sampleData())

      currentIdx <- annotated$itemNumber()
      maxIdx <- nrow(annotated$sampleData())

      keepIndices <- seq(max(1, currentIdx - 2), min(currentIdx + 7, maxIdx))
      keepIds <- as.character(annotated$sampleData()$id[keepIndices])

      cachedIds <- names(audioCache$files)
      toRemove <- setdiff(cachedIds, keepIds)

      for (id in toRemove) {
        filePath <- audioCache$files[[id]]
        if (!is.null(filePath)) {
          absolutePath <- file.path(audioTempDir, basename(filePath))
          if (file.exists(absolutePath)) {
            try(file.remove(absolutePath), silent = TRUE)
          }
        }
        audioCache$files[[id]] <- NULL
      }
    }) |>
      bindEvent(annotated$itemNumber())

    # Audio playback ----------------------------------------------------------
    observe({
      req(currentAudioName())
      js$playAudio(currentAudioName())
    }) |>
      bindEvent(input$playToken)

    # Play full IP
    observe({
      req(annotated$item())
      # Try to use pre-generated IP snippet if present
      ipRow <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT IP_id FROM token WHERE id = {annotated$item()}",
          .con = con
        )
      )
      ipId <- ipRow$IP_id[1]
      if (isTRUE(snippets)) {
        ipSnip <- file.path(snippetDir, paste0("ip_", ipId, ".wav"))
        if (file.exists(ipSnip)) {
          js$playAudio(paste0("snippets/", basename(ipSnip)))
          return()
        }
      }

      ipInfo <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT files.audio_file,
                  MIN(t_start.timevalue) AS start_time,
                  MAX(t_end.timevalue) AS end_time
           FROM token
           JOIN files ON token.file_id = files.id
           JOIN timeline t_start ON token.start_timestamp_id = t_start.id
           JOIN timeline t_end ON token.end_timestamp_id = t_end.id
           WHERE token.IP_id = (SELECT IP_id FROM token WHERE id = {annotated$item()})
           GROUP BY files.audio_file",
          .con = con
        )
      )

      sound <- paste0(pathAudio, ipInfo$audio_file) |>
        tuneR::readWave(
          from = max(0, ipInfo$start_time - 0.3),
          to = ipInfo$end_time + 0.3,
          units = "seconds"
        )

      s <- phonTools::makesound(
        sound@left,
        filename = "tmpIP",
        fs = sound@samp.rate
      )

      fileName <- paste0("tmpIP", sample(1:10000, 1), ".wav")
      absolutePath <- file.path(audioTempDir, fileName)
      relativeName <- paste0("audio/", fileName)
      phonTools::writesound(s, filename = absolutePath)

      js$playAudio(relativeName)
    }) |>
      bindEvent(input$playIP)

    # Open in Praat -----------------------------------------------------------
    createTextGrid <- function(tokenId, sound) {
      tokens <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT t.id, t.event_text, ts.timevalue AS time_start, te.timevalue AS time_end
           FROM token t
           JOIN timeline ts ON t.start_timestamp_id = ts.id
           JOIN timeline te ON t.end_timestamp_id = te.id
           WHERE t.IP_id = (SELECT IP_id FROM token WHERE id = {tokenId})
           ORDER BY ts.timevalue",
          .con = con
        )
      )

      if (nrow(tokens) == 0) {
        return(NULL)
      }

      minTime <- min(tokens$time_start)
      tokens <- tokens |>
        dplyr::mutate(
          time_start_norm = time_start - minTime,
          time_end_norm = time_end - minTime
        )

      soundDuration <- round(length(sound@left) / sound@samp.rate, 4)

      tg <- rPraat::tg.createNewTextGrid(tMin = 0, tMax = soundDuration) |>
        rPraat::tg.insertNewIntervalTier(1, "GAT")

      for (i in seq_len(nrow(tokens))) {
        if (tokens$time_end_norm[i] > soundDuration) {
          next
        }

        tryCatch(
          {
            tg <- rPraat::tg.insertInterval(
              tg,
              tierInd = 1,
              tStart = tokens$time_start_norm[i],
              tEnd = tokens$time_end_norm[i],
              label = tokens$event_text[i]
            )
          },
          error = function(e) {
            # Skip overlapping intervals
          }
        )
      }

      tg
    }

    openInPraat <- function(tokenId) {
      praatExe <- file.path(pathPraat, "Praat.exe")

      ipInfo <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT f.audio_file,
                  MIN(ts.timevalue) AS start_time,
                  MAX(te.timevalue) AS end_time
           FROM token t
           JOIN files f ON t.file_id = f.id
           JOIN timeline ts ON t.start_timestamp_id = ts.id
           JOIN timeline te ON t.end_timestamp_id = te.id
           WHERE t.IP_id = (SELECT IP_id FROM token WHERE id = {tokenId})
           GROUP BY f.audio_file",
          .con = con
        )
      )

      if (nrow(ipInfo) == 0) {
        return(NULL)
      }

      audioPath <- paste0(pathAudio, ipInfo$audio_file)
      startTime <- max(0, ipInfo$start_time - 0.3)
      endTime <- ipInfo$end_time + 0.3

      sound <- tuneR::readWave(
        audioPath,
        from = startTime,
        to = endTime,
        units = "seconds"
      )

      tmpAudioPath <- file.path(pathPraat, "tmpPraat.wav") |>
        normalizePath(winslash = "/", mustWork = FALSE)
      tmpTextGridPath <- file.path(pathPraat, "tmpPraat.TextGrid") |>
        normalizePath(winslash = "/", mustWork = FALSE)

      tuneR::writeWave(sound, tmpAudioPath)

      tg <- createTextGrid(tokenId, sound)
      if (!is.null(tg)) {
        rPraat::tg.write(tg, fileNameTextGrid = tmpTextGridPath)
      }

      praatScript <- c(
        paste0('Read from file: "', tmpAudioPath, '"'),
        paste0('Read from file: "', tmpTextGridPath, '"'),
        'selectObject: "Sound tmpPraat"',
        'plusObject: "TextGrid tmpPraat"',
        'View & Edit'
      )

      scriptPath <- file.path(pathPraat, "openFile.praat") |>
        normalizePath(winslash = "/", mustWork = FALSE)
      readr::write_lines(praatScript, scriptPath)

      cmd <- paste0(
        '"',
        normalizePath(praatExe, winslash = "/"),
        '" --send "',
        scriptPath,
        '"'
      )
      system(cmd, wait = FALSE)
    }

    observe({
      req(annotated$item())
      openInPraat(annotated$item())
    }) |>
      bindEvent(input$openPraat)

    # Transcript module -------------------------------------------------------
    transcriptServer(
      id = "transcript",
      con = con,
      item_id = reactive(annotated$item()),
      dirAudio = pathAudio,
      audioTempDir = audioTempDir,
      snippetDir = if (isTRUE(snippets)) snippetDir else NULL
    )
  }
}
