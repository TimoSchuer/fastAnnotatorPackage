# Fast Annotator - Shiny Modules
# Category Set, Annotation, and Transcript modules

# Category Set UI ----------------------------------------------------------
#' @keywords internal
categorySetUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::selectInput(
      ns("activeSet"),
      "Kategorien-Set",
      choices = c("(Alle)" = ""),
      selected = ""
    ),
    shiny::actionButton(
      ns("manageSets"),
      "Sets verwalten",
      icon = shiny::icon("layer-group"),
      class = "btn-outline-secondary btn-sm w-100"
    )
  )
}

# Category Set Server ------------------------------------------------------
#' @keywords internal
categorySetServer <- function(id, con, sample) {
  stopifnot(is.reactive(sample))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Trigger to refresh set list after create/edit/delete
    setVersion <- reactiveVal(0)

    # Available sets for current sample
    availableSets <- reactive({
      req(sample())
      setVersion()

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT cs.id, cs.name
           FROM category_set cs
           JOIN subsample s ON cs.subsample_id = s.id
           WHERE s.name = {sample()}
           ORDER BY cs.name",
          .con = con
        )
      )
    })

    # Update set dropdown when sample or sets change
    observe({
      sets <- availableSets()
      choices <- c("(Alle)" = "")
      if (nrow(sets) > 0) {
        named <- setNames(as.character(sets$id), sets$name)
        choices <- c(choices, named)
      }
      current <- input$activeSet
      sel <- if (!is.null(current) && current %in% choices) current else ""
      updateSelectInput(session, "activeSet", choices = choices, selected = sel)
    })

    # Get categories for the active set (ordered)
    activeCategoryIds <- reactive({
      if (is.null(input$activeSet) || input$activeSet == "") {
        return(NULL)
      }

      setId <- as.integer(input$activeSet)
      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT ac.id, ac.name
           FROM category_set_item csi
           JOIN annotation_category ac ON csi.category_id = ac.id
           WHERE csi.set_id = {setId}
           ORDER BY csi.sort_order",
          .con = con
        )
      )
    })

    # All categories available for current sample (for the modal)
    sampleCategories <- reactive({
      req(sample())
      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT ac.id, ac.name
           FROM annotation_category ac
           JOIN subsample_category sc ON ac.id = sc.category_id
           JOIN subsample s ON sc.subsample_id = s.id
           WHERE s.name = {sample()}
           ORDER BY sc.category_order",
          .con = con
        )
      )
    })

    # --- Modal dialog for managing sets ---
    observeEvent(input$manageSets, {
      req(sample())

      sets <- availableSets()

      showModal(modalDialog(
        title = "Kategorien-Sets verwalten",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Schliessen"),

        bslib::layout_columns(
          col_widths = c(5, 7),

          # Left: existing sets
          bslib::card(
            bslib::card_header("Vorhandene Sets"),
            bslib::card_body(
              class = "p-2",
              if (nrow(sets) > 0) {
                tagList(
                  shiny::selectInput(
                    ns("editSet"),
                    "Set auswaehlen",
                    choices = setNames(sets$id, sets$name)
                  ),
                  div(
                    class = "d-flex gap-1",
                    shiny::actionButton(
                      ns("loadSet"),
                      "Laden",
                      icon = shiny::icon("pen"),
                      class = "btn-outline-primary btn-sm"
                    ),
                    shiny::actionButton(
                      ns("deleteSet"),
                      "Loeschen",
                      icon = shiny::icon("trash"),
                      class = "btn-outline-danger btn-sm"
                    )
                  )
                )
              } else {
                tags$p(class = "text-muted", "Keine Sets vorhanden.")
              }
            )
          ),

          # Right: create/edit form
          bslib::card(
            bslib::card_header("Set erstellen / bearbeiten"),
            bslib::card_body(
              class = "p-2",
              shiny::textInput(
                ns("setName"),
                "Set-Name",
                value = "",
                placeholder = "z.B. Phonologie"
              ),
              shiny::selectizeInput(
                ns("setCategories"),
                "Kategorien (Reihenfolge = Drag & Drop)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  plugins = list("drag_drop", "remove_button"),
                  placeholder = "Kategorien auswaehlen..."
                )
              ),
              div(
                class = "d-flex gap-1",
                shiny::actionButton(
                  ns("saveSet"),
                  "Speichern",
                  icon = shiny::icon("save"),
                  class = "btn-primary btn-sm"
                ),
                shiny::actionButton(
                  ns("saveAsNewSet"),
                  "Als neu speichern",
                  icon = shiny::icon("plus"),
                  class = "btn-success btn-sm"
                )
              )
            )
          )
        )
      ))

      # Populate category choices
      cats <- sampleCategories()
      updateSelectizeInput(
        session,
        "setCategories",
        choices = setNames(cats$id, cats$name),
        selected = NULL
      )
    })

    # Editing set ID tracker (NULL = creating new)
    editingSetId <- reactiveVal(NULL)

    # Load an existing set into the form
    observeEvent(input$loadSet, {
      req(input$editSet)
      setId <- as.integer(input$editSet)
      editingSetId(setId)

      setInfo <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT name FROM category_set WHERE id = {setId}",
          .con = con
        )
      )
      updateTextInput(session, "setName", value = setInfo$name[1])

      setCats <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT category_id FROM category_set_item
           WHERE set_id = {setId}
           ORDER BY sort_order",
          .con = con
        )
      )

      allCats <- sampleCategories()
      updateSelectizeInput(
        session,
        "setCategories",
        choices = setNames(allCats$id, allCats$name),
        selected = as.character(setCats$category_id)
      )
    })

    # Save (update existing set)
    observeEvent(input$saveSet, {
      req(input$setName, input$setCategories)
      setName <- trimws(input$setName)
      catIds <- as.integer(input$setCategories)

      if (nchar(setName) == 0 || length(catIds) == 0) {
        showNotification(
          "Name und Kategorien duerfen nicht leer sein.",
          type = "error"
        )
        return()
      }

      subsampleId <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT id FROM subsample WHERE name = {sample()}",
          .con = con
        )
      )$id[1]

      tryCatch(
        {
          if (!is.null(editingSetId())) {
            sid <- editingSetId()
            DBI::dbExecute(
              con,
              glue::glue_sql(
                "UPDATE category_set SET name = {setName} WHERE id = {sid}",
                .con = con
              )
            )
            DBI::dbExecute(
              con,
              glue::glue_sql(
                "DELETE FROM category_set_item WHERE set_id = {sid}",
                .con = con
              )
            )
            for (i in seq_along(catIds)) {
              DBI::dbExecute(
                con,
                glue::glue_sql(
                  "INSERT INTO category_set_item (set_id, category_id, sort_order)
                   VALUES ({sid}, {catIds[i]}, {i})",
                  .con = con
                )
              )
            }
            showNotification(
              paste0("Set '", setName, "' aktualisiert."),
              type = "message"
            )
          } else {
            DBI::dbExecute(
              con,
              glue::glue_sql(
                "INSERT INTO category_set (subsample_id, name)
                 VALUES ({subsampleId}, {setName})",
                .con = con
              )
            )
            sid <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]
            for (i in seq_along(catIds)) {
              DBI::dbExecute(
                con,
                glue::glue_sql(
                  "INSERT INTO category_set_item (set_id, category_id, sort_order)
                   VALUES ({sid}, {catIds[i]}, {i})",
                  .con = con
                )
              )
            }
            editingSetId(sid)
            showNotification(
              paste0("Set '", setName, "' erstellt."),
              type = "message"
            )
          }
          setVersion(setVersion() + 1)
        },
        error = function(e) {
          showNotification(paste0("Fehler: ", e$message), type = "error")
        }
      )
    })

    # Save as new (always creates a new set)
    observeEvent(input$saveAsNewSet, {
      req(input$setName, input$setCategories)
      setName <- trimws(input$setName)
      catIds <- as.integer(input$setCategories)

      if (nchar(setName) == 0 || length(catIds) == 0) {
        showNotification(
          "Name und Kategorien duerfen nicht leer sein.",
          type = "error"
        )
        return()
      }

      subsampleId <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT id FROM subsample WHERE name = {sample()}",
          .con = con
        )
      )$id[1]

      tryCatch(
        {
          DBI::dbExecute(
            con,
            glue::glue_sql(
              "INSERT INTO category_set (subsample_id, name)
               VALUES ({subsampleId}, {setName})",
              .con = con
            )
          )
          sid <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]
          for (i in seq_along(catIds)) {
            DBI::dbExecute(
              con,
              glue::glue_sql(
                "INSERT INTO category_set_item (set_id, category_id, sort_order)
                 VALUES ({sid}, {catIds[i]}, {i})",
                .con = con
              )
            )
          }
          editingSetId(sid)
          setVersion(setVersion() + 1)
          showNotification(
            paste0("Neues Set '", setName, "' erstellt."),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste0("Fehler: ", e$message), type = "error")
        }
      )
    })

    # Delete set
    observeEvent(input$deleteSet, {
      req(input$editSet)
      sid <- as.integer(input$editSet)

      tryCatch(
        {
          DBI::dbExecute(
            con,
            glue::glue_sql(
              "DELETE FROM category_set WHERE id = {sid}",
              .con = con
            )
          )
          editingSetId(NULL)
          updateTextInput(session, "setName", value = "")
          setVersion(setVersion() + 1)
          showNotification("Set geloescht.", type = "message")
          removeModal()
        },
        error = function(e) {
          showNotification(paste0("Fehler: ", e$message), type = "error")
        }
      )
    })

    # Return the active set's category data (or NULL for "all")
    return(list(
      activeCategoryIds = activeCategoryIds,
      activeSetId = reactive(input$activeSet)
    ))
  })
}


# Annotation UI -----------------------------------------------------------
#' @keywords internal
annotateUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        class = "bg-primary text-white",
        "Annotation"
      ),
      bslib::card_body(
        class = "overflow-auto",
        # Navigation row
        bslib::layout_columns(
          col_widths = c(2, 2, 2, 3, 3),
          col_widths_sm = c(6, 6, 12, 6, 6),
          # Jump to item number
          div(
            shiny::numericInput(
              ns("ItemNumber"),
              label = "Gehe zu Item",
              value = 1,
              min = 1,
              max = 10000,
              step = 1
            ),
            shiny::actionButton(
              ns("skipToItem"),
              label = "Springe",
              icon = shiny::icon("forward-fast"),
              class = "btn-sm"
            )
          ),
          # Jump to item ID
          div(
            shiny::textInput(
              ns("skipItemID"),
              label = "Item ID",
              value = ""
            ),
            shiny::actionButton(
              ns("skipToItemID"),
              label = "Springe zu ID",
              icon = shiny::icon("forward-fast"),
              class = "btn-sm"
            )
          ),
          # Bad audio checkbox
          div(
            class = "pt-4",
            shiny::checkboxInput(
              ns("badAudio"),
              "Audio nicht auswertbar",
              value = FALSE
            )
          ),
          # Previous button
          div(
            class = "pt-4 text-end",
            shiny::actionButton(
              ns("prevItem"),
              label = "Vorheriges",
              icon = shiny::icon("arrow-up"),
              class = "btn-outline-secondary"
            )
          ),
          # Next button
          div(
            class = "pt-4",
            shiny::actionButton(
              ns("nextItem"),
              label = "Naechstes",
              icon = shiny::icon("arrow-down"),
              class = "btn-outline-secondary"
            )
          )
        )
      ),
      min_height = "200px",
      max_height = "250px",
      fill = FALSE
    ),
    bslib::card(
      bslib::card_body(
        # Annotation row
        bslib::layout_columns(
          col_widths = c(3, 3, 6),
          col_widths_sm = c(6, 6, 12),
          # Category selection
          div(
            shiny::selectizeInput(
              ns("annCatSkip"),
              "Kategorie waehlen",
              choices = NULL
            ),
            shiny::actionButton(
              ns("skipToAnn"),
              label = "Zur Kategorie",
              icon = shiny::icon("backward-fast"),
              class = "btn-sm btn-outline-primary"
            )
          ),
          # Current category
          div(
            shiny::selectizeInput(
              ns("annCat"),
              "Aktuelle Kategorie",
              choices = NULL
            ),
            # Category navigation
            div(
              class = "d-flex gap-1",
              shiny::actionButton(
                ns("prevAnnCat"),
                label = "",
                icon = shiny::icon("arrow-left"),
                class = "btn-sm btn-outline-secondary"
              ),
              shiny::actionButton(
                ns("nextAnnCat"),
                label = "",
                icon = shiny::icon("arrow-right"),
                class = "btn-sm btn-outline-secondary"
              )
            )
          ),
          # Annotation value and save buttons
          div(
            shiny::selectizeInput(
              ns("annValue"),
              "Annotation Wert",
              choices = NULL,
              selected = NULL,
              multiple = FALSE,
              width = "100%",
              options = list(
                create = TRUE,
                placeholder = "Wert eingeben oder auswaehlen..."
              )
            ),
            # Save buttons row
            div(
              class = "d-flex gap-1 flex-wrap",
              shiny::actionButton(
                ns("saveAnn"),
                label = "Speichern",
                icon = shiny::icon("save"),
                class = "btn-primary btn-sm"
              ),
              shiny::actionButton(
                ns("saveAndPrevItem"),
                label = "",
                icon = shiny::icon("arrow-up"),
                title = "Speichern + Vorheriges Item",
                class = "btn-success btn-sm"
              ),
              shiny::actionButton(
                ns("saveAndNextItem"),
                label = "",
                icon = shiny::icon("arrow-down"),
                title = "Speichern + Naechstes Item",
                class = "btn-success btn-sm"
              ),
              shiny::actionButton(
                ns("saveAndPrevCat"),
                label = "",
                icon = shiny::icon("arrow-left"),
                title = "Speichern + Vorherige Kategorie",
                class = "btn-info btn-sm"
              ),
              shiny::actionButton(
                ns("saveAndNextCat"),
                label = "",
                icon = shiny::icon("arrow-right"),
                title = "Speichern + Naechste Kategorie",
                class = "btn-info btn-sm"
              ),
              shiny::checkboxInput(
                ns("resetCatOnItemChange"),
                label = "Reset Kat.",
                value = TRUE,
                width = "auto"
              )
            )
          )
        )
      ),
      min_height = "300px",
      max_height = "450px",
      fill = FALSE
    )
  )
}


# Annotation Server -------------------------------------------------------
#' @keywords internal
annotateServer <- function(
  id,
  con,

  sample,
  categoryOrder = NULL,
  categorySet = NULL
) {
  stopifnot(DBI::dbIsValid(con))
  stopifnot(is.reactive(sample))

  moduleServer(id, function(input, output, session) {
    # Cache ALL category IDs for the sample (always full set)
    allCategoryIds <- reactive({
      req(sample())
      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT ac.id, ac.name, sc.category_order
           FROM annotation_category ac
           JOIN subsample_category sc ON ac.id = sc.category_id
           JOIN subsample s ON sc.subsample_id = s.id
           WHERE s.name = {sample()}
           ORDER BY sc.category_order;",
          .con = con
        )
      )
    }) |>
      bindEvent(sample())

    # Effective category IDs: filtered by active set if one is selected
    categoryIds <- reactive({
      req(allCategoryIds())

      if (!is.null(categorySet) && !is.null(categorySet$activeCategoryIds())) {
        setCats <- categorySet$activeCategoryIds()
        if (nrow(setCats) > 0) {
          return(
            setCats |>
              mutate(category_order = row_number())
          )
        }
      }

      allCategoryIds()
    })

    # Get categories for current sample (derived from effective IDs)
    categories <- reactive({
      req(categoryIds())

      cats <- categoryIds() |>
        mutate(categoryOrder = as.integer(category_order)) |>
        arrange(categoryOrder) |>
        pull(name)

      updateSelectizeInput(
        session,
        "annCatSkip",
        choices = cats,
        selected = cats[1],
        server = TRUE
      )

      cats
    }) |>
      bindEvent(categoryIds())

    # Get sample data (token IDs) - single optimized query
    sampleData <- reactive({
      req(sample())
      req(categories())

      data <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT t.id
           FROM token t
           JOIN subsample_token st ON t.id = st.token_id
           JOIN subsample s ON st.subsample_id = s.id
           WHERE s.name = {sample()}",
          .con = con
        )
      )

      # Order by category if specified
      if (!is.null(categoryOrder()) && categoryOrder() %in% categories()) {
        catId <- categoryIds() |> filter(name == categoryOrder()) |> pull(id)

        orderData <- DBI::dbGetQuery(
          con,
          glue::glue_sql(
            "SELECT token_id, value
             FROM annotation
             WHERE category_id = {catId}",
            .con = con
          )
        )

        data <- data |>
          dplyr::left_join(orderData, by = c("id" = "token_id")) |>
          mutate(order = case_when(is.na(value) ~ 0, TRUE ~ 1)) |>
          arrange(order, id)
      }

      data
    }) |>
      bindCache(c(sample(), categoryOrder())) |>
      bindEvent(c(sample(), categoryOrder()))

    # Cache all annotation values for all categories (fetch once per sample)
    allCategoryValues <- reactive({
      req(categoryIds())

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT DISTINCT a.value, ac.name as category_name
           FROM annotation a
           JOIN annotation_category ac ON a.category_id = ac.id
           WHERE ac.id IN ({categoryIds()$id*})
           AND a.value IS NOT NULL
           AND a.value != ''
           ORDER BY ac.name, a.value",
          .con = con
        )
      )
    }) |>
      bindEvent(categoryIds())

    # Pre-compute values per category as a named list
    categoryValuesList <- reactive({
      req(allCategoryValues())
      req(categories())

      allCategoryValues() |>
        split(~category_name) |>
        lapply(function(x) x$value)
    })

    # Get values for current category (fast lookup from pre-computed list)
    categoryValues <- reactive({
      req(categoryValuesList())
      req(categories())
      req(annCat())

      catName <- categories()[annCat()]
      categoryValuesList()[[catName]] %||% character(0)
    })

    # Current item index
    item <- reactiveVal(1)

    # Current annotation category index
    annCat <- reactiveVal(1)

    # Counter to track annotation saves (for cache invalidation)
    annotationVersion <- reactiveVal(0)

    # Pre-fetch all annotations for current sample tokens
    allAnnotations <- reactive({
      annotationVersion()
      req(sampleData())
      req(categoryIds())

      tokenIds <- sampleData()$id

      if (length(tokenIds) == 0) {
        return(data.frame())
      }

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT a.token_id, a.category_id, a.value, ac.name as category_name
           FROM annotation a
           JOIN annotation_category ac ON a.category_id = ac.id
           WHERE a.token_id IN ({tokenIds*})
           AND ac.id IN ({categoryIds()$id*})",
          .con = con
        )
      )
    })

    # Update category dropdown only when categories change
    shiny::observe({
      cat <- categories()
      updateSelectizeInput(
        session,
        "annCat",
        choices = cat,
        selected = cat[annCat()]
      )
    }) |>
      bindEvent(c(annCat(), categories()))

    # Update annotation value - critical path for item/category changes
    shiny::observe({
      req(sampleData())
      req(allAnnotations())
      req(categories())

      currentTokenId <- sampleData()$id[item()]
      currentCat <- categories()[annCat()]

      val <- allAnnotations() |>
        filter(token_id == currentTokenId, category_name == currentCat) |>
        pull(value)

      existingValues <- categoryValues()

      updateSelectizeInput(
        session,
        "annValue",
        choices = existingValues,
        selected = if (length(val) > 0) val[1] else ""
      )
    }) |>
      bindEvent(c(item(), annCat()))

    # Helper to reset category to annCatSkip if checkbox is checked
    resetCategoryIfNeeded <- function() {
      if (isTRUE(input$resetCatOnItemChange) && !is.null(input$annCatSkip)) {
        idx <- which(categories() == input$annCatSkip)
        if (length(idx) > 0) {
          annCat(idx[1])
        }
      }
    }

    # Navigation: next/previous item
    observeEvent(input$nextItem, {
      req(sampleData())
      if (item() < nrow(sampleData())) {
        item(item() + 1)
        resetCategoryIfNeeded()
      }
    })

    observeEvent(input$prevItem, {
      if (item() > 1) {
        item(item() - 1)
        resetCategoryIfNeeded()
      }
    })

    # Jump to specific item number
    observeEvent(input$skipToItem, {
      req(sampleData())
      if (input$ItemNumber >= 1 && input$ItemNumber <= nrow(sampleData())) {
        item(input$ItemNumber)
      }
    })

    # Jump to specific item ID
    shiny::observe({
      req(input$skipItemID)
      row <- which(sampleData()$id == as.numeric(input$skipItemID))
      if (length(row) > 0) {
        item(row[1])
      }
    }) |>
      shiny::bindEvent(input$skipToItemID)

    observeEvent(input$nextAnnCat, {
      req(categories())
      if (annCat() < length(categories())) {
        annCat(annCat() + 1)
      }
    })

    observeEvent(input$prevAnnCat, {
      if (annCat() > 1) {
        annCat(annCat() - 1)
      }
    })

    observeEvent(input$skipToAnn, {
      annCat(which(categories() == input$annCatSkip))
    })

    # Save annotation when category changes (use cached category ID)
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)
    }) |>
      bindEvent(input$annCat, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Save annotation on button click
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)
    }) |>
      bindEvent(input$saveAnn)

    # Save and go to next item
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)

      if (item() < nrow(sampleData())) {
        item(item() + 1)
        resetCategoryIfNeeded()
      }
    }) |>
      bindEvent(input$saveAndNextItem)

    # Save and go to previous item
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)

      if (item() > 1) {
        item(item() - 1)
        resetCategoryIfNeeded()
      }
    }) |>
      bindEvent(input$saveAndPrevItem)

    # Save and go to next category
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)

      if (annCat() < length(categories())) {
        annCat(annCat() + 1)
      }
    }) |>
      bindEvent(input$saveAndNextCat)

    # Save and go to previous category
    observe({
      req(sampleData())
      req(categories())
      req(categoryIds())

      category_id <- categoryIds() |>
        filter(name == categories()[annCat()]) |>
        pull(id)

      DBI::dbExecute(
        con,
        glue::glue_sql(
          "INSERT INTO annotation (token_id, category_id, value)
           VALUES ({sampleData()$id[item()]}, {category_id}, {input$annValue})
           ON DUPLICATE KEY UPDATE value = {input$annValue}",
          .con = con
        )
      )
      annotationVersion(annotationVersion() + 1)

      if (annCat() > 1) {
        annCat(annCat() - 1)
      }
    }) |>
      bindEvent(input$saveAndPrevCat)

    # Handle bad audio checkbox
    observe({
      req(sampleData())

      badAudio <- if (input$badAudio) "TRUE" else "FALSE"

      DBI::dbBegin(con)
      DBI::dbExecute(
        con,
        glue::glue_sql(
          "UPDATE token SET badAudio = {badAudio} WHERE id = {sampleData()$id[item()]}",
          .con = con
        )
      )
      DBI::dbCommit(con)
    }) |>
      bindEvent(input$badAudio)

    # Calculate annotation progress based on categoryOrder category
    annotationProgress <- reactive({
      req(sample())
      req(sampleData())
      req(categoryIds())
      req(categoryOrder())
      annotationVersion()

      total <- nrow(sampleData())

      catId <- categoryIds() |>
        filter(name == categoryOrder()) |>
        pull(id)

      if (length(catId) == 0) {
        return(list(annotated = 0, total = total))
      }

      annotated <- DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT COUNT(DISTINCT a.token_id) as n
           FROM annotation a
           JOIN subsample_token st ON a.token_id = st.token_id
           JOIN subsample s ON st.subsample_id = s.id
           WHERE s.name = {sample()}
           AND a.category_id = {catId}
           AND a.value IS NOT NULL
           AND a.value != ''",
          .con = con
        )
      )$n

      list(annotated = annotated, total = total)
    })

    # Return values for parent module
    return(list(
      item = reactive(sampleData()$id[item()]),
      annCat = reactive(categories()[annCat()]),
      itemNumber = reactive(item()),
      annotationProgress = reactive(annotationProgress()),
      sampleData = reactive(sampleData()),
      annotationVersion = reactive(annotationVersion())
    ))
  })
}


# Transcript UI -----------------------------------------------------------
#' @keywords internal
transcriptUI <- function(id) {
  ns <- NS(id)

  bslib::layout_columns(
    col_widths = c(9, 3),
    col_widths_sm = c(12, 12),
    bslib::card(
      bslib::card_header("Transkript"),
      bslib::card_body(
        class = "overflow-auto p-2",
        DT::dataTableOutput(ns("transcript"))
      ),
      min_height = "500px",
      fill = TRUE
    ),
    bslib::card(
      bslib::card_header("Transkript abspielen"),
      bslib::card_body(
        class = "overflow-auto",
        numericInput(
          ns("rangeTranscript"),
          "Zeitbereich (\u00b1Sekunden)",
          value = 10,
          min = 0,
          max = 60,
          step = 5
        ),
        howler::howlerOutput(ns("audioTranscript")),
        howler::howlerSeekSlider(ns("audioTranscript")),
        howler::howlerSeekTime(ns("audioTranscript")),
        br(),
        div(
          class = "d-flex gap-2",
          howler::howlerPlayPauseButton(ns("audioTranscript")),
          howler::howlerStopButton(ns("audioTranscript"))
        ),
        hr(),
        shiny::sliderInput(
          ns("sliderTranscript"),
          "Ausschnitt waehlen",
          min = 0,
          max = 1,
          value = c(0.1, 0.8),
          step = 0.01
        )
      ),
      min_height = "500px",
      fill = FALSE
    )
  )
}

# Transcript Server -------------------------------------------------------
#' @keywords internal
transcriptServer <- function(id, con, item_id, dirAudio, audioTempDir) {
  stopifnot(is.reactive(item_id))

  moduleServer(id, function(input, output, session) {
    # Cache file info for current item
    fileInfo <- reactive({
      req(item_id())

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT t.file_id, t.IP_id, f.audio_file
           FROM token t
           JOIN files f ON t.file_id = f.id
           WHERE t.id = {item_id()}",
          .con = con
        )
      )
    }) |>
      bindCache(item_id())

    # Track current file_id to avoid unnecessary transcript reloads
    currentFileId <- reactiveVal(NULL)

    observe({
      req(fileInfo())
      newFileId <- fileInfo()$file_id
      if (is.null(currentFileId()) || currentFileId() != newFileId) {
        currentFileId(newFileId)
      }
    })

    # Get transcript data - only refetch when file_id actually changes
    transcript <- reactive({
      req(currentFileId())

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT
             t.id,
             t.IP_id,
             t.event_text,
             t.speaker_id,
             s.name as speaker_name,
             ts.timevalue as timevalue_start,
             te.timevalue as timevalue_end
           FROM token t
           JOIN speaker s ON t.speaker_id = s.id
           JOIN timeline ts ON t.start_timestamp_id = ts.id
           JOIN timeline te ON t.end_timestamp_id = te.id
           WHERE t.file_id = {currentFileId()}",
          .con = con
        )
      ) |>
        mutate(
          timevalue_start = round(timevalue_start, 2),
          timevalue_end = round(timevalue_end, 2)
        ) |>
        summarise(
          speaker_name = first(speaker_name),
          event_text = paste(event_text, collapse = " "),
          timevalue_start = min(timevalue_start),
          timevalue_end = max(timevalue_end),
          .by = IP_id
        ) |>
        dplyr::arrange(timevalue_start)
    }) |>
      bindCache(currentFileId()) |>
      bindEvent(currentFileId())

    # Filter transcript by time range
    filteredTranscript <- reactive({
      req(transcript())
      req(fileInfo())

      currentIP <- fileInfo()$IP_id

      ipTimes <- transcript() |>
        filter(IP_id == currentIP) |>
        summarise(
          start_time = min(timevalue_start),
          end_time = max(timevalue_end)
        )

      transcript() |>
        dplyr::filter(
          timevalue_start >= (ipTimes$start_time - input$rangeTranscript) &
            timevalue_end <= (ipTimes$end_time + input$rangeTranscript)
        ) |>
        dplyr::select(
          IP_id,
          name = speaker_name,
          event_text,
          timevalue_start,
          timevalue_end
        )
    })

    # Render transcript table
    output$transcript <- DT::renderDataTable(
      {
        req(filteredTranscript())
        filteredTranscript() |>
          select(-any_of(c("timevalue_start", "timevalue_end")))
      },
      server = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        ordering = FALSE,
        searching = FALSE
      )
    )

    # Update slider when filtered transcript changes
    observe({
      req(filteredTranscript())
      req(nrow(filteredTranscript()) > 0)

      minTime <- min(filteredTranscript()$timevalue_start) - 0.5
      maxTime <- max(filteredTranscript()$timevalue_end) + 0.5

      updateSliderInput(
        session,
        "sliderTranscript",
        min = minTime,
        max = maxTime,
        value = c(minTime + 0.5, maxTime - 0.5),
        step = 0.05
      )
    }) |>
      bindEvent(fileInfo()$IP_id, input$rangeTranscript)

    # Track when we need to regenerate audio
    audioTrigger <- reactive({
      req(input$sliderTranscript)
      req(fileInfo())
      list(
        file = fileInfo()$audio_file,
        ip = fileInfo()$IP_id,
        start = input$sliderTranscript[1],
        end = input$sliderTranscript[2]
      )
    })

    # Create audio file for transcript playback
    nameTranscript <- reactive({
      req(audioTrigger())
      trigger <- audioTrigger()

      soundTr <- paste0(dirAudio, trigger$file) |>
        tuneR::readWave(
          from = trigger$start,
          to = trigger$end,
          units = "seconds"
        )

      sT <- phonTools::makesound(
        soundTr@left,
        filename = "tmpTranscript",
        fs = soundTr@samp.rate
      )

      fileName <- paste0("tmpTranscript", sample(1000:2000, 1), ".wav")
      absolutePath <- file.path(audioTempDir, fileName)
      relativeName <- paste0("audio/", fileName)

      phonTools::writesound(sT, filename = absolutePath)
      relativeName
    }) |>
      bindEvent(
        audioTrigger(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    # Reset audio when file changes
    observe({
      howler::stopHowl("audioTranscript")
      howler::seekHowl("audioTranscript", 0)
    }) |>
      bindEvent(nameTranscript())

    # Render audio player
    observe({
      output$audioTranscript <- howler::renderHowler(
        howler::howler(c(sound = nameTranscript()))
      )
    }) |>
      bindEvent(nameTranscript(), ignoreNULL = TRUE)
  })
}
