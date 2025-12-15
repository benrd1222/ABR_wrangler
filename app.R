library(shiny)
library(bslib)
library(fs)
library(shinyFiles)

script_path <- "ABR_wrangler.R"

ui <- fluidPage(
  input_dark_mode(id = "mode"),
  theme = bs_theme(bootswatch = "darkly", primary = "#00bc8c"),
  titlePanel("ABR Data Wrangler"),
  card(
    card_header("Project Location"),

    shinyDirButton(
      "dir",
      "Select Project Folder",
      "Choose your project folder"
    ),
    verbatimTextOutput("dir_path_display", placeholder = TRUE),
  ),
  card(
    card_header("Run the Wrangler"),
    uiOutput("run_script_button_ui")
  ),
  card(
    card_header("Execution Logs"),
    verbatimTextOutput("execution_output")
  )
)

server <- function(input, output, session) {
  roots <- c(Home = fs::path_home(), getVolumes()())

  shinyDirChoose(input, "dir", roots = roots, session = session)

  selected_path <- reactive({
    # Use isolate or observeEvent if needed, but for now keep it simple
    if (is.null(input$dir) || length(input$dir) == 0) {
      return("")
    }
    path <- parseDirPath(roots, input$dir)

    # Check if a path was actually selected (length > 0)
    if (length(path) == 0) {
      return("")
    }

    return(path)
  })

  output$dir_path_display <- renderText({
    if (length(selected_path()) == 0) {
      "No directory selected"
    } else {
      selected_path()
    }
  })

  # Run Button Logic ----
  # Only show the button if a project path is provided
  output$run_script_button_ui <- renderUI({
    project_path_set <- length(selected_path()) > 0

    # Check if the hardcoded script file exists in the current directory
    script_exists <- file.exists(script_path)

    if (project_path_set && script_exists) {
      actionButton(
        "run_script",
        paste("Run", script_path),
        class = "btn-primary btn-lg",
        width = "100%"
      )
    } else {
      # Provide feedback on what's missing
      status_msg <- "Status: Missing required inputs."
      if (!project_path_set) {
        status_msg <- paste(status_msg, "Select Project Directory.")
      }
      if (!script_exists) {
        status_msg <- paste(
          status_msg,
          "Place",
          script_path,
          "in the app directory."
        )
      }

      div(style = "color: orange;", status_msg)
    }
  })

  # Execution Logic ----
  execution_status <- eventReactive(input$run_script, {
    current_path <- selected_path()
    if (current_path == "") {
      return(paste(
        "ERROR: Cannot run. Project directory selection failed or was canceled."
      ))
    }

    log_messages <- character()
    log_messages <- c(
      log_messages,
      paste("--- STARTING EXECUTION:", Sys.time(), "---")
    )

    # Write Config File
    config_list <- list(
      project_root = current_path
      # add extra parameters to the yaml in the future
    )

    # alternative to have the config saved at the projdir, but that would require
    # reworking the other script
    # config_path <- file.path(current_path, "config.yaml")
    config_path <- "config.yaml"

    tryCatch(
      {
        yaml::write_yaml(config_list, file = config_path)
        log_messages <- c(
          log_messages,
          paste0("Config written to: ", config_path)
        )
      },
      error = function(e) {
        log_messages <- c(
          log_messages,
          paste0("ERROR writing config: ", e$message)
        )
        return(paste(log_messages, collapse = "\n")) # Stop execution
      }
    )

    # Run the external R script using system2()
    log_messages <- c(log_messages, "\n--- EXECUTING SCRIPT ---")

    # Use system2 to run the Rscript command
    # stdout/stderr are captured in results
    results <- system2(
      command = "Rscript",
      args = script_path,
      stdout = TRUE,
      stderr = TRUE
    )

    # Process the output
    exit_status <- attr(results, "status")

    if (is.null(exit_status) || exit_status == 0) {
      log_messages <- c(log_messages, "Script Execution Successful.")
    } else {
      log_messages <- c(
        log_messages,
        paste0("Script Failed (Exit Code: ", exit_status, ").")
      )
    }

    log_messages <- c(log_messages, "\n--- Script Output (STDOUT/STDERR) ---")
    log_messages <- c(log_messages, results)
    log_messages <- c(
      log_messages,
      paste("\n--- END EXECUTION:", Sys.time(), "---")
    )

    return(paste(log_messages, collapse = "\n"))
  })

  output$execution_output <- renderText({
    execution_status()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
