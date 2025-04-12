# --- R Shiny App: HTML to PDF Converter (v2) ---

# 1. Check and Install Required Packages
if (!(require("shiny"))){install.packages("shiny")}
if (!(require("pagedown"))){install.packages("pagedown")}
if (!(require("tools"))){install.packages("tools")}

# Load Packages
library(shiny)
library(pagedown)
library(tools) # For file_path_sans_ext

# Increase max file upload size (e.g., to 30MB)
options(shiny.maxRequestSize=30*1024^2)

# --- User Interface (UI) ---
ui <- fluidPage(
    titlePanel("HTML to PDF Converter with Preview"),
    
    sidebarLayout(
        sidebarPanel(
            width = 3, # Make sidebar slightly narrower
            h4("Instructions:"),
            tags$ol(
                tags$li("Upload your HTML file (.html)."),
                tags$li("Preview the HTML in the 'HTML Preview' tab (optional)."),
                tags$li("Wait for PDF conversion (status updates below)."),
                tags$li("Click 'Download PDF' when ready.")
            ),
            hr(),
            fileInput("htmlfile", "1. Upload HTML File:",
                      multiple = FALSE,
                      accept = c("text/html", ".html")),
            hr(),
            h4("Status:"),
            verbatimTextOutput("status"),
            hr(),
            # Dynamic UI for the download button
            uiOutput("downloadBtnUI"),
            hr(),
            tags$small("Note: PDF conversion requires Google Chrome or Chromium installed.")
        ),
        
        mainPanel(
            width = 9, # Make main panel wider
            tabsetPanel(
                type = "tabs",
                tabPanel("App Information",
                         h4("App Information"),
                         p("This application uses the 'pagedown' R package to convert an uploaded HTML file into a PDF document, adding a basic header and page numbers."),
                         p("The quality of the conversion depends on the complexity of the HTML, CSS, and JavaScript within the source file. The app leverages the printing capabilities of Google Chrome/Chromium."),
                         p("You can preview the uploaded HTML structure in the 'HTML Preview' tab before downloading the PDF.")
                ),
                tabPanel("HTML Preview",
                         h4("Preview of Uploaded HTML"),
                         p("Rendering may differ slightly from a standalone browser."),
                         # Use uiOutput for dynamic iframe rendering
                         uiOutput("htmlPreviewFrame")
                )
                # Tab for PDF Preview could be added here if implemented later
                # tabPanel("PDF Preview", ...)
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive values to store state
    status_message <- reactiveVal("Please upload an HTML file.")
    pdf_output_path <- reactiveVal(NULL)
    html_input_path <- reactiveVal(NULL) # Store path for preview
    download_ready <- reactiveVal(FALSE)
    base_filename <- reactiveVal("converted_document") # Default filename base
    
    # --- Display Status ---
    output$status <- renderText({
        status_message()
    })
    
    # --- Handle File Upload ---
    observeEvent(input$htmlfile, {
        inFile <- input$htmlfile
        if (is.null(inFile)) {
            status_message("File input cleared.")
            download_ready(FALSE)
            pdf_output_path(NULL)
            html_input_path(NULL)
            return(NULL)
        }
        
        # Reset state
        status_message("HTML file uploaded. Ready for Preview or Conversion.")
        download_ready(FALSE)
        pdf_output_path(NULL)
        html_input_path(inFile$datapath) # Store path for preview iframe
        base_filename(tools::file_path_sans_ext(inFile$name)) # Use input filename base
        
        # Trigger the conversion automatically on upload for simplicity,
        # or add a button later if manual trigger is preferred.
        convert_to_pdf(inFile$datapath, base_filename())
        
    })
    
    # --- Function for PDF Conversion ---
    convert_to_pdf <- function(html_path, filename_base) {
        status_message("Starting PDF conversion process...")
        download_ready(FALSE) # Disable download during conversion
        
        tempPdfPath <- tempfile(pattern = filename_base, fileext = ".pdf")
        
        # Check for Chrome/Chromium
        chrome_path <- tryCatch(pagedown::find_chrome(), error = function(e) NULL)
        if (is.null(chrome_path)) {
            status_message("Error: Google Chrome/Chromium not found. Conversion aborted.")
            return(NULL)
        }
        status_message(paste("Found Chrome/Chromium.\nAttempting conversion to:", basename(tempPdfPath)))
        
        # --- Define Header and Footer Templates ---
        # Simple header with filename
        header_template <- paste0("<div style='font-size: 9px; width: 100%; text-align: center;'>",
                                  "Document: ", htmltools::htmlEscape(filename_base), ".html", # Escape filename
                                  "</div>")
        
        # Footer with page numbers
        footer_template <- paste0("<div style='font-size: 9px; width: 100%; text-align: center;'>",
                                  "Page <span class='pageNumber'></span> of <span class='totalPages'></span>",
                                  "</div>")
        
        # Perform conversion
        conversion_result <- tryCatch({
            pagedown::chrome_print(
                input = html_path,
                output = tempPdfPath,
                wait = 2,           # Adjust if needed for complex pages
                verbose = 0,
                options = list(
                    displayHeaderFooter = TRUE,
                    headerTemplate = header_template,
                    footerTemplate = footer_template,
                    marginTop = "1cm",    # Margin for header
                    marginBottom = "1cm", # Margin for footer
                    marginLeft = "1cm",
                    marginRight = "1cm",
                    printBackground = TRUE # Often needed to render background colors/images
                )
            )
            TRUE # Success
        }, error = function(e) {
            status_message(paste("Error during PDF conversion:", e$message))
            FALSE # Failure
        })
        
        # Update status and state
        if (conversion_result) {
            status_message("Conversion successful! Click Download PDF.")
            pdf_output_path(tempPdfPath)
            download_ready(TRUE)
        } else {
            pdf_output_path(NULL)
            download_ready(FALSE)
        }
    }
    
    # --- Render HTML Preview Frame ---
    output$htmlPreviewFrame <- renderUI({
        req(html_input_path()) # Require html_input_path to be set
        
        # Use session$fileUrl for safer access to temp files
        # CORRECTED: Use 'name' argument instead of 'key'
        preview_url <- session$fileUrl(
            name = basename(html_input_path()), # Use file name as unique identifier
            file = html_input_path(),
            contentType = 'text/html'
        )
        
        tags$iframe(
            src = preview_url,
            width = '100%',
            height = '700px', # Adjust height as needed
            style = "border: 1px solid #ddd;"
        )
    })
    
    # --- Dynamically Generate Download Button ---
    output$downloadBtnUI <- renderUI({
        if(download_ready()) {
            downloadButton("downloadPdf", "2. Download PDF")
        } else {
            # Optionally show a disabled button or placeholder
            tags$button("Download PDF", disabled = NA, class = "btn btn-default shiny-download-link") # Disabled look
        }
    })
    
    # --- PDF Download Handler ---
    output$downloadPdf <- downloadHandler(
        filename = function() {
            paste0(base_filename(), "_converted.pdf")
        },
        content = function(file) {
            req(pdf_output_path(), file.exists(pdf_output_path())) # Ensure file exists
            file.copy(pdf_output_path(), file, overwrite = TRUE)
        },
        contentType = "application/pdf"
    )
    
} # End Server


# --- Run the Application ---
shinyApp(ui = ui, server = server)
