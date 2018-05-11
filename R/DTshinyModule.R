#' DTmoduleUI
#'
#' @param id 
#' @param ... 
#'
#' @export
#'
DTmoduleUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("content"))
}

#' DTmodule
#'
#' @param input 
#' @param output 
#' @param session 
#' @param data 
#' @param checkbox.colname 
#' @param checked.rows.id.prefix 
#' @param header 
#'
#' @return reactiveValues
#' @export
#' 
#' @importFrom data.table data.table %like%
#' @importFrom futile.logger flog.debug
#' @importFrom DT renderDataTable datatable dataTableOutput
DTmodule <- function(input,
                     output,
                     session,
                     data,
                     checkbox.colname = "Select",
                     checked.rows.id.prefix = "Row",
                     header = NULL) {
  
  stopifnot(!is.null(data) && is.data.frame(data))
  
  state <- reactiveValues(data = data.table::data.table(data),
                          checked.rows = NULL,
                          checkbox.colname = checkbox.colname,
                          checked.rows.id.prefix = checked.rows.id.prefix,
                          table.id = session$ns("mainTable"))
  
  output$content <- renderUI({
    fluidPage(
      box(width = 12,
          if (!is.null(header) && nchar(header) > 0) {
            tagList(
              h3(strong(header), align = "center"),
              hr()
            )
          },
          column(6,offset = 6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                 actionButton(inputId = session$ns("addRowHead"), label = "Add a new row"),
                 actionButton(inputId = session$ns("delRowHead"), label = "Delete selected rows"),
                 HTML('</div>')
          ),
          column(12, DT::dataTableOutput(session$ns("mainTable"))),
          tags$script(HTML(sprintf('
                                   $(document).on("click", "input", function () {
                                   var checkboxes = document.getElementsByName("row_selected");
                                   var checkboxesChecked = [];
                                   for (var i=0; i < checkboxes.length; i++) {
                                   if (checkboxes[i].checked) {
                                   checkboxesChecked.push(checkboxes[i].value);
                                   }
                                   }
                                   Shiny.onInputChange("%s", checkboxesChecked);
                                   })', session$ns("checkedRows")))),
          tags$script(sprintf("
                              $(document).on('click', '#%s button', function () {
                              Shiny.onInputChange('%s',this.id);
                              Shiny.onInputChange('%s', Math.random())
                              });", session$ns("mainTable"), session$ns("lastClickId"), session$ns("lastClick")))
      )
          )
    })
  
  output$mainTable <- DT::renderDataTable({
    DT <- state$data
    DT[[checkbox.colname]] <-
      sprintf('<input type="checkbox" name = "row_selected" value = "%s"><br>',
              paste0(checked.rows.id.prefix, 1:nrow(DT)))
    
    DT[["Actions"]] <- paste0(
      '
      <div class="btn-group" role="group" aria-label="Basic example">
      <button type="button" class="btn btn-secondary delete" id=delete_',
      1:nrow(state$data),
      '>Delete</button>
      <button type="button" class="btn btn-secondary modify"id=modify_',
      1:nrow(state$data),
      '>Modify</button>
      </div>
      '
    )
    
    DT::datatable(DT, escape = FALSE, editable = TRUE)
  })
  
  observeEvent(input$addRowHead,{
    new.row <- state$data[nrow(state$data) + 1, ]
    flog.debug("Adding new row")
    state$data <- rbind(new.row, state$data)
  })
  
  observeEvent(input$delRowHead, {
    
    if (!shiny::isTruthy(input$checkedRows)) {
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        tags$span("First select rows to delete by checkbox."),
        size = "s"
      ))
    }
    
    checked.rows <- req(input$checkedRows)
    row.to.del <- as.numeric(gsub(checked.rows.id.prefix, "", checked.rows))
    flog.debug(sprintf("Removing rows: %s", 
                       paste0(checked.rows, collapse = ",")))
    state$data <- state$data[-row.to.del]
  })
  
  observe({
    state$checked.rows <- req(input$checkedRows)
    futile.logger::flog.debug(
      sprintf("Checked rows: %s", paste0(state$checked.rows, collapse = ", ")))
  })
  
  observeEvent(input$lastClick, {
    if (input$lastClickId %like% "delete") {
      row.to.del = as.numeric(gsub("delete_", "", input$lastClickId))
      flog.debug(sprintf("Deleting row: %s", 
                         paste0(row.to.del, collapse = ",")))
      state$data = state$data[-row.to.del]
    } else if (input$lastClickId %like% "modify") {
      showModal(modal_modify)
    }
  })
  
  return(state)
  }