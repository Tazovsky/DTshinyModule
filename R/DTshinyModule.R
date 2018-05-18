#' DTmoduleUI
#'
#' @param id 
#' @param ... 
#'
#' @export
#' @rdname DTmodule
#' @author Kamil Foltyński
DTmoduleUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(shinyjs::useShinyjs(),
          uiOutput(ns("content")))
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
#' @param add.row 
#' @param delete.row 
#' @param actions.colname 
#' @param is.checked.rule 
#'
#' @return reactiveValues
#' @export
#' @rdname DTmodule
#' @author Kamil Foltyński
#' @importFrom data.table data.table %like%
#' @importFrom futile.logger flog.debug flog.threshold
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom shiny isTruthy
#' @importFrom  dplyr mutate if_else row_number
DTmodule <- function(input,
                     output,
                     session,
                     data,
                     checkbox.colname = "Select",
                     actions.colname = "Actions",
                     checked.rows.id.prefix = "Row",
                     header = NULL,
                     add.row = TRUE,
                     delete.row = TRUE,
                     is.checked.rule = NULL) {
  
  
  stopifnot(!is.null(is.checked.rule) && class(is.checked.rule) == "call")
  stopifnot(!is.null(data) && is.data.frame(data))
  
  state <- reactiveValues(data = data.table::data.table(data),
                          checked.rows = NULL,
                          checkbox.colname = checkbox.colname,
                          checked.rows.id.prefix = checked.rows.id.prefix,
                          table.id = session$ns("mainTable"))
  
  output$content <- renderUI({
    fluidPage(
      shinyjs::useShinyjs(),
      box(width = 12,
          if (!is.null(header) && nchar(header) > 0) {
            tagList(
              h3(strong(header), align = "center"),
              hr()
            )
          },
          # if (any(isTRUE(c(add.row, delete.row)))) {
          column(6, offset = 6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                 if (add.row) actionButton(inputId = session$ns("addRowHead"), label = "Add a new row"),
                 if (delete.row) actionButton(inputId = session$ns("delRowHead"), label = "Delete selected rows"),
                 HTML('</div>')
          ),
          #},
          column(12, DT::dataTableOutput(session$ns("mainTable"))),
          tags$script(HTML(sprintf('
                                   $(document).on("click", "input", function() {
                                   var checkboxes = document.getElementsByName("row_selected");
                                   var checkboxesChecked = [];
                                   for (var i = 0; i < checkboxes.length; i++) {
                                     if (checkboxes[i].checked) {
                                        checkboxesChecked.push(checkboxes[i].value);
                                     } else {
                                        checkboxesChecked.push("-" + checkboxes[i].value);
                                     }
                                   }
                                   // return last selections in quantity of all checkboxes
                                   Shiny.onInputChange("%s",
                                   checkboxesChecked.slice(checkboxesChecked.length - checkboxes.length, 
                                   checkboxesChecked.length));
                                   })
                                   ', session$ns("checkedRows")))),
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
    
    if (shiny::isTruthy(checkbox.colname)) {
      
      if (is.null(is.checked.rule)) {
        DT[[checkbox.colname]] <-
          getCheckboxTag(FALSE, 1:nrow(DT), checked.rows.id.prefix)
      } else {
        DT <- DT %>%
          base::as.data.frame() %>%
          mutate(!!checkbox.colname := if_else(
            eval(is.checked.rule),
            getCheckboxTag(TRUE, row_number(), checked.rows.id.prefix),
            getCheckboxTag(FALSE, row_number(), checked.rows.id.prefix)
          )) %>%
          data.table::data.table()
      }
    }
    
    if (shiny::isTruthy(actions.colname))
      DT[[actions.colname]] <- paste0(
        '
        <div class="btn-group" role="group" aria-label="Basic example">
        <button type="button" class="btn btn-secondary delete" id=delete_',
        1:nrow(state$data),
        '>Delete</button>
        </div>
        ')
    
    DT::datatable(
      DT,
      escape = FALSE,
      editable = TRUE,
      options = list(pageLength = nrow(DT))
    )
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


#' getCheckboxTag
#' 
#' Returns HTML checkbox tag
#' 
#' @param checked logical
#' @param rnum row number
#' @param checked.rows.id.prefix string
#'
#' @return string
#' @export
#'
getCheckboxTag <- function(checked = FALSE, rnum, checked.rows.id.prefix) {
  if (missing(checked.rows.id.prefix))
    stop("Missing argument 'checked.rows.id.prefix'")
  
  tag.code <- '<input type="checkbox" name = "row_selected" value = "%s" %s><br>'
  
  if (checked)
    sprintf(tag.code, paste0(checked.rows.id.prefix, rnum), "checked")
  else
    sprintf(tag.code, paste0(checked.rows.id.prefix, rnum), "")
}