library(DBI)
library(jsonlite)
library(pool)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = 'ffdb_db'
)
onStop(function() {
  poolClose(pool)
})

getDocumentNames <- function (con, template_name='dlmtool') {
    res <- dbSendQuery(con, paste0(
        "SELECT DISTINCT document_name",
        " FROM document",
        " WHERE template_name = '", template_name, "'",
        " ORDER BY document_name",
        ""))
    out <- dbFetch(res)
    dbClearResult(res)
    return(out[,1])
}

getDLMToolData <- function (con, document_name) {
    res <- dbSendQuery(con, paste0(
        "SELECT content",
        " FROM document",
        " WHERE template_name = 'dlmtool'",
        " AND document_name = '", document_name, "'",
        " ORDER BY version DESC",
        " LIMIT 1",
        ""))
    out <- dbFetch(res)
    dbClearResult(res)
    out <- fromJSON(out$content)
    return(out)
}

# Convert ffdb list with _headings into a regular data frame
ffdbToDataFrame <- function (l) {
    as.data.frame(sapply(
        l[['_headings']]$fields,
        function (x) l[[x]],
        USE.NAMES = TRUE
    ), row.names=l[['_headings']]$values, stringsAsFactors = FALSE)
}


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  conn <- poolCheckout(pool)
  updateSelectInput(session, "document_name", choices = getDocumentNames(conn))
  poolReturn(conn)

  output$catchPlot <- renderPlot({
    if (nchar(input$document_name) == 0) {
        return()
    }

    conn <- poolCheckout(pool)
    out <- getDLMToolData(conn, input$document_name)
    poolReturn(conn)
    catch <- ffdbToDataFrame(out$catch)

    barplot(
        as.numeric(catch$catch),
        names.arg = rownames(catch),
    )
  })
}

# conn <- poolCheckout(pool)
# getDocumentNames(conn)
# doc <- getDLMToolData(conn, getDocumentNames(conn)[[1]])
# catch <- ffdbToDataFrame(doc$catch)
# poolClose(pool)
