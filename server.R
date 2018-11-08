library(DBI)
library(jsonlite)
library(pool)
library(ggplot2)
library(patchwork)


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
    # Convert list of all column (string) vectors to integer, if possible, handling NA's.
    column_names <- l[['_headings']]$fields
    out <- lapply(l[column_names], function (x) utils::type.convert(x, as.is = TRUE))

    # Turn this into a data.frame, put row headings in
    row_names <- l[['_headings']]$values
    out <- as.data.frame(out, row.names=row_names, stringsAsFactors = FALSE)

    return(out)
}


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Open DB pool
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = 'ffdb_db'
  )
  onStop(function() {
    poolClose(pool)
  })

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

    theme_set(theme_bw())
    catch$year <- rownames(catch)
    p1<-ggplot(catch, aes(year,catch)) +
        geom_bar(stat="identity") +
        ylab("Catch (in tonnes)") +
        theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(catch$year)
    p2<-ggplot(catch, aes(year,abundance_index_1)) +
        geom_bar(stat="identity") +
        ylab("Relative abundance") +
        theme(text = element_text(size=11), axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank()) +
        scale_x_discrete(catch$year)
    p1 + p2 +plot_layout(ncol = 1, heights = c(2, 2))
  })
}

# conn <- poolCheckout(pool)
# getDocumentNames(conn)
# doc <- getDLMToolData(conn, getDocumentNames(conn)[[1]])
# catch <- ffdbToDataFrame(doc$catch)
# poolClose(pool)
