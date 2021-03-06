library(unittest)

source('server.R')

ok_group("ffdbToDataFrame", {
  ffdb <- list(
      catch = c("NA", "200", "104.5"),
      abundance_index = c("", "100", "104.5"),
      some_string = c("cuthbert", "dibble", "grub"),
      "_headings" = list(
          fields = c('catch', 'abundance_index', 'some_string'),
          values = c('2000', '2001', '2002')
      )
  )
  ok(identical(ffdbToDataFrame(ffdb), data.frame(
      catch = c(NA, 200.0, 104.5),  # "NA" converted to NA
      abundance_index = c(NA, 100.0, 104.5),  # "" converted to NA
      some_string = c("cuthbert", "dibble", "grub"),  # Haven't converted string vectors into NA
  row.names = c('2000', '2001', '2002'), stringsAsFactors = FALSE)), "NA's converted correctly")
})
