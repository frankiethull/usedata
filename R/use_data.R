#' usedata::use_data calls usethis::use_data and usethis::use_r
#'
#' @param ... a single dataset you want to document
#' @param title a title to give for the doc script
#' @param description a description of your data for the doc script
#' @param source the source of the dataset
#' @param internal true or false passes to usethis::use_data
#' @param overwrite true or false passes to usethis::use_data
#' @param compress compression for usethis::use_data
#' @param version  version defaults to 3 for usethis::use_data
#' @param ascii  ascii true or false for usethis::use_data
#'
#' @returns saves off data in the pkg then creates and fills out a R script doc
#' @export
use_data <- \(...,
              title       = "dataset",
              description = "dataset of observations for something",
              source      = "data source comes from",
              internal = FALSE, overwrite = TRUE, compress = "bzip2", version = 3, ascii = FALSE){

  # get data frame name --------------------------------
  data_name <- usethis:::get_objs_from_dots(usethis:::dots(...))

  # save the data ---------------------------------------
  usethis::use_data(...,
                    internal  = internal,
                    overwrite = overwrite,
                    compress  = compress,
                    version   = version,
                    ascii     = ascii)

  # create the documentation script ---------------------
  usethis::use_r(name = data_name, open = FALSE)


  # make generic documentation --------------------------
  data_header <- paste0(
    '#\' ', title, '\n',
    '#\'\n',
    '#\' ', description, '\n',
    '#\'\n',
    '#\' @format ## `', data_name, '`\n',
    '#\' a data frame with ', nrow(...), ' rows and ', ncol(...), ' columns: \n ',
    "#' \\describe{\n"
  )

  # column descriptions
  col_names <- names(...)
  col_types <- sapply(..., \(x) class(x)[1])
  for (i in seq_along(col_names)) {
    data_header <- c(
      data_header,
      sprintf("#'   \\item{%s}{A %s column.}\n", col_names[i], col_types[i])
    )
  }


  data_footer <- paste0(
    '#\\}\n',
    '#\' @source ', source, '\n',
    '#\'\n',
    '"', data_name, '"', '\n'
  )

  data_doc <- c(data_header, data_footer)
  # data_doc |> cat()

  # write off initial documentation ---------------------

  r_file <- file.path("R", paste0(data_name, ".R"))
  writeLines(data_doc, r_file)

  message(sprintf("Data '%s' has been added to the package and documented in 'R/%s.R'.", data_name, data_name))
  message("Run 'devtools::document()' to generate the help files.")

}
