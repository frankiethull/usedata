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
#' @param use_llm an optional TRUE/FALSE argument to use an LLM to add additional detail to the data documentation
#' @param model a local ollama model to use for the LLM if LLM `use_llm` is set to TRUE
#'
#' @returns saves off data in the pkg then creates and fills out a R script doc
#' @export
use_data <- \(...,
              title       = "dataset",
              description = "dataset of observations for something",
              source      = "data source comes from",
              internal = FALSE, overwrite = TRUE, compress = "bzip2", version = 3, ascii = FALSE,
              use_llm = FALSE, model = "gemma3:4b"){

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

  # optional llm call statement -------------------------
   if(use_llm){
      data_doc <- use_llm(...      = ...,
                          data_doc = data_doc,
                          model    = model)
   }

  # write off initial documentation ---------------------
  r_file <- file.path("R", paste0(data_name, ".R"))
  writeLines(data_doc, r_file)

  message(sprintf("Data '%s' has been added to the package and documented in 'R/%s.R'.", data_name, data_name))
  message("Run 'devtools::document()' to generate the help files.")

}



# internal function for llm data documentation
use_llm <- \(...,
             data_doc,
             model){

  #data <- ...
  # data meta information for llm:
  data_str     <- capture.output(utils::str(...))
  data_summary <- capture.output(base::summary(...))
  data_head    <- capture.output(utils::head(...))
  dput_head    <- capture.output(dput(utils::head(...)))


  # create a chat instance for data documentation:
  chat <- ellmer::chat_ollama(system_prompt = "
                              you are an technical writing assistant, data analyzer, and
                              expert in data documentation for R packages.
                              you will be given context about a dataset and drafted documentation.
                              your job is to add additional insights from the context and
                              edit the documentation. **it is important that you adhere to the
                              documentation format and only return the edited documentation without backticks**.
                              ",
                              model = model)

  # ask the llm to edit the data_doc and return it:
  # prompt <-  paste0(
  #               "## drafted documentation: \n",
  #
  #                     data_doc, "\n\n\n",
  #
  #               "## additional context: \n",
  #
  #                    "data structure: \n",
  #                     paste(data_str, collapse = "\n"), "\n\n",
  #
  #                    "data summary: \n",
  #                     paste(data_summary, collapse = "\n"), "\n\n",
  #
  #                    "dput of data head: \n",
  #                     paste(dput_head, collapse = "\n")
  #                    )

  # shorten the context:
  prompt <-  paste0(
                "## drafted data documentation: \n",

                     data_doc, "\n\n\n",

                "## additional context for editing the documentation: \n",

                    "dput(head(dataset)): \n",
                     paste(dput_head, collapse = "\n")
                    )



  chat$chat(prompt)
}
