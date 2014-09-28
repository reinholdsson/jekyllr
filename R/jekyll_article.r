jekyll_article <- function (
  variant = "markdown_github",
  preserve_yaml = T, 
  toc = FALSE,
  toc_depth = 3,
  fig_width = 7,
  fig_height = 5, 
  fig_retina = NULL,
  includes = NULL,
  pandoc_args = NULL
){
  args <- c("--standalone")
  args <- c(args, rmarkdown:::pandoc_toc_args(toc, toc_depth))
  args <- c(args, rmarkdown:::includes_to_pandoc_args(includes))
  args <- c(args, pandoc_args)
  if (preserve_yaml) {
    post_processor <- function(metadata, input_file, output_file, 
                               clean, verbose) {
      input_lines <- readLines(input_file, warn = FALSE)
      partitioned <- rmarkdown:::partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter, "", 
                          readLines(output_file, warn = FALSE))
        
        # Remove rmd yml header (ugly solution!)
        output_lines <- output_lines[-(1:3)]
        
        # Fix markdown to work with jekyll theme
        output_lines <- gsub(" {.r}", "{r}", output_lines, perl = T)
        
        # Add yml header
        header_yml <- readLines('header.yml', warn = FALSE)
        output_lines <- c("---", header_yml, "---", "", output_lines)
        
        writeLines(output_lines, output_file, useBytes = TRUE)
      }
      output_file
    }
  }
  else {
    post_processor <- NULL
  }
  
  myknit_opts <- rmarkdown:::knitr_options(opts_chunk = list(fig.path='../../images/', dev = 'CairoPNG', dpi = 720, echo = T))
  
  rmarkdown:::output_format(
    knitr = myknit_opts,
    pandoc = rmarkdown:::pandoc_options(
      to = variant,
      from = rmarkdown:::from_rmarkdown(),
      args = args
    ),
    clean_supporting = FALSE,
    post_processor = post_processor
  )
}
