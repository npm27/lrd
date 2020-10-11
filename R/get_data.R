#' Get Dataset
#'
#' This function allows you to import the current datasets avaliable from
#' \href{https://github.com/orgs/SemanticPriming/}{Semantic Priming GitHub Group}.
#'
#' @param corpus Include a two letter code to download the Open Subtitles corpus for
#' text models. You can view the corpora on
#' \href{http://opus.nlpl.eu/OpenSubtitles.php}{their website}. Note: these files
#' can be very large, so they may take up a lot of memory to download. They are
#' text based files that are read using `readLines`.
#' @param bibtexID The bibtex ID of the dataset you are trying to load.
#' You can leave all parameters blank to load just the metadata.
#' @param citation Include the citation for the dataset you loaded - will only
#' load if you include a bibtex ID.
#' @param language If you include a bibtex ID, you will get back the language of
#' the dataset, if you do not include a bibtex ID, it will return a list of
#' datasets in that language.
#' @param variables If you include a bibtex ID, you will get back the variables
#' included the dataset, if you do not include a bibtex ID, it will return a list of
#' datasets that include that variable (can also be paired with language).
#' Use the column names from the metadata as your filter.
#' @return
#' \item{metadata}{The metadata list of avaliable datasets}
#' \item{loaded_data}{The dataset you requested to load}
#' \item{language}{The language of the dataset you requested to load}
#' \item{variables}{The variables of the dataset you requested to load}
#' \item{datasets}{Possible datasets based on your language and variable names}
#'
#' @keywords metadata, datasets, linguistic norms
#' @import utils
#' @export
#' @examples
#'
#' get_dataset()
#' get_dataset(bibtexID = "Birchenough2017", citation = TRUE)
#' get_dataset(language = "English", variables = c("aoa", "freq"))


get_dataset <- function(corpus = NULL,
                        bibtexID = NULL,
                        citation = NULL,
                        language = NULL,
                        variables = NULL
                        ) {

  metadata <- load_metadata()
  variable_return <- list(metadata = metadata)

  if (!is.null(corpus)){

    #data link
    #https://raw.githubusercontent.com/SemanticPriming/LAB-data/master/included_data.csv
    #http://opus.nlpl.eu/download.php?f=OpenSubtitles/v2018/mono/OpenSubtitles
con <- gzcon(url(paste(
"http://opus.nlpl.eu/download.php?f=OpenSubtitles/v2018/mono/OpenSubtitles.",
corpus, ".gz", sep="")))
      variable_return$subtitle <- readLines(con, encoding = "utf8")
  }

  if (!is.null(bibtexID)) {
    data_link <- metadata$link[metadata$bibtex == bibtexID]
    variable_return$loaded_data <- read.csv(url(data_link), stringsAsFactors = F)

    if (!is.null(citation)){

      variable_return$citation <- paste0(metadata$author[metadata$bibtex == bibtexID], ". (",
                        metadata$year[metadata$bibtex == bibtexID], "). ",
                        metadata$ref_title[metadata$bibtex == bibtexID], ". ",
                        metadata$ref_journal[metadata$bibtex == bibtexID], ", ",
                        metadata$ref_volume[metadata$bibtex == bibtexID], ", ",
                        metadata$ref_page[metadata$bibtex == bibtexID], ". doi: ",
                        metadata$ref_doi[metadata$bibtex == bibtexID]
      )
    }

    if (!is.null(language)){
      variable_return$language <- metadata$language[metadata$bibtex == bibtexID]
    }

    if (!is.null(variables)){
      temp <- metadata[metadata$bibtex == bibtexID, 26:ncol(metadata)]
      variable_return$variables <- colnames(temp)[temp == 1]
    }

  } else {

    if (!is.null(language) & !is.null(variables)) { #both

      temp <- metadata[ tolower(metadata$language) == tolower(language) , ]

      for (var in variables){
        if (var %in% colnames(metadata)){
          temp <- temp[ temp[ , var] == 1 , ]
          }
        }

      variable_return$datasets <- temp

    } else if (!is.null(language)){ #just language

      variable_return$datasets <- metadata[ tolower(metadata$language) == tolower(language) , ]

      } else if (!is.null(variables)){ #just variables

      temp <- metadata

      for (var in variables){
        if (var %in% colnames(metadata)){
          temp <- temp[ temp[ , var] == 1 , ]
        }
      }

      variable_return$datasets <- temp

    }

  }

  return(variable_return)
}

#' @rdname get_data
