#' transliterate Function
#'
#' This functions converts character vectors from one character set to another.
#' @param data Data into which the function is applied
#' @param model Path to the text file which contains the model for transliteration
#' @keywords ELAN linguistics
#' @export
#' @examples
#' transliterate("тест", "kpv-cyr2ipa")

transliterate <- function(data, model){

        multiple_replace <- function(pattern, replacement, x, ...){
          if (length(pattern)!=length(replacement)){
            stop("Pattern and replacement do not have the same length!")}
          result <- x
          for (i in 1:length(pattern)){
            result <- stringi::stri_replace_all(regex = pattern[i], replacement = replacement[i], str = result, ...)}
          result
        }

        pattern <- read.csv(file = model, sep = ",")
        cyr <- as.character(pattern[,1])
        lat <- as.character(pattern[,2])
        multiple_replace(cyr, lat, data)
}
