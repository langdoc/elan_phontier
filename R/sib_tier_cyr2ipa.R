#' sib_tier_cyr2ipa Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' sib_tier_cyr2ipa(eaf_file = "test.eaf", participant = "JAI-M-1939", linguistic_type = "sib")

sib_tier_cyr2ipa <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20140404IgusevJA/kpv_izva20140404IgusevJA.eaf", participant = "JAI-M-1939", linguistic_type = "sib"){

        library(plyr)
        library(dplyr)

        `%>%` <- dplyr::`%>%`

        eaf_xml <- xml2::read_xml(eaf_file)

        dplyr::data_frame(
                content = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                        xml2::xml_text(),
                annot_id = eaf_xml %>%
                        xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                  participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_ID"),
                ref_id = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_REF"),
                ts1 = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF1"),
                ts2 = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF2"),
                participant = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("PARTICIPANT"),
                tier_id = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("TIER_ID"),
                type = eaf_xml %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                       participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("LINGUISTIC_TYPE_REF")) -> content


#        content %>% mutate(Content = paste0(Content, " / / ", reference, " / 0 / ", current_hash)) -> content

        # In the data frame content there is now the original content of the tier
        # We replace this with transliterated variant

        content %>% dplyr::mutate(content = paste0(elanphontier::transliterate(tolower(content), "R/ikdp2ipa.csv"), " /  / ")) -> content

        tier <- XML::newXMLNode("TIER", attrs = c(DEFAULT_LOCALE = "en",
                                                  LINGUISTIC_TYPE_REF = linguistic_type,
                                                  PARENT_REF = paste0("ref@", participant),
                                                  #                                                  LANG_REF = lang,
                                                  PARTICIPANT = participant,
                                                  TIER_ID = paste0("sib@", participant)))

        #        This adds annotation element under tier, so it already looks like this:
        #
        #        <TIER LINGUISTIC_TYPE_REF="sib" PARENT_REF="ref@JAI-M-1939" PARTICIPANT="JAI-M-1939" TIER_ID="sib@JAI-M-1939">
        #               <ANNOTATION/>
        #         </TIER>

        #       Next step is to populate that tier

        plyr::d_ply(content, .variables = "annot_id", function(x){
                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)
                alignable_annotation <- XML::newXMLNode("ALIGNABLE_ANNOTATION",
                                                        attrs = c(ANNOTATION_ID = x$annot_id,
                                                                  TIME_SLOT_REF1 = x$ts1,
                                                                  TIME_SLOT_REF2 = x$ts2),
                                                        parent = annotation)
                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", x$content, parent = alignable_annotation)
                alignable_annotation
        })


        doc <- XML::xmlParse(eaf_file)

        # Note: add speaker attribute so that we don't delete too many tiers!

        XML::removeNodes(doc[paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")])

        # doc
        eaf_to_be_written <- XML::getNodeSet(doc, "//ANNOTATION_DOCUMENT")

        XML::xmlChildren(eaf_to_be_written[[1]]) <- XML::addChildren(eaf_to_be_written[[1]], tier)

        XML::xmlChildren(eaf_to_be_written[[1]]) <- c(XML::xmlChildren(eaf_to_be_written[[1]]))[c(order(factor(names(eaf_to_be_written[[1]]),
                                                        levels = c("HEADER",
                                                                   "TIME_ORDER",
                                                                   "TIER",
                                                                   "LINGUISTIC_TYPE",
                                                                   "LOCALE",
                                                                   "LANGUAGE",
                                                                   "CONSTRAINT",
                                                                   "CONTROLLED_VOCABULARY",
                                                                   "EXTERNAL_REF"))))]


        XML::saveXML(eaf_to_be_written[[1]], eaf_file)

}
