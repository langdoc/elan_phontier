#' eaf_translit_tier This function takes one tier by its name and applies selected transliteration into it.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' sib_tier_cyr2ipa(eaf_file = "test.eaf", participant = "JAI-M-1939", linguistic_type = "sib")

eaf_translit_tier <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva19591216-05582_2az/kpv_izva19591216-05582_2az.eaf", origin_tier = "phonet-UPA@MXV-F-1937", target_tier = "orth@MXV-F-1937", linguistic_type = "orthT", wanted_participant = "MXV-F-1937"){
        
        `%>%` <- dplyr::`%>%`
        
        file <- xml2::read_xml(eaf_file)
        
        dplyr::data_frame(
                Content = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                        xml2::xml_text(),
                annot_id = file %>%
                        xml2::xml_find_all(paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_ID"),
                ref_id = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("ANNOTATION_REF"),
                ts1 = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF1"),
                ts2 = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                        xml2::xml_attr("TIME_SLOT_REF2"),
                participant = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("PARTICIPANT"),
                tier_id = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("TIER_ID"),
                type = file %>%
                        xml2::xml_find_all(
                                paste0("//TIER[@TIER_ID='", origin_tier, "']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                        xml2::xml_attr("LINGUISTIC_TYPE_REF")) -> content_origin
       
        FRelan::read_tier(eaf_file, "orthT") %>% 
                dplyr::filter(participant == wanted_participant) -> content_target
        
        
        # In the data frame content there is now the original content of the tier
        # We replace this with transliterated variant
  #      content_origin %>% filter(ref_id == "a162")
        
        content_target$content <- content_origin %>% dplyr::mutate(transliterated = sle2016partanen::transliterate(tolower(Content), "R/evv-izva2ikdp.csv")) %>% 
                .$transliterated
        
        tier <- XML::newXMLNode("TIER", attrs = c(DEFAULT_LOCALE = "en",
                                                  LINGUISTIC_TYPE_REF = linguistic_type,
                                                  PARENT_REF = paste0("ref@", participant),
                                                  #                                                  LANG_REF = lang,
                                                  PARTICIPANT = participant,
                                                  TIER_ID = paste0("orth@", participant)))
        
        #        This adds annotation element under tier, so it already looks like this:
        #
        #        <TIER LINGUISTIC_TYPE_REF="sib" PARENT_REF="ref@JAI-M-1939" PARTICIPANT="JAI-M-1939" TIER_ID="sib@JAI-M-1939">
        #               <ANNOTATION/>
        #         </TIER>
        
        #       Next step is to populate that tier
        #       New id is necessary i
        content_target$id <- as.numeric(rownames(content_target))
        plyr::d_ply(content_target, .variables = "id", function(x){
                annotation <- XML::newXMLNode("ANNOTATION", parent = tier)
                alignable_annotation <- XML::newXMLNode("REF_ANNOTATION",
                                                        attrs = c(ANNOTATION_ID = x$annot_id,
                                                                  ANNOTATION_REF = x$ref_id),
                                                        parent = annotation)
                annotation_value <- XML::newXMLNode("ANNOTATION_VALUE", x$content, parent = alignable_annotation)
                alignable_annotation
        })
        
        tier
        
        doc <- XML::xmlParse(eaf_file)
        
        # Note: add speaker attribute so that we don't delete too many tiers!
        
        XML::removeNodes(doc[paste0("//TIER[@TIER_ID='", target_tier, "']")])
        
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
        
#        new_filename <- gsub("(.+)(.eaf)$", "\\1-ipa.eaf", eaf_file)
        
        XML::saveXML(eaf_to_be_written[[1]], eaf_file)
        
}
