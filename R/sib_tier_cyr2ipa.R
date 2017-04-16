#' sib_tier_cyr2ipa Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' sib_tier_cyr2ipa(eaf_file = "test.eaf", participant = "JAI-M-1939", linguistic_type = "sib")

sib_tier_cyr2ipa <- function(search_pattern = '(ɕ|ʑ)', eaf_file = 'kpv_izva20140323-2horse_farm-b-test.eaf', participant = 'AXH-M-1979', linguistic_type = 'wordT', target_type = 'sib', study = 'izva_sibilants'){

        `%>%` <- dplyr::`%>%`

        elan_hits <- FRelan::read_eaf(eaf_file) %>%
          dplyr::filter(participant == participant) %>%
          dplyr::mutate(ipa = elanphontier::transliterate(token, model = 'ikdp2ipa.csv')) %>%
          dplyr::group_by(reference) %>%
          dplyr::mutate(token_position = 1:n()) %>%
          dplyr::mutate(token_sum = n()) %>%
          dplyr::ungroup() %>%
          dplyr::filter(stringr::str_detect(ipa, search_pattern)) %>%
          dplyr::mutate(utterance_length = time_end - time_start) %>%
#          dplyr::select(ipa, token_position, token_sum, utterance_length) %>%
          dplyr::mutate(token_length = utterance_length / token_sum) %>% # more could happen here
          dplyr::mutate(token_start = ceiling(time_start + (token_position * token_length))) %>% # round up
          dplyr::mutate(token_end = trunc((time_start + (token_position + 1) * token_length))) # round down

          # This downloads from GitHub Wiki the tier definition

          tier_info <- tibble::tibble(lines = readr::read_lines('https://raw.githubusercontent.com/wiki/langdoc/FRechdoc/Individual-tiers.md')) %>%
            dplyr::filter(stringr::str_detect(lines, '^    ')) %>% tidyr::separate(lines, into = c('field', 'value'), sep = ': ') %>%
            dplyr::filter(! is.na(value)) %>% t() %>% tibble::as_tibble()

          names(tier_info) <- as.character(stringr::str_trim(tier_info[1,]))
          tier_info <- tier_info[-1,]

          tier_info <- tier_info %>% dplyr::filter(study == study)

          # This tests whether linguistic type with wanted name already exists

          eaf_xml <- xml2::read_xml(eaf_file)

          if (eaf_xml %>% xml2::xml_find_all(paste0('//LINGUISTIC_TYPE[@LINGUISTIC_TYPE_ID=\'', target_type,'T\']')) %>% length == 0){

                        eaf_xml %>% xml2::xml_find_first('//LINGUISTIC_TYPE') %>%
                            xml2::xml_add_sibling('LINGUISTIC_TYPE',
                                  CONSTRAINTS=tier_info$constraints,
                                  GRAPHIC_REFERENCES='false',
                                  LINGUISTIC_TYPE_ID = tier_info$type_name,
                                  TIME_ALIGNABLE = tier_info$time_alignable)
          } else {

                        eaf_xml %>% xml2::xml_find_first(paste0('//LINGUISTIC_TYPE[@LINGUISTIC_TYPE_ID=\'', target_type,'T\']')) %>%
                            xml2::xml_replace('LINGUISTIC_TYPE',
                                  CONSTRAINTS=tier_info$constraints,
                                  GRAPHIC_REFERENCES='false',
                                  LINGUISTIC_TYPE_ID = tier_info$type_name,
                                  TIME_ALIGNABLE = tier_info$time_alignable)
          }

          # eaf_xml %>%
          #   xml_find_all("//TIER[@PARTICIPANT='AXH-M-1980']") %>% xml_set_attr('PARTICIPANT', 'AXH-M-1980')

          # The data frame `elan_hits` contains now tokens, their lengths, start times and end times
          # It is necessary to add timeslots for those incoming annotations, and each of them also needs
          # a new annotation id. This kind of annotations do not have a reference annotation on
          # higher tiers, which causes some additional problems in my opinion, but the connection
          # can still always be deducted by the time values (child is inside its parent)

          # We also have to grab some new values, mainly max time slot id and max annotation id

          max_ts <- eaf_xml %>% xml_find_all('//TIME_SLOT') %>%
            xml_attr('TIME_SLOT_ID') %>%
            str_extract('\\d+') %>%
            as.numeric %>%
            max

          max_id <- eaf_xml %>% xml_find_all('//ANNOTATION/*') %>%
            xml_attr('ANNOTATION_ID') %>%
            str_extract('\\d+') %>%
            as.numeric %>%
            max

          elan_hits <- elan_hits %>%
            tidyr::gather(type, time, token_start:token_end) %>%
            arrange(time) %>%
            mutate(id = 1:n()) %>%
            mutate(new_ts = paste0('ts', max_ts + id))

          # here we pick the pointer to the time slot node from the rest
          # notice that this is a pointer, not the node though
          # modifying this MODIFIES the whole tree

          ts_node <- eaf_xml %>% xml_find_first('//TIME_ORDER')

          add_timeslot <- function(data, root = ts_node){
            xml_add_child(root, 'TIME_SLOT', TIME_SLOT_ID = data$new_ts, TIME_VALUE = data$time)
          }

          elan_hits %>% split(.$id) %>% walk(., ~ add_timeslot(.x))

          tier_location <- eaf_xml %>% xml_find_first('//TIER')

          tier <- xml_add_sibling(tier_location, 'TIER',
                                LINGUISTIC_TYPE_REF = tier_info$type_name,
                                PARENT_REF = paste0(tier_info$parent_prefix, participant),
                                PARTICIPANT = participant,
                                TIER_ID = paste0(tier_info$prefix, participant),
                                .where = 'after')

          add_align_annotation <- function(data, root = tier){
            annotation <- xml_add_child(root, 'ANNOTATION')
              alignable_annotation <- xml_add_child(annotation, 'ALIGNABLE_ANNOTATION',
                                                    ANNOTATION_ID = data$annotation_id,
                                                    TIME_SLOT_REF1 = data$ts1,
                                                    TIME_SLOT_REF2 = data$ts2)
                xml_add_child(alignable_annotation, 'ANNOTATION_VALUE', data$ipa)
          }

          elan_hits %>% select(-time, -id) %>%
            spread(key = type, value = new_ts) %>%
            mutate(ts1 = token_start,
                   ts2 = token_end) %>%
            mutate(annotation_id = paste0('a', max_id + 1:n())) %>%
            split(.$annotation_id) %>%
            walk(., ~ add_align_annotation(.x))

          xml2::xml_validate(eaf_xml, read_xml('http://www.mpi.nl/tools/elan/EAFv2.8.xsd'))

          write_xml(eaf_xml, 'happy_end.eaf')

          ## TODO:
          ##
          ##       update last used value

}
