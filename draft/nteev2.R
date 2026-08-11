###---------------------------------------------------
###   NTEE CODE UTILITIES
###---------------------------------------------------
#  (sourced from the NCCS Nonprofit Open Data Collective)

#' Normalize legacy NTEE codes
#'
#' Converts raw NTEE codes into a normalized 3-character form used by all
#' downstream classification functions. Handles padding, blank codes, specialty
#' nonprofits (digits 2-3 of 01-19), and 5-character legacy codes.
#'
#' @param x Character vector of raw NTEE codes.
#' @return A character vector of normalized 3-character NTEE codes.
#' @examples
#' get_clean_ntee( c("B29", "B0129", "B8443", "E0521", "", "A115") )
#' @export
get_clean_ntee <- function( x ) {
  x <- toupper( trimws(x) )
  x[ is.na(x) | x == "" ] <- "Z99"
  x[ nchar(x) == 4 ] <- stringr::str_pad( x[ nchar(x) == 4 ], width = 5, side = "right", pad = "0" )
  letter    <- substr( x, 1, 1 )
  first_two <- substr( x, 2, 3 )
  last_two  <- substr( x, 4, 5 )
  first_two_num <- suppressWarnings( as.numeric(first_two) )
  first_two_num[ is.na(first_two_num) ] <- 99
  ntee   <- substr( x, 1, 3 )
  ntee45 <- paste0( letter, last_two )
  ntee00 <- paste0( letter, "00" )
  specialty3 <- first_two_num <= 19 & nchar(x) == 3
  specialty5 <- first_two_num <= 19 & nchar(x) == 5
  ntee[ specialty3 ] <- ntee00[ specialty3 ]
  ntee[ specialty5 ] <- ntee45[ specialty5 ]
  ntee
}

#' Derive top-level NTEE industry classification
#'
#' Maps raw NTEE codes to their 3-letter high-level industry group.
#' Special overrides: B40-B43/B50 ? UNI (Universities), E20-E24 ? HOS (Hospitals).
#'
#' @param x Character vector of NTEE codes (raw or cleaned).
#' @return Character vector of 3-letter industry codes.
#' @examples
#' get_industry( c("B29", "B8443", "E0521", "Q12", "Z99") )
#' @export
get_industry <- function( x ) {
  x <- toupper( trimws(x) )
  x[ is.na(x) | x == "" ] <- "Z99"
  x[ nchar(x) == 4 ] <- stringr::str_pad( x[ nchar(x) == 4 ], width = 5, side = "right", pad = "0" )
  letter   <- substr( x, 1, 1 )
  last_two <- substr( x, 4, 5 )
  ntee_ind <- substr( x, 1, 3 )
  ntee_ind[ nchar(x) == 5 ] <- paste0( letter[ nchar(x) == 5 ], last_two[ nchar(x) == 5 ] )
  dplyr::case_when(
    ntee_ind %in% c("B40","B41","B42","B43","B50")  ~ "UNI",
    ntee_ind %in% c("E20","E21","E22","E24")         ~ "HOS",
    substr(ntee_ind,1,1) == "A"                      ~ "ART",
    substr(ntee_ind,1,1) == "B"                      ~ "EDU",
    substr(ntee_ind,1,1) %in% c("C","D")             ~ "ENV",
    substr(ntee_ind,1,1) %in% c("E","F","G","H")     ~ "HEL",
    substr(ntee_ind,1,1) %in% LETTERS[9:16]          ~ "HMS",
    substr(ntee_ind,1,1) == "Q"                      ~ "IFA",
    substr(ntee_ind,1,1) %in% c("R","S","T","U","V","W") ~ "PSB",
    substr(ntee_ind,1,1) == "X"                      ~ "REL",
    substr(ntee_ind,1,1) == "Y"                      ~ "MMB",
    TRUE                                              ~ "UNU"
  )
}

#' Classify nonprofit organization type from NTEE code
#'
#' Determines the functional type of the organization (regular, advocacy,
#' research, funding, etc.) based on digits 2-3 of the NTEE code.
#'
#' @param x Character vector of NTEE codes.
#' @return Character vector of organization type codes:
#'   AA (alliance), MT (management/technical), PA (planning/advocacy),
#'   RP (research), MS (single-org support), MM (multi-org support),
#'   NS (general nonprofit support), RG (regular program organization).
#' @examples
#' get_org_type( c("B01", "B02", "B05", "B11", "B29") )
#' @export
get_org_type <- function( x ) {
  x <- toupper( trimws(x) )
  x[ x == "" | is.na(x) ] <- "Z99"
  two <- suppressWarnings( as.numeric( substr(x, 2, 3) ) )
  dplyr::case_when(
    two == 1  ~ "AA",
    two == 2  ~ "MT",
    two == 3  ~ "PA",
    two == 5  ~ "RP",
    two == 11 ~ "MS",
    two == 12 ~ "MM",
    two == 19 ~ "NS",
    TRUE      ~ "RG"
  )
}

#' Generate full NTEE v2 code
#'
#' Combines the normalized NTEE code, industry group, and organization type
#' into the three-part structure: `[INDUSTRY]-[NTEE]-[ORGTYPE]`.
#'
#' @param x Character vector of raw NTEE codes.
#' @return Character vector of NTEE v2 codes (e.g., `"EDU-B29-RG"`).
#' @examples
#' get_nteev2( c("B29", "B8443", "E0521", "B01", "B0129") )
#' @export
get_nteev2 <- function( x ) {
  paste0( get_industry(x), "-", get_clean_ntee(x), "-", get_org_type(x) )
}

#' Convert NTEE V2 codes to NTMAJ12 major group labels
#'
#' @param nteev2 Character vector of NTEE V2 codes.
#' @return Factor vector with 12 levels (ARTS, EDUCATION, HIGHER ED, etc.).
#' @examples
#' get_ntmaj12( c("HOS-E05-RG", "EDU-B29-RG") )
#' @export
get_ntmaj12 <- function( nteev2 ) {
  ind <- c(
    "ART" = "ARTS {ART}",          "EDU" = "EDUCATION {EDU}",
    "UNI" = "HIGHER ED {UNI}",     "ENV" = "ENVIRONMENT {ENV}",
    "HEL" = "HEALTH {HEL}",        "HOS" = "HOSPITALS {HOS}",
    "IFA" = "INTERNATIONAL {IFA}", "PSB" = "PUBLIC BENEFIT  {PSB}",
    "MMB" = "MEMBERSHIP {MMB}",    "UNU" = "UNCLASSIFIED {UNU}",
    "REL" = "RELIGION {REL}",      "HMS" = "HUMAN SERVICES {HMS}"
  )
  factor( substr(nteev2, 1, 3), levels = names(ind), labels = ind )
}

