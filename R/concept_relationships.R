# Helper functions
escape_sql_string <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

validate_cdm_schema <- function(cdm_schema) {
  if (!is.character(cdm_schema) || length(cdm_schema) != 1 || nchar(cdm_schema) == 0) {
    stop("cdm_schema must be a single non-empty character string")
  }
  return(TRUE)
}

validate_connection <- function(connection) {
  if (is.null(connection)) {
    stop("connection cannot be NULL. Please establish a database connection first.")
  }
  return(TRUE)
}

#' Query Concept Relationships
#'
#' Retrieves comprehensive relationship information for a set of SNOMED concept IDs,
#' including hierarchical relationships (ancestors/descendants), direct relationships,
#' and synonyms from the OMOP CDM.
#'
#' @param snomed_ids Integer vector. SNOMED concept IDs to query relationships for
#' @param cdm_schema Character string. The name of the CDM schema
#' @param connection DatabaseConnector connection object
#' @param only_standard Logical. If TRUE, only return standard concepts. Default is TRUE
#' @param only_direct Logical. If TRUE, only return direct relationships. Default is FALSE
#' @param print_sql Logical. If TRUE, prints the SQL query before executing. Default is FALSE
#'
#' @return Data frame with relationship information
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- create_db_connection()
#' snomed_ids <- c(201826, 4103703, 4229881)
#' 
#' relationships <- query_concept_relationships(
#'   snomed_ids = snomed_ids,
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   connection = conn
#' )
#' }
query_concept_relationships <- function(
    snomed_ids,
    cdm_schema,
    connection,
    only_standard = TRUE,
    only_direct = FALSE,
    print_sql = FALSE
) {
  
  # Input validation
  validate_connection(connection)
  validate_cdm_schema(cdm_schema)
  
  if (length(snomed_ids) == 0) {
    stop("snomed_ids cannot be empty")
  }
  
  if (!is.numeric(snomed_ids) && !is.integer(snomed_ids)) {
    stop("snomed_ids must be a numeric or integer vector")
  }
  
  # Prepare SNOMED IDs string
  snomed_id_string <- paste(unique(snomed_ids), collapse = ", ")
  
  # Build query using glue
  query <- glue::glue("
    WITH
    ctl AS (
      SELECT
        {toupper(as.character(only_standard))}  AS only_standard,
        {toupper(as.character(only_direct))} AS only_direct
    ),
    
    seed AS (
      SELECT c.*
      FROM {cdm_schema}.concept c
      WHERE c.concept_id IN ({snomed_id_string})
    ),
    
    descendants AS (
      SELECT
        s.concept_id                   AS seed_concept_id,
        s.concept_name                 AS seed_concept_name,
        'HIERARCHY_DESCENDANT'         AS rel_category,
        'descendant_of'                AS direction,
        ca.min_levels_of_separation    AS levels_of_separation,
        CAST(NULL AS VARCHAR)          AS relationship_id,
        c2.concept_id                  AS related_concept_id,
        c2.concept_name                AS related_concept_name,
        c2.vocabulary_id,
        c2.domain_id,
        c2.standard_concept,
        c2.invalid_reason
      FROM seed s
      JOIN {cdm_schema}.concept_ancestor ca
        ON ca.ancestor_concept_id = s.concept_id
      JOIN {cdm_schema}.concept c2
        ON c2.concept_id = ca.descendant_concept_id
      JOIN ctl ON 1=1
      WHERE s.concept_id <> c2.concept_id
        AND c2.invalid_reason IS NULL
        AND (NOT ctl.only_standard OR c2.standard_concept = 'S')
        AND (NOT ctl.only_direct  OR ca.min_levels_of_separation = 1)
    ),
    
    ancestors AS (
      SELECT
        s.concept_id                   AS seed_concept_id,
        s.concept_name                 AS seed_concept_name,
        'HIERARCHY_ANCESTOR'           AS rel_category,
        'ancestor_of'                  AS direction,
        ca.min_levels_of_separation    AS levels_of_separation,
        CAST(NULL AS VARCHAR)          AS relationship_id,
        c1.concept_id                  AS related_concept_id,
        c1.concept_name                AS related_concept_name,
        c1.vocabulary_id,
        c1.domain_id,
        c1.standard_concept,
        c1.invalid_reason
      FROM seed s
      JOIN {cdm_schema}.concept_ancestor ca
        ON ca.descendant_concept_id = s.concept_id
      JOIN {cdm_schema}.concept c1
        ON c1.concept_id = ca.ancestor_concept_id
      JOIN ctl ON 1=1
      WHERE s.concept_id <> c1.concept_id
        AND c1.invalid_reason IS NULL
        AND (NOT ctl.only_standard OR c1.standard_concept = 'S')
        AND (NOT ctl.only_direct  OR ca.min_levels_of_separation = 1)
    ),
    
    cr_out AS (
      SELECT
        s.concept_id        AS seed_concept_id,
        s.concept_name      AS seed_concept_name,
        'RELATIONSHIP'      AS rel_category,
        'outgoing'          AS direction,
        CAST(NULL AS INTEGER) AS levels_of_separation,
        cr.relationship_id  AS relationship_id,
        c2.concept_id       AS related_concept_id,
        c2.concept_name     AS related_concept_name,
        c2.vocabulary_id,
        c2.domain_id,
        c2.standard_concept,
        c2.invalid_reason
      FROM seed s
      JOIN {cdm_schema}.concept_relationship cr
        ON cr.concept_id_1 = s.concept_id
      JOIN {cdm_schema}.concept c2
        ON c2.concept_id = cr.concept_id_2
      JOIN ctl ON 1=1
      WHERE cr.invalid_reason IS NULL
        AND CURRENT_DATE BETWEEN cr.valid_start_date AND cr.valid_end_date
        AND c2.invalid_reason IS NULL
        AND (NOT ctl.only_standard OR c2.standard_concept = 'S')
    ),
    
    cr_in AS (
      SELECT
        s.concept_id        AS seed_concept_id,
        s.concept_name      AS seed_concept_name,
        'RELATIONSHIP'      AS rel_category,
        'incoming'          AS direction,
        CAST(NULL AS INTEGER) AS levels_of_separation,
        cr.relationship_id  AS relationship_id,
        c1.concept_id       AS related_concept_id,
        c1.concept_name     AS related_concept_name,
        c1.vocabulary_id,
        c1.domain_id,
        c1.standard_concept,
        c1.invalid_reason
      FROM seed s
      JOIN {cdm_schema}.concept_relationship cr
        ON cr.concept_id_2 = s.concept_id
      JOIN {cdm_schema}.concept c1
        ON c1.concept_id = cr.concept_id_1
      JOIN ctl ON 1=1
      WHERE cr.invalid_reason IS NULL
        AND CURRENT_DATE BETWEEN cr.valid_start_date AND cr.valid_end_date
        AND c1.invalid_reason IS NULL
        AND (NOT ctl.only_standard OR c1.standard_concept = 'S')
    ),
    
    syn AS (
      SELECT
        s.concept_id            AS seed_concept_id,
        s.concept_name          AS seed_concept_name,
        'SYNONYM'               AS rel_category,
        'synonym_of_seed'       AS direction,
        CAST(NULL AS INTEGER)   AS levels_of_separation,
        CAST(NULL AS VARCHAR)   AS relationship_id,
        s.concept_id            AS related_concept_id,
        cs.concept_synonym_name AS related_concept_name,
        s.vocabulary_id,
        s.domain_id,
        s.standard_concept,
        s.invalid_reason
      FROM seed s
      JOIN {cdm_schema}.concept_synonym cs
        ON cs.concept_id = s.concept_id
    )
    
    SELECT DISTINCT
      seed_concept_id,
      seed_concept_name,
      rel_category,
      direction,
      relationship_id,
      levels_of_separation,
      related_concept_id,
      related_concept_name,
      vocabulary_id,
      domain_id,
      standard_concept,
      invalid_reason
    FROM (
      SELECT * FROM descendants
      UNION ALL
      SELECT * FROM ancestors
      UNION ALL
      SELECT * FROM cr_out
      UNION ALL
      SELECT * FROM cr_in
      UNION ALL
      SELECT * FROM syn
    ) u
    ORDER BY
      rel_category,
      direction,
      COALESCE(levels_of_separation, 999),
      vocabulary_id,
      related_concept_name
  ")
  
  # Optionally print SQL
  if (print_sql) {
    cat("Executing SQL:\n")
    cat(as.character(query), "\n\n")
  }
  
  # Execute query
  message("Querying relationships for ", length(unique(snomed_ids)), " SNOMED concept IDs...")
  result <- DatabaseConnector::querySql(connection, as.character(query))
  message("Query completed. Returned ", nrow(result), " relationship rows")
  
  return(result)
}

#' Extract SNOMED IDs from Mapping Results
#'
#' Convenience function to extract unique SNOMED concept IDs from the results
#' of map_icd_to_snomed().
#'
#' @param mapping_results Data frame. Results from map_icd_to_snomed()
#' @param as_string Logical. If TRUE, returns comma-separated string. Default is FALSE
#'
#' @return Integer vector of unique SNOMED concept IDs, or character string if as_string=TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' results <- map_icd_to_snomed(conn, cdm_schema, "like", "C16%")
#' snomed_ids <- extract_snomed_ids(results)
#' }
extract_snomed_ids <- function(mapping_results, as_string = FALSE) {
  
  if (!("SNOMED_CONCEPT_ID" %in% names(mapping_results))) {
    stop("Input must contain a 'SNOMED_CONCEPT_ID' column")
  }
  
  snomed_ids <- unique(mapping_results$SNOMED_CONCEPT_ID)
  
  if (as_string) {
    return(paste(snomed_ids, collapse = ", "))
  }
  
  return(snomed_ids)
}
