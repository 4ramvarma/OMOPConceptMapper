#' Build ICD to SNOMED Mapping Query
#'
#' Constructs a SQL query to map ICD9CM or ICD10CM codes to SNOMED concepts
#' through the OMOP CDM concept and concept_relationship tables.
#'
#' @param cdm_schema Character string. The name of the CDM schema 
#'   (e.g., "healthverity_marketplace_omop_20250331")
#' @param pattern_type Character string. Either "like" or "in". Determines how to filter codes:
#'   * "like": Use SQL LIKE pattern matching (for prefix/wildcard searches)
#'   * "in": Use SQL IN clause (for exact code matches)
#' @param pattern_values Character vector. The ICD codes or patterns to search for:
#'   * For pattern_type="like": Single pattern string (e.g., "C16%")
#'   * For pattern_type="in": Vector of exact codes (e.g., c("C16.0", "C16.1"))
#' @param source_vocabulary_id Character vector. Source vocabulary IDs to search.
#'   Default is c("ICD10CM", "ICD9CM")
#' @param target_vocabulary_id Character string. Target vocabulary ID. Default is "SNOMED"
#' @param relationship_id Character string. The relationship type. Default is "Maps to"
#'
#' @return Character string containing the SQL query
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for all ICD codes starting with C16
#' sql <- build_icd_to_snomed_query(
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "like",
#'   pattern_values = "C16%"
#' )
#' 
#' # Search for specific ICD codes
#' sql <- build_icd_to_snomed_query(
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "in",
#'   pattern_values = c("C16.0", "C16.1", "C16.2")
#' )
#' }
build_icd_to_snomed_query <- function(
    cdm_schema,
    pattern_type = c("like", "in"),
    pattern_values,
    source_vocabulary_id = c("ICD10CM", "ICD9CM"),
    target_vocabulary_id = "SNOMED",
    relationship_id = "Maps to"
) {
  
  # Validate inputs
  .validate_cdm_schema(cdm_schema)
  pattern_type <- match.arg(pattern_type)
  
  # Build WHERE clause based on pattern type
  if (pattern_type == "like") {
    if (length(pattern_values) != 1L) {
      stop("For pattern_type='like', provide a single pattern string (e.g., 'C16%')")
    }
    pat <- .escape_sql_string(pattern_values)
    condition <- sprintf("concept_code LIKE '%s'", pat)
    
  } else if (pattern_type == "in") {
    if (length(pattern_values) < 1L) {
      stop("For pattern_type='in', provide a non-empty vector of codes")
    }
    vals <- sprintf("'%s'", .escape_sql_string(pattern_values))
    condition <- sprintf("concept_code IN (%s)", paste(vals, collapse = ", "))
  }
  
  # Build fully qualified table names
  concept_tbl <- sprintf("%s.concept", cdm_schema)
  concept_relationship_tbl <- sprintf("%s.concept_relationship", cdm_schema)
  
  # Build vocabulary filter for source vocabularies
  vocab_filter <- sprintf("'%s'", .escape_sql_string(source_vocabulary_id))
  vocab_condition <- sprintf("vocabulary_id IN (%s)", paste(vocab_filter, collapse = ", "))
  
  # Compose SQL query
  query <- sprintf(
    "WITH filtered_relationships AS (
      SELECT concept_id_1, concept_id_2
      FROM %s
      WHERE relationship_id = '%s'
    ),
    filtered_icd AS (
      SELECT concept_id, concept_code, concept_name, vocabulary_id
      FROM %s
      WHERE %s
        AND %s
        AND invalid_reason IS NULL
    ),
    filtered_snomed AS (
      SELECT concept_id, concept_code, concept_name
      FROM %s
      WHERE vocabulary_id = '%s'
        AND invalid_reason IS NULL
    )
    SELECT
      icd.concept_id      AS icd_concept_id,
      icd.concept_code    AS icd_code,
      icd.concept_name    AS icd_name,
      icd.vocabulary_id   AS icd_vocabulary,
      snomed.concept_id   AS snomed_concept_id,
      snomed.concept_code AS snomed_code,
      snomed.concept_name AS snomed_name
    FROM filtered_icd AS icd
    JOIN filtered_relationships AS m
      ON icd.concept_id = m.concept_id_1
    JOIN filtered_snomed AS snomed
      ON m.concept_id_2 = snomed.concept_id;",
    concept_relationship_tbl,
    .escape_sql_string(relationship_id),
    concept_tbl,
    vocab_condition,
    condition,
    concept_tbl,
    .escape_sql_string(target_vocabulary_id)
  )
  
  return(query)
}

#' Map ICD Codes to SNOMED Concepts
#'
#' Executes a query to map ICD9CM or ICD10CM codes to SNOMED concepts.
#' This is a convenience wrapper around build_icd_to_snomed_query() that
#' also executes the query.
#'
#' @inheritParams build_icd_to_snomed_query
#' @param connection DatabaseConnector connection object
#' @param print_sql Logical. If TRUE, prints the SQL query before executing. Default is FALSE
#'
#' @return Data frame with columns:
#'   * icd_concept_id: ICD concept ID
#'   * icd_code: ICD code
#'   * icd_name: ICD concept name
#'   * icd_vocabulary: Vocabulary ID (ICD9CM or ICD10CM)
#'   * snomed_concept_id: Mapped SNOMED concept ID
#'   * snomed_code: SNOMED code
#'   * snomed_name: SNOMED concept name
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- create_db_connection()
#' 
#' # Map ICD codes with LIKE pattern
#' results <- map_icd_to_snomed(
#'   connection = conn,
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "like",
#'   pattern_values = "C16%",
#'   print_sql = TRUE
#' )
#' 
#' # Map specific ICD codes
#' results <- map_icd_to_snomed(
#'   connection = conn,
#'   cdm_schema = "healthverity_marketplace_omop_20250331",
#'   pattern_type = "in",
#'   pattern_values = c("C16.0", "C16.1", "C16.2")
#' )
#' }
map_icd_to_snomed <- function(
    connection,
    cdm_schema,
    pattern_type = c("like", "in"),
    pattern_values,
    source_vocabulary_id = c("ICD10CM", "ICD9CM"),
    target_vocabulary_id = "SNOMED",
    relationship_id = "Maps to",
    print_sql = FALSE
) {
  
  # Validate connection
  .validate_connection(connection)
  
  # Build SQL query
  sql <- build_icd_to_snomed_query(
    cdm_schema = cdm_schema,
    pattern_type = pattern_type,
    pattern_values = pattern_values,
    source_vocabulary_id = source_vocabulary_id,
    target_vocabulary_id = target_vocabulary_id,
    relationship_id = relationship_id
  )
  
  # Optionally print SQL
  if (print_sql) {
    cat("Executing SQL:\n")
    cat(sql, "\n\n")
  }
  
  # Execute query
  message("Querying ICD to SNOMED mappings...")
  results <- DatabaseConnector::querySql(connection, sql)
  message("Found ", nrow(results), " mappings")
  
  return(results)
}