#' This function will look inside a database table and sample data from the
#' table. This is a slower sample using SQL's `ORDER BY RANDOM()`.
#'
#' @param conn (`DBI` database connection object) This is the connection object
#' to a database. Refer to `DBI::dbConnect()` for details.
#' @param samples (integer) Number of samples to randomly sample.
#' @param table_name (character) Name of the table in the database to extract
#' data from.
#'
#' @return (tibble) Returns a tibble that is randomly sampled from the chosen
#' table in the databse.
#' @export
#'
#' @examples \dontrun{
#'
#' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' conn <- DBI::dbConnect(RSQLite::SQLite(), local_db_path)
#' tables <- RSQLite::dbListTables(conn) %>%
#'                  pluck(1)
#'
#' sampled_data <-
#'         sample_db(
#'         conn = conn,
#'         samples = 1000,
#'         table_name = tables
#'         )
#'
#' }
sample_db <- function(conn,
                      samples,
                      table_name){

  DBI::dbGetQuery(conn,statement = glue::glue("SELECT *
                                              FROM {table_name}
                                              ORDER BY RANDOM()
                                              LIMIT {samples};"))

}

#' This function will look inside a database table and counts the number of
#' rows inside that table.
#'
#' @param conn (`DBI` database connection object) This is the connection object
#' to a database. Refer to `DBI::dbConnect()` for details.
#' @param table_name (character) Name of the table in the database to extract
#' data from.
#'
#' @return (tibble) Returns a tibble that is randomly sampled from the chosen
#' table in the databse.
#' @export
#'
#' @examples \dontrun{
#'
#' local_db_path <- "data/joined_lm_trig_data/mydb.db"
#' conn <- DBI::dbConnect(RSQLite::SQLite(), local_db_path)
#' tables <- RSQLite::dbListTables(conn) %>%
#'                  pluck(1)
#'
#' sampled_data <-
#'         find_db_size(
#'         conn = conn,
#'         table_name = tables
#'         )
#'
#' }
find_db_size <- function(conn,
                         table_name){

 dplyr::tbl(conn,table_name) %>%
    dplyr::count() %>%
    dplyr::collect()

}

fast_db_sample <- function(conn,samples,
                           max_value,
                           filter_statement = NULL,
                           return_query = FALSE){

  tables <- RSQLite::dbListTables(conn)

  max_value <- ifelse(
    class(max_value) == "numeric", max_value,  max_value$n[1]
  )

  random_values <- floor(stats::runif(1,1, max_value - samples - 1))

  if(is.null(filter_statement)){

    if(return_query){

      test <- glue::glue("SELECT * FROM {tables}
                          LIMIT {random_values},{samples};")

    }else{

      test <- DBI::dbGetQuery(conn,statement = glue::glue(
        "SELECT * FROM {tables}
                              LIMIT {random_values},{samples};"))
    }


  }else{

    if(return_query){

      test <- glue::glue("SELECT * FROM {tables}
                         {filter_statement}
                          LIMIT {random_values},{samples};")

    }else{

      test <- DBI::dbGetQuery(conn,statement = glue::glue(
        "SELECT * FROM {tables}
                              {filter_statement}
                              LIMIT {random_values},{samples};"))


    }

  }


  return(test)

}

connect_db <- function(path = "data/joined_lm_trig_data/raw_lm_trig.db"){

  conn <- DBI::dbConnect(RSQLite::SQLite(), path)

  return(conn)

}

write_table_sql_lite <- function(.data,
                                 table_name,
                                 conn){
  DBI::dbWriteTable(conn, table_name, .data)
}

append_table_sql_lite <- function(.data,
                                  table_name,
                                  conn){

  RSQLite::dbAppendTable(conn = conn,name = table_name,value = .data)

}
