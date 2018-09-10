safe_load("RPostgreSQL")
safe_load("keyring")

# -------------------------------------------------
get_continuing_df <- function( 
                              base_table="continuing_rr", 
                              include_barb=FALSE
                              ) {

  type_code_limit = ifelse( include_barb, 11, 10 )
  query  <-  paste0( "
    SELECT pin, gender, age, state, lga, scheme, item_code, type_code, type_name, supply_date, quantity, unit_wt, ddd_mg_factor, days_multiplier from continuing."
    , base_table
    , " r JOIN continuing.item i USING (item_code) 
      JOIN public.generictype USING (type_code)
    where (type_code < ", type_code_limit, "  
    AND EXTRACT( YEAR FROM supply_date ) != '2017'
    AND state in ('NSW', 'VIC')
    AND (lga like '1%' OR lga like '2%'))"
      )

  my_db_get_query( query ) %>%
    as.tibble() %>%
    mutate( n_dose = (unit_wt * quantity / ddd_mg_factor ),
           agen=ifelse( age=='100+', 101, as.numeric( age )),
           age = cut( agen, 
                           c(0,19,44,64,9999), 
                           labels=qw("0-19 20-44 45-64 65+")
                           )
           ) %>%
    rename(sex=gender) 
}

# -------------------------------------------------
get_usage_df <- function( ) {

  query  <-  paste( "
    SELECT * from continuing.usage 
          "
          , sep = ""
      )
  my_db_get_query( query ) %>%
    # TODO mutate  - map ". " LGA to appropriate state 99 LGA
    as.tibble() %>%
  return( . )
}


# -------------------------------------------------
my_dbWriteTable <- function ( df, table_name ) {
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
          host = "localhost", port = 5432,
          user = "dewoller", password = key_get('mofi', 'dewoller'))
  on.exit(dbDisconnect(con))
  dbWriteTable( con, table_name, df )
}
# -------------------------------------------------

# -------------------------------------------------
my_db_get_query <- function ( query ) {

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "mofi",
          host = "localhost", port = 5432,
          user = "dewoller", password = key_get('mofi', 'dewoller'))
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query )

}
# -------------------------------------------------
# get size dataframe -------------------------------------------------
get_lga_size_df<- function ( state_id ) {
  query = paste(
          "
          select lga, sum(area_albers_sqkm) as size
          from lga_full_mb_2011
          WHERE "
          , get_lga_restriction( state_id )
          , " group by 1"
          , sep = ''
        )
  df_size <- my_db_get_query( query ) %>%
    mutate_at( c(  "lga" ), funs( factor(.) ) ) %>%
    ungroup() %>%
    as.tibble()
  return( df_size )
}



# get Population dataframe -------------------------------------------------
get_population_df<- function ( state_id = 0 ) {
  df_seifa = get_seifa_df( state_id)
  query = paste(
          "
            SELECT supply_year, lga, age, sex, sum(population) as population
            FROM abs_population_asgs_2011 p
            WHERE "
            , get_lga_restriction( state_id )
            , " group by 1,2,3,4  -- group and summarise out the 5 year age group levels
          UNION
            SELECT supply_year, lga, age, sex, population
            FROM abs_population_asgs_2011_projection_2016
            WHERE "
            , get_lga_restriction( state_id )
          , sep = ''
        )

  df_population <- my_db_get_query( query ) %>%
    left_join( df_seifa, by = "lga") %>%
    mutate( state = get_state_code_from_lga( lga ) ) %>%
    mutate( state = get_state_code_from_lga( lga ) ) %>%
    mutate_at( qw( "supply_year lga age sex state"), funs( factor(.) ) ) %>%
    ungroup() %>%
    as.tibble()
  return( df_population )
}


#  -------------------------------------------------
get_population_grouped_old <- function( rollup_level = c() )  {
  # get population grouped by certain rollup_level
  # expects global df_population containing supply_year and population details
  # calculates persondays and population for each grouping (eg seifa, lga )
  # if no supply_year in rollup_level, get yearly average

  if (length( intersect( rollup_level, c("supply_year"))) > 0) {
	df_population %>%
			filter( is_valid_supply_year(supply_year) ) %>%
			group_by_( .dots=rollup_level ) %>%
			summarize( person_days = sum(population * my_year_length(supply_year)),
					   population=sum(population)  )
  } else {
    rollup_level_1 = c(rollup_level, "supply_year")
    df_population %>%
      filter( is_valid_supply_year(supply_year) ) %>%
      group_by_( .dots=rollup_level_1 ) %>%
      summarize( person_days = sum(population * my_year_length(supply_year)),
                population=sum(population)  ) %>%
      ungroup() %>%
      group_by_( .dots=rollup_level ) %>%
      summarize( person_days = mean( person_days) ,
                population=mean(population)  )
  }
}

#  -------------------------------------------------
join_population_grouped_old <- function( dataset, rollup_level=c(), join_key = rollup_level, df_population. = df_population )  {

  pop_names = names(df_population.)
  # can only join and group on what we have available, 
  # allows some looseness on rollup_level vars
  available_rollup_vars = intersect( pop_names, rollup_level)
  join_key = intersect(pop_names, join_key )

  if ( length( available_rollup_vars ) == 0 )  {
    # fallback to average total database population if no variables to join with
    dataset %>% 
      mutate(temp=1) %>% 
      inner_join( get_population_grouped( ) %>% mutate(temp=1)
                 ,by="temp") %>%
      select(-temp) 
  } else {
    dataset %>%
      inner_join( get_population_grouped( available_rollup_vars, df_population. ),  by=join_key)
  }
}

#  -------------------------------------------------
is_valid_supply_year <- function(supply_year) {
  as.character(supply_year) %in% qw("2013 2014 2015 2016")
}

# get lga_restriction -------------------------------------------------
get_lga_restriction <- function ( state_id ) {
  if( 0 %in% state_id ) {
     "TRUE"
  } else {
    if (1 %in% state_id ) {  # add in canberra if NSW
      state_id = c( state_id, 8)
    }
   paste0( '(',  
    paste0( "lga like '", state_id ,"%'", collapse = ' OR ' ),
    ')'
    )
  }
}

# -------------------------------------------------
# -------------------------------------------------

which_urbanisation_single <- function( lga, class_type ) {
  if ( substring( class_type,1,1 ) == "R" ) {
    rv = "Rural"
  } else if ( substring( class_type,1,1 ) == "U" ) {
    rv = "Urban"
  } else if ( substring( lga,1,1 ) == "8" ) {
    rv = "Urban"
  } else if ( substring( class_type,1,1 ) == "N" ) {
    rv = "Rural"
  } else {
    rv = paste("error", lga, class_type )
  }
  rv
}

# -------------------------------------------------
which_urbanisation <- function( lga, class_type ) {
  mapply( which_urbanisation_single, lga, class_type )
}

# get Seifa dataframe -------------------------------------------------
get_seifa_df <- function( state_id ) {
  # missing the #99 values, so need to left join in

  seifa_query   <-   paste( "
                           SELECT l.lga, s.value as score, l.class_type, ct.class_name
                           FROM lga_class_2014  l
                           LEFT JOIN lga_class_type ct USING (class_type)
                           LEFT JOIN seifa_2011 s using (lga)
                           WHERE s.measure_code = 'SCORE'
                           and s.index_code = 'IRSD'
                           and "
                           , get_lga_restriction( state_id )
                           , sep = ""
                           )

  df_seifa <- my_db_get_query( seifa_query ) %>%
    as.tibble() %>%
    rename(irsd_score_raw = score ) %>%
    mutate(
           seifa  = 
             ntile(  irsd_score_raw , 4)  %>%
             ordered(  levels = 1:4, labels = c("Least", "Moderate", "High", "Very High"))
           ) %>%
    mutate( urbanization = which_urbanisation( lga, class_type ))  %>%
    mutate_at( c( "class_type", "class_name", "urbanization"), funs( factor(.) ) ) %>%
    select( lga, irsd_score_raw, seifa, everything())

  #
  return( df_seifa )
}


#  -------------------------------------------------
calculate_ddd <- function( dataset, ds_group_by , location_join, location_select = c()) {
  # given a dataset with n_dose, group it by location_join
  # one a location levels (empty, State or lga) and optionally supply_year)
  # and  
  # and calculate the ddd by joining it with df_populaiton on the 
  # intersection of location_join and names( df_population)

  dataset  %>%  # group by everything we want to group by to get total doses
    group_by_( .dots=c( location_join, ds_group_by )) %>%
      summarise( n_dose=sum(n_dose)) %>%
      # join up with population on the all the same variables
      join_population_grouped( location_join ) %>%
      mutate( ddd = (n_dose * 1000 * 10) / (person_days)) %>%  # calculate DDD
      select_( .dots=c( location_join, ds_group_by, location_select, "ddd" )) %>%
      ungroup()
}




# -------------------------------------------------

get_state_capital_xlim <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    c(capital_tl_lon, capital_br_lon) %>% sort()
}
# -------------------------------------------------

get_state_capital_ylim <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    c(capital_tl_lat, capital_br_lat) %>% sort()
}

# -------------------------------------------------

get_state_capital_bb <- function( state_id ) { 
  get_state_geo_df ( state_id ) %$%
    matrix(c(capital_tl_lat,capital_tl_lon, capital_br_lat,capital_br_lon), nrow=2, byrow=TRUE)
}

# -------------------------------------------------

get_state_geo_df <- function( state_id ) {

  state_geo_query   <-   paste( "
                               SELECT * FROM state_geo
                               WHERE  state_id = "
                               ,  state_id
                               , sep = ""
                               )

  my_db_get_query( state_geo_query ) %>%
    summarise(
              state_id = first( state_id )
              , capital = first( capital )
              , capital_tl_lat = first( capital_tl_lat )
              , capital_tl_lon = first( capital_tl_lon )
              , capital_br_lat = first( capital_br_lat )
              , capital_br_lon = first( capital_br_lon )
              , state_center_lat = mean( state_center_lat )
              , state_center_lon = mean( state_center_lon )
              , state_tl_lat = max( state_tl_lat )
              , state_tl_lon = max( state_tl_lon )
              , state_br_lat = min( state_br_lat )
              , state_br_lon = min( state_br_lon )
              ) %>%
  mutate_at( c( "state_id", "capital"), funs( factor(.) ) ) %>%
  as.tibble() %>%
  return( . )
}


