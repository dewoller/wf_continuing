#!/usr/bin/env Rscript

source( "lib/functions_preload.R")
detach_all_packages()

safe_load("wrapr" )
safe_load("data.table")
safe_load("seas")
safe_load("magrittr")
#safe_load("ggplot2")
#safe_load("plyr")
#safe_load("tidyr")
safe_load("stringr")
safe_load("broom")
safe_load("knitr")
safe_load("kableExtra")
safe_load("pander")
safe_load("lubridate")
# new map functions
safe_load("tmap", 'https://cloud.r-project.org')
safe_load("tmaptools", 'https://cloud.r-project.org')
safe_load("grid")

safe_load("readstata13" )
safe_load("foreign" )
safe_load("wrapr" )   # for the qc function

safe_load("ordinal" )
safe_load("DataCache" )
safe_load("shiny" )
safe_load("tidyverse")


# old map functions
#safe_load("maptools")
#gpclibpermit()
#safe_load("rcolorbrewer")
#safe_load("ggmap")
#safe_load("ggrepel")

#source ("../r/loginlocal.r")

keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))


destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}

save_data_single <- function( item ) {
	fwrite( eval(parse(text = item)), paste0(item, ".csv"))
}
save_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, save_data_single )
	} else {
		save_data_single( item ) 
	}
}
#
read_data_single <- function( item ) {
	 do.call("<<-",list(item, fread( paste0(item, ".csv")) %>% as.tibble()))
}
read_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, read_data_single )
	} else {
		read_data_single( item ) 
	}
}
# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
	sapply( year, function(year) { year.length(as.character(year )) })
}


# seqNext ------------------------------------------------------------------
seqNext <- function(x1, y1) {
  dat <- data.frame( x= x1
  			 , y=y1) 
  unname(
    predict(lm(y ~ x, data=dat), newdata=list(x=c(2016)))
  )
}

# bothDiff  ------------------------------------------------------------------
bothDiff <- function ( set1, set2, by=intersect(  names( set1 ), names( set2))) {

  print("first - second")
  set1 %>%
    anti_join( set2, by=by ) %>%
    arrange_( by ) %>%
    print()

  print("second - first")
  set2 %>%
    anti_join( set1, by=by ) %>%
    arrange_( by ) %>%
    print()

}

# ------------------------------------------------------------------
slurpTable <- function( name ) {

	state_geo_query = paste( "
			SELECT * FROM ", name
			, sep=""
			)
	dbGetQuery(con, state_geo_query ) %>% 
		as.tibble() %>%
	return( . )
}

# -------------------------------------------------
get_state_code_restriction <- function( state_id) { 
#
state = c( "NSW" , "VIC" , "QLD" , "WA" , "SA" , "TAS" , "NT" , "ACT" , "UNK")
sapply( state_id, function( x )  {
                    ifelse( x == 1
                        , "'NSW', 'ACT'"
                        , paste(
                                "'"
                                , state[ as.numeric( x ) ]
                                , "'"
                                , sep=""
                                )) 
                  }
    )
}

# -------------------------------------------------
get_state_code_from_lga <- function( lga ) { 
#
state = c( "NSW" , "VIC" , "QLD" , "WA" , "SA" , "TAS" , "NT" , "ACT" , "UNK")
sapply( substr( lga, 1, 1) , function( x )  {
                    ifelse( x >= '1' & x <='999999999'
                            , state[ as.numeric( x ) ]
                            , "UNK"
                            )
                  }
    )
}


# -------------------------------------------------
get_significant_trend <- function( df, ddd_direction_labels)	{

	df  %>% 
		mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
		filter( supply_year != 2012 & supply_year != 2016 ) %>%
		group_by(lga, supply_year) %>% 
		summarise( no_doses=sum(no_doses)) %>%
		inner_join( 
			df_population %>%
				mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
				filter( supply_year != 2012 | supply_year != 2016 ) %>%
				group_by( lga, supply_year) %>%
				summarize( person_days = sum(population * my_year_length(supply_year)))
			, by=c("lga", "supply_year") 
		) %>%
		mutate( ddd = (no_doses * 1000) / (person_days)) %>%
		as_tibble() %>%
		select( lga, supply_year, ddd) %>%
		split(.$lga)   %>%
		map(~  # p value for each lga
			lm(ddd ~ as.integer(supply_year), data=.)   %>% 
			tidy() %>% 
			filter(term != "(Intercept)") %>% 
			select(estimate, p.value) 
		) %>%
		do.call("rbind", . ) %>%
		mutate( lga = as.factor(rownames(.))) %>%
		mutate( direction=ifelse( round(p.value, 2) >.05 , "None" , ifelse( estimate>0, "Rising", "Falling"))
			, direction=factor(direction, levels=ddd_direction_labels)) 
}
# -------------------------------------------------


singleFacetPlot_boxplot = function( df_raw_input, var1, var2 ) {

		gb = c("lga", var1, var2 )
		df_raw_input %>%
			group_by_at( vars(gb)) %>%
			summarise( no_users = n()) %>%
			ggplot() +
			geom_boxplot( mapping=aes_string(x = var1, y= "no_users" , color=var1, fill=var1)) + 
			ggtitle( paste("The range of LGA number of users for each", var1, "facetted by", var2)) +
			facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}

# test_multiplots  --------------------------------------------------------------
test_multiplots <- function () {

  variables = qw("gender age state year ")
  variables = qw("year state")
  user_status_df %>% 
    filter( continuing_user ) %>% 
    generate_multiplots( variables )
}

# multiplots  --------------------------------------------------------------

generate_multiplots <- function ( df_raw_input, variables = qw("gender age") , output="screen" ) {
		
	to_plot = combn( variables, 2 , simplify=FALSE)
	to_plot=c(to_plot, lapply(to_plot, rev))
	plot_list = list()

	for (i in 1:length(to_plot)) {
		cat( to_plot[[ i ]][1], to_plot[[ i ]][2], i, "\n")
		plot_list[[i]] <- singleFacetPlot_boxplot( df_raw_input, to_plot[[ i ]][1], to_plot[[ i ]][2] )
        if (output=="screen") {
          print(plot_list[[i]])
        } else if (endsWith(output, "tiff")) {
          file_name = paste("graphics/boxplot_", to_plot[[ i ]][1], "_", to_plot[[ i ]][2], i, ".tiff", sep="")
          tiff(file_name)
          print(plot_list[[i]])
          dev.off()
        }
	}

    if (endsWith(output, ".pdf")) {
      pdf(output)
      for (i in 1:length(plot_list)) {
          print(plot_list[[i]])
      }
      dev.off()
    }
    return(plot_list)

}

#-------------------------------------------------------------------------------------------

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#-------------------------------------------------------------------------------------------

urbanisation_rollup_single <- function( code, desc ) {
	rv = as.character(desc)
	if (substr(as.character( code ), 1, 2) %in% qw("RA RT UD UF UR")) {
			rv =  sub("^([^ ]* [^ ]*) .*","\\1", rv) 
	}
	rv = sub( "Agriculture", "Agricultural", rv )
	rv
}
#-------------------------------------------------------------------------------------------
urbanisation_rollup <- function(code, desc) {
	mapply( urbanisation_rollup_single, code, desc )
}

#-------------------------------------------------------------------------------------------
is_geographic_LGA  <- function( lga ) {
  !endsWith( as.character(lga), '99') && 
    !startsWith( as.character( lga), 'UNK') && 
    (lga != '.')
}

#-------------------------------------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

#-------------------------------------------------------------------------------------------
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

#-------------------------------------------------------------------------------------------
sourcep <- function(file){
  coms <- parse(file)
  for (i in seq_along(coms)){
    print(coms[[i]])
    eval(coms[[i]],envir=.GlobalEnv)
    mess <- paste("Expression",i,"of",length(coms),"parsed. Press <return> to continue.")
    cat(mess)
    readLines(n=1)
  }
}

#-----------------------------------------------------------------------------
age_grouping_mofi <- function( age) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
  cut( age, 
      breaks=c(-1,19,44,64, Inf),
      labels=c("0-19", "20-44", "45-64", "65+"),
      ordered_result=TRUE)
}


#-----------------------------------------------------------------------------
age_grouping <- function( age, n=25 ) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
      paste0(sprintf("%02d", 
                     floor(as.numeric(age)/n)*n), 
             "-", 
             sprintf( "%02d", 
                     (floor(as.numeric(age) / n) + 1 ) * n - 1)
             )
  }



#-----------------------------------------------------------------------------

rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))




#-----------------------------------------------------------------------------

find_pill_group = function( supply_date, difference, ndays, threshold  = .25) {
  id=1
  sum_difference = 0
  sum_days = 0
  df=data.frame( supply_date, difference, ndays)
  rv=c()
  for (A in rows(df)) {
    sum_difference = sum_difference + A$difference
    sum_days = sum_days + A$ndays
    rv <- c( rv, id )
    if (is.na(A$difference ) || is.na(A$ndays)  || ( (sum_days / sum_difference ) < threshold)) {
      id<- id+1
    }
  }  
  rv
}



