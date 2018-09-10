
#+ datasetup1, echo=FALSE, include=FALSE
setwd("~/mydoc/research/mofi/continuing/R")
source("functions.R")


singleFacetPlot_boxplot = function( df_raw_input, df_population,  var1, var2 ) {

		gb = c("lga", var1, var2 )
		gbp = intersect( gb, c("lga", "sex", "age", "state") )  # this is the PK of population, take as much as possible
		df_raw_input %>%
			group_by_at( vars(gb)) %>%
			summarise( no_doses = sum(no_doses), quantity=sum(supplied_quantity)) %>%
			inner_join( df_population %>%
    					   group_by_at( vars( gbp) ) %>%
					   	   summarise( population = sum(population ),
					   	   				person_days = sum(population * my_year_length( supply_year ))
					   	   				) 
					   , by=gbp
					   ) %>%
			mutate( ddd =  no_doses * 1000 / person_days ) %>%
			ggplot() +
			geom_boxplot( mapping=aes_string(x = var1, y= "ddd" , color=var1, fill=var1)) + 
			ggtitle( paste("The range of LGA DDD's for each", var1, "facetted by", var2)) +
			facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}


# multiplots  --------------------------------------------------------------

generate_multiplots <- function () {
		
	state_id = 0
	df_population = get_population_df(state_id) %>%
		mutate( state=get_state_code( substr( lga, 1,1 )))

	df_raw_input = get_raw_df(state_id) %>%
 		mutate( state=get_state_code( substr( lga, 1,1 ))) %>%
 		filter( !is.na(state))


	variables = qw("state urbanization sex age type_name seifa scheme")
	#variables = qw("state urbanization")
	to_plot = combn( variables, 2 , simplify=FALSE)
	to_plot=c(to_plot, lapply(to_plot, rev))
	plot_list = list()

	for (i in 1:length(to_plot)) {
		cat( to_plot[[ i ]][1], to_plot[[ i ]][2], i, "\n")
		plot_list[[i]] <- singleFacetPlot_boxplot( df_raw_input, df_population, to_plot[[ i ]][1], to_plot[[ i ]][2] )

		file_name = paste("graphics/boxplot_", to_plot[[ i ]][1], "_", to_plot[[ i ]][2], i, ".tiff", sep="")
		tiff(file_name)
		print(plot_list[[i]])
		dev.off()
	}

	pdf("graphics/boxplot_combinations.pdf")
	for (i in 1:length(plot_list)) {
		print(plot_list[[i]])
	}
	dev.off()

}

