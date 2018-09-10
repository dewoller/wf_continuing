
print_map_proportion = function( target_category, name ) {

  df_lga_summary %>% 
    filter( as.integer(usage_category)==target_category ) %>%
    append_data( base_map, 
        ., 
        key.shp="LGA_CODE11", 
        key.data="lga" 
        ) %>%
    {.} -> df

  df %>%
    tm_shape( ) + 
    tm_polygons( "proportion", 
        title = paste("", name, "users as % of LGA")
        ) +
    tm_layout(frame=FALSE,
        legend.position = c("right", "top"), bg.color="lightgreen",
        inner.margins = c(.25,.02,.02,.25)) %>%
    {.} -> map

  df %>%
    tm_shape( xlim=get_state_capital_xlim(2),
        ylim=get_state_capital_ylim(2) ) +
    tm_polygons( "proportion", legend.show=FALSE) +
    tm_layout(frame=FALSE, bg.color="lightgreen")  %>%
    {.} -> m_melbourne

  df %>% 
    tm_shape( xlim=get_state_capital_xlim(1),
        ylim=get_state_capital_ylim(1) ) +
    tm_polygons( "proportion", legend.show=FALSE) +
    tm_layout(frame=FALSE, bg.color="lightgreen")  %>%
    {.} -> m_sydney

  print(map)	
# print insets
    print(m_sydney, vp=viewport(x= .55, y= .5, width= 0.2, height= 0.2))
    print(m_melbourne, vp=viewport(x= 0.27, y= 0.15, width= 0.2, height= 0.2))
}
p
