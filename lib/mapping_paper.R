
states_outline_map = get_australia_states_map( states )


##################################################################

printMap = function( df_map, title, filename )  {

  map_color_set_1= RColorBrewer::brewer.pal(6, "Oranges")[2:6]
  map_color_set_2= RColorBrewer::brewer.pal(6, "Blues")[2:6] 

  map_color_set_2= c(RColorBrewer::brewer.pal(5, "PuBuGn") )
  map_color_set_1= c(RColorBrewer::brewer.pal(5, "YlOrRd") )


  pic_width=1500*2
  pic_height=1100*2

  cc_box_width=0.15
  vp_sydney=viewport(x= .50, y= .52, width= cc_box_width, height= cc_box_width)
  vp_melbourne=viewport(x= 0.18, y= 0.18, width= cc_box_width, height= cc_box_width)
  vp_legend=viewport(x= 0.58, y= 0.2, width= 0.3, height= 0.3)
  bbox_oz = c( 140.9617 , -39.15839 , 159.1054 , -28.15702 )

  right_join( df_map, df_population %>% distinct( lga ) ) %>%
    mutate( value = ifelse( is.na(Value), 0, Value )) %>%
    select( lga, value ) %>%
    rbind( data.frame( lga=c(89399), value=0)) %>%
    mutate( value=as.ordered(value)) %>% 
    select( lga, value ) %>%
    append_data( base_map, 
                ., 
                key.shp="LGA_CODE11", 
                key.data="lga" 
                )  %>%
    {.} -> df_map

  df_map %>%
    tm_shape( ) + 
    tm_polygons( "value", 
                title = title,
                palette = map_color_set_1 ,
                showNA=FALSE,
                colorNA='#FFFFFF'
                ) +
    tm_shape( states_outline_map) + 
    tm_borders(  alpha=1, col="#000000"  )  +
    tm_layout(frame=FALSE,
              legend.position = c("right", "top"), bg.color="#FFFFFF",
              inner.margins = c(.25,.02,.02,.25),  # how far in from the bottom and right side (for insets)
              legend.show=FALSE) +
    tm_shape( capital_city_box(1)) + tm_borders(col="red")+
    tm_shape( capital_city_box(2)) + tm_borders(col="red") %>%
    {.} -> map

    print(map)

df_map %>%
  tm_shape( ) + 
  tm_polygons( "value", 
              title = title,
              palette = map_color_set_1 ,
              showNA=FALSE,
              colorNA='#FFFFFF'
              ) +
tm_layout(frame=FALSE,
          bg.color="#FFFFFF",
          legend.only=TRUE) %>%
{ . } -> m_legend

  print(m_legend, vp=vp_legend)

df_map %>%
  tm_shape( xlim=get_state_capital_xlim(2),
           ylim=get_state_capital_ylim(2) ) +
tm_polygons( "value", 
            palette = map_color_set_1 ,
            showNA=FALSE,
            colorNA='#FFFFFF',
            legend.show=FALSE) +
tm_layout(frame="red", 
          bg.color="#FFFFFF"
          )  %>%
{.} -> m_melbourne

  df_map %>% 
    tm_shape( xlim=get_state_capital_xlim(1),
             ylim=get_state_capital_ylim(1) ) +
tm_polygons( "value", 
            palette = map_color_set_1 ,
            showNA=FALSE,
            colorNA='#FFFFFF',
            legend.show=FALSE) +
tm_layout(frame="red", bg.color="#FFFFFF")  %>%
{.} -> m_sydney

  # print insets
  print(m_sydney, vp=vp_sydney)
print(m_melbourne, vp=vp_melbourne)
tmap_save( map, 
          insets_vp = list( vp_sydney, vp_melbourne, vp_legend ),
          insets_tm = list( m_sydney, m_melbourne, m_legend ),
          filename=filename, 
          width=pic_width, height=pic_height, asp=0
          ) 
}

##################################################################

require(sp)
#require(rgdal)

capital_city_box = function( state_id ) {
  coords = c( get_state_capital_xlim( state_id ), get_state_capital_ylim( state_id ))
  e <- as(raster::extent(coords), "SpatialPolygons")
  proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  e
}
###
