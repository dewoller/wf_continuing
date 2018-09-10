
a %>% 
  data.frame(c=.) %>% 
  tbl_df() %>% 
  mutate( c=str_replace( c, "^  *","")) %>%
  filter( c!="" & ! startsWith(c,"Top of" ))  %>% 
  mutate( l=str_length(c)) %>%
  filter( l >1)  %>%
  filter( l < 13) %>%
  ggplot( ) + geom_histogram( aes( x=l), bins=10)

  count(c, sort=TRUE)
