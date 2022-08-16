
generate_map <- function(df1, df2, state) {
  
  state_name <- enquo(state)
  
  colours <- c("#00204DFF", 
               "#414D6BFF", 
               #"#7C7B78FF", 
               "#BCAF6FFF",
               "#FFEA46FF"
               )
  
  names(colours) <- levels(fs_pin$cluster)
  
  df1 %>% 
    mutate(fs_pin = ifelse(township == "Hlaingtharya (East)", NA_real_, fs_pin)) %>% 
    filter(state == !!state_name) %>% 
    filter(township != "Cocokyun") %>%
    left_join(pcode3_shape, 
              by = "admin3_pcode") %>%
    st_as_sf() %>%
    ggplot() + 
    geom_sf(aes(fill = cluster), size = 0.1, colour = "gray20") + 
    geom_sf_text(aes(label = round(fs_pin, digits = 0) %>%
                       format(big.mark = ",")), 
                 size = 3) +
    geom_sf_text(aes(label = township.y), 
                 vjust = -.7, 
                 size = 3) +
    scale_fill_manual(values = colours) +
    theme_void() +
    labs(fill = "Priori-\ntisation\ngroup",
         caption = "Data source: ACLED; acleddata.com and FSC calculations", 
         subtitle = "A1 & A2: conflict affected, B: urban centres, \nC: middling needs, D: development priorities\nFigures show the PIN") +
    ggtitle(str_remove_all(
      str_c("Townships by prioritisation group, ", as_label(state_name)), 
      '"')) +
    theme(plot.caption=element_text(hjust = 0.5), 
          plot.background = element_rect(fill = "white", colour = NA)) + 
  
  df2 %>%   
    group_by(admin3_pcode = admin3_pcode_old, 
             state) %>% 
    summarise(beneficiaries = sum(new_beneficiaries), 
              .groups = "drop") %>%
    filter(state == !!state_name) %>% 
    right_join(pcode3_shape %>% 
                 filter(state == !!state_name), 
              by = "admin3_pcode") %>%
    filter(township != "Cocokyun") %>%
    mutate(beneficiaries = ifelse(township == "Hlaingtharya (East)", NA_real_, beneficiaries)) %>% 
    st_as_sf() %>%
    ggplot() + 
    geom_sf(aes(fill = beneficiaries), size = 0.1, colour = "gray20") + 
    geom_sf_text(aes(label = round(beneficiaries, digits = 0) %>%
                       format(big.mark = ",")), 
                 size = 3) +
    geom_sf_text(aes(label = township), 
                 vjust = -.7, 
                 size = 3) +
    scale_fill_viridis(direction = -1, 
                       end = .9,
                       labels = comma, 
                       na.value = "grey90") +
    theme_void() +
    labs(fill = "Beneficiaries",
         caption = "Data source: FSC partners") +
    ggtitle(str_remove_all(
      str_c("Beneficiaries as of Q2 2022, ", as_label(state_name)), 
      '"')) +
    theme(plot.caption=element_text(hjust = 0.5), 
          plot.background = element_rect(fill = "white", colour = NA))
 
}


generate_map(fs_pin, fsc, "Shan (South)") 

ggsave("./plots/shan_south_pin_groups.png",
              dpi = 300, 
              units = "in", 
              width = 18, 
              height = 18, 
              device = "png")

# Always, always do Yangon last since it has so much tweaking
ggsave("./plots/yangon_pin_groups.png",
       dpi = 300, 
       units = "in", 
       width = 28, 
       height = 28, 
       device = "png")

show_col(viridis_pal(option = "cividis")(5))

# A2, C & D
# Kachin, Ayyeyarwady, Rakhine

# A1, A2, C & D
# Kayin, Sagaing, Magway, Kayah, Shan (North), Shan (South)

# A2, A2 & D
# Chin 

# A2, B, C
# Mandalay

#A2 & C
# Mon, Bago (East)

# C & D
# Shan (East)