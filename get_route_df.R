get_route_df <- function(player_name){
  
  print(player_name)
  
  route_df <- pbp %>%
    filter(pass_attempt == 1) %>%
    filter(passer_player_name == 'P.Mahomes') %>%
    group_by(posteam, passer_player_name, passer_player_id, route)   %>%
    summarize(targets = n(),
              completions = sum(complete_pass),
              epa = mean(epa, na.rm = TRUE),
              yards = mean(receiving_yards, na.rm = TRUE),
              air_yards = mean(air_yards),
              yards_after_catch = mean(yards_after_catch, na.rm = TRUE)) %>%
    ungroup() %>%  
    mutate(total_targets = sum(targets),
           route_target_pct = targets/total_targets) %>%
    filter(!is.na(route), route != 'WHEEL')  %>%
    left_join(team_avg_route_df, by = 'route') %>%
    mutate(#EPA
      epa_zscore = (epa - epa_avg)/epa_sd,
      epa_color = cut(epa_zscore, breaks = breaks, labels = colors, include.lowest = TRUE),
      epa_text_color = ifelse(abs(epa_zscore) < 0.5, 'black', 'white'),
      #Target Size
      route_target_pct_zscore = (route_target_pct - route_target_pct_avg)/route_target_pct_sd,
      epa_text_size = rescale(route_target_pct_zscore, to = c(2.5,5), 
                              from = c(-1.5, 1.5)),
      epa_text_size = ifelse(epa_text_size > 5, 
                             5, epa_text_size),
      epa_text_size = ifelse(epa_text_size < 2.5, 
                             2.5, epa_text_size),
      route_target_pct_size = rescale(route_target_pct_zscore, to = c(target_size_min,target_size_max), 
                                      from = c(-1.5, 1.5)),
      route_target_pct_size = ifelse(route_target_pct_size > target_size_max, 
                                     target_size_max, route_target_pct_size),
      route_target_pct_size = ifelse(route_target_pct_size < target_size_min, 
                                     target_size_min, route_target_pct_size),
      route_tgt_width = rescale(route_target_pct_zscore, to = c(line_size_min,line_size_max), 
                                from = c(-1.5, 1.5)),
      route_tgt_width = ifelse(route_tgt_width > line_size_max, line_size_max, route_tgt_width),
      route_tgt_width = ifelse(route_tgt_width < line_size_min, line_size_min, route_tgt_width)) %>%
    mutate(epa_color = ifelse(targets >= route_threshold, colors[epa_color], "#909090"),
           route_target_pct_size = ifelse(targets < route_threshold, 0, route_target_pct_size),
           route_tgt_width = ifelse(targets < route_threshold, line_size_min, route_tgt_width)) %>%
    left_join(team_colors, by = c('posteam' = 'team_abbr')) 
  
  
  
  
  player_name = (route_df %>% pull(passer_player_name))[1]
  player_id = (route_df %>% pull(passer_player_id))[1]
  player_color = (route_df %>% pull(team_color))[1]
  
  
  routes_under_min = tolower(paste(setdiff(team_avg_route_df$route, route_df$route), collapse = ","))
  routes_under_min =  ifelse(nchar(routes_under_min) == 0,
                             "", paste0(" | Routes Under Threshold: ", routes_under_min))
  
  color_legend_df <- data.frame(x = seq(-4, -3.5, by = 0.1),
                                y = rep(1.5, length(6)), colors = colors)
  
  
  
  
  #route_width = 2
  tree_width= 2.5
  route_end = 3.5
  
  slant_x = route_end - 0.5
  slant_y = 4
  
  in_out_x = 3
  in_out_y = 11
  
  corner_post_x = 2.5
  corner_go_post_y = 20
  
  flat_x = -2
  flat_y = -1
  
  angle_x=-0.75
  angle_y=4
  
  route_font_size = 5
  
  
  g <- route_df %>% ggplot() +
    ###Right Sided Routes
    #Slant
    geom_segment(data = route_df %>% filter(route == 'SLANT'),
                 aes(x = 0, y = 1, xend=slant_x, yend=slant_y,  color =epa_color, linewidth = route_tgt_width),
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'SLANT'),
               aes(x = slant_x, y = slant_y, color = epa_color, fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'SLANT'), 
              aes(label =  percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, size = epa_text_size),
              x = slant_x, y = slant_y,
              family = 'Roboto', fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'SLANT'), 
              aes(color = epa_color), label = 'Slant',
              x = slant_x-0.5, y = slant_y-1.5,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    #Cross
    geom_curve(data = route_df %>% filter(route == 'CROSS'),
               aes(x = 0, y = 1, xend = route_end, yend = 7.5, color =epa_color, linewidth = route_tgt_width), 
               curvature = -0.15, lineend='round')+
    geom_point(data = route_df %>% filter(route == 'CROSS'),
               aes(x = route_end, y = 7.5, color = epa_color, fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'CROSS'), 
              aes(label =  percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, size = epa_text_size),
              x = route_end, y = 7.5,family = 'Roboto', fontface='bold')  + 
    geom_text(data = route_df %>% filter(route == 'CROSS'), 
              aes(color = epa_color), label = 'Cross',
              x = route_end+0.5, y = 7.5+1,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    #Hitch               
    geom_segment(data = route_df %>% filter(route == 'HITCH'),
                 aes(x = 0, y = 10, xend = 0.5, yend = 8,  color =epa_color, linewidth = route_tgt_width), 
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'HITCH'),
               aes(x = 0.5, y = 8, color = epa_color, fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'HITCH'), 
              aes(label =  percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, size = epa_text_size),
              x = 0.5, y = 8,  
              family = 'Roboto',  fontface='bold')  + 
    geom_text(data = route_df %>% filter(route == 'HITCH'), 
              aes(color = epa_color), label = 'Hitch',
              x = 0.5, y = 8 - 1.5 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    #In               
    geom_segment(data = route_df %>% filter(route == 'IN'),
                 aes(x = 0, y = 11, xend = in_out_x, yend = in_out_y, 
                     color =epa_color, linewidth = route_tgt_width), 
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'IN'),
               aes(x = in_out_x, y = in_out_y, color = epa_color, fill = epa_color,
                   size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'IN'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, size = epa_text_size),
              x = in_out_x, y = in_out_y,  
              family = 'Roboto',  fontface='bold')  + 
    geom_text(data = route_df %>% filter(route == 'IN'), 
              aes(color = epa_color), label = 'In',
              x = in_out_x - 1, y = in_out_y +1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    #Post               
    geom_segment(data = route_df %>% filter(route == 'POST'),
                 aes(x = 0, y = 15, xend = corner_post_x, yend = corner_go_post_y, color =epa_color, 
                     linewidth = route_tgt_width), 
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'POST'),
               aes(x = corner_post_x, y = corner_go_post_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'POST'), 
              aes(label =  percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, size = epa_text_size),
              x = corner_post_x, y = corner_go_post_y, 
              family = 'Roboto',  fontface='bold')  + 
    geom_text(data = route_df %>% filter(route == 'POST'), 
              aes(color = epa_color), label = 'Post',
              x = corner_post_x  + 0.75, y = corner_go_post_y- 1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    ###Left Sided Routes
    #Out               
    geom_segment(data = route_df %>% filter(route == 'OUT'),
                 aes(x = 0, y = 11, xend = -in_out_x, yend = in_out_y,
                     color =epa_color, linewidth = route_tgt_width), 
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'OUT'),
               aes(x = -in_out_x, y = in_out_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'OUT'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), 
                  color = epa_text_color, size = epa_text_size),
              x = -in_out_x, y = in_out_y,  
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'OUT'), 
              aes(color = epa_color), label = 'Out',
              x = -in_out_x + 1, y = in_out_y +1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') +  
    #Corner               
    geom_segment(data = route_df %>% filter(route == 'CORNER'),
                 aes(x = 0, y = 15, xend = -corner_post_x, yend = corner_go_post_y, 
                     color =epa_color, linewidth = route_tgt_width), 
                 lineend='round')  +
    geom_point(data = route_df %>% filter(route == 'CORNER'),
               aes(x = -corner_post_x, y = corner_go_post_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'CORNER'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, 
                  size = epa_text_size),
              x = -corner_post_x, y = corner_go_post_y,  
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'CORNER'), 
              aes(color = epa_color), label = 'Corner',
              x = -corner_post_x  - 0.75, y = corner_go_post_y- 1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') + 
    #Flat
    geom_curve(data = route_df %>% filter(route == 'FLAT'),
               aes(x = 0, y = 0, xend = flat_x, yend = flat_y, color =epa_color, linewidth = route_tgt_width),
               lineend='round',  curvature = 0.05,)  +
    geom_point(data = route_df %>% filter(route == 'FLAT'),
               aes(x = flat_x, y = flat_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'FLAT'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color,
                  size = epa_text_size),
              x = flat_x, y = flat_y, 
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'FLAT'), 
              aes(color = epa_color), label = 'Flat',
              x = flat_x - 0.75, y = flat_y + 1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') +  
    #Angle   
    geom_segment(data = route_df %>% filter(route == 'ANGLE'), 
                 aes(color =epa_color, linewidth = route_tgt_width),
                 x = 0, y = 0, xend=-1, yend=2, lineend='round') +
    geom_segment(data = route_df %>% filter(route == 'ANGLE'), 
                 aes(color =epa_color, linewidth = route_tgt_width),
                 x = -1, y = 2, xend=-0.75, yend=4, 
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'ANGLE'),
               aes(x = angle_x, y = angle_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'ANGLE'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color,
                  size = epa_text_size),
              x = angle_x, y = angle_y,  
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'ANGLE'), 
              aes(color = epa_color), label = 'Angle',
              x = -1.1, y = 2 ,  size = route_font_size,hjust = 1,
              family = 'Roboto Slab', fontface='bold') +  
    ###Other Routes
    #Screen               
    geom_curve(data = route_df %>% filter(route == 'SCREEN'), 
               aes(label = percent_format(accuracy = .1)(route_target_pct), 
                   linewidth = route_tgt_width, color = epa_color),
               x = 0, y = 0, xend = .5, yend = -1.5, curvature = 0.5, lineend='round') +
    geom_point(data = route_df %>% filter(route == 'SCREEN'),
               aes(x = .5, y = -1.5, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'SCREEN'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, 
                  size = epa_text_size),
              x = .5, y = -1.5,  
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'SCREEN'), 
              aes(color = epa_color), label = 'Screen',
              x =  .5 + 0.75, y = -1.5 + 1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') +  
    #Go               
    geom_segment(data = route_df %>% filter(route == 'GO'),
                 aes(x = 0, y = 15, xend = 0, yend = corner_go_post_y, 
                     linewidth = route_tgt_width, color = epa_color),
                 lineend='round') +
    geom_point(data = route_df %>% filter(route == 'GO'),
               aes(x = 0, y = corner_go_post_y, color = epa_color, 
                   fill = epa_color, size = route_target_pct_size)) +
    geom_text(data = route_df %>% filter(route == 'GO'), 
              aes(label = percent_format(accuracy = .1)(route_target_pct), color = epa_text_color, 
                  size = epa_text_size),
              x = 0, y = corner_go_post_y,  
              family = 'Roboto',  fontface='bold') + 
    geom_text(data = route_df %>% filter(route == 'GO'), 
              aes(color = epa_color), label = 'Go',
              x =  0 + 0.5, y = corner_go_post_y - 1 ,  size = route_font_size,
              family = 'Roboto Slab', fontface='bold') +  
    #Tree Base
    geom_segment(x = 0, y = 0, xend=0, yend=15, linewidth = tree_width, lineend='round',
                 color = '#303030') +
    geom_segment(x = 0.5, y = 0, xend=4, yend=0, linewidth = 0.25, linetype = 'dashed',
                 color = '#303030') +
    geom_text(x = 2, y = 0.1,  label = 'Line of Scrimmage',
              family = 'Roboto Mono', color = '#303030', size =2, vjust = 0) +
    geom_segment(x = -3.9, y = 11, xend= -3.9, yend=corner_go_post_y, linewidth = 0.25, linetype = 'dashed',
                 color = '#303030') +
    geom_text(x = -4, y = 16, label = 'Sideline', angle = 90,
              family = 'Roboto Mono', color = '#303030', size =2, vjust = 0) +
    geom_segment(x = 3.9, y = 11, xend= 3.9, yend=corner_go_post_y, linewidth = 0.25, linetype = 'dashed',
                 color = '#303030') +
    geom_text(x = 4, y = 16, label = 'Field', angle = 270,
              family = 'Roboto Mono', color = '#303030', size =2, vjust = 0) +
    #Titles & Captions
    geom_text(x = -3.5, y = 23.5, label = player_name, color = '#404040',
              family = 'Roboto Black', size = 10, hjust = 0) +
    geom_text(x = -3.5, y = 22.25, 
              label = paste0('Min. 10 Targets/Route'), 
              color = '#606060',
              family = 'Roboto Slab', size = 4, hjust = 0) +
    labs(caption = "Data: NFL NGS via nflfastR | Viz: @SaurabhOnTap") +
    ##Legends
    #geom_rect(color = '#606060', xmin = -4.5, xmax = -2, ymin = 9.5, ymax = 2, alpha = 0) +
    #Metric Legend
    #geom_text(label = 'League Avg Tgt Frequency',
    #          x = -4.25, y = 9, color = '#606060', family = 'Roboto Mono', size = 3, hjust = 0) +
    geom_point(x = -3.25, y = 4.5, color = '#606060', size = 17.5, shape =21) +
    geom_text(label = 'Pct\nof\nTgts',
              x = -3.25, y = 4.5, color = '#606060', family = 'Roboto Mono', size = 2) +
    
    #Size Legend
    geom_text(x = -4.25, y = 6, label = 'Target Frequency vs Avg', color = '#606060',
              family = 'Roboto Mono', size = 3, hjust=0) +
    geom_segment(x = -4.25, y = 5, xend=-3.75, yend=5, linewidth = 0.5, lineend='round',
                 color = '#606060') +
    geom_point(x = -3.75, y = 5, color = '#606060', size = 2) +
    geom_segment(x = -4.25, y = 4, xend=-3.75, yend=4, linewidth = 1, lineend='round',
                 color = '#606060') +
    geom_point(x = -3.75, y = 4, color = '#606060', size = 4) +
    #EPA Legend
    geom_text(x = -4.25, y = 2.5, label = 'EPA/Route vs Avg', color = '#606060',
              family = 'Roboto Mono', size = 3, hjust=0) +
    geom_point(data = color_legend_df, 
               aes(x = x, y = y, color = colors), size = 2) +
    geom_text(x = -3.8, y = 1, label = 'Below', color = '#de425b',
              family = 'Roboto Mono', size = 2, hjust=1) +
    geom_text(x = -3.7, y = 1, label = 'Above', color = '#488f31',
              family = 'Roboto Mono', size = 2, hjust=0) +
    scale_size_identity() +
    scale_linewidth_identity() +
    scale_color_identity() +
    xlim(-4, 4) +
    ylim(-2, 23) +
    theme_saurabh() +
    theme(axis.title=element_blank()  ,
          axis.ticks=element_blank(),
          axis.text=element_blank())+
    guides(fill="none",
           color='none') + 
    geom_nfl_headshots(aes(player_gsis = player_id), 
                       x = -4, y = 23, height = 0.08)
  
  ggsave(paste0('Route Charts/', gsub("\\.", "", player_name), "_route_tree.png"), height = 9, width = 9, plot =g) 
  
  
}
