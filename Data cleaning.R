library(dplyr)
library(plyr)
library(readr)
library(data.table)
library(ggplot2)
library(MASS)

plays <- read_csv('C:/Users/naomi/Downloads/Rice 2023 DVWs/rice23_datavolley.csv')
plays_2 <- plays %>% 
  mutate(attack_type = case_when(
    skill_subtype == 'Hard spike' | lag(skill_subtype) == 'Hard spike' ~ 'hard attack',
    skill_subtype == 'Tip' | skill_subtype == 'Soft spike/topspin' | 
      lag(skill_subtype) == 'Tip' | lag(skill_subtype) == 'Soft spike/topspin' ~ 'soft attack',
    TRUE ~ NA_character_
  ))

plays_2 <- plays_2 %>%
  relocate(attack_type, .after = skill_subtype)



# OUTSIDE & MIDDLE 32s ANALYSIS

      oh_kills <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          (skill == 'Dig' & start_zone == 4 & evaluation_code == '=' & lag(skill != 'Block' & team == lag(team), default = FALSE)) | 
            (skill == 'Attack' & (attack_code == 'X5' | attack_code == 'V5' | attack_code == 'X7') & evaluation_code == '#' & lead(skill != 'Block')))
      
      oh_dug <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          skill == 'Dig' & 
            evaluation_code != '=' & 
            start_zone == 4 & lag(skill) == 'Attack'
        )
      
      combined_oh <- rbind(oh_kills, oh_dug)
      
      
      oh_clusters<-kmeans(data.frame(combined_oh$end_coordinate_x, combined_oh$end_coordinate_y), 4)
      # Adjusted bounced balls and dug balls
      kde2d(x = combined_oh$end_coordinate_x, y = combined_oh$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
        image(col = viridis::viridis_pal()(400))
      title(main='Outside & Middle 32 Attacks')
      points(x= oh_clusters$centers[,1], y = oh_clusters$centers[,2], col = 'white', pch = 16)
      # Draw horizontal lines
      segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
      # Draw vertical lines
      segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)
      
      # Make usable df for altered k-means algorithm
      oh_coordinates <- data.frame(
        end_coordinate_x = combined_oh$end_coordinate_x,
        end_coordinate_y = combined_oh$end_coordinate_y,
        attack_type = combined_oh$attack_type
      )
      
# RIGHTSIDE & MIDDLE SLIDE ANALYSIS
      
      rh_kills <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          (skill == 'Dig' & start_zone == 2 & evaluation_code == '=' & lag(skill != 'Block' & team == lag(team), default = FALSE)) | 
            (skill == 'Attack' & (attack_code == 'X6' | attack_code == 'V6' | attack_code == 'CF' | attack_code == 'CB') & evaluation_code == '#' & lead(skill != 'Block')))
      
      rh_dug <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          skill == 'Dig' & 
            evaluation_code != '=' & 
            start_zone == 2 & lag(skill) == 'Attack'
        )
      
      combined_rh <- rbind(rh_kills, rh_dug)
      
      
      rh_clusters<-kmeans(data.frame(combined_rh$end_coordinate_x, combined_rh$end_coordinate_y), 4)
      # Adjusted bounced balls and dug balls
      kde2d(x = combined_rh$end_coordinate_x, y = combined_rh$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
        image(col = viridis::viridis_pal()(400))
      title(main='Rightside & Slide Attacks')
      points(x= rh_clusters$centers[,1], y = rh_clusters$centers[,2], col = 'white', pch = 16)
      # Draw horizontal lines
      segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
      # Draw vertical lines
      segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)
      
      # Make usable df for altered k-means algorithm
      rh_coordinates <- data.frame(
        end_coordinate_x = combined_rh$end_coordinate_x,
        end_coordinate_y = combined_rh$end_coordinate_y,
        attack_type = combined_rh$attack_type
      )
      
# MIDDLE ANALYSIS
      
      mb_kills <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          (skill == 'Dig' & start_zone == 3 & evaluation_code == '=' & lag(skill != 'Block' & team == lag(team), default = FALSE)) | 
            (skill == 'Attack' & (attack_code == 'X1' | attack_code == 'X2') & evaluation_code == '#' & lead(skill != 'Block')))
      
      mb_dug <- plays_2 %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          skill == 'Dig' & 
            evaluation_code != '=' & 
            start_zone == 3 & lag(skill) == 'Attack'
        )
      
      combined_mb <- rbind(mb_kills, mb_dug)
      
      
      mb_clusters<-kmeans(data.frame(combined_mb$end_coordinate_x, combined_mb$end_coordinate_y), 3)
      # Adjusted bounced balls and dug balls
      kde2d(x = combined_mb$end_coordinate_x, y = combined_mb$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
        image(col = viridis::viridis_pal()(400))
      title(main='Middle Attacks')
      points(x= mb_clusters$centers[,1], y = mb_clusters$centers[,2], col = 'white', pch = 16)
      # Draw horizontal lines
      segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
      # Draw vertical lines
      segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)
      
      # Make usable df for altered k-means algorithm
      mb_coordinates <- data.frame(
        end_coordinate_x = combined_mb$end_coordinate_x,
        end_coordinate_y = combined_mb$end_coordinate_y,
        attack_type = combined_mb$attack_type
      )
      
sum(nrow(mb_coordinates), nrow(oh_coordinates), nrow(rh_coordinates))
      
      
      # PIPE ANALYSIS
      
      pipe_kills <- plays %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          (skill == 'Dig' & (start_zone == 6 | start_zone == 8) & evaluation_code == '=' & lag(skill != 'Block' & team == lag(team), default = FALSE)) | 
            (skill == 'Attack' & (attack_code == 'XP' | attack_code == 'VP') & evaluation_code == '#' & lead(skill != 'Block')))
      
      pipe_dug <- plays %>% 
        filter(
          !is.na(end_coordinate_x), 
          !is.na(end_coordinate_y), 
          skill == 'Dig' & 
            evaluation_code != '=' & 
            (start_zone == 6 | start_zone == 8) & lag(skill) == 'Attack'
        )
      
      combined_pipe <- rbind(pipe_kills, pipe_dug)
      
      
      pipe_clusters<-kmeans(data.frame(combined_pipe$end_coordinate_x, combined_pipe$end_coordinate_y), 4)
      # Adjusted bounced balls and dug balls
      kde2d(x = combined_pipe$end_coordinate_x, y = combined_pipe$end_coordinate_y, h = 0.3, n = 400, lims = c(0,4,0,7)) |> 
        image(col = viridis::viridis_pal()(400))
      title(main='Pipe Attacks')
      points(x= pipe_clusters$centers[,1], y = pipe_clusters$centers[,2], col = 'white', pch = 16)
      # Draw horizontal lines
      segments(x0 = 0.5, x1 = 3.5, y0 = c(0.5, 2.5, 3.5, 4.5, 6.5), y1 = c(0.5, 2.5, 3.5, 4.5, 6.5), col = 'white', lwd = 2)
      # Draw vertical lines
      segments(x0 = c(0.5, 3.5), x1 = c(0.5, 3.5), y0 = 0.5, y1 = 6.5, col = 'white', lwd = 2)
      
      # Make usable df for altered k-means algorithm
      pipe_coordinates <- data.frame(
        end_coordinate_x = combined_pipe$end_coordinate_x,
        end_coordinate_y = combined_pipe$end_coordinate_y,
        attack_type = combined_pipe$skill_subtype
      )
      
      
  
  
  