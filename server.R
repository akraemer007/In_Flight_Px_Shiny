library(shiny)
library(dplyr)
# library("devtools")
# install_github("looker/lookr")
library(ggplot2)
library(ggiraph)
library(ggthemes)
library(tidyr)
library(magrittr)
library(stringr)
library(RPostgreSQL)

# setwd('/Users/andrewkraemer/R/InFlightPx')
# source("../LookRApiCredentials.R")
# library(LookR)
# 
# looker_setup(   id = IdLookR,
#                 secret = secretLookR,
#                 api_path = api_pathLookR
# )
# 
# # Starting Look || pulls from "CareCardSummaryTidy" Look
# CPD <- run_look(1061)
# 
# CarePathData <- tbl_df(CPD) # so I don't have to rerun the look everytime
# CarePathData <- as.data.frame(as.matrix(CarePathData),stringsAsFactors = F)
# # str(CarePathData)
# 
# save(CarePathData, file="CarePathData.Rda")

load(file="CarePathData.Rda")

# Renaming CarePaths Sanely
names(CarePathData) <- gsub("care_cards\\.|patient_information\\.|organization_tree\\.", "",names(CarePathData))

numericFields <- c('guide_item_completion_percentage_active', 'completed_sum', 'completed_and_expired_sum', 'completed_and_overdue_sum',
                   'current_offset_from_surgery','patient_care_path_id') # to systemize this I could just have it look at the last x amount that I could update, since measures are always the last columns
CarePathData[ ,numericFields] <- lapply( numericFields, function(x) as.numeric( CarePathData[ ,x] ) )

# str(CarePathData)

# Data Cleaning --------------------------------

# totall extranious code for andrew practicing R

CarePathData <- filter(CarePathData, organization_parent_name != 'Aultman')
org_list <- CarePathData %>% 
  distinct(organization_parent_name) %>% 
  arrange(organization_parent_name) %>% 
  mutate( rownbr = row_number(),
          org_name = paste('Client', rownbr, sep = " ")) %>% 
  select(-rownbr)

CarePathData.HiddenClients <- CarePathData %>%
  left_join(org_list) %>% 
  mutate(organization_parent_name = org_name) %>% 
  select(-org_name)


b <- c(c(-8:7)*7,999)
l <- c(c(-7:8)*7)

CarePathData.Tidy <- CarePathData.HiddenClients %>%
  mutate(
    current_offset_from_surgery_wk = cut(
      current_offset_from_surgery,
      breaks = b,
      labels = l,
      include.lowest = TRUE
    ),
    completion_percentage = coalesce(guide_item_completion_percentage_active, 0),
    completion_percentage.due = completed_sum / completed_and_overdue_sum
  ) %>%
  filter(completed_and_expired_sum > 0
         ,!is.na(current_offset_from_surgery_wk)
         ) %>%
  select(-current_offset_from_surgery,
         -guide_item_completion_percentage_active) %>%
  group_by(organization_parent_name,
           # care_path_name,
           current_offset_from_surgery_wk) %>%
  arrange(organization_parent_name, 
          # care_path_name,
          current_offset_from_surgery_wk) %>% 
  mutate(rank = min_rank(patient_care_path_id)) 

# shiny server ----------------------------------
shinyServer(function(input, output) {
  
  output$CarePath <- renderggiraph({
    Client <- input$Client
    Denominator <- input$Denominator
    
    CarePathData.Tidy.OneClient <- CarePathData.Tidy %>% 
      filter( organization_parent_name == Client ) 
      # %>% 
      # mutate( yvar = Denominator,
      #         yvar = as.numeric(yvar))
    # yvar_name <- names(axis_vars)[axis_vars == input$Denominator]
    
    gg <- ggplot( CarePathData.Tidy.OneClient ,aes(x = current_offset_from_surgery_wk, y = completion_percentage.due, group = rank))+
      geom_point_interactive( aes( tooltip = patient_care_path_id ), color = 'lightblue', size = 3, position = position_dodge(width = 0.9), alpha = .75) +
      # facet_grid( care_path_name~. ) +
      scale_x_discrete( drop = FALSE ) +
      scale_y_continuous( labels = scales::percent, limits = c(0,1)) +
      geom_vline( aes(xintercept = which(levels(current_offset_from_surgery_wk) == '0') )  ) + #adds solid line to chart
      labs( title = 'Inflight Px', x = 'Days Offset from Surgery', y = 'Completion %') +
      theme_bw()

      ggiraph(code = print(gg), width = .7)
    
  })

})

