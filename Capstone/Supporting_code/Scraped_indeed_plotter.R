
library("tidyverse")
library("stringr")

HoB_color_palet <- c("#07E597", "#FE4F00", "#2C435E", "#2E7D7B", "#0B191D")
complementary_color_palet <- c("#AD91A3", "#6D9DC5", "#AF125A", "#8FB8DE", "#FFD791" )
color_brewer <- c(HoB_color_palet, complementary_color_palet)

plot_hob_salary_vs_indeed_salary_dist <- function(
    df_indeed_scraped,
    first_salary= 2763,
    second_salary= 3087,
    third_salary= 3519
    ){
  #' Create density plot with vertical reference lines
  #' 4 vertical lines will be drawn onto the density plot based on the input integers
  #' and the median salary of the input dataset
  #' @param df_indeed_scraped dataset with scraped data from indeed
  #' @param first_salary integer containing first salary to compare
  #' @param second_salary integer containing second salary to compare
  #' @param third_salary integer containing third salary to compare
  #' @return density plot with 4 ref lines, 3x input salary, 1x median
  
  # bepaal constanten op basis van HoB vacature
  options(scipen=999)
  
  # bepaal median indeed salary
  indeed_median_salary <- median(df_indeed_scraped$salary,na.rm = TRUE)
  
  # maak density tables
  #https://stackoverflow.com/questions/41971150/add-vline-to-geom-density-and-shade-confidence-interval-of-mean-r
  dens <- ggplot_build(ggplot(df_indeed_scraped, aes(x= salary)) +
                         geom_density())
  
  
  # Bepaal de density (hoogte) van het 1ste, 2e en 3de salaris en de mediaan door deze te benaderen op basis van x en y verhoudingen
  dens_first_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = first_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_second_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = second_salary)[[2]])%>%
    .$dens.mean %>% .[1]
  
  dens_third_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = third_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  dens_indeed_median_salary <- dens$data[[1]] %>%
    mutate(dens.mean = approx(x, y, xout = indeed_median_salary)[[2]])%>%
    .$dens.mean %>% .[1] 
  
  # plot de verdeling inclusief salaris punten
  df_indeed_scraped %>%
    ggplot(
      aes(
        x= salary
        )
      ) +
    geom_density(alpha=0.5) +
    geom_segment( # first salary line
      aes(
        x=first_salary,xend=first_salary,
        y=0, yend= dens_first_salary,
        linetype= "HoB 1ste jr."
        )
    ) +
    geom_segment( # second salary line
      aes(
        x=second_salary, xend=second_salary,
        y=0, yend= dens_second_salary,
        linetype= "HoB 2de jr."
        )
    ) +
    geom_segment( # third salary line
      aes(
        x=third_salary, xend=third_salary,
        y=0, yend= dens_third_salary,
        linetype= "HoB 3de jr."
        )
    ) +
    geom_segment( # median indeed salary line
      aes(
        x=indeed_median_salary, xend=indeed_median_salary,
        y=0, yend= dens_indeed_median_salary,
        color = "median loon Indeed"),
      size = 1.0
    ) +
    labs(
      title="Verdeling maandsalaris op Indeed vs. HoB",
      y="Frequentie (%)",
      x="Maandsalaris (\u20ac)"
    ) +
    theme(
      plot.title=element_text(hjust=0.5),
      legend.title= element_blank()
    ) +
    scale_y_continuous( #prevent to many 0's
      breaks= seq(0.0001, 0.0005, 0.0002),
      labels=seq(0.01, 0.05, 0.02)
    ) +
    theme_classic()+
    scale_color_manual(values=c("median loon Indeed"="#07E597"))
}


plt_hob_vs_indeed_salary_dist_per_experience <- function(
    df_indeed_final,
    first_salary= 2763,
    second_salary= 3087,
    third_salary= 3519
){
  #' create a faceted density plot
  #' create a density plot for all values in "werkervaring" column
  #' on every facet plot ref lines will be drawn based on input salary params
  #' @param df_indeed_scraped dataset with scraped data from indeed
  #' @param first_salary integer containing first salary to compare
  #' @param second_salary integer containing second salary to compare
  #' @param third_salary integer containing third salary to compare
  #' @return density plot faceted on "werkervaring" column with added salary ref lines
  
  df_indeed_final %>%
    ggplot(aes(x= salary)) +
    geom_density(alpha=0.5) +
    geom_segment( # first salary line
      aes(
        x =first_salary, xend =first_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 1ste jr."
      )
    ) +
    geom_segment( # second salary line
      aes(
        x =second_salary, xend =second_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 2de jr."
      )
    ) +
    geom_segment( # third salary line
      aes( 
        x =third_salary, xend =third_salary,
        y= 0.0000, yend=0.003,
        linetype= "HoB 3de jr."
      )
    ) +
    facet_wrap(vars(cleaned_werkervaring_jaren)) +
    labs(
      title="Verdeling geboden salaris per jaren werkervaring",
      y= "Frequentie (%)",
      x= "Geboden salaris x1.000 (\u20ac)"
    ) +
    theme(
      plot.title=element_text(hjust=0.5))+
    scale_y_continuous( # limit 0's and max height
      breaks= seq(0.0000, 0.004, 0.001),
      labels=seq(0.00, 0.04, 0.01),
      limits= c(0, 0.003)
    ) +
    scale_x_continuous( # limit 0's and max width
      breaks= seq(2000, 6000, 1000),
      labels=seq(2, 6, 1),
      limits = c(2000, 6000)
    ) +
    theme_classic()
}

plot_indeed_median_salary_per_amount_skills <- function(
    df_indeed_final,
    add_ref_line= FALSE,
    ref_salary= 0
){
  #'create a column chart with cols per skill_count
  #'a column chart with skill_count on x and median salary per group on y
  #'a horizontal ref line is added based on input salary and boolean parameter
  #'if bars fall below refline the bars will be collored differently
  #'@param df_indeed_final dataframe with scraped indeed data
  #'@param add_ref_line if FALSE returns plot without ref line
  #'@param ref_salary if add_ref_line == TRUE and an integer provided add reffline on y-axis
  #'@return bar chart with or without reference line
  
  if(add_ref_line){ # check param for TRUE
    df_indeed_final %>%
      group_by(skill_count) %>%
      summarise(med= median(salary, na.rm = T)) %>% # ad median salary per group
      ggplot(
        aes(x=skill_count, y=med)) +
      geom_col(
        aes(
          fill = ifelse(med < ref_salary, '#07E597', '#2C435E') # color bars based on ref salary
        )
      )+
      geom_hline(linetype= 1,
                 yintercept = ref_salary # add horizontal line at y = ref_salary
      )+
      labs(
        title = "Mediaan salaris Indeed vs. aantal skills",
        subtitle = paste("Salaris \u20ac",ref_salary ,"als referentie lijn"), # dynamicaly create subtitle based on param
        y = "Mediaan salaris Indeed (\u20ac)",
        x = "Aantal skills"
      ) +
      theme_classic()+
      theme(
        legend.position= "none"
      )+
      scale_fill_manual(values=c('#07E597', '#2C435E')) # color bars
  }
  else{
    df_indeed_final %>%
      group_by(skill_count) %>%
      summarise(med= median(salary, na.rm = T)) %>%
      ggplot(
        aes(x=skill_count, y=med)) +
      geom_col(
        aes()
      )+
      labs(
        title = "Mediaan salaris Indeed vs. aantal skills",
        y = "Mediaan salaris Indeed (\u20ac)",
        x = "Aantal skills"
      )+
      theme_classic() +
      theme(
        legend.position= "none"
      )
  }

}


plot_indeed_timeseries_data <- function(df_indeed_skills, skill=""){
  #'plot timeserie with trendline for indeed data
  #'the trendline and timeserie can be plotted either for the whole dataset
  #'or for one skill
  #'@param df_indeed_skills a dataframe with exploded skills for skill plottint
  #'or normal dataframe for overall plotting
  #'@param skill the specific skill to plot
  
  if(skill==""){ #default to overal plot if skill is empty
    df_indeed_skills %>%
      group_by(listing_date) %>%
      summarise(req_per_day= n(), .groups= "keep") %>% # count over groups
      ggplot(
        aes(
          x= listing_date,
          y= req_per_day
        )
      ) +
      geom_line() +
      stat_smooth(method = "lm", # add linear trend line 
                  se= FALSE,
                  color="#07E597"
      ) +
      labs(
        title = "Instroom data vacatures ",
        y = "Aantal vacatures",
        x = "plaatsingsdatum"
      )+
      theme_classic() +
      scale_x_date(date_labels = "%a\n%d-%m", # make ticks on x-axis per week
                   date_breaks = "week"
      )
  }
  else{ # if skill is added
    #create dynamic title based on skill input
    var_title <- paste("Instroom vacatures voor de vaardigheid", skill)
      
    # get max count of single skill, used to keep height over plots consistent
    max_count <- df_indeed_skills_date_filtered %>%
      group_by(listing_date, skills) %>%
      summarise(req_per_day= n(), .groups= "keep") %>%
      .$req_per_day %>%
      max()
    
    df_indeed_skills %>%
      filter(skills == skill) %>%
      group_by(listing_date, skills) %>%
      summarise(req_per_day= n(), .groups= "keep") %>% # count per groups
      ggplot(
        aes(
          x= listing_date,
          y= req_per_day
            )
      ) +
      geom_line() +
      stat_smooth(method = "lm", # add trend line
                  se= FALSE,
                  color="#07E597"
                  )+
      labs(
        title = var_title, # use dynamic title
        y = "Aantal vacatures",
        x = "plaatsingsdatum"
      )+
      theme_classic() +
      scale_x_date(date_labels = "%a\n%d-%m",
                   date_breaks = "week"
                   ) +
      ylim(0, (max_count * 1.1)) # keep y-axis height consistent over different plots
    }
}

plot_job_type_per_company <- function(df_indeed_final, company_names){
  #' plot company appearances colored by job_type
  #' a horizontal barchart that show the frequentie of job posts by company
  #' each bar represents a company and is colored based on the count of job_types posted
  #'@param df_indeed_final scraped indeed daga
  #'@param company_names list of companies that should be analysed
  #'@returns horizontal bar plot with companies on y-axis and colored based on job_type
  
  df_indeed_final %>%
    filter(Company %in% company_names) %>%
    ggplot(
      aes(
        x = reorder( # decreasing order company
          Company, # order this
          Company, # based on this
          function(x) + length(x)  # given these values (counts == length)
        ),
        fill= job_type
      )
    ) +
    geom_bar() +
    coord_flip() + # turn horizontal
    labs(
      title= "Aantal vacatures per bedrijf per funtietype",
      y= "Bedrijf"
    )+
    theme_classic()+
    theme(axis.title.y=element_blank())+
    scale_fill_manual(values=HoB_color_palet) # Use HoB color palette for job_types
}

plot_skills_per_company <- function(df_indeed_skills, company_list,  skill_list){
  #' plot skill appearances colored by company
  #' a horizontal barchart that show the frequentie of  skills per company
  #' each bar represents a skill and is colored based on the frequency of companies
  #'@param df_indeed_skills scraped indeed data exploded on skills
  #'@param company_list list of companies that should be analysed
  #'@param skill_list list of skills that are of interest
  #'@returns horizontal bar plot with skills on y-axis and colored based by company
  df_indeed_skills_date_filtered %>%
    filter(
      Company %in% company_list &
        skills %in% skill_list) %>%
    ggplot(
      aes(
        x = reorder( # decreasing order skill
          skills, # order this
          skills, # using this
          function(x) + length(x) # given these values (counts == length)
        ),
        fill= Company
      )
    ) +
    geom_bar() + 
    coord_flip() + #make plot horizontal
    labs(
      title= "Aantal vacatures per skill per bedrijf"
    )+
    theme_classic()+
    theme(axis.title.y=element_blank())+
    scale_fill_manual(values=color_brewer) # use supplemental color palette on top of HoB's
}