  
  #============================================================Vincent Reperoga============================================================
  library("dplyr")
  library("tidyverse")
  library("ggplot2")
  library("scales")
  library("treemapify")
  library("wesanderson")
  
  
  #==============================================================Filtering of the Data Set==============================================================
  
  #loading the raw data sets
  bidding_oct_dec_2020 <- read.csv("D:\\BS AYTI 3\\1st Sem\\IT 116 Information Management 2\\Activities\\machine_excerise_1\\Banico_IM2_3A_ME1A_Bid Notice and Award Details Oct-Dec 2020.csv", skip = 3)
  View(bidding_oct_dec_2020)
  
  #filtering the BATAAN GENERAL HOSPITAL AND MEDICAL CENTER as data set interest
  bataan_hospital_dataset <- filter(bidding_oct_dec_2020, bidding_oct_dec_2020$Organization.Name == "BATAAN GENERAL HOSPITAL AND MEDICAL CENTER")
  view(bataan_hospital_dataset)
  
  bataan_hospital_dataset <- select(bataan_hospital_dataset, Organization.Name, Business.Category, Approved.Budget.of.the.Contract, Item.Name, Item.Desc, Quantity, Item.Budget)
  bataan_hospital_dataset$Quantity <- as.numeric(bataan_hospital_dataset$Quantity)
  
  #filtering the covid related items
  covid.related <- filter(bataan_hospital_dataset, bataan_hospital_dataset$Item.Desc == "COVID RELATED")
  view(covid.related)
  
  #filtering the pcr lab items
  pcr.lab <- filter(bataan_hospital_dataset, bataan_hospital_dataset$Item.Desc == "PCR Lab")
  view(pcr.lab)
  
  #filtering the covid use items
  covid.use <- filter(bataan_hospital_dataset, bataan_hospital_dataset$Item.Desc == "COVID USE")
  view(covid.use)
  
  #final filtered data set merge
  covid.data <- rbind(covid.related, covid.use, pcr.lab)
  view(covid.data)
  
  #saving the clean data set to csv
  write.csv(bataan_hospital_dataset,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\bataan_hospital_dataset_oct_dec_2020.csv")
  write.csv(covid.related,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\covid_related_dataset_oct_dec_2020.csv")
  write.csv(pcr.lab,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\pcr_lab_dataset_oct_dec_2020.csv")
  write.csv(covid.use,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\covid_use_dataset_oct_dec_2020.csv")
  write.csv(covid.data,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\covid_data_dataset_oct_dec_2020.csv")
  
  
  
  #==============================================================Visualizing the Data Set==============================================================
  
  options("scipen"=100, "digits"=4) #forcing R not to use exponential notation
  
  
  #1. Which among the business categories receives the largest percentage of BHGMC's approved contract budget?
  
  #get all the unique data in a row and order it alphabetically then remove the comma
  percent <- distinct(bataan_hospital_dataset, Business.Category, Approved.Budget.of.the.Contract)
  percent <- percent[order( percent$Business.Category),]
  percent$Approved.Budget.of.the.Contract <- as.numeric(gsub(',','',percent$Approved.Budget.of.the.Contract))
  
  #view(percent)
  
  #create a new column for the percentage
  percent <- percent %>% group_by(Business.Category) %>% 
    summarise(Approved.Budget.of.the.Contract= sum(Approved.Budget.of.the.Contract))%>%
    mutate(perc = (Approved.Budget.of.the.Contract/sum(Approved.Budget.of.the.Contract)))%>%
    mutate(labels = scales::percent(perc))
  
  #view(percent)
  
  #arrange in ascending order the approved budget contract
  percent <- percent[order( percent$Approved.Budget.of.the.Contract),]
  write.csv(percent,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\q1-BC-percentage.csv")
  budget <- paste0 (" Php ", percent$Approved.Budget.of.the.Contract)
  #view(percent)
  
  #colors of the graphs using wes_palette
  
  col <- wes_palette("Darjeeling1", n=18, type="continuous")
  
  #plotting the percentage of business category here using ggplot
  ggplot(percent, aes(area=perc,
                      fill=Business.Category,
                      label=labels))+
    geom_treemap()+
    scale_fill_manual(values = col)+
    labs(title=("Approved Contract Budget Percentage per Business Category of BGHMC in Oct-Dec 2020"))+
    geom_treemap_text(fontface = "bold",
                      colour = "#ffffff",
                      place = "centre",
                      grow = FALSE)+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    size =15),
          text = element_text(face = "bold",
                              size=9,
                              color = "#000000"))
  
  #plotting the ranking of the largest expenditure of business category uisng ggplot
  ggplot(percent, aes(x = reorder(Business.Category, Approved.Budget.of.the.Contract),
                      y = Approved.Budget.of.the.Contract, 
                      .desc = TRUE, 
                      fill = Business.Category))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = col)+
    labs(title=("Approved Contract Budget per Business Category of BGHMC in Oct-Dec 2020"))+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, 
                                    face = "bold", 
                                    size =10),
          text = element_text(face = "bold",
                              size=9, 
                              color = "#000000"),
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"))+
    xlab("Business Category")+
    ylab("Total Budget Allocation")+
    geom_text(aes(label=budget),
              position=position_dodge(width=1),
              hjust=-0.1,
              vjust=0.5,  
              label.padding = unit(0.25, "lines"),
              size=3)+
    coord_flip()
  
  #============================================================Jeffrey Banico============================================================
  
  #2. What are the items procured by BHGMC as measures to Covid-19 during the fourth quarter of the year 2020?  (item name and Quantity)
  
  #COVID-RELATED ITEMS
  no2.covid.related <- covid.related %>% 
    group_by(Item.Name) %>%
    summarise(Quantity= sum(Quantity))
  
  view(no2.covid.related)
  
  #shortening/renaming the values in the column Item.Name for easier viewing of the graph
  #(COVID-RELATED)
  no2.covid.related[no2.covid.related == "Critical cover with hood, (head to toe covering)"] <- "Critical Cover with Hood"
  no2.covid.related[no2.covid.related == "Disposable Surgical Mask 3 ply non woven with bact"] <- "Disposable Surgical Mask"
  no2.covid.related[no2.covid.related == "Face shield, non fog, transparent film protected"] <- "Face Shield"
  no2.covid.related[no2.covid.related == "Isolation gown, non woven, water repellant, free s"] <- "Isolation Gown"
  no2.covid.related[no2.covid.related == "N95 Facemask, fluid resistant, with bacterial filt"] <- "N95 Face Mask"
  no2.covid.related[no2.covid.related == "Nitrile Examination Gloves, non - sterile x 100's"] <- "Nitrile Examination Gloves"
  no2.covid.related[no2.covid.related == "OR Caps, non woven, disposable, gartered"] <- "OR Caps"
  no2.covid.related[no2.covid.related == "PPE Set Critical cover, (assorted sizes) 1 pc Isol"] <- "PPE Set Critical Cover"
  no2.covid.related[no2.covid.related == "Rubber boots, size"] <- "Rubber Boots"
  no2.covid.related[no2.covid.related == "Safety goggles closed with elastic bands"] <- "Safety Goggles"
  no2.covid.related[no2.covid.related == "Shoe cover, plastic   (50s/pack)"] <- "Shoe Cover"
  no2.covid.related[no2.covid.related == "Washable gown, micro fiber, free size"] <- "Washable Gown"
  
  
  write.csv(no2.covid.related,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\no2.covid.related.csv")
  
  #plotting the most procured covid-related items
  ggplot(no2.covid.related,
         aes(x = reorder(Item.Name, Quantity),
             y = Quantity, 
             .desc = TRUE, 
             fill = Item.Name))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = col)+
              geom_text(aes(label=Quantity), 
              position=position_dodge(width=1),
              hjust=-0.1, 
              vjust=0.2, 
              size=3)+
    labs(title=("Procured Covid-related Items of BGHMC in Oct-Dec 2020"))+
    theme(text = element_text(face = "italic",
                              size=11, 
                              color = "#000000"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, 
                                    face = "bold", 
                                    size =10),
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"))+
    xlab("Item Procured (Covid-related)")+
    ylab("Quantity")+
    scale_x_discrete(labels = label_wrap(20))+
    coord_flip()
  
  #COVID-USE ITEMS
  no2.covid.use <- covid.use %>%
    group_by(Item.Name) %>%
    summarise(Quantity= sum(Quantity))
  #shortening/renaming the values in the column Item.Name for easier viewing of the graph  
  no2.covid.use[no2.covid.use == "Favipiravir 200 mg"] <- "Favipiravir (200 mg)"
  no2.covid.use[no2.covid.use == "Oseltamivir (as phosphate) 75 mg"] <- "Oseltamivir (75 mg)"
  no2.covid.use[no2.covid.use == "Remdesivir 100mg Lyophilized Powder for Infusion w"] <- "Remdesivir (100 mg)"
  
  write.csv(no2.covid.use,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\no2.covid.use.csv")
  
  #plotting the most procured covid-use items
  ggplot(no2.covid.use,
         aes(x = reorder(Item.Name, Quantity),
             y = Quantity,
             .desc = TRUE,
             fill = Item.Name))+
    geom_bar(stat = "identity")+
    geom_text(aes(label=Quantity), 
              position=position_dodge(width=1),
              hjust=-0.1, 
              vjust=0.2, 
              size=4)+
    scale_fill_manual(values = c("#ADE8F4", "#8DB600", "#FFA500"),
                      labels = c("Favipiravir", "Oseltamivir", "Remdesivir"))+
    labs(title=("Procured Covid-use Items of BGHMC in Oct-Dec 2020"))+
    theme(text = element_text(face = "italic",
                              size=12, 
                              color = "#000000"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, 
                                    face = "bold", 
                                    size =10),
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"))+
    xlab("Item Procured (Covid-use)")+
    scale_x_discrete(labels = label_wrap(20))+
    coord_flip()
  
  #PCR LAB ITEMS
  no2.pcr.lab <- pcr.lab %>%
    group_by(Item.Name) %>%
    summarise(Quantity= sum(Quantity))
  
  #shortening/renaming the values in the column Item.Name for easier viewing of the graph
  #(PCR LAB)
  no2.pcr.lab[no2.pcr.lab == "1000ul Filtered tips 10x96/pack"] <- "Filtered Tips (1000 ul)"
  no2.pcr.lab[no2.pcr.lab == "10ul  Filtered tips 10x96/pack"] <- "Filtered Tips (10 ul)"
  no2.pcr.lab[no2.pcr.lab == "200ul Filtered tips 10x96/pack"] <- "Filtered Tips (200 ul)"
  no2.pcr.lab[no2.pcr.lab == "8 strip tube with cap 1.2ml low profile optically"] <- "8 Strip Tube with Cap (1.2 ml)"
  no2.pcr.lab[no2.pcr.lab == "Biohazard Autoclavable bag(80cmx60cm)"] <- "Biohazard Autoclavable Bag"
  no2.pcr.lab[no2.pcr.lab == "Microcentrifuge tube 1.5-2.0 ml 500/pack"] <- "Microcentrifuge Tube (1.5 - 2.0 ml)"
  no2.pcr.lab[no2.pcr.lab == "Microcentrifuge tube rack 2.0 ml"] <- "Microcentrifuge Tube Rack (2.0 ml)"
  no2.pcr.lab[no2.pcr.lab == "N95 Respirator and Surgical Mask 1860 (green)"] <- "N95 Respirator and Surgical Mask"
  no2.pcr.lab[no2.pcr.lab == "Nucleic Acid Diagnostic PCR kit (Flourescense Prob"] <- "Nucleic Acid Diagnostic PRC Kit"
  no2.pcr.lab[no2.pcr.lab == "PCR Plate low profile, unskirted, clear MLL9601"] <- "PCR Plate"
  no2.pcr.lab[no2.pcr.lab == "PCR Plate Sealing film, adhesive,opticaly clear MS"] <- "PCR Plate Sealing Film"
  no2.pcr.lab[no2.pcr.lab == "PCR tube Cooling Rack"] <- "PCR Tube Cooling Rack"
  no2.pcr.lab[no2.pcr.lab == "PPE set Type A"] <- "PPE Set (Type A)"
  no2.pcr.lab[no2.pcr.lab == "PPE Type B"] <- "PPE Set (Type B)"
  no2.pcr.lab[no2.pcr.lab == "Test Tube rack 5-10 ml( Autoclavable)"] <- "Test Tube Rack (5 - 10 ml)"
  no2.pcr.lab[no2.pcr.lab == "Viral RNA Extraction Kit with 8 strip tip 32T/kit"] <- "Viral RNA Extraction Kit"
  no2.pcr.lab[no2.pcr.lab == "VTM tube 3.0 ml with OPS & NPS 25's"] <- "VTM Tube (3.0 ml)"
  
  write.csv(no2.pcr.lab,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\no2.pcr.lab.csv")
  
  #plotting the most procured pcr lab items
  ggplot(no2.pcr.lab, aes(x = reorder(Item.Name, Quantity), 
                          y = Quantity, 
                          .desc = TRUE, 
                          fill = Item.Name))+
    geom_bar(stat = "identity")+
    geom_text(aes(label=Quantity), 
              position=position_dodge(width=1),
              hjust=-0.2, 
              vjust=0.5, 
              size=3)+
    scale_fill_manual(values = col)+
          theme(text = element_text(face = "italic",
                              size=10, 
                              color = "#000000"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, 
                                    face = "bold", 
                                    size =10),
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"))+
    xlab("Item Procured (PCR Lab)")+
    ylab("Quantity")+
    labs(title=("Procured PCR Lab Items of BGHMC in Oct-Dec 2020"))+
    scale_x_discrete(labels = label_wrap(20))+
    coord_flip()
  
  #TOP 3 MOST PROCURED ITEMS
  top.covid.related <- top_n(covid.related, n=3, Quantity)
  top.covid.use <- top_n(covid.use, n=3, Quantity)
  top.pcr.lab <- top_n(pcr.lab, n=3, Quantity)
  
  #merging
  top3 <- rbind(top.covid.related, top.pcr.lab, top.covid.use)
  
    #plotting
    ggplot(top3, aes(x = reorder(Item.Name, Quantity), 
                     y = Quantity, 
                     .desc = TRUE))+
      geom_bar(stat = "identity", 
               fill = "#166e6a")+
      facet_wrap(~ Item.Desc)+
      geom_text(aes(label=Quantity), 
                position = position_dodge(width=1), 
                angle = -90,
                hjust=0.5, 
                vjust=-1, 
                size=3)+
      labs(title=("Top 3 Most Procured Items of BGHMC per Item Description in Oct-Dec 2020"))+
      theme(text = element_text(face = "bold",
                                size=10, 
                                color = "#000000"),
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold", 
                                      size =10),
            panel.border = element_rect(color="black",
                                         fill=NA,
                                         size=1),  
            # Remove panel grid lines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "black"))+
      xlab("Item Procured")+
      ylab("Quantity")+
      scale_x_discrete(labels = label_wrap(20))+
      coord_flip()
    
    
  #============================================================Chistine Bolanos============================================================
    
  #3. What is the budget allocation per item procured by BHGMC as Covid-19 measures during the fourth quarter of the year 2020?
  
    #covid.related conversion and filtering
    covid.related$Item.Budget <- as.numeric(gsub(",","",covid.related$Item.Budget))
    
    xample1 <- covid.related %>%
      group_by(Item.Name) %>% summarise(Item.Budget= sum(Item.Budget))
    write.csv(xample1,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\xample1.csv")
    
    #covid.related plotting
    label1 <- paste0 (" Php ", xample1$Item.Budget)
    
    ggplot(xample1, aes(x=reorder(Item.Name, Item.Budget), 
                        y=Item.Budget, 
                        fill=Item.Name))+
      geom_bar(stat = "identity") +
      geom_text(aes(label=label1), 
                position=position_dodge(width=1),
                hjust=0, 
                vjust=0.3, 
                size=3)+
      scale_fill_manual(values= col)+
      labs(title=("Budget Allocation of BGHMC for Covid-related Items (Oct-Dec 2020)"))+
      theme(text = element_text(face = "italic",
                                size=10, 
                                color = "#000000"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold", 
                                      size =10),
            panel.border = element_blank(),  
            # Remove panel grid lines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey"))+
      xlab("Item Procured (Covid Related)")+
      ylab("Budget Allocation per Item")+
      scale_x_discrete(labels=label_wrap_gen(20))+
      coord_flip()
    
    #covid.use plotting
    covid.use$Item.Budget <- as.numeric(gsub(",","",covid.use$Item.Budget))
    
    xample2<- covid.use %>%
      group_by(Item.Name) %>%
      summarise(Item.Budget= sum(Item.Budget))
    write.csv(xample2,"C:\\Users\\Jeffrey Banico\\Desktop\\R\\midterm-proj\\dataset\\xample2.csv")
    
    label2 <- paste0 (" Php ", xample2$Item.Budget)
    
    ggplot(xample2, aes(x=reorder(Item.Name, Item.Budget),
                        y=Item.Budget,
                        fill=Item.Name))+
      geom_bar(stat = "identity") +
      geom_text(aes(label=label2),
                position=position_dodge(width=1),
                hjust=-0.1,
                vjust=1,
                size=3)+
      scale_fill_manual(values=c("#ADE8F4", "#8DB600", "#FFA500"))+
      theme(text = element_text(face = "italic",
                                size=12, 
                                color = "#000000"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold", 
                                      size =10),
            panel.border = element_blank(),  
            # Remove panel grid lines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey"))+
      xlab("Item Procured (Covid-Use)")+
      ylab("Budget Allocation per Item")+
      labs(title=("Budget Allocation of BGHMC for Covid-use Items (Oct-Dec 2020)"))+
      scale_x_discrete(labels=label_wrap_gen(20))+
      coord_flip()
    
    
    #PCR.lab plotting
    pcr.lab$Item.Budget <- as.numeric(gsub(",","",pcr.lab$Item.Budget))
    
    xample3<- pcr.lab%>%
      group_by(Item.Name) %>%
      summarise(Item.Budget= sum(Item.Budget))
    write.csv(xample3,"D:\\xample3.csv")
    
    
    
    label3 <- paste0 (" Php ", xample3$Item.Budget)
    
    ggplot(xample3, aes(x=reorder(Item.Name, Item.Budget),
                        y=Item.Budget, 
                        fill=Item.Name))+
      geom_bar(stat = "identity") +
      geom_text(aes(label=label3), position=position_dodge(width=1),
                hjust=-0.1, 
                vjust=0, 
                size=2.5)+
      
      scale_fill_manual(values=col)+
      theme(text = element_text(face = "italic",
                                size=8, 
                                color = "#000000"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, 
                                      face = "bold", 
                                      size =10),
            panel.border = element_blank(),  
            # Remove panel grid lines
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey"))+
      xlab("Item Procured (PCR Lab)")+
      ylab("Budget Allocation per Item")+
      labs(title=("Budget Allocation of BGHMC for PCR Lab Items (Oct-Dec 2020)"))+
      scale_x_discrete(labels=label_wrap_gen(20))+
      coord_flip()
    
    #total allocation budget of the BHGMC in terms of Covid related, covid use and pcr lab
    
    #get the total budget for covid.related dataset
    sum.covid.related <- xample1 %>% 
      summarise(sum(xample1$Item.Budget))
    
    #get the total budget for covid.use dataset
    sum.covid.use <- xample2 %>% 
      summarise(sum(xample2$Item.Budget))
    
    #get the total budget for pcr.lab dataset
    sum.pcr.lab <- xample3 %>% 
      summarise(sum(xample3$Item.Budget))
    
    #make a dataframe with the ItemDescription to be binded to the sumOfTables dataset
    BudgetTotal <- data.frame(
      Item.Description= c("Covid.Related.Items", "Covid.Use.Items", "PCR.Lab.Items"))
    
    #make a dataframe to compile all the total budget for each items
    sumOfTables<- data.frame(
      c(sum.covid.related$`sum(xample1$Item.Budget)`, 
        sum.covid.use$`sum(xample2$Item.Budget)`, 
        sum.pcr.lab$`sum(xample3$Item.Budget)`))
    
    #combine the BudgetTotal and sumOfTables dataframes
    Summary <- cbind(BudgetTotal,sumOfTables)
    
    #modify column names for proper naming standards
    names(Summary) <- c("Item.Description", "Total.Budget")
    
    #calculate the percentage for each total budget
    pct <- round(100*Summary$Total.Budget/sum(Summary$Total.Budget))
    
    #combine the calculation of the percentage to the dataframe summary
    Summary <- cbind(Summary, pct)
    
    #sorting percentage in ascending order
    Summary$Item.Description <- Summary$Item.Description[order(Summary$pct)]; 
    Summary$pct <- sort(Summary$pct)
    #converting Item.Description as factor level
    Summary$Item.Description.factor <- factor(Summary$Item.Description, 
                                              levels = as.character(Summary$Item.Description))
    #pie chart for the total budget allocation
    #label for the pie chart
    lbl <- paste(Summary$Item.Description.factor,"\n", paste(sep = " ", Summary$pct, "%"))

    ggplot(data = Summary, aes(x ="", 
                               y = -pct, fill = Item.Description.factor)) +
      geom_bar(width=2, 
               stat = "identity") +
      coord_polar("y", start=0) +
      geom_label(aes(label = lbl),
                position = position_stack(vjust = 0.6),
                size=4) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold",hjust = 0.5, size=15))+
      scale_fill_manual(values=c("#ADE8F4", "#8DB600", "#FFA500")) +
      labs(title    = "Summary of BGHMC's Budget Allocation for Covid Measures")
    