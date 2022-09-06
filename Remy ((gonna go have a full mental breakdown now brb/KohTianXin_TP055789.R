# KOH TIAN XIN
# TP055789



# install packages
install.packages("ggplot2") 
install.packages("ggpubr")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tables")
install.packages("tidyverse")
install.packages("ggridges")
install.packages("ggthemes")

# load packages
library(ggplot2)
library(ggpubr)
library(magrittr)
library(dplyr)
library(tables)
library(patchwork)
library(tidyverse)
library(scales)
library(ggridges)
library(ggthemes)

# DATA IMPORT
data <- read.csv("D:\\APU Stuff\\Bachelor of Computer Science (Intelligent Systems) [APD2F2202CS(IS)]\\Semester 1\\Programming for Data Analysis [CT127-3-2-PFDA]\\Assignment\\Remy ((gonna go have a full mental breakdown now brb\\Placement_Data_Full_Class.csv", header = TRUE)
View(data)

# DATA EXPLORATION
data_exploration = function(){
  # check values
  check_values = function(){
    print(summary(data))
    
    # check number of columns
    print(c("The number of columns",ncol(data))) # 25 columns
    
    # check number of rows
    print(c("The number of rows",nrow(data))) # 17007 rows
    
    #check NA values
    print(c("The number of missing value in salary before pre-processing",sum(is.na(data$salary)))) # 8265 rows
  }
  check_values()
  print(str(data))
}
data_exploration()

# DATA CLEANING
data_cleaning = function() {
  
  # check for duplicated data 
  ducplicate_data = function(){
    data %>% group_by(sl_no) %>% 
      filter(n() > 1) %>% summarise(count = n()) %>% print()
  }
  ducplicate_data() # no duplicate data
}
data_cleaning()

# DATA PREPROCESSING 
data_pre_processing = function(){
  
  assign_data = function(data){
    data$ssc_round <- round(data$ssc_p)
    data <- data %>% mutate(ssc_grade = case_when(ssc_round >= 0  & ssc_round <= 39 ~ 'E',
                                                  ssc_round >= 40  & ssc_round <= 49 ~ 'D',
                                                  ssc_round >= 50  & ssc_round <= 64 ~ 'C',
                                                  ssc_round >= 65  & ssc_round <= 79 ~ 'B',
                                                  ssc_round >= 80  & ssc_round <= 100 ~ 'A'))
    data$hsc_round <- round(data$hsc_p)
    data <- data %>% mutate(hsc_grade = case_when(hsc_round >= 0  & hsc_round <= 39 ~ 'F',
                                                  hsc_round >= 40  & hsc_round <= 44 ~ 'E',
                                                  hsc_round >= 45  & hsc_round <= 49 ~ 'D',
                                                  hsc_round >= 50  & hsc_round <= 59 ~ 'C',
                                                  hsc_round >= 60  & hsc_round <= 69 ~ 'B',
                                                  hsc_round >= 70  & hsc_round <= 100 ~ 'A'))
    data$degree_round <- round(data$degree_p)
    data <- data %>% mutate(degree_grade = case_when(degree_round >= 0  & degree_round <= 39 ~ 'F',
                                                     degree_round >= 40  & degree_round <= 49 ~ 'D',
                                                     degree_round >= 50  & degree_round <= 64 ~ 'C',
                                                     degree_round >= 65  & degree_round <= 74 ~ 'B',
                                                     degree_round >= 75  & degree_round <= 100 ~ 'A'))
    data$mba_round <- round(data$mba_p)
    data <- data %>% mutate(mba_grade = case_when(mba_round >= 0  & mba_round <= 39 ~ 'F',
                                                  mba_round >= 40  & mba_round <= 49 ~ 'D',
                                                  mba_round >= 50  & mba_round <= 64 ~ 'C',
                                                  mba_round >= 65  & mba_round <= 74 ~ 'B',
                                                  mba_round >= 75  & mba_round <= 100 ~ 'A'))
  }
  return(graded_data <- data %>% assign_data)
}
data_pre_processing()



# Question 1: What are the factors affecting the student's performance in school?

# Analysis 1-1: Find the number of the student with their grade performance in every state of education.
#--------------------------------------------------------------------------------
Q1A1 = function() {
  ssc_grade_count <- graded_data %>% group_by(ssc_grade) %>% summarise(counts = n())
  print(ssc_grade_count)
  
  a <- ggplot(ssc_grade_count, aes(x = ssc_grade, y = counts, fill = ssc_grade)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = counts), vjust = -0.3) +
      theme_minimal() +
      scale_fill_grey() +
      ggtitle("Quantity of student based on\nSecondary Student's Grade") +
      labs(x = "Secondary Education Grade", y = "Quantity of Student", fill = "Grade")
  
  hsc_grade_count <- graded_data %>% group_by(hsc_grade) %>% summarise(counts = n())
  print(hsc_grade_count)
  
  b <- ggplot(hsc_grade_count, aes(x = hsc_grade, y = counts, fill = hsc_grade)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = counts), vjust = -0.3) +
      theme_minimal() +
      scale_fill_brewer(palette="Blues") +
      ggtitle("Quantity of student based on Higher\nSecondary Student's Grade") +
      labs(x = "Higher Secondary Education Grade", y = "Quantity of Student", fill = "Grade")
  
  degree_grade_count <- graded_data %>% group_by(degree_grade) %>% summarise(counts = n())
  print(degree_grade_count)
  
  c <- ggplot(degree_grade_count, aes(x = degree_grade, y = counts, fill = degree_grade)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_minimal() +
    scale_fill_brewer(palette="Reds") +
    ggtitle("Quantity of student based on\nUndergraduate Grade") +
    labs(x = "Undergraduate Grade", y = "Quantity of Student", fill = "Grade")
  
  mba_grade_count <- graded_data %>% group_by(mba_grade) %>% summarise(counts = n())
  print(mba_grade_count)
  
  d <- ggplot(mba_grade_count, aes(x = mba_grade, y = counts, fill = mba_grade)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    theme_minimal() +
    scale_fill_brewer(palette="Greens") +
    ggtitle("Quantity of student based on\nPostgraduate Grade") +
    labs(x = "Postgraduate Grade", y = "Quantity of Student", fill = "Grade")
  
  return(a + b + c + d)
} 
Q1A1()
#---------------------------------------------------------------------------------
# Students in higher secondary education score better than in other education level.
# This is because SPM is much easier to score and SPM is actually easier than other exams.
# According to an article in The Star (Goh, 2013), examiners say the passing grade for certain
# subjects could be as low as 20 marks, or possibly lower. Students can score better marks
# because trial SPM exam is harder and this will encourage students to study harder to get ready
# for SPM exam. 



# Analysis 1-2: Find the relationship between parent's job and education and
# student's performance.
#--------------------------------------------------------------------------------
Q1A2 = function() {
  mother_data <- graded_data %>%
    group_by(Mjob, Medu) %>%
    summarise(mean_all = mean(ssc_round + hsc_round + degree_round + mba_round)/4)
  print(mother_data)
  
  a1 <- ggplot(mother_data, aes(y=mean_all, x=factor(Mjob), fill = factor(Medu))) + 
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() +
    scale_fill_brewer(palette="Reds") +
    ggtitle("Average of Every Education\nLevel's Percentage Based\non Mother's Education and Job") +
    labs(x="Mother's Job",
         y="Average of Student's Performance (%)",
         fill = "Mother's Level\nof Education") +
    coord_cartesian(ylim = c(70,80)) +
    scale_x_discrete(breaks = c("at_home", "health", "other", "services", "teacher"),
                     labels = c(at_home = "Homemaker", health = "Health-Related",
                                other = "Others", services = "Service-Related", teacher = "Teacher"))
  
  father_data <- graded_data %>%
    group_by(Fjob, Fedu) %>%
    summarise(mean_all = mean(ssc_round + hsc_round + degree_round + mba_round)/4)
  print(father_data)
  
  b1 <- ggplot(father_data, aes(y=mean_all, x=factor(Fjob), fill = factor(Fedu))) + 
    geom_bar(position="dodge", stat="identity") +
    theme_minimal() +
    scale_fill_brewer(palette="Blues") +
    ggtitle("Average of Every Education\nLevel's Percentage Based\non Father's Education and Job") +
    labs(x="Father's Job",
         y="Average of Student's Performance (%)",
         fill = "Father's Level\nof Education") +
    coord_cartesian(ylim = c(70,80))+
    scale_x_discrete(breaks = c("at_home", "health", "other", "services", "teacher"),
                     labels = c(at_home = "Homemaker", health = "Health-Related",
                                other = "Others", services = "Service-Related", teacher = "Teacher"))
  
  c1 <- ggarrange(a1, b1, heights = c(5, 5),
                  ncol = 1, nrow = 2, align = "v")
  return(c1)
}
Q1A2()
#--------------------------------------------------------------------------------
# Based on the graphs, father's job and education does not seem to affect too much of a 
# difference in student's performance in school. On the other hand, mother that has low 
# education level with other kind of jobs causes student's performance to be better.
# Mother's job and education level has a higher probability to affect student's performance. 
# The types of mother's job may be related to entrepreneur or opening a business since they do not
# requires high level of education. As stated in the website (Walker, 2020), many massively
# successful entrepreneurs today rose without any college degrees. All they needed was a little
# motivation, start up money and big ideas.



# Analysis 1-3: Find the relationship between family environment and student's
# choice regarding the board of education.
#--------------------------------------------------------------------------------
Q1A3 = function() {
  environment_data <- graded_data %>% group_by(internet, famsup, ssc_b) %>% summarise(count = n())
  print(environment_data)
  
  a2 <- ggplot(environment_data, aes(x = ssc_b, y = count, shape=internet, color= famsup)) + 
    geom_point(size = 5) +
    ggtitle("Number of Students in Board of Secondary School") + 
    labs(x="Board of Secondary Education",
         y="Number of Students",
         shape = "Internet Access",
         color = "Family Support")+
    scale_shape_discrete(breaks = c("no", "yes"), labels = c(no = "No", yes = "Yes")) +
    scale_color_discrete(breaks = c("no", "yes"), labels = c(no = "No", yes = "Yes"))
    
  
  environment_data <- graded_data %>% group_by(internet, famsup, hsc_b) %>% summarise(count = n())
  print(environment_data)
  
  b2 <- ggplot(environment_data, aes(x = hsc_b, y = count, shape=internet, color= famsup)) + 
    geom_point(size = 5) +
    ggtitle("Number of Students in Board of Higher Secondary School") + 
    labs(x="Board of Higher\nSecondary Education",
         y="Number of Students",
         shape = "Internet Access",
         color = "Family Support")+
    scale_shape_discrete(breaks = c("no", "yes"), labels = c(no = "No", yes = "Yes")) +
    scale_color_discrete(breaks = c("no", "yes"), labels = c(no = "No", yes = "Yes"))
  
  c2 <- ggarrange(a2, b2, heights = c(5, 5),
                  ncol = 1, nrow = 2, align = "v", common.legend = TRUE)
  
  d2 <- annotate_figure(c2,
                        top = text_grob("Number of students in board of secondary and higher secondary\nschool based on family support and internet access.",
                                        color = "Red",
                                        face = "bold",
                                        size = 16))
  return(d2)
}
Q1A3()
#--------------------------------------------------------------------------------
# Based on the graph, there are larger amount of students with internet access at home compared
# to the students who does not have internet access at home. The secondary education graph has shown that 
# students in private school mostly are not having family educational support. This can be
# a part of the students are retrieving scholarships from school. This can be concluded with
# students with no internet access at home will more likely to choose to study in central or state school.
# Students with no family support will also more likely to choose to study in central or state higher
# education school.



# Analysis 1-4: Find the relationship between paid classes and student's performance.
#--------------------------------------------------------------------------------
Q1A4 = function() {
  facet_wrap_names <- as_labeller(
    c(`ssc_grade` = "Secondary",
      `hsc_grade` = "Higher Secondary",
      `degree_grade` = "Undergraduate",
      `mba_grade` = "Postgraduate"))
  
  paid_data <- gather(graded_data, key="lvl_edu", value="grade", 27,29,31,33)
  paid_data <- paid_data %>% group_by(lvl_edu, paid, grade) %>% summarise(count = n())
  print(paid_data)
  
  a3 <- ggplot(paid_data, aes(x=factor(grade), y=count, group=factor(paid))) +
    geom_line(aes(color=factor(paid)))+
    geom_point(aes(color=factor(paid)))+
    facet_wrap(~factor(lvl_edu), labeller = facet_wrap_names) +
    scale_color_manual(values=c("#FF0000", "#33FF66"),
                       breaks = c("no", "yes"),
                       labels = c(no = "No", yes = "Yes")) +
    theme_dark() +
    ggtitle("Number of Students in every education level\nbased on availability of Paid Classes") + 
    labs(x="Grade", y="Number of Students", color = "Attended\nAny Paid\nClasses?")

  return(a3)
}
Q1A4()
#--------------------------------------------------------------------------------
# Based on the graph shown, although there is not much of a difference in student's
# performance with or without any paid classes. Yet there is a slightly difference 
# in between which there are more students in every education level that does not sign
# up for any extra paid classes. In higher secondary, there are more students who does
# not sign up for any paid classes yet still able to score an average score(B and C grade)
# In MBA level, we can see that there are more students who does not sign up for paid
# classes which also able to get high marks.



# Analysis 1-5: Find the relationship between board of education and student's performance.
#--------------------------------------------------------------------------------
Q1A5 = function() {
  facet_wrap_names <- as_labeller(
    c(`hsc_b` = "Higher Secondary", `ssc_b` = "Secondary"))
  
  board_data <- gather(graded_data, key="lvl_edu", value="grade", 27,29)
  board_data <- gather(board_data, key="lvl_edu_b", value="board", 14,16 )
  
  data_count <- board_data %>% group_by(lvl_edu_b, board) %>% summarise(count = n())
  board_data <- board_data %>% group_by(lvl_edu_b, board, grade) %>% summarise(count = n())
  
  #filter failed student count
  fail_count <- board_data %>% group_by(lvl_edu_b, board, grade) %>% filter(grade == "F")
  
  print(board_data)
  print(data_count)
  print(fail_count)
  
  a4 <- ggplot(board_data, aes(x=factor(board), y=count, group=factor(grade))) +
    geom_line(aes(color=factor(grade)))+
    geom_point(aes(color=factor(grade)))+
    facet_wrap(~factor(lvl_edu_b), labeller = facet_wrap_names) +
    scale_color_manual(values=c("#FF0033", "#CC9900", "#66FF00", "#33CCFF", "#FFCCFF", "#CC00FF")) +
    theme_minimal() +
    ggtitle("Student's Grade based on their choice of Board of Education") + 
    labs(x="Board of Education", y="Number of Students", color = "Grade\nAchieved")
  
  # to see number of students with their chosen board of education
  b4 <- ggplot(data_count, aes(x=lvl_edu_b, y=count, fill=board)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_grey() +
    theme_minimal() +
    geom_text(aes(label=count), vjust=1.6, color="white", position = position_dodge(0.9), size=3) +
    ggtitle("Number of students by Board of Education") + 
    labs(x="Level of Education", y="Number of Students", fill = "Board") +
    coord_cartesian(ylim = c(11000,11600)) +
    scale_x_discrete(breaks = c("hsc_b", "ssc_b"),
                     labels = c(hsc_b = "Higher Secondary", ssc_b = "Secondary"))
  
  # to see number of students that has failed
  c4 <- ggplot(fail_count, aes(x=factor(lvl_edu_b),y=count, fill=board)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(breaks=c(0,1,2)) +
    theme_minimal()+
    ggtitle("Number of failed students by Board of Education") + 
    labs(x="Level of Education", y="Number of Students", fill = "Board") +
    scale_x_discrete(breaks = c("hsc_b", "ssc_b"),
                     labels = c(hsc_b = "Higher Secondary", ssc_b = "Secondary"))
  
  d4 <- ggarrange(a4,
                  ggarrange(b4, c4,
                            ncol = 2, labels = c("B", "C")),
                  heights = c(2, 0.8),
                  ncol = 1,
                  labels = "A")
  
  return(d4)
}
Q1A5()
#--------------------------------------------------------------------------------
# According to the graph, 100% of the students that studied in central school have
# passed their exams in secondary and higher secondary school. There were 0% of failure
# for students studied in central school. In fact in Graph C, there were two students who failed
# their studies in private higher secondary school. Graph B shows that there were lesser
# student who studied in state school than in private school yet the students in state school
# achieved more better results than in private school. State schools are generally schools
# that educate all children with free of charge. Based on a study by The National Center for
# Education Statistics showed public school teachers tend to have more experience
# and qualifications than those at private schools as public school teachers must be certified
# (Parenting Special Needs Magazine, 2018).
  
  

# Question 2: What will be affected by living in rural or urban areas?

# Analysis 2-1: Find the relationship between students living in rural or urban areas
# paid classes and extra-curricular activities.
#--------------------------------------------------------------------------------
Q2A1 = function() {
  
  address_data <- graded_data %>% group_by(address, paid) %>% summarise(count=n())
  print(address_data)
  
  address_activity_data <- graded_data %>% group_by(address, activities) %>% summarise(count=n())
  print(address_activity_data)
  
  a5 <- ggplot(address_data, aes(address, count)) +
    geom_linerange(
      aes(x = address, ymin = 4000, ymax = count), 
      color = "lightgray", size = 3.5)+
    geom_point(aes(color = paid), size = 7)+
    ggpubr::color_palette("jco")+
    theme_pubclean() +
    ggtitle("Number of students with or without paid classes\nthat live in Rural or Urban area") + 
    labs(x="Address", y="Number of Students", color = "Any Paid Classes?") +
    scale_x_discrete(breaks = c("R", "U"), labels = c(R = "Rural", U = "Urban"))
  
  b5 <- ggplot(address_activity_data, aes(address, count)) +
    geom_linerange(
      aes(x = address, ymin = 4000, ymax = count), 
      color = "lightgray", size = 3.5)+
    geom_point(aes(color = activities), size = 7)+
    ggpubr::color_palette("jco")+
    theme_pubclean() +
    ggtitle("Number of students with or without extra-curricular\nactivities that live in Rural or Urban area") + 
    labs(x="Address", y="Number of Students", color = "Any Extra-curricular Activities?") +
    scale_x_discrete(breaks = c("R", "U"), labels = c(R = "Rural", U = "Urban"))
  
  repeat {
    print("(A) Number of students with/without Paid Classes based on area.");
    print("(B) Number of students with/without Extra-curricular Activities based on area.");
    print("(C) Quit")
    option = as.character(readline(prompt = "Select between A, B or C: "));
    results = switch(option, "A" = print(a5), "B" = print(b5), "C" = break, "error");
  }
}
Q2A1()
#--------------------------------------------------------------------------------
# Based on the graph shown, there are more students living in urban area than in the
# rural area. However, this does not prevent students in rural area to sign up for 
# extra classes. There are more students in urban areas that does not participate in 
# any extra classes. This may because students in urban area are able to easily get in
# contact with more extra resources either from their local friends or from within the town
# itself. People living in rural areas may find it difficult to get more resources, thus
# some family decided to sign their child up for any extra classes. The result also shows that
# students living in rural area are signing up for extra-curricular activities This may be
# these activities are free of charge and the people living in rural areas have more of their
# free time. There are also a high number of students living in urban area signing up
# for extra-curricular activities. This can be caused by higher standards in the town or
# there are more variety of activities to choose from in the town rather than in countryside.



# Analysis 2-2: Find the relationship between students living in rural or urban areas
# and family support.
#--------------------------------------------------------------------------------
Q2A2 = function() {
  address_family_data <- graded_data %>% group_by(address, famsup) %>% summarise(count=n())
  print(address_family_data)
  
  a6 <- ggplot(address_family_data, aes(address, count)) +
  geom_linerange(
    aes(x = address, ymin = 4000, ymax = count), 
    color = "lightgray", size = 3.5)+
  geom_point(aes(color = famsup), size = 7)+
  ggpubr::color_palette("jco")+
  theme_pubclean() +
  ggtitle("Number of students with or without family support\nthat live in Rural or Urban area") + 
  labs(x="Address", y="Number of Students", color = "Any Family Support?") +
  scale_x_discrete(breaks = c("R", "U"), labels = c(R = "Rural", U = "Urban"))
   
  return(a6)
}
Q2A2()
#--------------------------------------------------------------------------------
# Based on Graph B, it shows that more students living in urban and rural area does
# not have family support for their studies. This could be students nowadays are
# more independent for taking care of themselves.



# Analysis 2-3: Find the relationship between students living in rural or urban areas
# and student's performance.
#--------------------------------------------------------------------------------
Q2A3 = function() {
  facet_wrap_names <- as_labeller(
    c(`ssc_grade` = "Secondary",
      `hsc_grade` = "Higher Secondary",
      `degree_grade` = "Undergraduate",
      `mba_grade` = "Postgraduate"))
  
  address_performance_data <- gather(graded_data, key="lvl_edu", value="grade", 27,29,31,33)
  address_performance_data <- address_performance_data %>%
    group_by(lvl_edu, address, grade) %>%
    summarise(count = n())
  print(address_performance_data)
  
   
  a7 <- ggscatter(address_performance_data, x = "grade", y = "count",
            conf.int = TRUE,
            color = "address",
            shape = "address",
            size = 3)+
    stat_cor(aes(color = grade), label.x = 3) +
    theme_stata() +
    ggtitle("Number of student's performance grade based on address") + 
    labs(x="Grade", y="Number of Students", color = "Address", shape = "Address") +
    scale_color_discrete(breaks = c("R", "U"), labels = c(R = "Rural", U = "Urban")) +
    scale_shape_discrete(breaks = c("R", "U"), labels = c(R = "Rural", U = "Urban")) +
    facet_wrap(~factor(lvl_edu), labeller = facet_wrap_names)
  
  return(a7)
}
Q2A3()
#--------------------------------------------------------------------------------
# Based on the graph, the students that live in urban areas are more than students that live in
# rural areas. In higher secondary school, there are still students live in rural area that scored 
# grade E and F. Based on the graphs, the students in rural areas are also able to score better marks
# if compared with students in urban areas. Although the students in urban areas scored better marks
# are more than students in rural areas, the students in rural areas are also competitive. This
# shows that no matter where students live, they can always achieve good score if they are hardworking.



# Question 3: What affects some students to not able to secure a job?

# Analysis 3-1: Determine the relationship between student's status of placement
# and working experience.
#--------------------------------------------------------------------------------
Q3A1 = function() {
  experience_data <- data %>% group_by(workex, status) %>% summarise(count=n())
  experience_data_no <- experience_data %>% filter(workex == "No")
  
  experience_data <- data %>% group_by(workex, status) %>% summarise(count=n())
  experience_data_yes <- experience_data %>% filter(workex == "Yes")
  
  experience_data_no$percentage <- round(experience_data_no$count/sum(experience_data_no$count) * 100,0)
  experience_data_no$pos <-  100 - (cumsum(experience_data_no$percentage) - 
                                      sapply(experience_data_no$percentage,
                                             function(x) cumsum(x) - 0.5 * x))
  
  experience_data_yes$percentage <- round(experience_data_yes$count/sum(experience_data_yes$count) * 100,0)
  experience_data_yes$pos <-  100 - (cumsum(experience_data_yes$percentage) -
                                       sapply(experience_data_yes$percentage,
                                              function(x) cumsum(x) - 0.5 * x))
  
  print(experience_data)
  
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  
  a8 <- ggplot(experience_data_no, aes(x="", y=percentage, fill=status))+
    geom_bar(width = 1, stat = "identity")+
    geom_text(aes(x="", y=pos, label=paste(percentage,"%"))) +
    scale_fill_manual(values = c("Not Placed" = "#b2df8a", "Placed" = "#238b45")) +
    ggtitle("With Working Experience") + 
    labs(y="Number of Students (%)", fill = "Status of Placement")
    
  a8 <- a8 + coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank()) 
  
  b8 <- ggplot(experience_data_yes, aes(x="", y=percentage, fill=status))+
    geom_bar(width = 1, stat = "identity")+
    geom_text(aes(x="", y=pos, label=paste(percentage,"%"))) +
    scale_fill_manual(values = c("Not Placed" = "#b2df8a", "Placed" = "#238b45")) +
    ggtitle("Without Working Experience") + 
    labs(y="Number of Students (%)", fill = "Status of Placement")
  
  b8 <- b8 + coord_polar("y", start=0) + blank_theme +
    theme(axis.text.x=element_blank())
  
  c8 <- ggarrange(a8, b8, 
            ncol = 2, nrow = 1,  align = "v", 
            widths = c(2, 2), heights = c(2, 2),
            common.legend = TRUE)
  
  d8 <- annotate_figure(c8,
                  top = text_grob("Visualising student's status of placement\nand working experience.",
                                  color = "Red",
                                  face = "bold",
                                  size = 16)
  )
  
  return(d8)
}
Q3A1()
#--------------------------------------------------------------------------------
# According to the pie chart shown, the students with working experience are more likely
# to get a placement for a job than students with no working experience. This is because
# nowadays corporate are more fast paced and they are looking for experienced students to
# save the time for teaching them to get used to the things they did. Not only that,
# experienced students can adapt faster and they know what they are doing and what they
# should be doing. Moreover, students with working experience knows how to solve most of
# problems occurred during work and they are better in problem solving. According to UCAS,
# there was a survey done that showed two thirds of employers prefer to look for graduates
# with relevant work experience because it helps them prepare for work and develop general
# business awareness (UCAS, 2019).



# Analysis 3-2: Determine the relationship between student's status of placement with
# student's age and address.
#--------------------------------------------------------------------------------
Q3A2 = function() {
  age_data <- graded_data %>% group_by(age, address, status) %>% summarise(count=n())
  print(age_data)
  
  a9 <- ggplot(age_data, aes(x = age, y = count, group = address))+
    geom_line(aes(linetype = address, color = address)) + 
    geom_point(aes(shape = address, color = address))+
    facet_grid(~status) +
    labs(title="Number of Student by Age,\nAddress and Status of Placement",
         x="Age (year)",
         y = "Number of Student",
         color = "Address",
         shape = "Address",
         linetype = "Address")+
    theme_economist()+
    scale_color_manual(values=c('#009999','#990000'),
                       breaks = c("R", "U"),
                       labels = c(R = "Rural",U = "Urban")) +
    scale_shape_discrete(breaks = c("R", "U"),
                         labels = c(R = "Rural", U = "Urban")) +
    scale_linetype_discrete(breaks = c("R", "U"),
                            labels = c(R = "Rural", U = "Urban"))
    
  return(a9)
}
Q3A2()
#--------------------------------------------------------------------------------
# Based on the graph, we can confirm that there are more students that lives in urban areas.
# The greatest number of students who get a placement in job are from the age of 18 and lives
# in urban areas. The least amount of student who does not get a placement in job are from the
# age of 21 and lives in rural areas. This may be caused by corporate nowadays are more inclined
# towards recruiting a younger graduate that are from urban areas. We can observe that there is
# a peak in the graduates at the age of 19 and 21 that lives in rural areas to get a job placement.
# This may because most graduates aged 19 and 21 performed well during their interview or other
# factors that may affect this. The reason why most younger graduates from urban areas are
# being accepted into the company may because young employees can bring fresh perspective.
# According to an article by UNICEF (Alba, 2019), most young workers are eager to learn and they
# are more willing to gain new experience and apply their skills in the workplace. This kind of
# enthusiasm is confidential for building a team full of productivity. Young graduates from
# urban areas will also give the corporate a huge advantage to help target the millennial market
# as they do know a lot in this market.



# Analysis 3-3: Determine the relationship between student's status of placement with
# student's performance.
#--------------------------------------------------------------------------------
Q3A3 = function() {
  performance_status_data <- graded_data %>% group_by(mba_grade, status) %>% summarise(count=n())
  print(performance_status_data)
  
  a10 <- ggplot(performance_status_data, aes(x=mba_grade, y=count, fill=status)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values=c('#FF3300','#66FF33')) +
    theme_classic() +
    geom_text(aes(label=count), vjust=1.6, color="black", position = position_dodge(0.9), size=3) +
    ggtitle("Number of students by MBA Grade") + 
    labs(x="Grade", y="Number of Students", fill = "Status of Placement")
  
  return(a10)
}
Q3A3()
#--------------------------------------------------------------------------------
# Based on the graph, the students with higher grade are more inclined to get placed for
# job. The number of students with high grade but does not get a placement in job is also
# considerably high. There is a possibility that the students who do not get a job placement
# might not looking for job at the right place or do not market themselves good enough. For
# example, a poor resume or digital footprint such as LinkedIn or Facebook. While the student's
# grade is impressive, they might not be practicing the knowledges they have acquired. Therefore,
# what the employers are looking for is not just knowledge, but also skills. This is the reason
# many colleges have practicums, and partner with companies to provide internships (Gagirov, 2016).



# Analysis 3-4: Determine the relationship between student's status of placement with
# student's work experience and employability test.
#--------------------------------------------------------------------------------
Q3A4 = function() {
  facet_wrap_names <- as_labeller(
    c(`No` = "Without Working Experience",
      `Yes` = "With Working Experience"))
  
  graded_data$etest_rounded <- round(graded_data$etest_p)
  employability_data <- graded_data %>%
    mutate(etest_range = case_when(etest_rounded >= 0  & etest_rounded <= 49 ~ '0-49',
                                   etest_rounded >= 50  & etest_rounded <= 59 ~ '50-59',
                                   etest_rounded >= 60  & etest_rounded <= 69 ~ '60-69',
                                   etest_rounded >= 70  & etest_rounded <= 79 ~ '70-79',
                                   etest_rounded >= 80  & etest_rounded <= 89 ~ '80-89',
                                   etest_rounded >= 90  & etest_rounded <= 100 ~ '90-100'))
  
  employability_data <- employability_data %>% group_by(etest_range, status, workex) %>% summarise(count = n())
  print(employability_data)
  
  a11 <- ggplot(employability_data, aes(x=etest_range, y=count, group=status)) +
    geom_line(aes(color=status))+
    geom_point(aes(color=status)) +
    facet_wrap(~workex, labeller = facet_wrap_names)+
    scale_color_manual(values=c('#FF3300','#33CC33')) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    geom_text(aes(label=count), vjust=1.6, color="black", position = position_dodge(0.9), size=3) +
    ggtitle("Number of students by employability percentage and\nstatus of placement based on working experience") + 
    labs(x="Employability Percentage (%)", y="Number of Students", color = "Status of Placement")
  
  return(a11)
}
Q3A4()
#--------------------------------------------------------------------------------
# According to what the graph has shown, there were a high number of students that have
# low employability score but with working experience are able to get placed for a job.
# On the other hand, there were low number of students that have high employability score
# that get placed for a job. This is because of there were lesser student who have scored
# high for employability score than student who scored low for employability test. This
# have shown that corporate nowadays are more inclined and more likely to recruit graduates
# with working experience even if the employability score of the student is low. 



# Question 4: What causes the corporate to offer the current amount of salary to
# the candidate?

# Analysis 4-1: Determine the relationship between student's amount of salary with
# student's performance in post graduation and the field of specialisation.
#--------------------------------------------------------------------------------
Q4A1 = function() {
  facet_wrap_names <- as_labeller(
    c(`Mkt&HR` = "Marketing &\nHuman Resource",
      `Mkt&Fin` = "Marketing &\nFinance"))
  
  salary_performance <- graded_data %>%
    group_by(salary, mba_grade, specialisation) %>% 
    filter(status == "Placed") 
  
  max(salary_performance$salary) #200,000
  min(salary_performance$salary) #500,000
  
  salary_performance <- salary_performance %>%
    mutate(salary_category = case_when(salary >= 0  & salary <= 200000 ~ '200,000',
                                        salary >= 200001  & salary <= 300000 ~ '200,001-300,000',
                                        salary >= 300001  & salary <= 400000 ~ '300,001-400,000',
                                        salary >= 400001  & salary <= 500000 ~ '400,001-500,000'))
  
  salary_performance <- salary_performance %>%
    group_by(salary_category, mba_grade, specialisation) %>% 
    summarise(count=n())
  print(salary_performance)
  
  a12 <- ggplot(salary_performance, aes(x=salary_category, y=count, fill=mba_grade)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~specialisation, ncol=2, labeller = facet_wrap_names) +
    scale_color_manual(values=c('#FF3300','#33CC33')) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=45, hjust=1))+
    geom_text(aes(label=count), vjust=1.6, color="black", position = position_dodge(0.9), size=3) +
    ggtitle("Number of students by salary,\nMBA grade and specialisation") + 
    labs(x="Salary (RM)", y="Number of Students", fill = "MBA Grade")
  
  return(a12)
}
Q4A1()
#--------------------------------------------------------------------------------
# According to the graph shown, most of the students received the
# salary from the range between RM200,001 to RM400,000. From the graph, we can see that
# there more students scored A in Marketing and Human Resource that received RM200,000
# of salary which is low compared to the other grade A students that are able to secure
# their salary of the range of RM200,001 to RM400,000. Marketing and Human Resources field
# may not need too much of the intelligence, thus there were more grade C and B students
# that has secured their job are receiving the salary of RM200,001 to RM400,000 compared
# to Marketing and Finance field. There were more grade A students in Marketing and Finance
# field that were receiving the salary at the range of RM200,001 to RM400,000.



# Analysis 4-2: Determine the relationship between student's amount of salary and
# student's working experience.
#--------------------------------------------------------------------------------
Q4A2 = function() {
  work_salary <- graded_data %>%
    group_by(salary, workex) %>% 
    filter(status == "Placed") 
  
  work_salary <- work_salary %>%
    mutate(salary_category = case_when(salary >= 0  & salary <= 200000 ~ '200,000',
                                        salary >= 200001  & salary <= 300000 ~ '200,001-300,000',
                                        salary >= 300001  & salary <= 400000 ~ '300,001-400,000',
                                        salary >= 400001  & salary <= 500000 ~ '400,001-500,000'))
  
  work_salary <- work_salary %>%
    group_by(salary_category, workex) %>% 
    summarise(count=n())
  print(work_salary)
  
  a13 <- ggplot(work_salary, aes(x=salary_category, y =count, fill=workex)) +
    geom_bar(stat ="identity", position="dodge")+
    scale_fill_manual(values=c("#9999CC", "#FFCC33"))+
    theme_minimal()+
    geom_text(aes(label=count), vjust=1.6, color="black", position = position_dodge(0.9), size=3) +
    ggtitle("Number of students by salary (RM)\nand working experience") + 
    labs(x="Salary (RM)", y="Number of Students", fill = "Working\nExperience")+
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  return(a13)
}
Q4A2()
#--------------------------------------------------------------------------------
# Based on the graph shown, we can determine that the graduates with
# or without working experience are mostly receiving the salary at the range of RM200,001
# to RM400,000. Other than that, the graduates who are receiving the salary at the range of
# RM400,001 to RM500,000 are mostly without working experience. This may because they perform
# well in interview. This can be concluded that the average and normally the range of the salary
# is between the range of RM200,001 to RM400,000 and the working experience of the graduates
# are not a factor affecting the range of salary to be given by the corporate.



# Analysis 4-3: Determine the relationship between student's amount of salary and
# employability test.
#--------------------------------------------------------------------------------
Q4A3 = function() {
  etest_salary <- graded_data %>%
    group_by(salary, etest_rounded) %>% 
    filter(status == "Placed") 
  min(etest_salary$etest_rounded) #50
  max(etest_salary$etest_rounded) #98
  
  etest_salary <- etest_salary %>%
    mutate(etest_category = case_when(etest_rounded >= 50  & etest_rounded <= 59 ~ '50-59',
                                       etest_rounded >= 60  & etest_rounded <= 69 ~ '60-69',
                                       etest_rounded >= 70  & etest_rounded <= 79 ~ '70-79',
                                       etest_rounded >= 80  & etest_rounded <= 89 ~ '80-89',
                                       etest_rounded >= 90  & etest_rounded <= 100 ~ '90-100'))  %>%
    mutate(salary_category = case_when(salary >= 0  & salary <= 200000 ~ '200,000',
                                       salary >= 200001  & salary <= 300000 ~ '200,001-300,000',
                                       salary >= 300001  & salary <= 400000 ~ '300,001-400,000',
                                       salary >= 400001  & salary <= 500000 ~ '400,001-500,000'))
  
  etest_salary <- etest_salary %>%
    group_by(etest_category, salary_category) %>% 
    summarise(count=n())
  print(etest_salary)
  
  a14 <- ggplot(etest_salary, aes(x=salary_category, y =count, fill=etest_category)) +
    geom_bar(stat ="identity", position="dodge")+
    theme_minimal()+
    scale_fill_brewer(palette="RdYlGn") +
    ggtitle("Number of students by salary (RM)\nand employability test (%)") + 
    labs(x="Salary (RM)", y="Number of Students", fill = "Employability\nTest (%)")+
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  return(a14)
}
Q4A3()
#--------------------------------------------------------------------------------
# Based on the graph, there are still a high number of students
# that scored 50% to 59% in their employability test and still able to secured a job with
# the salary of RM200,001 to RM400,000. Most of the students are in the range of RM200,001
# to RM300,000 based on their employability test. This can be concluded with the employability
# test are not the affecting factor to determine the amount of salary.



# Question 5: What causes students to further their studies in Master of Business
# Administration (Postgraduate)?

# Analysis 5-1: Determine the relationship between student's specialisation in MBA with
# performance and field of study in undergraduate.
#--------------------------------------------------------------------------------
Q5A1 = function() {
  facet_wrap_names <- as_labeller(
    c(`Mkt&HR` = "Marketing &\nHuman Resource",
      `Mkt&Fin` = "Marketing &\nFinance"))
  
  degree_specialisation <- graded_data %>%
    group_by(degree_grade, degree_t, specialisation) %>% 
    summarise(count=n())
  print(degree_specialisation)
  
  a15 <- ggplot(degree_specialisation, aes(x = degree_t, y = count, fill = degree_grade)) +
    geom_bar(stat ="identity", position="dodge")+
    facet_wrap(~specialisation, ncol = 2, labeller = facet_wrap_names)+
    theme_hc()+
    scale_fill_hc() +
    ggtitle("Student Specialisation by Degree Grade and Field") + 
    labs(x="MBA Specialisation", y="Number of Students", fill = "Degree Grade")+
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    scale_x_discrete(breaks = c("Comm&Mgmt", "Others", "Sci&Tech"),
                     labels = c('Comm&Mgmt' = "Communication &\nManagement",
                                Others = "Others", 'Sci&Tech' = "Science &\nTechnology"))
  
  return(a15)
}
Q5A1()
#--------------------------------------------------------------------------------
# Based on the graph shown, the students from communication and management that had
# scored grade C are more likely to choose to further their studies in marketing and
# human resources rather than in marketing and finance. The reason might be they understand
# that they might not be good in management field, thus they might choose human resource
# and marketing that does not need students to be good in managing. HR and marketing are
# the departments that have to communicate with the consumers by using visualisation
# and information (TalentAdore, n.d.). The messages will get spread widely through the
# channels used. Thus, there is no need to be involve in management field.



# Analysis 5-2: Determine the relationship between student's specialisation in MBA with
# parent's education level.
#--------------------------------------------------------------------------------
Q5A2 = function() {
  facet_wrap_names <- as_labeller(
    c(`Mkt&HR` = "Marketing &\nHuman Resource",
      `Mkt&Fin` = "Marketing &\nFinance"))
  
  parent_edu_specialisation <- gather(graded_data, key="parent", value="lvl_edu", 5:6)
  parent_edu_specialisation <- parent_edu_specialisation %>%
    group_by(parent, lvl_edu, specialisation) %>% 
    summarise(count=n())
  print(parent_edu_specialisation)
  
  a16 <- ggplot(parent_edu_specialisation, aes(x = lvl_edu, y = count, fill = parent)) +
    geom_bar(stat ="identity", position="dodge")+
    facet_wrap(~specialisation, ncol = 2, labeller = facet_wrap_names)+
    theme_hc()+
    ggtitle("Student Specialisation by Parent's Level of Education") + 
    labs(x="Level of Education", y="Number of Students", fill = "Parent")+
    scale_fill_manual(values=c("#6699FF", "#FF9999"),
                      breaks = c("Fedu", "Medu"),
                      labels = c(Fedu = "Father",
                                 Medu = "Mother"))+
    geom_text(aes(label=count), vjust=-0.5, color="black", position = position_dodge(0.9), size=3)
    
  return(a16)
}
Q5A2()
#--------------------------------------------------------------------------------
# Based on the graph shown above in Figure 46, more students with mother's level 4
# of education and father's level 1 of education would choose to study marketing and
# human resource.



# Analysis 5-3: Determine the relationship between student's specialisation in MBA with
# parent's job.
#--------------------------------------------------------------------------------
Q5A3 = function() {
  facet_wrap_names <- as_labeller(
    c(`Mkt&HR` = "Marketing &\nHuman Resource",
      `Mkt&Fin` = "Marketing &\nFinance"))
  
  parent_job_specialisation <- gather(graded_data, key="parent", value="job", 7:8)
  parent_job_specialisation <- parent_job_specialisation %>%
    group_by(parent, job, specialisation) %>% 
    summarise(count=n())
  print(parent_job_specialisation)
  
  a17 <- ggplot(parent_job_specialisation, aes(x = job, y = count, fill = parent)) +
    geom_bar(stat ="identity", position="dodge")+
    facet_wrap(~specialisation, ncol = 2, labeller = facet_wrap_names)+
    theme_classic()+
    ggtitle("Student Specialisation by Parent's Job") + 
    labs(x="Job", y="Number of Students", fill = "Parent")+
    scale_fill_manual(values=c("#6699FF", "#FF9999"),
                      breaks = c("Fjob", "Mjob"),
                      labels = c(Fjob = "Father",
                                 Mjob = "Mother"))+
    scale_x_discrete(breaks = c("at_home", "health", "other", "services", "teacher"),
                     labels = c(at_home = "Homemaker", health = "Health-Related",
                                other = "Others", services = "Service-Related", teacher = "Teacher"))+
    coord_cartesian(ylim = c(1500,1900)) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    geom_text(aes(label=count), vjust=-0.5, color="black", position = position_dodge(0.9), size=3)
  
  return(a17)
}
Q5A3()
#--------------------------------------------------------------------------------
# According to the graph shown, we can determine that more students with their mother's
# or father's job being others would choose to further their studies in marketing and
# human resource in postgraduate. This may because of mother's and father's job may be
# related to managing a business or even in the marketing scope. Parents whose job are
# service-related also brings much more influence to the students to further their studies
# in MBA. Adding on, parents have more experience in the world than their child, and therefore
# they have a better understanding of the right and wrong decisions for career paths.
# The parent would normally act as a guide to make sure that their child makes good life
# decisions and doesn't set unrealistic goals for themselves (Salisbury, 2019). Therefore,
# parent's job and experience might bring influence to the students.





