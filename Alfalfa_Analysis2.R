#Import data ----
setwd("C:/Users/Cole Ternes/Desktop/CPSC_courses/personalprojects/Alfalfa")
data.raw <- read.csv("raw_data.csv", stringsAsFactors = F)

#Library for plotting data
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(dplyr)
library(agricolae)

#Vectors ----
#No Bacteria / Sand
n_sa_name <- c()
n_sa_rep <- c()
n_sa_nodes <- c()
n_sa_mass <- c()

#No Bacteria / Terrain
n_te_name <- c()
n_te_rep <- c()
n_te_nodes <- c()
n_te_mass <- c()

#No Bacteria / Martian
n_ma_name <- c()
n_ma_rep <- c()
n_ma_nodes <- c()
n_ma_mass <- c()

#Yes Bacteria / Sand
y_sa_name <- c()
y_sa_rep <- c()
y_sa_nodes <- c()
y_sa_mass <- c()

#Yes Bacteria / Terrain
y_te_name <- c()
y_te_rep <- c()
y_te_nodes <- c()
y_te_mass <- c()

#Yes Bacteria / Martian
y_ma_name <- c()
y_ma_rep <- c()
y_ma_nodes <- c()
y_ma_mass <- c()

#Input data ----
#For-Loop to input data into vectors
for (i in 1:nrow(data.raw)) {
  if (data.raw[i,1] == "N_Sa") { #If Row i, Column 1 is No Bacteria / Sand
    n_sa_name <- c(n_sa_name, data.raw[i,1])
    n_sa_rep <- c(n_sa_rep, data.raw[i,4])
    n_sa_nodes <- c(n_sa_nodes, data.raw[i,5])
    n_sa_mass <- c(n_sa_mass, data.raw[i,6])
  } else if (data.raw[i,1] == "N_Te") { #If Row i, Column 1 is No Bacteria / Terrain
    n_te_name <- c(n_te_name, data.raw[i,1])
    n_te_rep <- c(n_te_rep, data.raw[i,4])
    n_te_nodes <- c(n_te_nodes, data.raw[i,5])
    n_te_mass <- c(n_te_mass, data.raw[i,6])
  } else if (data.raw[i,1] == "N_Ma") { #If Row i, Column 1 is No Bacteria / Martian
    n_ma_name <- c(n_ma_name, data.raw[i,1])
    n_ma_rep <- c(n_ma_rep, data.raw[i,4])
    n_ma_nodes <- c(n_ma_nodes, data.raw[i,5])
    n_ma_mass <- c(n_ma_mass, data.raw[i,6])
  } else if (data.raw[i,1] == "Y_Sa") { #If Row i, Column 1 is Yes Bacteria / Sand
    y_sa_name <- c(y_sa_name, data.raw[i,1])
    y_sa_rep <- c(y_sa_rep, data.raw[i,4])
    y_sa_nodes <- c(y_sa_nodes, data.raw[i,5])
    y_sa_mass <- c(y_sa_mass, data.raw[i,6])
  } else if (data.raw[i,1] == "Y_Te") { #If Row i, Column 1 is Yes Bacteria / Terrain
    y_te_name <- c(y_te_name, data.raw[i,1])
    y_te_rep <- c(y_te_rep, data.raw[i,4])
    y_te_nodes <- c(y_te_nodes, data.raw[i,5])
    y_te_mass <- c(y_te_mass, data.raw[i,6])
  } else if (data.raw[i,1] == "Y_Ma") { #If Row i, Column 1 is Yes Bacteria / Martian
    y_ma_name <- c(y_ma_name, data.raw[i,1])
    y_ma_rep <- c(y_ma_rep, data.raw[i,4])
    y_ma_nodes <- c(y_ma_nodes, data.raw[i,5])
    y_ma_mass <- c(y_ma_mass, data.raw[i,6])
  } 
}

#1) Preliminary Lineplot on Number of Nodes without Bacteria ----
#Dataframes for the Nodes in No Bacteria / Sand, Terrain, Martian
n_sa_nodes_df <- data.frame(x=n_sa_rep, y=n_sa_nodes)
n_te_nodes_df <- data.frame(x=n_te_rep, y=n_te_nodes)
n_ma_nodes_df <- data.frame(x=n_ma_rep, y=n_ma_nodes)

#Label the Treatment accordingly
n_sa_nodes_df$Treatment="Sand"
n_te_nodes_df$Treatment="Terrain"
n_ma_nodes_df$Treatment="Martian"

#Bind the dataframes together
n_nodes_df=rbind(n_sa_nodes_df, n_te_nodes_df, n_ma_nodes_df)

#Plot the graph
n_nodes_plot <- ggplot(n_nodes_df) +
  geom_line(aes(x, y, colour=Treatment)) +
  xlab("Replicates") +
  ylab("Number of Nodes") +
  theme_minimal() +
  ggtitle("Number of Nodes per Replicate without Bacteria")
n_nodes_plot

#2) Preliminary Lineplot on Number of Nodes with Bacteria ----
#Dataframes for the Nodes in Yes Bacteria / Sand, Terrain, Martian
y_sa_nodes_df <- data.frame(x=y_sa_rep, y=y_sa_nodes)
y_te_nodes_df <- data.frame(x=y_te_rep, y=y_te_nodes)
y_ma_nodes_df <- data.frame(x=y_ma_rep, y=y_ma_nodes)

#Label the Treatment accordingly
y_sa_nodes_df$Treatment="Sand"
y_te_nodes_df$Treatment="Terrain"
y_ma_nodes_df$Treatment="Martian"

#Bind the dataframes together
y_nodes_df=rbind(y_sa_nodes_df, y_te_nodes_df, y_ma_nodes_df)

#Plot the graph
y_nodes_plot <- ggplot(y_nodes_df) +
  geom_line(aes(x, y, colour=Treatment)) +
  xlab("Replicates") +
  ylab("Number of Nodes") +
  theme_minimal() +
  ggtitle("Number of Nodes per Replicate with Bacteria")
y_nodes_plot

#3) Preliminary Lineplot on Mass without Bacteria ----
#Dataframes for the Mass in No Bacteria / Sand, Terrain, Martian
n_sa_mass_df <- data.frame(x=n_sa_rep, y=n_sa_mass)
n_te_mass_df <- data.frame(x=n_te_rep, y=n_te_mass)
n_ma_mass_df <- data.frame(x=n_ma_rep, y=n_ma_mass)

#Label the Treatment accordingly
n_sa_mass_df$Treatment="Sand"
n_te_mass_df$Treatment="Terrain"
n_ma_mass_df$Treatment="Martian"

#Bind the dataframes together
n_mass_df=rbind(n_sa_mass_df, n_te_mass_df, n_ma_mass_df)

#Plot the graph
n_mass_plot <- ggplot(n_mass_df) +
  geom_line(aes(x, y, colour=Treatment)) +
  xlab("Replicates") +
  ylab("Shoot Mass (mg)") +
  theme_minimal() +
  ggtitle("Mass per Replicate without Bacteria")
n_mass_plot

#4) Preliminary Lineplot on Mass with Bacteria ----
#Dataframes for the Mass in Yes Bacteria / Sand, Terrain, Martian
y_sa_mass_df <- data.frame(x=y_sa_rep, y=y_sa_mass)
y_te_mass_df <- data.frame(x=y_te_rep, y=y_te_mass)
y_ma_mass_df <- data.frame(x=y_ma_rep, y=y_ma_mass)

#Label the Treatment accordingly
y_sa_mass_df$Treatment="Sand"
y_te_mass_df$Treatment="Terrain"
y_ma_mass_df$Treatment="Martian"

#Bind the dataframes together
y_mass_df=rbind(y_sa_mass_df, y_te_mass_df, y_ma_mass_df)

#Plot the graph
y_mass_plot <- ggplot(y_mass_df) +
  geom_line(aes(x, y, colour=Treatment)) +
  xlab("Replicates") +
  ylab("Shoot Mass (mg)") +
  theme_minimal() +
  ggtitle("Mass per Replicate with Bacteria")
y_mass_plot

#5) Barplot for Mean Number of Nodules ----
#Calculate and round the means
n_sa_mean_nodes <- mean(n_sa_nodes)
n_sa_mean_nodes <- round(n_sa_mean_nodes, digits=2)
n_sa_sd_nodes <- sd(n_sa_nodes)

n_te_mean_nodes <- mean(n_te_nodes)
n_te_mean_nodes <- round(n_te_mean_nodes, digits=2)
n_te_sd_nodes <- sd(n_te_nodes)

n_ma_mean_nodes <- mean(n_ma_nodes)
n_ma_mean_nodes <- round(n_ma_mean_nodes, digits=2)
n_ma_sd_nodes <- sd(n_ma_nodes)

y_sa_mean_nodes <- mean(y_sa_nodes)
y_sa_mean_nodes <- round(y_sa_mean_nodes, digits=2)
y_sa_sd_nodes <- sd(y_sa_nodes)

y_te_mean_nodes <- mean(y_te_nodes)
y_te_mean_nodes <- round(y_te_mean_nodes, digits=2)
y_te_sd_nodes <- sd(y_te_nodes)

y_ma_mean_nodes <- mean(y_ma_nodes)
y_ma_mean_nodes <- round(y_ma_mean_nodes, digits=2)
y_ma_sd_nodes <- sd(y_ma_nodes)

#Put the means in a dataframe and put the sd in a vector
mean_nodes_df <- data.frame(nodes_x=c("#1 Sand", "#1 Terran", "#1 Martian", "#2 Sand", "#2 Terran", "#2 Martian"), nodes_y=c(n_sa_mean_nodes, n_te_mean_nodes, n_ma_mean_nodes, y_sa_mean_nodes, y_te_mean_nodes, y_ma_mean_nodes))
nodes_sd <- c(n_sa_sd_nodes, n_te_sd_nodes, n_ma_sd_nodes, y_sa_sd_nodes, y_te_sd_nodes, y_ma_sd_nodes)

#Plot the graph
mean_nodes_plot <- ggplot(data=mean_nodes_df, aes(x=nodes_x, y=nodes_y, fill=nodes_x)) +
  geom_bar(stat="identity", position="dodge", width=0.75) +
  xlab("Without Bacteria                               With Bacteria") +
  ylab("Num. of Nodules") +
  theme_minimal() +
  ggtitle("Mean Num. of Nodules per Treatment") +
  geom_errorbar(aes(ymin=nodes_y-nodes_sd, ymax=nodes_y+nodes_sd), width=0.15, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#AD4624", "#D3D384", "#1A663C", "#FF6633", "#FFFF99", "#2BAD64")) +
  theme(axis.ticks.x=element_blank(), legend.position="none") +
  guides(fill=guide_legend(title="Treatments")) +
  geom_text(aes(label=c("d","d","d","a","b", "c"), hjust=2, vjust=-0.5))
mean_nodes_plot

#6) Barplot for Mean Shoot Mass ----
#Calculate and round the means
n_sa_mean_mass <- mean(n_sa_mass)
n_sa_mean_mass <- round(n_sa_mean_mass, digits=2)
n_sa_sd_mass <- sd(n_sa_mass)

n_te_mean_mass <- mean(n_te_mass)
n_te_mean_mass <- round(n_te_mean_mass, digits=2)
n_te_sd_mass <- sd(n_te_mass)

n_ma_mean_mass <- mean(n_ma_mass)
n_ma_mean_mass <- round(n_ma_mean_mass, digits=2)
n_ma_sd_mass <- sd(n_ma_mass)

y_sa_mean_mass <- mean(y_sa_mass)
y_sa_mean_mass <- round(y_sa_mean_mass, digits=2)
y_sa_sd_mass <- sd(y_sa_mass)

y_te_mean_mass <- mean(y_te_mass)
y_te_mean_mass <- round(y_te_mean_mass, digits=2)
y_te_sd_mass <- sd(y_te_mass)

y_ma_mean_mass <- mean(y_ma_mass)
y_ma_mean_mass <- round(y_ma_mean_mass, digits=2)
y_ma_sd_mass <- sd(y_ma_mass)

#Put the means in a dataframe and put the sd in a vector
mean_mass_df <- data.frame(mass_x=c("#1 Sand", "#1 Terran", "#1 Martian", "#2 Sand", "#2 Terran", "#2 Martian"), mass_y=c(n_sa_mean_mass, n_te_mean_mass, n_ma_mean_mass, y_sa_mean_mass, y_te_mean_mass, y_ma_mean_mass))
mass_sd <- c(n_sa_sd_mass, n_te_sd_mass, n_ma_sd_mass, y_sa_sd_mass, y_te_sd_mass, y_ma_sd_mass)

#Plot the graph
mean_nodes_plot <- ggplot(data=mean_mass_df, aes(x=mass_x, y=mass_y, fill=mass_x)) +
  geom_bar(stat="identity", position="dodge", width=0.75) +
  xlab("Without Bacteria                               With Bacteria") +
  ylab("Shoot Mass (mg)") +
  theme_minimal() +
  ggtitle("Mean Shoot Mass (mg) per Treatment") +
  geom_errorbar(aes(ymin=mass_y-mass_sd, ymax=mass_y+mass_sd), width=0.15, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#AD4624", "#D3D384", "#1A663C", "#FF6633", "#FFFF99", "#2BAD64")) +
  theme(axis.ticks.x=element_blank(), legend.position="none") +
  guides(fill=guide_legend(title="Treatments")) +
  geom_text(aes(label=c("c","b","d","b","a", "c"), hjust=2, vjust=-0.5))
mean_nodes_plot

#7) Boxplot for 2-Sample T-Test for Nodes ----
#Dataframe for Sand Treatment
t_test_n_sa_nodes_df <- data.frame("Treatment"=n_sa_name, "Nodes"=n_sa_nodes)
t_test_y_sa_nodes_df <- data.frame("Treatment"=y_sa_name, "Nodes"=y_sa_nodes)
t_test_sa_nodes_df <- rbind(t_test_n_sa_nodes_df, t_test_y_sa_nodes_df)

#Dataframe for Terrain Treatment
t_test_n_te_nodes_df <- data.frame("Treatment"=n_te_name, "Nodes"=n_te_nodes)
t_test_y_te_nodes_df <- data.frame("Treatment"=y_te_name, "Nodes"=y_te_nodes)
t_test_te_nodes_df <- rbind(t_test_n_te_nodes_df, t_test_y_te_nodes_df)

#Dataframe for Martian Treatment
t_test_n_ma_nodes_df <- data.frame("Treatment"=n_ma_name, "Nodes"=n_ma_nodes)
t_test_y_ma_nodes_df <- data.frame("Treatment"=y_ma_name, "Nodes"=y_ma_nodes)
t_test_ma_nodes_df <- rbind(t_test_n_ma_nodes_df, t_test_y_ma_nodes_df)

#Calculating the P-Value for each Treatment
#bartlett.test(Nodes ~ Treatment, data=t_test_sa_nodes_df)
#bartlett.test(Nodes ~ Treatment, data=t_test_te_nodes_df)
#bartlett.test(Nodes ~ Treatment, data=t_test_ma_nodes_df)

#Conducting the T-Test for each Treatment
#t.test(Nodes ~ Treatment, data=t_test_sa_nodes_df, var.equal=TRUE, conf.level=0.95)
#t.test(Nodes ~ Treatment, data=t_test_te_nodes_df, var.equal=TRUE, conf.level=0.95)
#t.test(Nodes ~ Treatment, data=t_test_ma_nodes_df, var.equal=TRUE, conf.level=0.95)

#Graph the Sand T-Test as a Boxplot
t_test_sa_nodes_plot <- ggboxplot(data=t_test_sa_nodes_df, x="Treatment", y="Nodes", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="", x="Sand Treatment") +
  coord_cartesian(ylim = c(0, 20)) +
  scale_fill_manual(values=c("#D3D384", "#FFFF99"))

#Graph the Terrain T-Test as a Boxplot
t_test_te_nodes_plot <- ggboxplot(data=t_test_te_nodes_df, x="Treatment", y="Nodes", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="", x="Terrain Treatment") +
  coord_cartesian(ylim = c(0, 20)) +
  scale_fill_manual(values=c("#1A663C", "#2BAD64"))

#Graph the Martian T-Test as a Boxplot
t_test_ma_nodes_plot <- ggboxplot(data=t_test_ma_nodes_df, x="Treatment", y="Nodes", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="# of Nodes", x="Martian Treatment") +
  coord_cartesian(ylim = c(0, 20)) +
  scale_fill_manual(values=c("#AD4624", "#FF6633"))

#Place the 3 Boxplots into 3 Panels of a larger Graph
t_test_nodes_plot <- ggarrange(t_test_ma_nodes_plot, t_test_sa_nodes_plot, t_test_te_nodes_plot, nrow=1) 
t_test_nodes_plot

#8) Boxplot for 2-Sample T-Test for Mass ----
#Dataframe for Sand Treatment
t_test_n_sa_mass_df <- data.frame("Treatment"=n_sa_name, "Mass"=n_sa_mass)
t_test_y_sa_mass_df <- data.frame("Treatment"=y_sa_name, "Mass"=y_sa_mass)
t_test_sa_mass_df <- rbind(t_test_n_sa_mass_df, t_test_y_sa_mass_df)

#Dataframe for Terrain Treatment
t_test_n_te_mass_df <- data.frame("Treatment"=n_te_name, "Mass"=n_te_mass)
t_test_y_te_mass_df <- data.frame("Treatment"=y_te_name, "Mass"=y_te_mass)
t_test_te_mass_df <- rbind(t_test_n_te_mass_df, t_test_y_te_mass_df)

#Dataframe for Martian Treatment
t_test_n_ma_mass_df <- data.frame("Treatment"=n_ma_name, "Mass"=n_ma_mass)
t_test_y_ma_mass_df <- data.frame("Treatment"=y_ma_name, "Mass"=y_ma_mass)
t_test_ma_mass_df <- rbind(t_test_n_ma_mass_df, t_test_y_ma_mass_df)

#Calculating the P-Value for each Treatment
#bartlett.test(Mass ~ Treatment, data=t_test_sa_mass_df)
#bartlett.test(Mass ~ Treatment, data=t_test_te_mass_df)
#bartlett.test(Mass ~ Treatment, data=t_test_ma_mass_df)

#Conducting the T-Test for each Treatment
#t.test(Mass ~ Treatment, data=t_test_sa_mass_df, var.equal=TRUE, conf.level=0.95)
#t.test(Mass ~ Treatment, data=t_test_te_mass_df, var.equal=TRUE, conf.level=0.95)
#t.test(Mass ~ Treatment, data=t_test_ma_mass_df, var.equal=TRUE, conf.level=0.95)

#Graph the Sand T-Test as a Boxplot
t_test_sa_mass_plot <- ggboxplot(data=t_test_sa_mass_df, x="Treatment", y="Mass", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="", x="Sand Treatment") +
  coord_cartesian(ylim = c(0, 35)) +
  scale_fill_manual(values=c("#D3D384", "#FFFF99"))

#Graph the Terrain T-Test as a Boxplot
t_test_te_mass_plot <- ggboxplot(data=t_test_te_mass_df, x="Treatment", y="Mass", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="", x="Terrain Treatment") +
  coord_cartesian(ylim = c(0, 35)) +
  scale_fill_manual(values=c("#1A663C", "#2BAD64"))

#Graph the Martian T-Test as a Boxplot
t_test_ma_mass_plot <- ggboxplot(data=t_test_ma_mass_df, x="Treatment", y="Mass", fill="Treatment") +
  stat_compare_means(method = "t.test") +
  scale_x_discrete(labels=c("Without Bacteria", "With Bacteria")) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(y="Mass (mg)", x="Martian Treatment") +
  coord_cartesian(ylim = c(0, 35)) +
  scale_fill_manual(values=c("#AD4624", "#FF6633"))


#Place the 3 Boxplots into 3 Panels of a larger Graph
t_test_mass_plot <- ggarrange(t_test_ma_mass_plot, t_test_sa_mass_plot, t_test_te_mass_plot, nrow=1) 
t_test_mass_plot

#9) 1-Way Anova for Nodes ----
#Creating the Data Frame for Nodes in all Treatments
anova_nodes_df <- data.frame(Treatment=data.raw[,1], Nodes=data.raw[,5])

#Specify the order of factor levels
anova_nodes_df <- mutate(anova_nodes_df, Treatment=factor(Treatment, levels=unique(Treatment)))

#Creating the Linear Model for the Anova
anova_nodes_model <- lm(Nodes ~ Treatment, data=anova_nodes_df)

#Conduct ANOVA
anova(anova_nodes_model)
summary(anova_nodes_model)

#Tukey comparisons in agricolae package
(HSD.test(anova_nodes_model, "Treatment"))

#10) 1-Way Anova for Mass ----
#Creating the Data Frame for Mass in all Treatments
anova_mass_df <- data.frame("Treatment"=data.raw[,1], "Mass"=data.raw[,6])

#Specify the order of factor levels
anova_mass_df <- mutate(anova_mass_df, Treatment=factor(Treatment, levels=unique(Treatment)))

#Creating the Linear Model for the Anova
anova_mass_model <- lm(Mass ~ Treatment, data=anova_mass_df)

#Conduct ANOVA
anova(anova_mass_model)
summary(anova_mass_model)

#Tukey comparisons in agricolae package
(HSD.test(anova_mass_model, "Treatment"))

#Questions ----
#Which soil mix provides the best outcome for the Alfalfa based on mass and height (length)?
#Does martian soil allow for conditions to foster bacterial infections (as seen through counting nodes, extracting bacteria, mass of nodes in each treatment)?