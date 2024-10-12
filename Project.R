load('/Users/manasarthak/Desktop/ICPSR_38780/DS0001/38780-0001-Data.rda')

#EDA

# Cleaning The Data
df=data.frame(da38780.0001)
#Removing columns that arent of statistical importance
df <- df[, !(names(df) %in% c('MSA', 'ASR_ID','CONTENTS','YEAR','ADJUST','CORE','AGENCNT',"SEQNO"))]
df=na.omit(df)
#Removing Agencies with 0 Population listed
df <- df[df$POP != 0, , drop = TRUE]
dim(df)
names(df)

#Importing important libraries
library(ggplot2)
library(dplyr)
library(car)
library(Dict)

#As each row corresponds to a particular offense type reported by a certain originating agency we will create certain
#variables which would be helpful for further statistical analysis

#Below we add certain variables added for each row corresponding to a certain offense type reported by a certain agency 

# Total Adult Males (TAM)
df$TAM <- rowSums(df[, grepl("^M(18|19|20|21|22|23|24|25_29|30_34|35_39|40_44|45_49|50_54|55_59|60_64|65)", names(df))])
#Total Juvenile Males(TJM)
df$TJM<-rowSums(df[,grepl("^M(0_9|10_12|13_14|15|16|17)",names(df))])
# Total Adult Females (TAF)
df$TAF <- rowSums(df[, grepl("^F(18|19|20|21|22|23|24|25_29|30_34|35_39|40_44|45_49|50_54|55_59|60_64|65)", names(df))])
#Total Juvenile Females(TFM)
df$TJF<-rowSums(df[,grepl("^F(0_9|10_12|13_14|15|16|17)",names(df))])
# Total Juveniles (TAJ)
df$TJ <- df$TJM+df$TJF
# Total Perpetrators
df$Total_Perps_Per_Agency_Per_Offense <- df$TAM + df$TAF + df$TJ

#Agency Wise Analysis
#Crime rate= No. of Perpetrators/ Total Population Under An Agency

agency_crime_counts <- df %>%
  group_by(ORI, STATE, STNAME,AGENCY) %>%
  summarise(total_crimes = n(), #will be 1 even if the agency reported no perpetrators(total reports would be a better name)
            total_perps = sum(Total_Perps_Per_Agency_Per_Offense), 
            criminal_rate = sum(Total_Perps_Per_Agency_Per_Offense) / POP[1],
            TAM=sum(TAM),
            TAF=sum(TAF),
            TJ=sum(TJ),
            TJM=sum(TJM),
            TJF=sum(TJF),
            TM=sum(TJM+TAM),
            TF=sum(TJF+TAF),
            TW=sum(AW+JW),
            TB=sum(AB+JB),
            TI=sum(AI+JI),
            TA=sum(AA+JA),
            TH=sum(AH+JH),
            TNH=sum(AN,JN),
            DIV=DIV[1],
            POP=POP[1],
            SUB=SUB[1]
            )
#population under 1000 had several criminal rates of more than 1(may be migrant crime? may be multiple offences? may be repeat offenders?)
agency_crime_counts<-agency_crime_counts[agency_crime_counts$POP>1000,]
agency_crime_counts

#Analyzing Crime Rates Across Different Agencies
#As there are many agencies we will only get the top and bottom performing in terms of crime rates
# Sorting the data by crime rate in descending order
sorted_agencies <- agency_crime_counts[order(agency_crime_counts$criminal_rate), ]
bottom_10_agencies <- tail(sorted_agencies, 10)
top_10_agencies <- head(sorted_agencies, 10)
top_10_agencies$AgencyWithState <- paste(trimws(top_10_agencies$AGENCY),"(",trimws(top_10_agencies$STATE), ")",sep = "")
bottom_10_agencies$AgencyWithState <- paste(trimws(bottom_10_agencies$AGENCY), "(", trimws(bottom_10_agencies$STATE), ")", sep = "")
top_10_agencies
bottom_10_agencies

ggplot(top_10_agencies, aes(x = reorder(AgencyWithState, criminal_rate), y = criminal_rate)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Top 10 Agencies Based on Crime Rate",
       x = "Agency",
       y = "Criminal Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bottom_10_agencies, aes(x = reorder(AgencyWithState, criminal_rate), y = criminal_rate)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(title = "Bottom 10 Agencies Based on Crime Rate",
       x = "Agency",
       y = "Crime Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot of Sorted Crime rates
ggplot(sorted_agencies, aes(x = seq_along(criminal_rate), y = criminal_rate)) +
  geom_line(color = "blue") +
  geom_point(shape = "o", color = "red", size = 2)+
  labs(title = "Distribution of Criminal Rates Across Agencies",
       x = "Agencies (Ordered by Crime Rate)",
       y = "Crime Rate") +
  theme_minimal()




#Create a summary data frame with proportions for each quartile category
#Also ranking each state and agency pair according to their proprotion of each category of agency


#Plot Pie Chart with visible proportions In Tableau


#Division-wise agencies performance
ggplot(sorted_agencies, aes(x = DIV, fill = CR_category))+
  geom_bar() +
  labs(title = "Region-wise Agency Count with Quartile Categories", x = "Region", y = "Count", fill = "Quartile Category") +
  scale_fill_manual(values = quartile_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#State compiled data, one for suburban agencies and one for non suburban

# Subset for suburban data
suburban_data <- sorted_agencies %>%
  filter(SUB == "(1) Suburban")
# Subset for non-suburban data
non_suburban_data <- sorted_agencies %>%
  filter(SUB == "(0) Non-suburban")


#Summarizing the data state wise
state_criminal_counts_suburban <- suburban_data %>%
  group_by(STATE,STNAME) %>%
  summarise(total_agencies = n(), #will be 1 even if the agency reported no perpetrators(total reports would be a better name)
            total_perps = sum(total_perps), 
            criminal_rate = sum(total_perps) / sum(POP),
            TAM=sum(TAM),
            TAF=sum(TAF),
            TJ=sum(TJ),
            TJM=sum(TJM),
            TJF=sum(TJF),
            TM=sum(TJM+TAM),
            TF=sum(TJF+TAF),
            TW=sum(TW),
            TB=sum(TB),
            TI=sum(TI),
            TA=sum(TA),
            TH=sum(TH),
            TNH=sum(TNH),
            DIV=DIV[1],
            POP=sum(POP))
state_criminal_counts_suburban


state_criminal_counts_non_suburban <- non_suburban_data %>%
  group_by(STATE,STNAME) %>%
  summarise(total_agencies = n(), #will be 1 even if the agency reported no perpetrators(total reports would be a better name)
            total_perps = sum(total_perps), 
            criminal_rate = sum(total_perps) / sum(POP),
            TAM=sum(TAM),
            TAF=sum(TAF),
            TJ=sum(TJ),
            TJM=sum(TJM),
            TJF=sum(TJF),
            TM=sum(TJM+TAM),
            TF=sum(TJF+TAF),
            TW=sum(TW),
            TB=sum(TB),
            TI=sum(TI),
            TA=sum(TA),
            TH=sum(TH),
            TNH=sum(TNH),
            DIV=DIV[1],
            POP=sum(POP))
state_criminal_counts_non_suburban

#Male-Female Perp-Split state wise
ggplot(state_criminal_counts, aes(x = STATE)) +
  geom_bar(aes(y = TM+TF, fill = "Total Males"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TF, fill = "Total Females"), stat = "identity", position = "stack") +
  labs(title = "State-wise Division of Perpetrators by Sex", x = "State", y = "Count", fill = "Division") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Adult-Juvenile Perp-Split state wise
ggplot(state_criminal_counts, aes(x = STATE)) +
  geom_bar(aes(y =total_perps, fill = "Total Adults"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TJ, fill = "Total Juveniles"), stat = "identity", position = "stack") +
  labs(title = "State-wise Division of Perpetrators by Legal Age", x = "State", y = "Count", fill = "Division") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Race-Split in Perpetrators
ggplot(state_criminal_counts, aes(x = STATE)) +
  geom_bar(aes(y =TW+TB+TA+TI, fill = "Indian"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TW+TB+TA, fill = "Asian"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TW+TB, fill = "Black"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TW, fill = "White"), stat = "identity", position = "stack") +
  labs(title = "Race-wise Division of Perpetrators", x = "State", y = "Count", fill = "Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hispanic/Nonhispanic Split in Perpetrators
ggplot(state_criminal_counts, aes(x = STATE)) +
  geom_bar(aes(y =TH+TNH, fill = "Non-Hispanic"), stat = "identity", position = "stack") +
  geom_bar(aes(y = TH, fill = "Hispanic"), stat = "identity", position = "stack") +
  labs(title = "State-wise Division of Perpetrators by Hispanicism", x = "State", y = "Count", fill = "Hispanicism") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Linear Regression Analysis

lm_sub<-lm(scale(criminal_rate)~scale(TAM)+scale(TJF)+scale(TJM)+scale(TW)+scale(TB)+
          scale(TI)+scale(TH)+scale(POP)+scale(total_agencies)+as.factor(DIV),data=state_criminal_counts_suburban)
summary(lm_sub)

lm_non_sub<-lm(scale(criminal_rate)~scale(TAM)+scale(TJF)+scale(TJM)+scale(TW)+scale(TB)+
          scale(TI)+scale(TH)+scale(POP)+scale(total_agencies)+as.factor(DIV),data=state_criminal_counts_non_suburban)
summary(lm_non_sub)

#IS there a correlation between total agencies and criminal rate?
ggplot(state_criminal_counts_suburban, aes(x = total_agencies, y = criminal_rate)) +
  geom_point() +
  labs(title = "Suburban Data: Total Agencies vs. Criminal Rate", x = "Total Agencies", y = "Criminal Rate") +
  theme_minimal()
ggplot(state_criminal_counts_non_suburban, aes(x = total_agencies, y = criminal_rate)) +
  geom_point() +
  labs(title = "Non-Suburban Data: Total Agencies vs. Criminal Rate", x = "Total Agencies", y = "Criminal Rate") +
  theme_minimal()

#IS there a correlation between suburban and criminal rate?
ggplot(state_criminal_counts_suburban, aes(x=DIV,y = criminal_rate, col = as.factor(DIV))) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Suburban Data: Criminal Rate vs Division",
       y = "Criminal Rate",
       x="Division",
       shape = "Division",
       color="Division") +
  theme_minimal() +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21, 22,23,24))

ggplot(state_criminal_counts_non_suburban, aes(x=DIV,y = criminal_rate, col = as.factor(DIV))) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Non Suburabn Data: Criminal Rate vs Division",
       y = "Criminal Rate",
       x="Division",
       shape = "Division",
       color="Division") +
  theme_minimal() +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21, 22,23,24))

#IS there a correlation between population and criminal rate?
ggplot(state_criminal_counts_suburban, aes(x = POP, y = criminal_rate)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Suburban Data: Criminal Rate vs Population",
       x = "Population",
       y = "Criminal Rate")+
  theme_minimal()

ggplot(state_criminal_counts_non_suburban, aes(x = POP, y = criminal_rate)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Non-Suburban Data: Criminal Rate vs Population",
       x = "Population",
       y = "Criminal Rate")+
  theme_minimal()

#Residual Plot
residuals_sub <- data.frame(residuals = residuals(lm_sub), predicted = fitted(lm_sub))
ggplot(residuals_sub, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Suburban Residual Plot",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

residuals_non_sub <- data.frame(residuals = residuals(lm_non_sub), predicted = fitted(lm_non_sub))
ggplot(residuals_non_sub, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Non Suburban Residual Plot",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()


#Q-Q Norm
qqnorm(residuals(lm_sub))
qqline(residuals(lm_sub))

qqnorm(residuals(lm_non_sub))
qqline(residuals(lm_non_sub))

#Shapiro-Wilk Normality Test
shapiro.test(residuals(lm_sub))
shapiro.test(residuals(lm_non_sub))

#Correlation
print(round(cor(state_criminal_counts_suburban[,c('TAM','TJF','TJM','TW','TB','TI','TH','POP','total_agencies')]),2))
print(round(cor(state_criminal_counts_non_suburban[,c('TAM','TJF','TJM','TW','TB','TI','TH','POP','total_agencies')]),2))

#Variance Inflation Factor
vif(lm_sub)
vif(lm_non_sub)

#"Naively" removing some of the variables with high VIF values and low estimated coeffiecint and high p-values 
#that doesnt have serious effect on our adjusted R - squared value
lm_sub_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+
          scale(TI)+scale(TH)+scale(POP)+scale(total_agencies)+as.factor(DIV),data=state_criminal_counts_suburban)
summary(lm_sub_2)

lm_non_sub_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+
               scale(TI)+scale(TH)+scale(POP)+scale(total_agencies)+as.factor(DIV),data=state_criminal_counts_non_suburban)
summary(lm_non_sub_2)

vif(lm_sub_2)
vif(lm_non_sub_2)
print(round(cor(state_criminal_counts[,c('TW','TB','TI','TH','POP','total_agencies')]),2))

#Reference- "https://nces.ed.gov/programs/digest/d22/tables/dt22_102.30.asp
#Reference- "https://beef2live.com/story-ranking-states-area-89-118259"
#Reference-"https://www.nytimes.com/elections/2016/results/president"

scraped_data1 <- data.frame(
  STATE = c(
    "(01) Alabama", "(02) Arizona", "(03) Arkansas", "(04) California", "(05) Colorado", "(06) Connecticut",
    "(07) Delaware", "(10) Georgia", "(11) Idaho", "(12) Illinois", "(13) Indiana", "(14) Iowa",
    "(15) Kansas", "(16) Kentucky", "(17) Louisiana", "(18) Maine", "(19) Maryland", "(20) Massachusetts",
    "(21) Michigan", "(22) Minnesota", "(23) Mississippi", "(24) Missouri", "(25) Montana", "(26) Nebraska",
    "(27) Nevada", "(28) New Hampshire", "(29) New Jersey", "(30) New Mexico", "(31) New York", "(32) North Carolina",
    "(33) North Dakota", "(34) Ohio", "(35) Oklahoma", "(36) Oregon", "(37) Pennsylvania", "(38) Rhode Island",
    "(39) South Carolina", "(40) South Dakota", "(41) Tennessee", "(42) Texas", "(43) Utah", "(44) Vermont",
    "(45) Virginia", "(46) Washington", "(47) West Virginia", "(48) Wisconsin", "(49) Wyoming", "(50) Alaska",
    "(51) Hawaii"
  ),
  pol_aff = c(
    "R", "R", "R", "D", "D", "D", "D", "R", "R", "D", "R", "R", "R", "R", "R", "D", "D", "D", "R",
    "D", "R", "R", "R", "R", "D", "D", "D", "D", "D", "R", "R", "R", "R", "D", "R", "D", "R", "R", "R",
    "R", "R", "D", "D", "D", "R", "R", "R", "R", "D"
  ),
  income = c(
      54800, 65800, 51900, 85200, 81700, 83500, 74400, 65700, 64600, 73300, 61000, 65400, 65800, 55400, 54100,
      62400, 91900, 91000, 63100, 79100, 48500, 60800, 60600, 67000, 67100, 82600, 90900, 55000, 76400, 60800,
      68400, 62100, 57700, 71100, 67300, 75400, 59600, 63100, 59400, 67900, 80300, 66800, 81000, 83400, 51800,
      68000, 68900, 80000, 88100
    ),
  area= c(
    52420.1, 113990.3, 53178.6, 163694.7, 104093.7, 5543.4, 2488.7, 59425.2, 83569, 57913.6, 36419.6, 56272.8,
    82278.4, 40407.8, 52378.1, 35379.7, 12405.9, 10554.4, 96713.5, 86935.8, 48131.8, 69707, 147039.7, 77347.8,
    110571.8, 9349.2, 8722.6, 121590.3, 54555, 53819.2, 70698.3, 44825.6, 69898.9, 98378.5, 46054.4, 1544.9,
    32020.5, 77115.7, 42144.3, 268596.5, 84996.9, 9616.4, 42774.9, 71298, 24230, 65496.4, 97813, 665384, 10931.7
  )
)

#Similar data except the state of Illinois cause we dont have any relevant data on that's state's suburban agencies 
scraped_data2 <- data.frame(
  STATE = c(
    "(01) Alabama", "(02) Arizona", "(03) Arkansas", "(04) California", "(05) Colorado", "(06) Connecticut",
    "(07) Delaware", "(10) Georgia", "(11) Idaho", "(13) Indiana", "(14) Iowa",
    "(15) Kansas", "(16) Kentucky", "(17) Louisiana", "(18) Maine", "(19) Maryland", "(20) Massachusetts",
    "(21) Michigan", "(22) Minnesota", "(23) Mississippi", "(24) Missouri", "(25) Montana", "(26) Nebraska",
    "(27) Nevada", "(28) New Hampshire", "(29) New Jersey", "(30) New Mexico", "(31) New York", "(32) North Carolina",
    "(33) North Dakota", "(34) Ohio", "(35) Oklahoma", "(36) Oregon", "(37) Pennsylvania", "(38) Rhode Island",
    "(39) South Carolina", "(40) South Dakota", "(41) Tennessee", "(42) Texas", "(43) Utah", "(44) Vermont",
    "(45) Virginia", "(46) Washington", "(47) West Virginia", "(48) Wisconsin", "(49) Wyoming", "(50) Alaska",
    "(51) Hawaii"
  ),
  pol_aff = c(
    "R", "R", "R", "D", "D", "D", "D", "R", "R", "R", "R", "R", "R", "R", "D", "D", "D", "R",
    "D", "R", "R", "R", "R", "D", "D", "D", "D", "D", "R", "R", "R", "R", "D", "R", "D", "R", "R", "R",
    "R", "R", "D", "D", "D", "R", "R", "R", "R", "D"
  ),
  income = c(
    54800, 65800, 51900, 85200, 81700, 83500, 74400, 65700, 64600, 61000, 65400, 65800, 55400, 54100,
    62400, 91900, 91000, 63100, 79100, 48500, 60800, 60600, 67000, 67100, 82600, 90900, 55000, 76400, 60800,
    68400, 62100, 57700, 71100, 67300, 75400, 59600, 63100, 59400, 67900, 80300, 66800, 81000, 83400, 51800,
    68000, 68900, 80000, 88100
  ),
  area= c(
    52420.1, 113990.3, 53178.6, 163694.7, 104093.7, 5543.4, 2488.7, 59425.2, 83569, 36419.6, 56272.8,
    82278.4, 40407.8, 52378.1, 35379.7, 12405.9, 10554.4, 96713.5, 86935.8, 48131.8, 69707, 147039.7, 77347.8,
    110571.8, 9349.2, 8722.6, 121590.3, 54555, 53819.2, 70698.3, 44825.6, 69898.9, 98378.5, 46054.4, 1544.9,
    32020.5, 77115.7, 42144.3, 268596.5, 84996.9, 9616.4, 42774.9, 71298, 24230, 65496.4, 97813, 665384, 10931.7
  )
)

final_data_sub<-merge(scraped_data2,state_criminal_counts_suburban,by="STATE")
final_data_non_sub<-merge(scraped_data1,state_criminal_counts_non_suburban,by="STATE")

lm_sub_3<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+scale(POP)+
          scale(TI)+scale(TH)+scale(area)+scale(income)+as.factor(pol_aff)+as.factor(DIV),data=final_data_sub)
summary(lm_sub_3)

lm_non_sub_3<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+scale(POP)+
               scale(TI)+scale(TH)+as.factor(pol_aff)+as.factor(DIV),data=final_data_non_sub)
summary(lm_non_sub_3)

#IS there a correlation between political_affiliation and criminal rate?
ggplot(final_data_sub, aes(x = as.factor(DIV), y = criminal_rate, col= as.factor(pol_aff))) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Suburban Data: Criminal Rate vs Division",
       x = "Division",
       y = "Criminal Rate",
       shape = "Suburban",
       color="Political Affiliation") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) 

ggplot(final_data_non_sub, aes(x = as.factor(DIV), y = criminal_rate, col= as.factor(pol_aff))) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3) +
  labs(title = "Non-Suburban Data: Criminal Rate vs Division",
       x = "Division",
       y = "Criminal Rate",
       shape = "Suburban",
       color="Political Affiliation") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) 


#IS there a correlation between median income and criminal rate?
ggplot(final_data_sub, aes(x =income, y = criminal_rate)) +
  geom_point() +
  labs(title = "Suburban Median State Income vs. Criminal Rate", x = "Median Income", y = "Criminal Rate") +
  theme_minimal()

ggplot(final_data_non_sub, aes(x =income, y = criminal_rate)) +
  geom_point() +
  labs(title = "Non-Suburban Median State Income vs. Criminal Rate", x = "Median Income", y = "Criminal Rate") +
  theme_minimal()

#IS there a correlation between area and criminal rate?
ggplot(final_data_sub, aes(x =area, y = criminal_rate)) +
  geom_point() +
  labs(title = "Suburban Median Area vs. Criminal Rate", x = "Area", y = "Criminal Rate") +
  theme_minimal()

ggplot(final_data_non_sub, aes(x =area, y = criminal_rate)) +
  geom_point() +
  labs(title = "Non-Suburban Median Area vs. Criminal Rate", x = "Area", y = "Criminal Rate") +
  theme_minimal()

#IS there a correlation between population and criminal rate?
ggplot(final_data_sub, aes(x =POP, y = criminal_rate)) +
  geom_point() +
  labs(title = "Suburban: Population vs. Criminal Rate", x = "Population", y = "Criminal Rate") +
  theme_minimal()

ggplot(final_data_non_sub, aes(x =POP, y = criminal_rate)) +
  geom_point() +
  labs(title = "Non-Suburban: Population vs. Criminal Rate", x = "Population", y = "Criminal Rate") +
  theme_minimal()

#Hypothesis testing
#Null: B=0 (for political affiliation)
#Alternate: B !=0
lm_sub_3_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+scale(POP)+
               scale(TI)+scale(TH)+scale(area)+scale(income)+as.factor(DIV),data=final_data_sub)
res_1<-anova(lm_sub_3_2,lm_sub_3)
res_1

lm_non_sub_3_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+scale(POP)+
                 scale(TI)+scale(TH)+as.factor(DIV),data=final_data_non_sub)
res_2<-anova(lm_non_sub_3_2,lm_non_sub_3)
res_2

#Null: B>=0 (for population)
#Alternate: B <0
lm_sub_3_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+as.factor(pol_aff)+
                 scale(TI)+scale(TH)+scale(area)+scale(income)+as.factor(DIV),data=final_data_sub)
res_1<-anova(lm_sub_3_2,lm_sub_3)
res_1

lm_non_sub_3_2<-lm(scale(criminal_rate)~scale(TW)+scale(TB)+as.factor(pol_aff)+
                     scale(TI)+scale(TH)+as.factor(DIV),data=final_data_non_sub)
res_2<-anova(lm_non_sub_3_2,lm_non_sub_3)
res_2

#Null: B<=0 (for white criminals)
#Alternate: B>0
lm_sub_3_2<-lm(scale(criminal_rate)~scale(POP)+scale(TB)+as.factor(pol_aff)+
                 scale(TI)+scale(TH)+scale(area)+scale(income)+as.factor(DIV),data=final_data_sub)
res_1<-anova(lm_sub_3_2,lm_sub_3)
res_1

lm_non_sub_3_2<-lm(scale(criminal_rate)~scale(POP)+scale(TB)+as.factor(pol_aff)+
                     scale(TI)+scale(TH)+as.factor(DIV),data=final_data_non_sub)
res_2<-anova(lm_non_sub_3_2,lm_non_sub_3)
res_2

#Null: B=0 (for region)
#Alternate: B!=0
lm_sub_3_2<-lm(scale(criminal_rate)~scale(POP)+scale(TB)+scale(TW)+as.factor(pol_aff)+
                 scale(TI)+scale(TH)+scale(area)+scale(income),data=final_data_sub)
res_1<-anova(lm_sub_3_2,lm_sub_3)
res_1

lm_non_sub_3_2<-lm(scale(criminal_rate)~scale(POP)+scale(TB)+scale(TW)+as.factor(pol_aff)+
                     scale(TI)+scale(TH),data=final_data_non_sub)
res_2<-anova(lm_non_sub_3_2,lm_non_sub_3)
res_2


##Some More EDA
#getting worst criminal rates top 5 states suburban and non suburban areas
sorted_suburban_criminal_rates <- state_criminal_counts_suburban[order(state_criminal_counts_suburban$criminal_rate), ]
bottom_5_suburban_states <- tail(sorted_suburban_criminal_rates, 5)
sorted_non_suburban_criminal_rates <- state_criminal_counts_non_suburban[order(state_criminal_counts_non_suburban$criminal_rate), ]
bottom_7_non_suburban_states <- tail(sorted_non_suburban_criminal_rates, 7)#Illinois and delaware out bcoz only one agency reported data in format
bottom_5_suburban_states
bottom_7_non_suburban_states

#Alaska, Hawaii, Tenessee
#Delaware, Louisiana,  Mississippi
#South Dakota and Wyoming

#Worst suburban states #Alaska, Hawaii, Tenessee
#We can see that the data has extreme values, so we will use the quartiles instead of mean, 
#for the categorization of agencies based on criminal_rate
quartiles <- quantile(sorted_agencies$criminal_rate, c(0, 0.25, 0.75, 1))
sorted_agencies$CR_category <- cut(
  sorted_agencies$criminal_rate,
  breaks = quartiles,
  labels = c("First Quartile", "Interquartile Range", "Last Quartile"),
  include.lowest = TRUE
)

# State-wise agencies performance
quartile_colors <- c("First Quartile" = "green", "Interquartile Range" = "lightblue", "Last Quartile" = "red")
ggplot(sorted_agencies, aes(x = STATE, fill = CR_category))+
  geom_bar() +
  labs(title = "State-wise Agency Analysis with Quartile Categories", x = "State", y = "Count", fill = "Quartile Category") +
  scale_fill_manual(values = quartile_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

selected_states <- c("(49) Wyoming","(40) South Dakota")  # Replace with the states of your choice

# Filter the data for the selected states
selected_data <- sorted_agencies %>%
  filter(STATE %in% selected_states)
selected_data

# Calculate the count of agencies in each category for each state
category_counts <- selected_data %>%
  group_by(STATE, CR_category) %>%
  summarize(count = n()) %>%
  ungroup()

# Calculate the total count of agencies for each state
state_totals <- category_counts %>%
  group_by(STATE) %>%
  summarize(total = sum(count))

# Calculate the percentage of agencies in each category for each state
category_percentages <- category_counts %>%
  left_join(state_totals, by = "STATE") %>%
  mutate(percentage = count / total * 100)

for (state in selected_states) {
  state_data <- category_percentages %>%
    filter(STATE == state)
  
  pie_chart <- ggplot(state_data, aes(x = "", y = percentage, fill = CR_category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Agency Distribution in", state),
         fill = "Quartile Category",
         x = NULL,
         y = NULL) +
    scale_fill_manual(values = quartile_colors) +
    theme_void() +
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5),size=10)
  
  print(pie_chart)
}











# region_criminal_counts <- state_criminal_counts %>%
#   group_by(DIV) %>%
#   summarise(total_states = n(), #will be 1 even if the agency reported no perpetrators(total reports would be a better name)
#             total_perps = sum(total_perps), 
#             criminal_rate = sum(total_perps) / sum(POP),
#             TAM=sum(TAM),
#             TAF=sum(TAF),
#             TJ=sum(TJ),
#             TJM=sum(TJM),
#             TJF=sum(TJF),
#             TM=sum(TJM+TAM),
#             TF=sum(TJF+TAF),
#             TW=sum(TW),
#             TB=sum(TB),
#             TI=sum(TI),
#             TA=sum(TA),
#             TH=sum(TH),
#             TNH=sum(TNH),
#             POP=sum(POP))
# region_criminal_counts
# 
# ggplot(state_criminal_counts, aes(x = DIV)) +
#   geom_bar(aes(y = TM+TF, fill = "Total Males"), stat = "identity", position = "stack") +
#   geom_bar(aes(y = TF, fill = "Total Females"), stat = "identity", position = "stack") +
#   labs(title = "Region-wise Division of Perpetrators by Sex", x = "Region", y = "Count", fill = "Division") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))










