# Lahmann's Analysis 

## For this project, we are going to perform analysis in different divisions to determine
## if there are correlations with attendance, performance, salary, and size. 

### Loading the Libraries

library(knitr)
library(tibble)
library(markdown)
library(MASS)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(lubridate)
library(broom)
library(dotwhisker)
library(cem)
library(car)
library(plyr)
library(tidyr)
library(scales)
library(plotly)
library(DT)

## Changing the Working Directory

setwd('./Documents/Baseball/2016')


## Reading the Datasets Based on Attendance, Pitching, Player Information, Salary, and Batting 
attendance <- read.csv("./teams.csv",na.strings=c("","NA"))
pitching <- read.csv("./pitching.csv")
player_info <- read.csv("./player.csv")
player_salary <- read.csv("./salaries.csv")
batters <- read.csv("./batting.csv", stringsAsFactors = FALSE)
fielding <- read_csv("./fielding.csv",col_types = cols())

## Looking through the First Set of Rows on Each Dataset
head(attendance, 5)
head(pitching, 5)
head(player_info, 5)
head(player_salary, 5)
head(batters, 5)
head(fielding, 5)

## Looking at the Structure of the Dataset
str(attendance)
str(pitching)
str(player_info)
str(player_salary)
str(batters)
str(fielding)

## Checking for NA Values
any(is.na(attendance))
any(is.na(pitching))
any(is.na(player_info))
any(is.na(player_salary))
any(is.na(batters))
any(is.na(fielding))

## Fixing the Attendance Data

attendance <- subset(attendance, attendance != 'NA')
attendance <- subset(attendance, yearID > 1969)
attendance[ , colSums(is.na(attendance)) == 0]

## Fixing the Pitching Data

pitching <- subset(pitching, pitching  !='NA')
pitching[ , colSums(is.na(pitching)) == 0]

## Fixing the Batters Data

batters <- subset(batters, batters !='NA')
batters$SF <- as.integer(batters$SF)

## Fixing the Player Info Data

player_info <- subset(player_info, player_info != 'NA')

## Fixing the Fielding Data
fieldING <- subset(fielding, fielding != 'NA')
fieldING <- subset(fielding, E != 'NA')
fielding = subset(fielding, yearID > 1920)

names(player_salary) <- c("yearID","teamID","lgID","playerID","salary")

## Add Salary to Batting and Pitching Information

batters <- dplyr::inner_join(batters,player_salary,by=c("playerID","teamID","lgID","yearID"))
pitching <- dplyr::inner_join(pitching,player_salary,by=c("playerID","teamID","lgID","yearID"))

# Get Rid of the Pitchers in Batters data

batters <- dplyr::anti_join(batters,pitching,by="playerID")


## Visualizing the Attendance for Baseball

ggplot(attendance, aes(yearID, attendance)) + geom_bar(stat="identity", aes(fill=G>157))+
  guides(fill=FALSE)+
  ggtitle("Total Home Attendance on an Annual Basis (Lockouts in 72, 81, 94, and 95)")+
  scale_y_continuous(labels = comma) +
  theme_minimal()

## Product on the Field
product <- ggplot(attendance, aes(W, attendance)) + geom_point(color="darkblue") + 
  labs(x="Wins", y="Attendance") +
  scale_y_continuous(labels = comma) +
  ggtitle("Wins Based on Attendance") +
  geom_smooth(method = "lm", col = "brown")

ggplotly(product)

hrattn <- ggplot(attendance, aes(HR, attendance)) + geom_point(color="brown") + 
  labs(x="Home Runs", y="Attendance") + 
  scale_y_continuous(labels = comma) +
  ggtitle("Home Runs Based on Attendance")+
  geom_smooth(method = "lm", col = "orange")

ggplotly(hrattn)


## Comparing them to the Losses on the Field

lossatn <- ggplot(attendance, aes(L, attendance)) + geom_point(color="magenta") + 
  labs(x="Losses", y="Attendance") +
  scale_y_continuous(labels = comma) +
  ggtitle("Losses Based on Attendance")+
  geom_smooth(method = "lm", col = "green") +
  theme_minimal()

ggplotly(lossatn)

## Comparing them to the Errors

errors <- ggplot(attendance, aes(E, attendance)) + geom_point(color="darkred") + 
  labs(x="Errors", y="Attendance") +
  scale_y_continuous(labels = comma) +
  ggtitle("Attendance Based on Errors")+
  geom_smooth(method = "lm", col = "mistyrose")

ggplotly(errors)

## Joining the Moneyball Data

salary <- select(player_salary, yearID, teamID, salary)
salary <- group_by(salary, yearID, teamID) %>% summarise(dollars = sum(salary))
team_salary <- left_join(attendance, salary)
team_salary <- subset(team_salary, yearID > 1984) #salary data starts at 1984


## Team Salary vs. Attendance

ggplot(team_salary, aes(dollars, attendance)) + geom_point(color="darkgreen",size=4, shape=36) + 
  labs(x="Team Salary", y="Attendance") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  ggtitle("Team Salary & Attendance")+
  geom_smooth(method = "lm", col = "blue")

# Creating the Moneyball Information (From 1969)

## creating a New Variable for Run Difference 

attendance$RD <- attendance$R - attendance$RA

## Relationship between wins and run difference

ggplot(data = attendance) + geom_smooth(mapping = aes(x = W, y = RD), color="darkblue") + 
  ggtitle('Relationship Between Wins and Run Difference') + 
  labs(x="Wins", y="Runs Difference")


## Creating a Linear Regression Model for Run Difference

wins_reg <- lm(W ~ RD, data = attendance)
summary(wins_reg)


## Histogram and qqplots for Wins Reg
hist(rnorm(wins_reg$residuals), col = "red", main = "Wins for Run Difference", xlab = "Residuals")

## QQ Plots for Wins Reg

qqnorm(wins_reg$residuals)
qqline(wins_reg$residuals, col = "green")


## Sample Mean Calculations for Wins Reg with Rnorm

winsregrx <- rnorm(wins_reg$residuals)
fitdistr(winsregrx, "normal")
wrfit <- fitdistr(winsregrx, "normal")
wrfit$estimate

## Sample Mean for Wins Reg 
sum(wins_reg$residuals)/length(wins_reg$residuals)

## Shapiro-Wilk Test for Wins Reg

wrnd <- rnorm(wins_reg$residuals)
shapiro.test(wrnd)


## Linear Regression Model for Scored Runs

runs_scored <- lm(R ~ AB + H + X2B + X3B + HR + BB + SB + CS, data = attendance)
summary(runs_scored)


## Creating a Logistic Regression Model for Scored Runs for Comparisions

runs_scored <- lm(R ~ AB + H + X2B + X3B + HR + BB + SB + CS, data = attendance, family = "binomial")
summary(runs_scored)


## Histogram for Runs Scored Residuals

hist(rnorm(runs_scored$residuals), col = "blue", main = "Runs Scored Residuals", xlab = "Residuals")

## QQ Plots for Runs Scored Residuals

qqnorm(runs_scored$residuals)
qqline(runs_scored$residuals, col = "salmon")


## Sample Mean Calculations for Scored Residuals with Rnorm

runsscoredrx <- rnorm(runs_scored$residuals)
fitdistr(runsscoredrx, "normal")
rsfit <- fitdistr(runsscoredrx, "normal")
rsfit$estimate

## Sample Mean for Runs Scored

sum(runs_scored$residuals)/length(runs_scored$residuals)


## Shapiro-Wilk Test for Scored Residuals

srnd <- rnorm(runs_scored$residuals)
shapiro.test(srnd)


## T-Test for Scored Residuals

srtest <- t.test(srnd)
srtest$p.value

## Runs Allowed Regression

runs_allowed <- lm(R ~ SV + IPouts + H + ER + HR + BB + SO + WP + HBP + BK, data = pitching)
summary(runs_allowed)

## Histogram for Runs Allowed

hist(rnorm(runs_allowed$residuals), col = "yellow", main = "Runs Allowed Residuals", xlab = "Residuals")


## QQ Plots for Runs Allowed

qqnorm(runs_allowed$residuals, col = "green")
qqline(runs_allowed$residuals, col = "darkred")

## Sample Mean Calculations for Wins Reg with Rnorm

raregrx <- rnorm(runs_allowed$residuals)
fitdistr(raregrx, "normal")
rafit <- fitdistr(raregrx, "normal")
rafit$estimate

## Sample Mean for Runs Allowed

sum(runs_allowed$residuals)/length(runs_allowed$residuals)


## T-Test for Wins Reg

testwr <- t.test(runs_allowed$residuals)
testwr$p.value

## Batting Averages

test<- subset(attendance, yearID >= 1985 & yearID < 2002)
test$obp <- (test$H + test$HBP + test$BB)/(test$AB + test$BB + test$HBP + test$SF)
test$slg <- ((test$BB) + (2 * test$X2B) + (3 * test$X3B) + (4 * test$HR))/(test$AB)
test$ba <- (test$H)/(test$AB)

## Comparision of OBP, SLG and BA
ggplot(test, aes(W)) + 
  geom_smooth(aes(y = obp,  color="obp")) + 
  geom_smooth(aes(y = slg,  color="slg")) + 
  geom_smooth(aes(y = ba,  color="ba")) + 
  ggtitle("Comparison of OBP, SLG and BA") +
  labs(x="Wins", y="%")


## Performance During the Seasons Between 2002-2016

get_wins <- attendance
get_wins$win_ratio <- get_wins$W/get_wins$G
oakland <- subset(get_wins, teamID == 'OAK' & yearID >= 2002)
athletics <- ggplot(data = oakland) + 
  geom_line(mapping = aes(x = yearID, y = win_ratio), color="darkseagreen") + 
  ggtitle("Oakland A's Performance During 2002-2016") + 
  labs(x="year", y="win-ratio") + 
  scale_x_continuous(breaks = round(seq(min(oakland$year), max(oakland$year), by = 1),1))

ggplotly(athletics)

## Salaries Overtime from 1985-2016 with Differences of Leagues

p <- ggplot(player_salary, aes(x=factor(yearID), y=salary, fill = lgID)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Annual Salary") +
  ggtitle("Breakdown with the League Differences in Salaries from 1985") +
  scale_x_discrete(breaks=unique(player_salary$yearID)[c(TRUE, FALSE)]) +
  scale_y_continuous(label=dollar) +
  theme_light(base_size=10)
ggplotly(p)


## Salaries Overtime from 1985-2016 Without the League

nofill_p <- ggplot(player_salary, aes(x=factor(yearID), y=salary)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Annual Salary") +
  scale_x_discrete(breaks=unique(player_salary$yearID)[c(TRUE, FALSE)]) +
  scale_y_continuous(label=dollar) +
  theme_light(base_size=10)
ggplotly(nofill_p)


## Batters Overtime from 1985-2016

homeruns_by_year <- batters %>% 
  group_by(yearID) %>% 
  summarise(HR = sum(HR))

p1 <- ggplot(homeruns_by_year, aes(x=yearID, y=HR)) +
  geom_point() +
  xlab("Year") +
  ylab("Home Runs") +
  scale_x_discrete(breaks=unique(homeruns_by_year$yearID)[c(TRUE, FALSE)]) +
  scale_y_continuous() +
  theme_light(base_size=10) + 
  ggtitle("Batters Overtime")
ggplotly(p1)



## Looking Up the One Hit Wonders 

## Consider only stats after 1950; keep only needed columns
hr_since50 <- batters %>% filter(yearID>=1950) %>% select(playerID,yearID,stint,HR);

## If a player has multiple "stints" in one season,calculate the total HR for the season
hr_season_since50 <- hr_since50 %>% group_by(playerID, yearID) %>% summarise(seasonHR=sum(HR));

## Calculate career total HR, career season count for players. Remove players who appeared for lesser than 5 seasons.
hr_season_since50 <- hr_season_since50 %>% group_by(playerID) %>% mutate(careerHR=sum(seasonHR),career_year=n()) %>% filter(career_year>=5);

## Calculate career average HR per season for each player. Calculate the ratio of single year HR to career average
yearly_hr_vs_career_avg <- hr_season_since50 %>% mutate(career_per_season=careerHR/career_year, year_vs_avg=seasonHR/career_per_season, round = 2);

## Joining Player Data for Birth and Year

hr_fluke_ratio_and_age <- inner_join(yearly_hr_vs_career_avg, player_info, by="playerID") %>% 
  mutate(age=yearID-birthYear)%>% 
  select(playerID, nameFirst, nameLast, yearID, age, seasonHR, careerHR, career_year, career_per_season, year_vs_avg) %>% 
  ungroup();


## Keep only the rows for which the player hit more than 2x HRs than career average, and HR>=10 on that year.

out<-filter(hr_fluke_ratio_and_age, seasonHR>=10, year_vs_avg>=2) %>% arrange(desc(year_vs_avg));



## Most Deviation Form

datatable(out, class='compact');


## Most Home Runs in One Season

out2 <- out %>% arrange(desc(seasonHR));
datatable(out2, class='compact');


## Visualization of the One-Hit Wonders

onehit <- ggplot(out, aes(age,year_vs_avg)) + 
  geom_point(color = "blue", alpha = 0.4) + 
  labs(x="Player Age", y="Season HR per Career Average") + 
  ggtitle("Players With One Season HR More Than 2x Career Average") + 
  theme_minimal();
ggplotly(onehit)

# Taking a Look at the Closers


options(dplyr.width = Inf)

## Looking at Data After 1985
pitching <- pitching  %>% filter(yearID>=1985) %>% 
  select(playerID,yearID,stint,W,L,G,GS,GF,SV,IPouts,H,BB,ER,SO,R)
## Calculating the totals for the season based on stints
pitching <- pitching  %>% 
  group_by(playerID, yearID) %>% 
  summarise(W=sum(W), L=sum(L), G=sum(G), GS=sum(GS),GF=sum(GF),SV=sum(SV),IPouts=sum(IPouts),H=sum(H),BB=sum(BB),ER =sum(ER),SO=sum(SO),R=sum(R)) %>% 
  ungroup();

## Players not started this year

rp_pitching <- pitching %>% filter(GS==0);

## Getting player Names and Information
player_info <- player_info %>% 
  separate(debut, c("debut"), sep="-") %>% 
  select(playerID, birthYear, nameFirst, nameLast, debut);
rp_pitching <- inner_join(rp_pitching, player_info, by="playerID")%>% 
  mutate(id_year=paste(playerID, yearID, sep="_"));
player_salary <- player_salary %>% 
  mutate(id_year = paste(playerID, yearID, sep="_")) %>% 
  select(id_year,salary);
rp_pitching <- inner_join(rp_pitching, player_salary, by="id_year");

## Percentage of Finishing Off

rp_pitching<-rp_pitching %>% 
  mutate(gf_ratio=GF/162, gf_percent=GF/G)

## Calculating the Mean Salary Annually and Analyzing the WHIP (Walk + Hit Per Inning Pitched)

rp_pitching<-rp_pitching %>% 
  group_by(yearID) %>% 
  mutate(season_avg_salary=mean(salary)) %>% 
  ungroup() %>% 
  mutate(salary_ratio=salary/season_avg_salary, season=yearID-as.numeric(debut), whip=(H+BB)/IPouts*3, era=ER/IPouts*27);

## Replacing the Datapoints with NA

rp_pitching<-do.call(data.frame,lapply(rp_pitching, function(x) replace(x, is.infinite(x),NA)))


## Looking at Salaries Per Peformance
cor(rp_pitching$salary_ratio, rp_pitching$whip,use="pairwise.complete")
cor(rp_pitching$salary_ratio, rp_pitching$era,use="pairwise.complete")


## Who are the Closers?

closers<- rp_pitching %>% filter(SV > 15);
nonclosers<- rp_pitching %>% filter(SV <= 15);
fa_closers <- closers %>% filter(season>=7&IPouts>=150);
fa_nonclosers <- nonclosers %>% filter(season>=7&IPouts>=150);
nrow(fa_closers)

## Non- closers

nrow(fa_nonclosers)

## Boxplots of Closers vs. Non-Closers

boxplot(fa_closers$gf_percent*100, fa_nonclosers$gf_percent*100,  
        names=c("Closers", "Non-Closers"), 
        ylab="% of Finishing a Game (Being last pitcher in game)",
        main = "Closers vs. Non-Closers",
        col = "lightcoral")

summary(fa_closers$gf_percent)

summary(fa_nonclosers$gf_percent)

## Last Pitcher in Game to Finish Up

plot(density(fa_closers$gf_percent*100), xlim=c(0,100),
     col="blue", xlab="% of Finishing a Game (Being Last Pitcher in Game)", main="")
lines(density(fa_nonclosers$gf_percent*100), col="darkred")
legend(0,0.06,c("Non-Closers", "Closers"),c("darkred", "blue"))


## Closers vs. Non-Closers Performance

summary(fa_closers$whip)
summary(fa_nonclosers$whip)
summary(fa_closers$salary_ratio)
summary(fa_nonclosers$salary_ratio)

## WHIP Difference

boxplot(fa_closers$whip, fa_nonclosers$whip, 
        names=c("Closers", "Non-Closers"), ylab="WHIP (Hit+Walk per Inning)",
        main = "Difference of WHIP",
        col = "darkolivegreen")


boxplot(fa_closers$salary_ratio, fa_nonclosers$salary_ratio, 
        names=c("Closers", "Non-Closers"), ylab="Salary / Avg. Relief Pitcher Salary",
        main = "Salary Ratio on WHIP Differences",
        col = "goldenrod")

## Scatterplot Differences

plot(fa_closers$salary_ratio,fa_closers$whip, 
     xlab="Salary / Avg. Relief Pitcher Salary", ylab="WHIP (Hit+Walk per Inning)", 
     main = "Difference Between the Salary Ratio on Relief Pitchers",
     xlim=c(0,10), ylim=c(0.6,2.0), col = "green", pch = 19)
points(fa_nonclosers$salary_ratio, fa_nonclosers$whip, col=2, pch = 19)
legend(7,2.0,c("Closers", "Non-Losers"),c("green", "red"))


low_whip_closers<-fa_closers %>% filter(whip<=1.20)
low_whip_nonclosers<-fa_nonclosers %>% filter(whip<=1.20)
nrow(low_whip_closers)
nrow(low_whip_nonclosers)

## Low- WHIP Nonclosers

boxplot(low_whip_closers$whip, low_whip_nonclosers$whip, 
        names=c("Closers", "Non-Closers"), ylab="WHIP (Hit+Walk per Inning)",
        main = "Low WHIP Non-Closers",
        col = "skyblue")

## Difference with Relief-Pitchers

boxplot(low_whip_closers$salary_ratio, low_whip_nonclosers$salary_ratio, 
        names=c("Closers", "Non-Closers"), ylab="Salary / Avg. Relief Pitcher Salary",
        main = "Difference with Relief Pitchers",
        col = "maroon")


plot(low_whip_closers$salary_ratio,low_whip_closers$whip, 
     xlab= "Salary / Avg. Relief Pitcher Salary", ylab="WHIP (Hit+Walk per Inning)",
     main = "Difference of Salaries",
     xlim=c(0,10), ylim=c(0.6,1.3), col = "blue", pch = 16)
points(low_whip_nonclosers$salary_ratio, low_whip_nonclosers$whip, col=2, pch = 16)
legend(7,1.3,c("Closers", "Non-Losers"),c("blue", "red"))

## Low-ERA Closers to Filter
low_era_closers<-fa_closers %>% filter(era<=3.0)
low_era_nonclosers<-fa_nonclosers %>% filter(era<=3.0)
nrow(low_era_closers)

nrow(low_era_nonclosers)

## Comparisons with ERA on Closers vs. Non-Closers

boxplot(low_era_closers$era, low_era_nonclosers$era, 
        names=c("Closers", "Non-Closers"), ylab="ERA (ER per 9 Innings)",
        main = "Comparisons with ERA on Closers vs. Non-Closers",
        col = "red")

boxplot(low_era_closers$salary_ratio, low_era_nonclosers$salary_ratio, 
        names=c("Closers", "Non-Closers"), ylab="Salary / Avg. Relief Pitcher Salary",
        main = "Salary Ratio Between the Closers vs. Non-Closers",
        col = "khaki")


plot(low_era_closers$salary_ratio,low_era_closers$era, 
     xlab="Salary / Avg. Relief Pitcher Salary", ylab="ERA (ER per 9 Innings)", 
     main = "Salaries for ERA Comparisons",
     xlim=c(0,10), ylim=c(0.6,3.0), col = "gold", pch = 16)
points(low_era_nonclosers$salary_ratio, low_era_nonclosers$era, pch = 16, col = "green")
legend(7,1.0,c("Closers", "Non-Losers"),c("gold", "green"))


# Errors History

## Sum the errors By yearID
errors_by_year = tapply(fieldING$E, fieldING$yearID, sum)

## Make the Year as a Factor

years = factor(fieldING$yearID)
years = levels(years)


## New Dataset with Total number of Errors by Year
ebyt2 = cbind(errors_by_year, years)
ebyt2 = data.frame(ebyt2)

## Plotting the Errors by Year

ggplot(ebyt2, aes(years, errors_by_year)) + 
  geom_bar(stat = 'identity', col = "darkred", fill = "skyblue") + 
  labs(title="Errors By Year Since 1920", x = "Years", y = "Errors By Year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_discrete(breaks = pretty(errors_by_year, n=10))

## Looking & Plotting Errors by Position

atopper = ddply(fieldING, "POS", summarise, errors_by_position = sum(E))


ggplot(atopper, aes(POS, errors_by_position)) + 
  geom_bar(stat='identity', fill = "red") + 
  labs(title="Errors by Position Since 1920", x = "Position", y = "Total # of Errors by Position")

## Looking and Plotting Errors by Team

btopper = ddply(fieldING, "teamID", summarise, errors_by_team = sum(E))


ggplot(btopper, aes(teamID, errors_by_team)) + 
  geom_bar(stat='identity', fill = 'violet') + 
  labs(title = "Errors by Team Since 1920", x = "Team", y = "Total # of Errors")





