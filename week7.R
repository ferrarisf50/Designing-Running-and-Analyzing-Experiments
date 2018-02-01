###
## Scenario:   Study in which men and women were shown a virtual human avatar 
##             that was itself either male or female, and asked to craft a persona 
##             and write a day-in-the-life scenario for that avatar. 
##
## Statistics: Factorial ANOVA, repeated measures ANOVA, main effects, 
##             interaction effects, the Aligned Rank Transform for 
##             "nonparametric ANOVAs"
###

# read in data file of avatars
avatar = read.csv("avatars.csv")
View(avatar)
avatar$Subject = factor(avatar$Subject) # convert to nominal factor
avatar$Posture_Order = factor(avatar$Posture_Order) # convert to nominal factor
summary(avatar)

# explore the Positives data
library(plyr)
ddply(avatar, ~ Sex * Avatar, function(data) summary(data$Positives))
ddply(avatar, ~ Sex * Avatar, summarise, Positives.mean=mean(Positives), Positives.sd=sd(Positives))

# histograms for two factors
hist(avatar[avatar$Sex == "Male" & avatar$Avatar == "Male",]$Positives)
hist(avatar[avatar$Sex == "Male" & avatar$Avatar == "Female",]$Positives)
hist(avatar[avatar$Sex == "Female" & avatar$Avatar == "Male",]$Positives)
hist(avatar[avatar$Sex == "Female" & avatar$Avatar == "Female",]$Positives)
boxplot(Positives ~ Sex * Avatar, data=avatar, xlab="Sex.Avatar", ylab="Positives") # boxplots
with(avatar, interaction.plot(Sex, Avatar, Positives, ylim=c(0, max(avatar$Positives)))) # interaction plot

with(avatar, interaction.plot(Avatar, Sex,Positives)) 

# now perform the two-way mixed factorial repeated measures ANOVA
library(ez)
m = ezANOVA(dv=Positives, between=c("Sex","Avatar"), wid=Subject, data=avatar)
m$Mauchly # sig. so use GGe correction
m$ANOVA


library(reshape2)
avatar.wide = dcast(avatar, Subject + Sex ~ Avatar, value.var="Positives") # go wide
View(avatar.wide)
male = t.test(avatar.wide$Male ~ Sex, data=avatar.wide) # wom avat vs. men avat Positives for men
female = t.test(avatar.wide$Female ~ Sex, data=avatar.wide) # wom avat vs. men avat Positives for women
p.adjust(c(male$p.value, female$p.value), method="holm")



# ---------------------------------

notes = read.csv("notes.csv")
View(notes)
notes$Subject = factor(notes$Subject) # convert to nominal factor
notes$Order = factor(notes$Order) # convert to nominal factor
summary(notes)

# explore the Words data
library(plyr)
ddply(notes, ~ Phone * Notes, function(data) summary(data$Words))
ddply(notes, ~ Phone * Notes, summarise, Words.mean=mean(Words), Words.sd=sd(Words))

boxplot(Words ~ Phone * Notes, data=notes, xlab="Phone.Notes", ylab="Words") # boxplots
with(notes, interaction.plot(Phone, Notes, Words, ylim=c(0, max(notes$Words)))) # interaction plot
with(notes, interaction.plot(Notes,Phone, Words)) # interaction plot

# test for a Posture order effect to ensure counterbalancing worked
library(ez)
m = ezANOVA(dv=Words, between=Phone, within=Order, wid=Subject, data=notes)
m$Mauchly # n.s.
m$ANOVA

m = ezANOVA(dv=Words, between=Phone, within=Notes, wid=Subject, data=notes)
m$Mauchly # sig. so use GGe correction
m$ANOVA

# manual post hoc pairwise comparisons in light of sig. interaction
library(reshape2)
notes.wide = dcast(notes, Subject + Phone ~ Notes, value.var="Words") # go wide
View(notes.wide)
addon = t.test(notes.wide$'Add-on' ~ Phone, data=notes.wide) # iPhone vs. Android with addon
builtin = t.test(notes.wide$'Built-in' ~ Phone, data=notes.wide) # iPhone vs. Androin builtin
p.adjust(c(addon$p.value, builtin$p.value), method="holm")

# just curious: also compare iPhone 'sit' and 'walk'
iphone=t.test(notes.wide[notes.wide$Phone == "iPhone",]$'Add-on', notes.wide[notes.wide$Phone == "iPhone",]$'Built-in', paired=TRUE)
boxplot(notes.wide[notes.wide$Phone == "iPhone",]$'Add-on', notes.wide[notes.wide$Phone == "iPhone",]$'Built-in',xlab="iPhone.Add-on vs. iPhone.Built-in", ylab="Words") # custom boxplot

android = t.test(notes.wide[notes.wide$Phone == "Android",]$'Add-on', notes.wide[notes.wide$Phone == "Android",]$'Built-in', paired=TRUE)
boxplot(notes.wide[notes.wide$Phone == "Android",]$'Add-on', notes.wide[notes.wide$Phone == "Android",]$'Built-in',xlab="Android.Add-on vs. Android.Built-in", ylab="Words") # custom boxplot

p.adjust(c(android$p.value, iphone$p.value), method="holm")

# --------------------------------------------------------------------
# This file describes a study of people viewing a positive or 
# negative film clip before going onto social media and then judging 
# the value of the first 100 posts they see there. The number of 
# valued posts was recorded. Examine the data and indicate what kind 
# of experiment design this was

socialvalue = read.csv("socialvalue.csv")
View(socialvalue)
socialvalue$Subject = factor(socialvalue$Subject) # convert to nominal factor
socialvalue$Order = factor(socialvalue$Order) # convert to nominal factor
socialvalue$SocialOrder = factor(socialvalue$SocialOrder) # convert to nominal factor
socialvalue$ClipOrder = factor(socialvalue$ClipOrder) # convert to nominal factor
summary(socialvalue)

# explore the Valued data
library(plyr)
ddply(socialvalue, ~ Clip * Social, function(data) summary(data$Valued))
ddply(socialvalue, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))

boxplot(Valued ~ Clip * Social, data=socialvalue, xlab="Keyboard.Posture", ylab="Valued") # boxplots
with(socialvalue, interaction.plot(Social, Clip, Valued, ylim=c(0, max(socialvalue$Valued)))) # interaction plot
with(socialvalue, interaction.plot(Clip, Social, Valued)) # interaction plot


library(ez)
m = ezANOVA(dv=Valued, within=c("ClipOrder","SocialOrder"), wid=Subject, data=socialvalue)
m$Mauchly # n.s.
m$ANOVA 

library(ez)
m = ezANOVA(dv=Valued, within=c("Clip","Social"), wid=Subject, data=socialvalue)
m$Mauchly # n.s.
m$ANOVA 

# manual post hoc pairwise comparisons in light of sig. interaction
library(reshape2)
socialvalue.wide = dcast(socialvalue, Subject ~ Social * Clip, value.var="Valued") # go wide
View(socialvalue.wide)
facebook = t.test(socialvalue.wide$Facebook_negative, socialvalue.wide$Facebook_positive,paired=TRUE, var.equal=TRUE) # iPhone vs. Android with addon
twitter = t.test(socialvalue.wide$Twitter_negative, socialvalue.wide$Twitter_positive,paired=TRUE, var.equal=TRUE) # iPhone vs. Androin builtin
p.adjust(c(facebook$p.value, twitter $p.value), method="holm")


#-------------

# Aligned Rank Transform on Error_Rate
library(ARTool) # for art, artlm
m = art(Valued ~ Clip * Social + (1|Subject), data=socialvalue) # uses LMM
anova(m) # report anova


library(phia)
testInteractions(artlm(m, "Clip:Social"), pairwise=c("Clip", "Social"), adjustment="holm")
