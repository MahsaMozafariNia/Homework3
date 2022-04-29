#A)
df=read.csv("D:/Old_Data/math/Data science toseeh/Files/battery.csv")

#B)
names(df)
dim(df)
summary(df)
str(df)
head(df)
#c)
# there is no missing values.

#D)
mf=lm(cycles~.,data=df)
mf$coefficients
summary(mf)
# vaghti beta hat baraye motoghayeri mosbate(manfi) ast yani agar motoghayere mostaghel yek vahed afzayesh peida konad-
#motoghayere pasokh be tore motovaset be andaze beta hat afzayesh(kahesh) peida mikonad.
#charge: mizan jariyani ke baes charge batteri dar yek saat mishe. agar in mizan yek amper afzayesh peida konad, 
#tedade dorhaye batteri be tore motovaset 460 ta kam mishavad.
#agar temp 1 daraje afzayesh peida konad be tore motovaset tedade cycle ha 18 ta ziad mishe.
summary(mf)
#banabar meghdare p value baraye har motoghayer agar farze h_0 ra hazfe an motoghayer begirim be in natije miresim ke h_0-
#baraye charge va temp rad mishe. yani in 2 motoghayer bayad dar regression bashand va tasir darand dar tosife motoghayere pasokh.
#chon p_value=0.001 yani farze h_0(zaribe hame motoghayer ha sefr bashe) rad mishe.
#ama bayad bebinim kodom motoghayer ha ra dar regression bezarim. 
#baraye in kar:vif(mf)
library(faraway)
vif(mf)
#mibinim ke motoghayer ha vabastegi nadarand pas niaz nist az regression hazf shavand.
#hal bayad normal boodane residual ha ra barresi konim chon agar normal nabashand regression khob nist.
#baraye barresi normal boodan mitavan hist rasm kard.
#rahe digar rasme residual ha dar barabare fitted values ha ast. bayad variyance ra barresi konim ke aya abet ast ya pattern khsi darad.
#baraye motmaen shodan az vojoode ya adame vojood pattern mitavan farze h_0 ra adame vojood gereft va regression anjam dad.
mf=lm(cycles~charge+temp+depth+endvolt,data=df)
summary(mf)
vif(mf)
mf=lm(cycles~charge+temp+depth,data=df)
summary(mf)
vif(mf)
mf=lm(cycles~charge+temp,data=df)
summary(mf)
vif(mf)
#model mishe faghat roye temp va charge
hist(mf$residuals)
#ehtemlan normal nist.
plot(mf$fitted.values,mf$residuals,xlab="y hat",ylab="residual")
qqplot(mf$residuals,mf$fitted.values)
qqnorm(mf$residuals)
#agar khate vasel noghte aval va akhar ra rasm konim mibin yek khate saf nadarim ke hame noght hole an bashand pas normal nist.
#regression bayad khati bashe ama vojoode rabete ra motovajeh nemisham pas azmoone farz anjam midim.
farze=lm(abs(mf$residuals)~mf$fitted.values)
summary(farze)
#chon pvalue kamtar az alpha ast farza h_0 yani naboode rabete rad mishe pas rabete darand. yani variyance residual ha sabet nist pas normal nist.
#baraye normal bodan ya naboodan az anderson test estefade mikonim.
library(nortest)
ad.test(mf$residuals)
#farze h_0 ine ke tozie normal darim ama chon p_value kheili kame pas rad mishe yani tozie normal nist. 
#pas aval bayad transformation anjam beshe ta regression dorost bashe. dar haghighat tranformation baes mishe tozie be normal nazdik beshe 
#va dar natije kheili az dadehaye part dige part nistand.
#agar log(cycle) dar nazar begirim, tozie residual ha ke ba tozie cycle yeksane normal mishe va az tarafi r squared az 0.1 be 0.6 mirese.
hist(df$cycles)
hist(log(df$cycles))
#mibinim normal shode ama baraye etminam marahele zir ra mirim.
#pas regression jadid roye log(cycle) tarif mikonim va dobare barresi mikonim bebinim normal hast ya na.
h=influence(mf)$hat
length(h)
which(h>3*2/75)
#dar in marhale hich noghte parti dar fazaye X ba modele mf_log nayaftim.
#halfnormal(yani noghti ra peida mikone ke paramet haye tozi ba parametr tozi baghiye noghat motafavete)
halfnorm(influence(mf)$hat, nlab = 3, ylab="Leveragesm")
#dar in marhale dar har do model noghate 28 va 48 ra be onvane noghte part dar nazar gerefte yani 2 noghtei ke h ziad dashtand.
#rahe badi ba 3 barabar va -3 barabare enheraf meyere data moghyese konim.
colMeans(df)
sqrt(diag(var(df)))
df[28,]
#enherafe meyar d[28,] dar cycle az 3 barabre enherefe meyre df$cycle bishtare pas ehtemalan part ast.
df[48,]
#in ham hamintor manande d[28,].
#dar akhar baraye motmen shodane outlir bodan dar fazaye X bayad r standard ra mohasebe kard.
rstandard=rstandard(mf)
plot(rstandard, mf$fitted.values,ylab="Fitted Values",xlab="StudentizedResiduals")
points(rstandard[28],mf$fitted.values[28],col="red")
points(rstandard[22],mf$fitted.values[22],col="blue")
which(rstandard>3)
#pas hich yek az in noghat dar fazaye X part nistand.
#ama agar model ra mf migereftim chi?
rstandard_mf=rstandard(mf)
plot(rstandard_mf, mf$fitted.values,ylab="Fitted Values",xlab="StudentizedResiduals")
points(rstandard_mf[28],mf$fitted.values[28],col="red")
text(rstandard_mf[28]+0.3,mf$fitted.values[28],labels =28,col="red",cex=1)
points(rstandard_mf[48],mf$fitted.values[48],col="blue")
text(rstandard_mf[48]-0.3,mf$fitted.values[48],labels =48,col="blue",cex=1)
which(rstandard_mf>3)
# dar inja ham hamintor vali noghti darim ke mige parte hon az 3 bishtere. 
#an noghte 11 hast ke dar fazaye X ba modele mf part ast. ama vaghti model ra taghir dadim va normal shod an noghte dg part nist.
#ama noghate part manaye digari ham darand. yani noghte part ast agar roye regression tasir bezare.
#baraye inkar yek bar regression ra ba hazfe satre nazire noghte morede nazar anjam midim bebinim cheghadr taghir karde.

#hal be soraghe noghate part dar fazaye pasokh mirim.baraye inkar:
t=rstudent(mf)
which(t==max(abs(t)))
T=function(x){
  2*(1-pt(abs(t[x]), df=75-5-1))
}
ti=apply(as.matrix(seq(1,75)), 1, T)
which(ti<0.0006)
plot(df$cycles,df$charge)
points(df$cycles[54],df$charge[54],col="red")
#hal farze h_0 ra part naboodane in noghte migirim
#pvalue:
2*(1-pt(max(abs(t)), df=75-5-1))
alpha=0.05/75
#chon pvalue az alpha kamtar ast pas h_0 rad mishe va dar natije noghte 54 rad fazaye y part ast.
#hal mikhahim bebinim aya ba hazfe in noghte regression behtar mishe ya na.
cook=cooks.distance(mf)
length(cook)
halfnorm(cook, nlab = 3, ylab="Cooks distance")
#in dastoor mige 2 noghte 58 va 54 ra hazf konim.
#aval hazf mikonim va bad dobare regression anjam midim bebinim chi mishe
plot(df$charge,df$cycles,col="gray")
lines(sort(df$charge),mf$fitted.values,col="black")
df63=df[-63,]
mf63=lm(cycles~temp+charge,data=df63)
lines(sort(df63$charge),mf63$fitted.values,col="red")
lines(sort(df19$charge),mf19$fitted.values,col="blue")
points(df$charge[19],df$cycles[19],col="blue")
points(df$charge[11],df$cycles[11],col="yellow")

 