clear
cls
import excel "C:\Users\akdog\OneDrive\Masaüstü\ZeynepOzerProje\saglikRandevusuVeriSeti.xlsx", firstrow clear
browse
list Showed_up 
describe
tabulate Showed_up
sum
sum Showed_up, detail
histogram Age
encode Gender, generate(gender_num)
histogram gender_num
tab Showed_up Gender
encode Showed_up, gen(Showed_up_num)
histogram Showed_up_num
correlate Showed_up_num Age
regress Showed_up_num Age
pwcorr Showed_up_num Age
list if _n<= 10
count if missing(Showed_up_num)
graph box Showed_up_num


swilk Age
swilk Showed_up
ranksum Age, by(Showed_up_num)
ttest Age, by(Showed_up_num)
tabulate Showed_up_num Gender, chi2




tabulate Hipertension Showed_up_num, chi2
tabulate Diabetes Showed_up_num, chi2
tabulate Alcoholism Showed_up_num, chi2
tabulate Handcap Showed_up_num, chi2
tabulate Scholarship Showed_up_num, chi2
tabulate SMS_received Showed_up_num, chi2
ttest Datediff, by(Showed_up_num)
