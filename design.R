.libPaths("D:/r/library")
library(dplyr)

#######function######
All_com_matrix=function(selected_factor,Factor_pool,sample_size){
  FP=c(1:Factor_pool)
  AllCom=data.frame(t(combn(FP,selected_factor)))
  combnation_number=length(combn(FP,selected_factor))/selected_factor
  c=c(rep(0,combnation_number))
  AllCom=mutate(AllCom, A=c,B=c,C=c,D=c,E=c,F=c,G=c,H=c,I=c,J=c,K=c,L=c,M=c,N=c,O=c)
  L=vector()
  for (j in 1:selected_factor) {
    L[j]=AllCom[j]
    for (i in 1:combnation_number) {
      for (k in FP) {
        AllCom[[k+selected_factor]][i]=ifelse(L[[j]][i]==k,1,AllCom[[k+selected_factor]][i])
      }
    }
  }
  Sub_com=sample_n(AllCom,sample_size)
  Sub_com=Sub_com%>%
    select((selected_factor+1):(selected_factor+Factor_pool))%>%
    mutate(Nr=c(rep(selected_factor,sample_size)),ID=c(1:sample_size))
  return(Sub_com)
}


  

######Calculation##########
  for (calculation in 1:10) {
Factor_pool=14
Sample_size_each_level=50
Lv=5
Sample_size=Sample_size_each_level*Lv
Comb_6=All_com_matrix(6,Factor_pool,Sample_size_each_level)
Comb_7=All_com_matrix(7,Factor_pool,Sample_size_each_level)
Comb_8=All_com_matrix(8,Factor_pool,Sample_size_each_level)
Comb_9=All_com_matrix(9,Factor_pool,Sample_size_each_level)
Comb_10=All_com_matrix(10,Factor_pool,Sample_size_each_level)
Com_all=rbind(Comb_6,Comb_7,Comb_8,Comb_9,Comb_10)
Com_all$ID=c(1:Sample_size)

###################number summary of 3-order interaction###
ImpI_3=data.frame(t(combn(c(1:Factor_pool),3)))
in_3=numeric(0)
int_3=list()
N=length(combn(c(1:Factor_pool),3))/3
F3_com=c(1:N)
Sample_population=c(1:Sample_size)
for (i in F3_com) {
  in_3=numeric(0)     
  for (k in Sample_population) {
    if(Com_all[k,ImpI_3[[1]][i]]==1 & Com_all[k,ImpI_3[[2]][i]]==1 & Com_all[k,ImpI_3[[3]][i]]==1)
      in_3=append(in_3,Com_all[k,Factor_pool+2])
  }
  int_3[[i]]=in_3
}
##########number of replicates for 3-order interactions#####
sum_n=vector()
for (i in 1:N) {
  sum_n[i]=which.max(int_3[[i]])
}
sum_max=max(sum_n)
sum_min=min(sum_n)
sum_mean=mean(sum_n)
##########same replicates between every two interactions######
interact_3=combn(c(1:364),2)
same=vector()
for (i in 1:66066) {
  same[i]=length(intersect(int_3[[interact_3[1,i]]],int_3[[interact_3[2,i]]]))
}
same_max=max(same)
same_min=min(same)
same_mean=mean(same)

if (sum_min>30 & same_max<35) Good_data=Com_all
  }
Com_all=Good_data
###################number summary of 4 order interaction###
ImpI_4=data.frame(t(combn(c(1:Factor_pool),4)))
in_4=numeric(0)
int_4=list()
M=length(combn(c(1:Factor_pool),4))/4
F4_com=c(1:M)
Sample_population=c(1:Sample_size)

for (i in F4_com) {
  in_4=numeric(0)     
  for (k in Sample_population) {
    if(Com_all[k,ImpI_4[[1]][i]]==1 & Com_all[k,ImpI_4[[2]][i]]==1 & Com_all[k,ImpI_4[[3]][i]]==1 & Com_all[k,ImpI_4[[4]][i]]==1)
      in_4=append(in_4,Com_all[k,Factor_pool+2])
  }
  int_4[[i]]=in_4
}

sum_n=vector()
for (i in 1:M) {
  sum_n[i]=which.max(int_4[[i]])
}
sum_max=max(sum_n)
sum_min=min(sum_n)
sum_mean=mean(sum_n)

same=length(intersect(int_4[[1]],int_4[[7]]))

write.csv(Com_all,"C:/Projects/IRF/ML_layout.csv")