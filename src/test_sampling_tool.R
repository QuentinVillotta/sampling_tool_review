# Charger les bibliothèques nécessaires
library(dplyr)

# Function 

Ssize<-function (x,A,p,E) {(qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))}

create_targets<-function(sframe, topup="LAALAL", conf_level=0.9, pror=0.5, e_marg=0.1, buf=0.1 ){
  sframe |> 
    dplyr::group_by(strata_id) |> 
    dplyr::summarise(
      Population = sum(pop_numbers,na.rm=T)
    ) |> 
    dplyr::mutate(
      target = ifelse(
        topup=="Enter sample size",
        100,
        ceiling(Ssize(Population,conf_level,pror,e_marg))
      ) |> as.numeric(),
      target.with.buffer = ifelse(
        topup=="Enter sample size",
        target,
        as.numeric(ceiling(target * (1+buf)))
      )
    )
}


# Simuler une population de 100 écoles, chacune avec 100 élèves
set.seed(123)  # Pour la reproductibilité
sframe <- expand.grid(
  unite_primaire =  paste0("psu_", 1:100),
  # unite_secondaire = paste0("ssu_", 1:10),
  pop_group = paste0("pop_group_", 1:3)
) %>% 
  arrange(unite_primaire, pop_group)
# Ajouter une variable de performance académique (par exemple, des scores aléatoires)
sframe <- sframe %>%
  mutate(nb_hh = round(rnorm(n(), mean = 2000, sd = 200)))

sframe %>% 
  write.csv(.,file = "~/Desktop/sframe_test.csv", row.names = FALSE)

# PArameters
# Stratification
IS_STRATIFIED = "Stratified"
STRATA = "pop_group"
# CLuster sampling
COL_PSU="unite_primaire"
COL_POP="nb_hh"


# STEP : format_sampling_frame -------------------------------------------------
sframe['id_sampl'] <- paste0("id_",rownames(sframe))
# IF stratified
if(IS_STRATIFIED == "Stratified"){
  sframe$strata_id<-sframe[[as.character(STRATA)]]
} else {
  sframe$strata_id<-rep("all",nrow(sframe))
}


# CLuster sampling
sframe$psu_id<-sframe[[as.character(COL_PSU)]]
sframe$pop_numbers<-sframe[[as.character(COL_POP)]]
# -- 

sumdist<-sframe %>% dplyr::group_by(strata_id) %>%  dplyr::summarise(SumDist = sum(pop_numbers,na.rm=T))
sframe<-merge(sframe,sumdist,by="strata_id")
proba<-as.numeric(sframe$pop_numbers)/as.numeric(sframe$SumDist)
sframe<-cbind(sframe,proba)
sframe<-sframe[!is.na(sframe$proba),]

# CLuster sampling 
sframe$psu_id<-as.factor(sframe$psu_id)
# END STEP : format_sampling_frame ---------------------------------------------

# Make sample
sampling_target <- create_targets(sframe)

# CLuster sample --------------------------------------------------------------
# target <- 70

buf <- 0.1
cls <- 5
ICC <- 0.06
target<-as.numeric(as.character(sampling_target[["target"]][1]))
dist<-as.character(sampling_target[["strata_id"]][1])

# CLuster sampling ------------------------------------------------------------

dbr <- sframe[as.character(sframe$strata_id)==dist,]
dbr <- dbr[dbr$pop_numbers>=cls,]

size_sample = ceiling(as.numeric(target*(1+buf))/cls)
out <- sample(as.character(dbr$id_sampl),
              size_sample,
              prob=dbr$proba,
              replace=TRUE)
stop<-F

d<-as.data.frame(table(out))[,2]
ms<-sum(d)/nrow(as.data.frame(d))
DESS<-1+(ms*cls-1)*ICC
targ<-DESS*(target*(1+buf))/cls	
