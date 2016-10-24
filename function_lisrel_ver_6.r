#rm(list = ls())
PATH <- "C:/Users/Administrator/Documents/MEGA/MEGAsync/Eriko_lisrel/model_bnc/model_bnc"



extractInfo <- function(PATH,... ){

DiR <- setwd(PATH)

#dat.files  <- list.files(pattern="Corrs|Stds|Means")
outputs  <- list.files(pattern="*.OUT")
#fit <- list.files(pattern = "FitP_|FitV_")


## list2env(
##   lapply(setNames(dat.files, make.names(gsub("*.dat$", "", dat.files))),
##          function(d) read.table(d, stringsAsFactors = FALSE )), envir = .GlobalEnv)
##
##
## list2env(
##   lapply(setNames(outputs, make.names(gsub("*.OUT$", "", outputs))),
##         readLines), envir = .GlobalEnv)
##
## list2env(
##   lapply(setNames(fit, make.names(gsub("*.txt$", "", fit))),
##         function(e) read.table(e, stringsAsFactors = FALSE, header = FALSE, sep = "\n", fill = TRUE)), envir = .GlobalEnv)



    cors_name <<-  lapply(dir(PATH,
                              pattern = "CorrsP|CorrsV"),
                          function(d) read.table(d,
                                                 stringsAsFactors = FALSE ))

    corP_name <<- cors_name[1]

 corV_name <<- cors_name[2]

    stds_names <<-  lapply(dir(PATH,
                               pattern = "StdsP|StdsV"),
                           function(d) read.table(d,
                                                  stringsAsFactors = FALSE ))

    stdP_name <<- stds_names[1]

 stdV_name <<- stds_names[2]

    means_names <<- lapply(dir(PATH,
                               pattern = "MeansP|MeansV"),
                           function(d) read.table(d,
                                                  stringsAsFactors = FALSE ))

    meanP_name <<- means_names[1]

 meanV_name <<-  means_names[2]

    fit_names <<- lapply(dir(PATH,
                             pattern = "FitP_|FitV_"),
                         function(e) read.table(e,
                                                stringsAsFactors = FALSE,
                                                header = FALSE,
                                                sep = "\n",
                                                fill = TRUE) )

    fitP_name <<- fit_names[1]

 fitV_name <<- fit_names[2]

    outs_names <<- lapply(dir(PATH,
                              pattern ="*.OUT"),
                          function(xx) readLines(xx))

    outP_name <<-  outs_names[1]


outV_name <<-  outs_names[2]

}



lisreltoR <- function(corP_name, stdP_name,meanP_name,fitP_name,outP_name,NAME = "filename.xlsx", model = "base" ){



require(xlsx)

options(scipen= 999)

### Reads in all the matrices. Also, using gsub() function, I'm
### changing the letter "D" that Lisrel writes by default in the
### scientific numbers. So, I'm changing letter "D" for letter
### "e".

#model <- "strong"


psy <-  as.data.frame(corP_name)
psy.Sub <-  apply(psy, 2,
                  function(r) gsub("D","e",r))
psy.Sub <- apply(psy.Sub,2,
                 as.numeric)
##Beta matrix
beta <- as.data.frame(stdP_name)

beta.Sub <-  apply(beta,
                   2,
                   function(r) gsub("D","e",r))

beta.Sub <- apply(beta.Sub,
                  2,
                  as.numeric)
## Means file
means <-  as.data.frame(meanP_name)

mean.Sub <- apply(means,
                   2,
                  function(r) gsub("D","e",r))

mean.Sub <- apply(mean.Sub,
                  2,
                  as.numeric)

###Global goodness of fit file


gf <- as.data.frame(fitP_name)

gf.Sub <- apply(gf,
                   1,
                function(r) gsub("D","e",r))


gf.Sub.2 <- grep("[[:digit:]]",
     unlist(strsplit(gf.Sub,
                     split = "[[:space:]]",
                     perl = TRUE)),
     perl=T, value=T)



gf.Sub.2 <- as.numeric(gf.Sub.2)


## Read in the lisrel output to extract the SRMR by group


#out <- unlist(mget(outP_name, .GlobalEnv))


outSp <-  strsplit(unlist(outP_name),
                   split = "/n")

##Get phantom construct's names


ph.names <- unlist(
    outSp[grep("LE", outSp, value = FALSE)[1]+1]
    )


ph.names <- grep("TDVv|TDVp",
                 unlist(strsplit(ph.names,
                                 split = "[[:space:]]")),
                 value = TRUE)

### Get sample size per group
N <- as.numeric(
    grep("[[:digit:]]",
         unlist(strsplit(grep("NO=",
                              unlist(
                                  strsplit(grep("NO=",
                                                   outSp,
                                                   value = TRUE),
                                           split = "[[:space:]]")
                              ),
                              value = TRUE),
                         split = "=")),
         value = TRUE)
)

## Extract Squared Multiple Correlations for Y - Variables from the output

    yv.line <- grep("Squared Multiple Correlations for Y",
                    outSp,
                    value = FALSE ) + 4

    yv.g1 <-  as.numeric(grep("[[:digit:]]",
                              unlist(strsplit(unlist(c(outSp[yv.line[1]],
                                                       outSp[yv.line[2]])),
                                              split = "[[:space:]]")),
                              value = TRUE))

    yv.g2 <-  as.numeric(grep("[[:digit:]]",
                              unlist(strsplit(unlist(c(outSp[yv.line[2]],
                                                       outSp[yv.line[3]])),
                                              split = "[[:space:]]")),
                              value = TRUE))

    yv.g3 <-  as.numeric(grep("[[:digit:]]",
                              unlist(strsplit(unlist(c(outSp[yv.line[4]],
                                                       outSp[yv.line[5]])),
                                              split = "[[:space:]]")),
                              value = TRUE))

    yv.g4 <-  as.numeric(grep("[[:digit:]]",
                              unlist(strsplit(unlist(c(outSp[yv.line[6]],
                                                       outSp[yv.line[7]])),
                                              split = "[[:space:]]")),
                              value = TRUE))
### Get Y - Variables names

    yv.names <- grep("[[:alpha:]]",
                     unlist(strsplit(unlist(outSp[c(grep("Squared Multiple Correlations for Y",
                    outSp,
                    value = FALSE )[1] +2,
    grep("Squared Multiple Correlations for Y",
                    outSp,
                    value = FALSE )[2] + 2)]),
                          split = "[[:space:]]")),
    value = TRUE)


### Squared Multiple Correlations for Y --- TABLE ----

     yv.tab <- data.frame(rbind(yv.g1, yv.g2, yv.g3, yv.g4),
                         row.names = c("group 1",
                                       "group 2",
                                       "group 3",
                                       "group 4"))
names(yv.tab) <-  yv.names



### Create labels per group using the sample size. Ex:
### Stand Females (n=1324, group 1)

    labels <- c(paste("Stand Females (n=", N[1],","," group 1)", sep= ""),
                paste("Comp Females (n=", N[2],","," group 2)", sep= ""),
                paste("Stand Males (n=", N[3],","," group 3)", sep= ""),
                paste("Comp Males (n=", N[4],","," group 4)", sep= ""))




####### Manipulates matrices and files ##############################################
## Psy matrix


### Group1 psy matrix
mx.psy.g1 <- as.data.frame(matrix(c(1,psy.Sub[6,5], psy.Sub[8,1], psy.Sub[9,4], psy.Sub[11,2], psy.Sub[13,1],
                     psy.Sub[6,5], 1, psy.Sub[8,2], psy.Sub[9,5], psy.Sub[11,3], psy.Sub[11,3],
                     psy.Sub[8,1], psy.Sub[8,2], 1, psy.Sub[9,6], psy.Sub[11,4], psy.Sub[13,3],
                     psy.Sub[9,4], psy.Sub[9,5], psy.Sub[9,6],1, psy.Sub[11,5], psy.Sub[13,4],
                     psy.Sub[11,2], psy.Sub[11,3], psy.Sub[11,4], psy.Sub[11,5], 1, psy.Sub[13,5],
                     psy.Sub[13,1], psy.Sub[13,2], psy.Sub[13,3] ,psy.Sub[13,4], psy.Sub[13,5],1),
           nrow = 6,
           ncol = 6,
           byrow= TRUE),
           row.names = ph.names
           )

mx.psy.g1 <- round(mx.psy.g1, 3)

mx.psy.g1[upper.tri(mx.psy.g1, diag = FALSE)] <- NA
mx.psy.g1[is.na(mx.psy.g1)] <- ""

    mx.psy.g1 <- rbind(c(paste("Stand Females (n= ", N[1],","," group 1)", sep= ""),"","","","","","")
                       ,mx.psy.g1)



#names(mx.psy.g1) <- ph.names

### Group2 psy matrix
mx.psy.g2 <- as.data.frame(
    matrix(c(1,psy.Sub[19,5],psy.Sub[21,1],psy.Sub[22,4],psy.Sub[24,2],psy.Sub[26,1],
             psy.Sub[19,5],1, psy.Sub[21,2], psy.Sub[22,5], psy.Sub[24,3], psy.Sub[26,2],
             psy.Sub[21,1],  psy.Sub[21,2], 1,  psy.Sub[21,6],  psy.Sub[24,4],  psy.Sub[26,3],
             psy.Sub[22,4],  psy.Sub[22,5],  psy.Sub[22,6], 1,  psy.Sub[24,5], psy.Sub[26,4],
             psy.Sub[24,2],  psy.Sub[24,3], psy.Sub[24,4],  psy.Sub[24,5], 1,psy.Sub[26,5],
             psy.Sub[26,1], psy.Sub[26,2], psy.Sub[26,3], psy.Sub[26,4], psy.Sub[26,5],1),
       nrow = 6,
       ncol = 6,
       byrow= TRUE,
       ),
    row.names = ph.names
)

mx.psy.g2 <- round(mx.psy.g2, 3)

mx.psy.g2[upper.tri(mx.psy.g2, diag = FALSE)] <- NA
mx.psy.g2[is.na(mx.psy.g2)] <- ""

 mx.psy.g2 <- rbind(c(paste("Comp Females (n= ", N[2],","," group 2)", sep= ""),"","","","","","")
                       ,mx.psy.g2)

### Group3 psy matrix
mx.psy.g3 <- as.data.frame(
    matrix(c(1, psy.Sub[32,5],psy.Sub[34,1],psy.Sub[35,4], psy.Sub[37,2], psy.Sub[39,1],
             psy.Sub[32,5], 1, psy.Sub[34,2], psy.Sub[35,5],  psy.Sub[37,3], psy.Sub[39,2],
             psy.Sub[34,1],psy.Sub[34,2],1, psy.Sub[35,6], psy.Sub[37,4], psy.Sub[39,3],
             psy.Sub[35,4],psy.Sub[35,5], psy.Sub[35,6], 1, psy.Sub[37,5], psy.Sub[39,4],
             psy.Sub[37,2], psy.Sub[37,3], psy.Sub[37,4], psy.Sub[37,5], 1, psy.Sub[39,5],
             psy.Sub[39,1], psy.Sub[39,2], psy.Sub[39,3], psy.Sub[39,4], psy.Sub[39,5],1 ),
       nrow = 6,
       ncol = 6,
       byrow= TRUE),
    row.names = ph.names
)


mx.psy.g3 <- round(mx.psy.g3, 3)


mx.psy.g3[upper.tri(mx.psy.g3, diag = FALSE)] <- NA
mx.psy.g3[is.na(mx.psy.g3)] <- ""

 mx.psy.g3 <- rbind(c( paste("Stand Males (n= ", N[3],","," group 3)", sep= ""),"","","","","","")
                       ,mx.psy.g3)


### Group4 psy matrix
mx.psy.g4 <- as.data.frame(
    matrix(c(1,psy.Sub[45,5],psy.Sub[47,1],psy.Sub[48,4], psy.Sub[50,2], psy.Sub[52,1],
             psy.Sub[45,5],1, psy.Sub[47,2], psy.Sub[48,5], psy.Sub[50,3], psy.Sub[52,2],
             psy.Sub[47,1], psy.Sub[47,2], 1, psy.Sub[48,6], psy.Sub[50,4], psy.Sub[52,3],
             psy.Sub[48,4], psy.Sub[48,5],psy.Sub[48,6],1, psy.Sub[50,5], psy.Sub[52,4],
             psy.Sub[50,2],psy.Sub[50,3], psy.Sub[50,4], psy.Sub[50,5],1,psy.Sub[52,5],
             psy.Sub[52,1], psy.Sub[52,2], psy.Sub[52,3], psy.Sub[52,4], psy.Sub[52,5],1),
       nrow = 6,
       ncol = 6,
       byrow= TRUE),
    row.names = ph.names
)


mx.psy.g4 <- round(mx.psy.g4, 3)


mx.psy.g4[upper.tri(mx.psy.g4, diag = FALSE)] <- NA
mx.psy.g4[is.na(mx.psy.g4)] <- ""

 mx.psy.g4 <- rbind(c(paste("Comp Males (n= ", N[4],","," group 4)", sep= ""),"","","","","","")
                       ,mx.psy.g4)






### Getting values from Beta matrix


beta.g1 <- round(c(beta.Sub[beta.p[1,1],beta.p[1,2]],
             beta.Sub[beta.p[1+4,1],beta.p[1+4,2]],
             beta.Sub[beta.p[1+8,1],beta.p[1+8,2]],
             beta.Sub[beta.p[1+12,1],beta.p[1+12,2]],
             beta.Sub[beta.p[1+16,1],beta.p[1+16,2]],
             beta.Sub[beta.p[1+20,1],beta.p[1+20,2]]),3)


beta.g2 <-  round(c(beta.Sub[beta.p[2,1],beta.p[2,2]],
             beta.Sub[beta.p[2+4,1],beta.p[2+4,2]],
             beta.Sub[beta.p[2+8,1],beta.p[2+8,2]],
             beta.Sub[beta.p[2+12,1],beta.p[2+12,2]],
             beta.Sub[beta.p[2+16,1],beta.p[2+16,2]],
             beta.Sub[beta.p[2+20,1],beta.p[2+20,2]]),3)




beta.g3 <-  round(c(beta.Sub[beta.p[3,1],beta.p[3,2]],
             beta.Sub[beta.p[3+4,1],beta.p[3+4,2]],
             beta.Sub[beta.p[3+8,1],beta.p[3+8,2]],
             beta.Sub[beta.p[3+12,1],beta.p[3+12,2]],
             beta.Sub[beta.p[3+16,1],beta.p[3+16,2]],
             beta.Sub[beta.p[3+20,1],beta.p[3+20,2]]),3)



beta.g4 <-  round(c(beta.Sub[beta.p[4,1],beta.p[4,2]],
             beta.Sub[beta.p[4+4,1],beta.p[4+4,2]],
             beta.Sub[beta.p[4+8,1],beta.p[4+8,2]],
             beta.Sub[beta.p[4+12,1],beta.p[4+12,2]],
             beta.Sub[beta.p[4+16,1],beta.p[4+16,2]],
             beta.Sub[beta.p[4+20,1],beta.p[4+20,2]]),3)


beta.tab <- as.data.frame(
    rbind(c("Standard Deviations","","","","",""),
        beta.g1,
          beta.g2,
          beta.g3,
          beta.g4)
    )

    labels.2 <- c("",
        paste("Stand Females (n=", N[1],","," group 1)", sep= ""),
                paste("Comp Females (n=", N[2],","," group 2)", sep= ""),
                paste("Stand Males (n=", N[3],","," group 3)", sep= ""),
                paste("Comp Males (n=", N[4],","," group 4)", sep= ""))



    beta.tab.2 <- cbind(beta.tab, labels.2)


    names(beta.tab.2) <- c(ph.names,"")
### up to here extracts Sd

### From here I extract regression paths
beta.p <- which(beta.Sub %% 1 > 0, arr.ind=TRUE)


if(model == "long"){


    regg.g1 <<- as.data.frame(
        cbind(beta.Sub[beta.p][2] ,beta.Sub[beta.p][10],
              beta.Sub[beta.p][18] ,beta.Sub[beta.p][26],
              beta.Sub[beta.p][34])
    )

names(regg.g1) <- ph.names[-6]



    regg.g2 <<- as.data.frame(
        cbind(beta.Sub[beta.p][4] ,beta.Sub[beta.p][12],
              beta.Sub[beta.p][20] ,beta.Sub[beta.p][28],
              beta.Sub[beta.p][36])
    )

names(regg.g2) <- ph.names[-6]


    regg.g3 <<- as.data.frame (
        cbind(beta.Sub[beta.p][6] ,beta.Sub[beta.p][14],
              beta.Sub[beta.p][22] ,beta.Sub[beta.p][30],
              beta.Sub[beta.p][38])
              )
names(regg.g3) <- ph.names[-6]

    regg.g4 <<- as.data.frame(
        cbind(beta.Sub[beta.p][8] ,beta.Sub[beta.p][16],
              beta.Sub[beta.p][24] ,beta.Sub[beta.p][32],
              beta.Sub[beta.p][40])
    )

names(regg.g4) <- ph.names[-6]

} else if(model == "base") {


    regg.g1 <<- as.data.frame(
        cbind(beta.Sub[beta.p][2] ,beta.Sub[beta.p][3],
             beta.Sub[beta.p][4] ,beta.Sub[beta.p][5],
             beta.Sub[beta.p][6])
    )

names(regg.g1) <- ph.names[-1]


    regg.g2 <<- as.data.frame(
        cbind(beta.Sub[beta.p][8] ,beta.Sub[beta.p][9],
             beta.Sub[beta.p][10] ,beta.Sub[beta.p][11],
             beta.Sub[beta.p][12])
    )

names(regg.g2) <- ph.names[-1]

    regg.g3 <<- as.data.frame(
        cbind(beta.Sub[beta.p][14] ,beta.Sub[beta.p][15],
             beta.Sub[beta.p][16] ,beta.Sub[beta.p][17],
             beta.Sub[beta.p][18])
    )

names(regg.g3) <- ph.names[-1]

    regg.g4 <<- as.data.frame(cbind(
        beta.Sub[beta.p][20], beta.Sub[beta.p][21],
             beta.Sub[beta.p][22], beta.Sub[beta.p][23],
        beta.Sub[beta.p][24])
        )

names(regg.g4) <- ph.names[-1]

} else{

    print("Regression paths were not extracted")
}



### Getting values form means file


mean.g1 <- round(mean.Sub[1,],3)
mean.g2 <- round(mean.Sub[3,],3)
mean.g3 <- round(mean.Sub[5,],3)
mean.g4 <- round(mean.Sub[7,],3)


## Estimates Cohen's d
diff1 <- round(mean.g1 - mean.g2, 3)
diff2 <- round(mean.g3 - mean.g4, 3)

effect1 <-  round((diff1/ ((beta.g1 + beta.g2)/2)),3)
effect2 <-  round((diff2/ ((beta.g3 + beta.g4)/2)),3)


bLank <- c("","","","","","")


mean.tab <- as.data.frame(
    rbind(c("Means","","","","",""),
        mean.g1,
          mean.g2,
          diff1,
          effect1,
          bLank,
          mean.g3,
          mean.g4,
          diff2,
          effect2)
)


means_info <- c("",
        paste("Stand Females (n=", N[1],","," group 1)", sep= ""),
                   paste("Comp Females (n=", N[2],","," group 2)", sep= ""),
                  "Mean diff1",
                  "Cohen's d1",
                  "",
                  paste("Stand Males (n=", N[3],","," group 3)", sep= ""),
                  paste("Comp Males (n=", N[4],","," group 4)", sep= ""),
                  "Mean diff2",
                  "Cohen's d2")

mean.tab <- cbind(mean.tab,means_info)


names(mean.tab) <- c(ph.names, "")


means_std_tab <- rbind(beta.tab.2,c("","","","","","",""), mean.tab)


##$ Getting values from global fit file ###

###Note: the chi square is rounded in the txt file so I had to
### get the value from the .out file.

Chi <-  as.numeric(grep("[0-9]$",
                   unlist(strsplit(grep("Maximum Likelihood Ratio Chi-Square ",
                                         outSp,
                                         value = TRUE),
                                        split = "[[:space:]]")),
                        value= TRUE))




fit.table <- data.frame(df = round(gf.Sub.2[4],3),
                        chi.sq = Chi,
                        CFI = round(gf.Sub.2[44],3),
                        TLI = round(gf.Sub.2[42],3),
                        RMSEA = round(gf.Sub.2[20],3)
                       )


## Get SRMR per group
SRMR <- as.numeric(grep("[0-9]$",
                        unlist(strsplit(grep("Standardized RMR ",
                                             outSp,
                                             value = TRUE),
                                        split = "[[:space:]]")),
                        value= TRUE))





fit.table <- cbind(fit.table,t(SRMR))


names(fit.table) <- c("df",
                      "chi.sq",
                      "CFI",
                      "TLI",
                      "RMSEA",
                      "SRMR1",
                      "SRMR2",
                      "SRMR3",
                      "SRMR4")


### Merge all teh correlation matrix
corr <- rbind(mx.psy.g1,bLank, mx.psy.g2,bLank, mx.psy.g3,bLank, mx.psy.g4)



##Prepare matrices to be written



write.xlsx(fit.table, file= NAME, sheetName="Model_fit")
write.xlsx(corr, file= NAME, sheetName="Correlations", append=TRUE,col.names = FALSE)
write.xlsx(means_std_tab , file = NAME, sheetName="Means and Stds", append=TRUE, row.names = FALSE)
write.xlsx(yv.tab, file = NAME, sheetName="Sq.Multi.Corr.Y", append=TRUE, row.names = TRUE)

## Reset working directory
on.exit(setwd(DiR))

}







