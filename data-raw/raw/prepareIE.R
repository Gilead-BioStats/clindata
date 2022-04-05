################################################################################ .
# Custom code to derive a simple IE all version
# Not standardized for use generally and takes simplifications
# to create the dataset
################################################################################ .
library(usethis)
library(devtools)

track_a1 <- rbind( raw_ie_a1[,c("SUBJID","SCRNID","INVID","PROJECT","SUBJECT","SITEID","IECAT_STD","IETESTCD_STD","IETEST_STD","IEORRES","IESTRESC_STD")])
track_a1$PROTVER_STD <- "A1"

track_a2 <- rbind( raw_ie_a2[,c("SUBJID","SCRNID","INVID","PROJECT","SUBJECT","SITEID","IECAT_STD","IETESTCD_STD","IETEST_STD","IEORRES","IESTRESC_STD")])
track_a2$PROTVER_STD <- "A2"

track_both <- rbind(track_a1, track_a2)


a1index <- names( table(track_a1$IETESTCD_STD) )
a2index <- names( table(track_a2$IETESTCD_STD) )

track <- NULL
for(i in 1:nrow(raw_ic_elig)){

  keep <- raw_ic_elig[i,c("SUBJID","SCRNID","INVID","PROJECT","SUBJECT","SITEID")]
  keeppv <- raw_ic_elig$PROTVER_STD[i]

  if( paste(keep$SCRNID,keep$INVID,sep="-") %in%
      c( paste(track_a1$SCRNID, track_a1$INVID, sep="-") ,
         paste(track_a2$SCRNID, track_a2$INVID, sep="-") ) ) next

  if( keeppv=="A1" ){
    keepadd <- data.frame( IECAT_STD = c( rep("EXCL",14), rep("INCL",9)),
                           IETESTCD_STD = a1index,
                           IETEST_STD = a1index,
                           IEORRES = c( rep(0,14), rep(1,9)),
                         #  IESTRESC_STD = c( rep("N",14), rep("Y",9)),
                           PROTVER_STD = "A1")
  }

  if( keeppv=="A2" ){
    keepadd <- data.frame( IECAT_STD = c( rep("EXCL",15), rep("INCL",9)),
                           IETESTCD_STD = a2index,
                           IETEST_STD = a2index,
                           IEORRES = c( rep(0,15), rep(1,9)),
                         #  IESTRESC_STD = c( rep("N",14), rep("Y",9)),
                           PROTVER_STD = "A2")
  }

  track <- rbind(track, data.frame(
    do.call("rbind", replicate(
      nrow(keepadd), keep, simplify = FALSE)), keepadd) )

}

raw_ie_all <- track %>%
  filter(SUBJID != "")

usethis::use_data(raw_ie_all, overwrite=TRUE)

