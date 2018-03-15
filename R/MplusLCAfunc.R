#' mplusbasicmix: A package for batch running and tabulating latent class and latent profile analyses Through Mplus.
#'
#' @param filename This will be the name of the Mplus .inp files created. Requires quotes. Number of classses in the file will be appended to the end of the filename. For insance "filename1.inp, filename2.inp..."
#' @param ext for now, just for testing. Just write ".inp"--Quotes required.
#' @param namedata this is the name of .dat dataset that will be created by the namesare_data set. Quotes required. Example: "dataname"
#' @param namesare_data This is an R data frame (no quotes) that is the complete dataset used. No quotes. Still must be Mplus compatible in terms of no alpha characters. However, the dataset must have column names.No quotes.
#' @param usevar_data This is an R data frame (no quotes) that is a subset of the names_are data. These are the variables used in analysis. You can have the namesare_data and Use_var data be the same.
#' @param missflag This is the missing flag (no quotes, just a number) corresponding to missing in Mplus. What value do you have that connotes missing.
#' @param classes Number of classes you want to run (no quotes, just a number). If you enter 6, the function will create and run 1:6 classes and tabulate.
#' @param starts Number of starts (no quotes, just a number)
#' @param refinestarts Number of final starts you wish to run (no quotes)
#' @param categoricalist (no quotes). This is the "categorical are" option in mplus. Subset of data that include categorical variables. Must include for dichotomous or other categorical. If LPA do not put anything here.
#' @import dplyr
#' @importFrom magrittr %>%
#' @export


mplusbasicmix2 <- function(filename, ext, namedata, namesare_data, usevar_data, missflag, classes, starts, refinestarts, categoricallist=NULL){
    #defines variables to loop into
    mplusinptoclass <- list()
    cl<- 1:classes
    savedata <- list()
    workdir <- getwd()
    #return(workdir)



    #creating the file for saving/exporting the .dat file Mplus will use for analysis
    tablefile <- paste(namedata, ".dat", sep="")
    write.table(namesare_data, file=tablefile, row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)


    #should use a loop eventually instead. Get names of variables in data set
    varlistnames <- names(namesare_data)
    varlistpaste <- paste(varlistnames, collapse="\n")

    variableusenames <- names(usevar_data)
    variableusepaste <- paste(variableusenames, collapse="\n")

    #need to get this logical for building tables later and running LPA or LCA
    cat.null=is.null(categoricallist)

    #storage of categorical variables
    cat.list <- names(categoricallist)
    cat.list.paste <- paste(cat.list, collapse="\n")


#gets filenames to create for input files.
    filename1<- paste(filename, cl, ext, sep="")
    filename2 <- paste(filename,cl, ".dat", sep = "")



#this is for creating the syntax for an LPA and LCA .inp. There's probably a smoother way of doing this.
    data <- paste("data: File is", tablefile, ";", sep=" ")
    variablelist <- paste("Variable: Names are", (varlistpaste), ";")
    usev <- paste("Usev=", variableusepaste, ";")
    categorical <- paste("categorical=", cat.list.paste,";")
    missflag1 <- paste("Missing are all","(",missflag,")", ";")
    analysis <- paste("Analysis: type=Mixture;")
    starts <- paste("starts=", starts, refinestarts, ";", sep =" ")
    processors <- "processors=4(starts);"
    output <- paste("Output:", "sampstat", "Tech11", "Tech14", ";")
    plot <- paste("plot: type=plot3;", "series=", variableusepaste,"(*)", ";", sep="\n")


    #FOR LPA
    if(cat.null==TRUE){
        varlpa <- usevar_data %>% mutate_if(is.numeric, funs(ifelse(. == missflag, NA, .))) %>%
            na.omit()
        listcount <- apply(varlpa, 2, function(x)length(unique(x)))
        #iterates through class numbers with different output
        for(i in 1:classes){
            classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
            savedata[[i]] <- paste("savedata: results are ",i,".dat", sep="" )
            mplusinptoclass[[i]] <- paste(data, variablelist, usev, missflag1, classes[[i]],
                                          analysis, starts, processors, output, plot, savedata[[i]], sep="\n")}
    }
    #FOR LCA
    else{
        categoricallist2 <- categoricallist %>% mutate_if(is.numeric, funs(ifelse(. == missflag, NA, .))) %>%
            na.omit()

        categoricallist3 <- categoricallist2%>%
            mutate_if(is.numeric, funs(factor(.)))

        ncat<- lapply(categoricallist3, nlevels)

        for(i in 1:classes){
            classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
            savedata[[i]] <- paste("savedata: results are ", filename, i,".dat", ";", sep="" )
            mplusinptoclass[[i]] <- paste(data, variablelist, usev, categorical, missflag1, classes[[i]], analysis, starts, processors, output, plot, savedata[[i]], sep="\n")}

    }
    #create each mplus file
    for(q in 1:length(filename1)){
        cat(mplusinptoclass[[q]], file = filename1[[q]])}


    #No longer using a batfile, just executing vector commands

#thanks to mplus auto for this code here. Creates the mplus command for unix/mac
    if (.Platform$OS.type == "unix"){
        if (Sys.info()["sysname"] == "Darwin")Mplus_command <- "/Applications/Mplus/mplus"
        else Mplus_command <- "mplus" #linux is case sensitive
    }


#for windows
    bat.string <- noquote(paste("mplus.exe ", file.path(getwd()), "/", filename, cl, ext, sep = ""))
    #bat.stringlin <- noquote(paste(Mplus_command, " ", file.path(getwd()), "/", filename, cl, ext, sep = ""))
    print(shQuote(bat.string), type = "cmd")
    #print(shQuote(bat.stringlin), type="sh")

    #bat.file.name <- paste(filename, ".bat", sep = "")
    #cat(bat.string, file=bat.file.name, sep="\n")



    #shell.exec(file.path(getwd(), bat.file.name))
    # replacement for shell.exe (doesn't exist on MAC)

#runs for windows
    if(.Platform$OS.type=="windows"){
        lapply(bat.string, function(x)system2("cmd.exe", input=x))}

    #lapply(bat.string, shell.exec(file.path(getwd(), filename1)))}
#runs for mac/unix
    else{
        lapply(filename1, function(x){system2(Mplus_command, args=c(shQuote(x)))})
    }




#values to store for building tables. Makes use of this information in building the table. Can get rid of some of this now
    if(cat.null==FALSE){
        returnlist <- list(filename2, cat.null, ncat, cl, ncol(categoricallist))}
    else{
        returnlist <- list(filename2, cat.null, cl, ncol(usevar_data), listcount)}



    #This creates a fit table for your LCA

#From when this was a seperate function:
#lcatab <- function(lcamod){

#creates all the lists and tables so for loops can be run. I can probably get rid of some of these
    numclass <- length(cl)
    fitlist <- list()
    newfit <- list()
    tablefit2class <- list()
    tablefit2class2<- list()
    tablefit2class3<- list()
    tablefit2class4<- list()

    fitmulti <- data.frame()
    #cat.null <- lcamod[[2]]

    #read in the .dat output files. Loops through to read in multiple files from free format
    fit.list <- lapply(returnlist[[1]], read.table, blank.lines.skip = FALSE, fill = TRUE, sep = "", header=FALSE)

    #if categorical--lca. Calculates number of rows that contain parameters and standard errors to find log likelihood
    if(cat.null==FALSE ){
        ncatdat <- as.data.frame(returnlist[[3]])
        claslist <- as.data.frame(returnlist[[4]])
        itemnum <- returnlist[[5]]

        # breaking the calculation down. number of thresholds per item per class
        itemcat <- as.data.frame(ncatdat-1)
        itemthresh <- apply(itemcat, 2, function(x)x*claslist)
        itemthreshtot <- Reduce(`+`, itemthresh)
        classthresh <- apply(claslist, 2, function(cl)cl-1)
        rowrem <- apply(classthresh, 2, function(x)x+itemthreshtot)
        rowrem2 <- lapply(rowrem, function(x)x/10)
        rowround <- lapply(rowrem2, function(x)(ceiling(x)*2))
        #rowuse <- rowround$'lcamod[[4]]'
        rowremfin <- as.list(t(rowround[[1]]))

    }

#this calculates rows for LPA, but faulty right now.

    else{
        claslist <- as.data.frame(returnlist[[3]])
        classthresh <- apply(claslist, 2, function(cl)cl-1)
        itemnum <- returnlist[[4]]
        means <- apply(claslist, 2, function(x)x*itemnum)
        meanvar <- means*2
        paramtot <- apply(meanvar, 2, function(x)x+classthresh)
        rowtot <- apply(paramtot, 2, function(x)x/10)
        rowround <- apply(rowtot, 2, function(x)(ceiling(x)*2))
        rowremfin <- as.list(rowround)
    }

#this is for testing
    print(rowremfin)


    # a test to see if there's missing data. Returns TRUE if missing data. Returns FALSE otherwise
    mistest <- any(usevar_data==missflag)


    #this just constructs the table for the first one class model. Doesn't need to differ based on missing

    for(l in 1:length(fit.list)){
        newfit[[l]] <- fit.list[[l]][-c(1:rowremfin[[l]]),]}
    tablefit1class <- as.data.frame(newfit[[1]])

#for testings
    print(fit.list)
    #View(fit.list)
    #print(newfit)



    #getsrid of uneeded fit criteria
   tablefit1class <- tablefit1class[1, -c(2, 4, 8:10)]

    #makes the table conformable with the extra fit criteria
    tablefit1class[6:11] <- NA



    #labels the columns
    names(tablefit1class) <- c("H0_LogLikelihood", "Num_Free_Par", "BIC", "SaBIC", "Entropy", "Condition Number", "LMR adjust p-value", "BootStrap P-value", "LRTS", "BF", "cmP_K" )



    #this constructs the list for the classes greater than 1 and no missing
    if(mistest == FALSE){
        for(m in 2:length(newfit))
        {
            tablefit2class[[m]] <-newfit[[m]][1, -c(2,4,8:10)]
            tablefit2class2[[m]] <- newfit[[m]][2, -c(1:3, 5:10)]
            tablefit2class3[[m]] <- newfit[[m]][3, -c(1, 3:5,7)]}

        #this constructs the table for classes greater than 1
        tablefit2class <- Reduce("rbind", tablefit2class)
        tablefit2class2 <- Reduce("rbind", tablefit2class2)
        tablefit2class3 <- Reduce("rbind", tablefit2class3)



        #puts a list of the tables together for classes greater than 1 and labels them
        listslca_plus <- list(tablefit2class, tablefit2class2, tablefit2class3)
        combiningmulti <- Reduce("cbind", listslca_plus)
        }

#Creates table when missing values present (with MCAR)
    else{
        for(m in 2:length(newfit)){
            tablefit2class[[m]] <-newfit[[m]][1, -c(2,4,8:10)]
            tablefit2class2[[m]] <- newfit[[m]][2, -c(1:9)]
            tablefit2class3[[m]] <- newfit[[m]][3, -c(1:7, 9:10)]}
        tablefit2class4[[m]] <- newfit[[m]][4, 2]

        #this constructs the table for classes greater than 1
        tablefit2class <- Reduce("rbind", tablefit2class)
        tablefit2class2 <- Reduce("rbind", tablefit2class2)
        tablefit2class3 <- Reduce("rbind", tablefit2class3)
        tablefit2class4 <- Reduce("rbind", tablefit2class4)


        #puts a list of the tables together for classes greater than 1 and labels them
        listslca_plus <- list(tablefit2class, tablefit2class2, tablefit2class3, tablefit2class4)
        }


# puts the columns together from the multiple fit criteria
    combiningmulti <- Reduce("cbind", listslca_plus)

#adds the extra columns
    combiningmulti[9:11] <- NA

    #labels the columns
    names(combiningmulti) <- c("H0_LogLikelihood", "Num_Free_Par", "BIC", "SaBIC", "Entropy", "Condition Number", "LMR adjust p-value", "BootStrap P-value", "LRTS", "BF", "cmP_K" )


    #merges the 1 and many class model together
    finalmerge <- rbind(tablefit1class, combiningmulti)

    #labels the rows
    row.names(finalmerge) <- c(1:nrow(finalmerge))

    #computes fit criteria
    finalmerge <- finalmerge %>%
        mutate(LRTSdiff=-2*(H0_LogLikelihood-lag(H0_LogLikelihood, default = first(H0_LogLikelihood[2]))))%>%
        mutate(SIC=.5*BIC) %>%
        mutate(BF=exp(SIC-lag(SIC, default = first(SIC[2])))) %>%
        mutate(expsicmax =exp(SIC-max(SIC)))%>%
        mutate(cmP_K=expsicmax/(sum(expsicmax)))


    #gets rid of an empty row because I'm lazy
    finalmerge <- finalmerge[,-9]


    #BICplot <-  plot(finalmerge[,3])
    finalret <- list(fit.list, finalmerge)
    lapply(newfit, function(x)View(x))
    View(finalmerge)
    return(finalmerge)


#this isn't working
write.csv(finalmerge, file=paste(filename, ".csv", sep=""))
}
