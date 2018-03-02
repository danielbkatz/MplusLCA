#' @import dplyr

#' @export

mplusbasicmix2 <- function(filename, ext, title_mp, namedata, data_set, variableuse, missflag, classes, starts, refinestarts, categoricallist=NULL){
    #defines variables to loop into
    mplusinptoclass <- list()
    cl<- 1:classes
    savedata <- list()

    #creating the file for saving/exporting the .dat file Mplus will use for analysis
    tablefile <- paste(namedata, ".dat", sep="")
    write.table(data_set, file=tablefile, row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)

    #should use a loop eventually instead. Get names of variables in data set
    varlistnames <- names(data_set)
    varlistpaste <- paste(varlistnames, collapse="\n")

    variableusenames <- names(variableuse)
    variableusepaste <- paste(variableusenames, collapse="\n")

    #variable for storing information if there is are categorical variables
    cat.null=is.null(categoricallist)

    #storage of categorical variables
    cat.list <- names(categoricallist)
    cat.list.paste <- paste(cat.list, collapse="\n")



    filename1<- paste(filename, cl, ext, sep="")
    filename2 <- paste(filename,cl, ".dat", sep = "")


    #this is for creating the syntax for an LPA and LCA .inp. There's probably a smoother way of doing this.
    fintitle <- paste("Title:", title_mp, ";", sep=" ")
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
        varlpa <- variableuse %>% mutate_if(is.numeric, funs(ifelse(. == missflag, NA, .))) %>%
            na.omit()
        listcount <- apply(varlpa, 2, function(x)length(unique(x)))
        #iterates through class numbers with different output
        for(i in 1:classes){
            classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
            savedata[[i]] <- paste("savedata: results are ",i,".dat", sep="" )
            mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, missflag1, classes[[i]],
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
            mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, categorical, missflag1, classes[[i]], analysis, starts, processors, output, plot, savedata[[i]], sep="\n")}

    }
    #create each mplus file
    for(q in 1:length(filename1)){
        cat(mplusinptoclass[[q]], file = filename1[[q]])}


    #creating batch file
    bat.string <- noquote(paste("call mplus.exe ", filename, cl, ext, sep = ""))

    #bat.file.name <- paste(filename, ".bat", sep = "")
    #cat(bat.string, file=bat.file.name, sep="\n")


    #running batch file


    #shell.exec(file.path(getwd(), bat.file.name))
    # replacement for shell.exe (doesn't exist on MAC)
    if (exists("shell.exec", where = "package:base")){
        lapply(bat.string, function(x)system2("cmd.exe", input=x))}
    #lapply(bat.string, shell.exec(file.path(getwd(), filename1)))}

    else{comm <-  lapply(paste("open", bat.string))
    lapply(comm, function(x){system(x)})
    }




    if(cat.null==FALSE){
        returnlist <- list(filename2, cat.null, ncat, cl, ncol(categoricallist))}
    else{
        returnlist <- list(filename2, cat.null, cl, ncol(variableuse), listcount)}







#This creates a fit table for your LCA
#lcatab <- function(lcamod){

    #creates all the lists and tables so for loops can be run. I can probably get rid of some of these
    numclass <- length(cl)
    fitlist <- list()
    newfit <- list()
    tablefit2class <- list()
    tablefit2class2<- list()
    tablefit2class3<- list()
    fitmulti <- data.frame()
    #cat.null <- lcamod[[2]]

    #read in the .dat output files
    fit.list <- lapply(returnlist[[1]], read.table, blank.lines.skip = FALSE, fill = TRUE, sep = "")

    #if categorical--lca
    if(cat.null==FALSE){
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
        #return(rowremfin)}
    }


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


    #this just constructs the table for the first one class model
    for(l in 1:length(fit.list)){
        newfit[[l]] <- fit.list[[l]][-c(1:rowremfin[[l]]),]}
    tablefit1class <- as.data.frame(newfit[[1]])

    #getsrid of uneeded fit criteria
    tablefit1class <- tablefit1class[1, -c(2, 4, 8:10)]

    #makes the table conformable
    tablefit1class[6:11] <- NA

    #labels the rows
    names(tablefit1class) <- c("H0_LogLikelihood", "Num_Free_Par", "BIC", "SaBIC", "Entropy", "Condition Number", "LMR adjust p-value", "BootStrap P-value", "LRTS", "BF", "cmP_K" )

    #this constructs the list for the classes greater than 1
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

    BICplot <-  plot(finalmerge[,3], type="p")
    finalret <- list(BICplot, finalmerge)
    return(finalmerge)



    #this isn't working
    write.csv(finalmerge, file=print(namefile))
}
