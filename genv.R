# # Find out if GUI libs are available
loadGuiLibs <- function(){
	if ( interactive() && require("gWidgets") && require("gWidgetsRGtk2") && require("gWidgetstcltk") ) {
		options(guiToolkit = "RGtk2")
		return(TRUE)
	}
	return(FALSE)
}
loadGuiLibs()

# Useful functions

lsobj <- function(
        pos = 1,
        pattern = NULL,
        order.by = c("Size", "Type"),
        decreasing = TRUE,
        size.unit = c("M", "K", "G"),
        n = 10,
        type = NULL,
        screen = TRUE,
        ignore.case = TRUE,
        ...
) {
#       based on the posting: 
#       http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
        napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
#       names <- ls(pos = pos, pattern = pattern)
        names <- ls(pos = pos)
        names <- names[!sapply(names, is.null)]
        if(!is.null(pattern)) names <- grep(pattern, names, value = TRUE, ignore.case = ignore.case, ...)

        if(length(names) == 0) return(NULL)
        size.unit <- match.arg(size.unit)
        unit.value <- switch(size.unit, M = 1048576, K = 1024, G = 1073741824)
        order.by <- match.arg(order.by)
        obj.class <- napply(names, function(x) as.character(class(x))[1])
        obj.mode <- napply(names, mode)
        obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
        obj.size <- napply(names, object.size) / unit.value
        obj.dim <- t(napply(names, function(x) tryCatch(as.numeric(base::dim(x))[1:2], error=function(e) c(NA, NA))))
        #obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
        vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
        obj.dim[vec, 1] <- napply(names, length)[vec]
        out <- data.frame(obj.type, obj.size, obj.dim, stringsAsFactors=FALSE)
        names(out) <- c("Type", "Size", "Rows", "Columns")
        out <- out[order(out[[order.by]], decreasing = decreasing), ]
        names(out) <- c("Type", paste("Size[", size.unit, "]", sep=""), "Rows", "Columns")
        if(!is.null(type)){
                out <- out[out[, "Type"] == type, , drop = FALSE]
        }
        N <- nrow(out)
        if(N > 0){
                if(isTRUE(screen)){
                        e <- 0
                        answer <- ""
                        while(e < N && !isTRUE(identical(answer, "q"))){
                                s <- e + 1
                                e <- min(N, e + n)
                                print(out[s:e, ])
                                if(e < N) answer <- readline("'Enter' to continue, 'q Enter' to quit: ")
                        }
                } else {
                        return(out)
                }
        }
}

flookup <- function(pattern) {
        pattern <- substitute(pattern)
        funcs <- lsobj(type = "function", pattern = pattern, screen = FALSE, ignore.case = TRUE)
        if(is.null(funcs)){
                cat("No functions found \n")
                return()
        }

        funcs_n <- row.names(funcs)
        if(length(funcs_n) == 1) {
                cat(funcs_n, "<- ")
                print(args(funcs_n))
        } else {
                return(funcs)
        }
}




glistView <- function(l, l.name){
	stopifnot(loadGuiLibs())
	offspring.val <- function(path){
		ll <- l
		for( i in path ) ll <- ll[[as.integer(i)]]
		ll
	}

	display_func <- function(obj){
		emessage <- "Object does not provide 'toString' method"
		rv <- tryCatch(toString(obj), error=function(e) emessage)
		if(rv != emessage) rv <- paste(substring(rv, 1, (nchar(emessage)-4)), "...")
		rv
	}

	offspring <- function(path, ...) {
		ll <- l
		if(length(path) > 0) {
			for(i in path) ll <- ll[[as.integer(i)]]
		}
		if(length(ll) == 0){
			out <- empty_dataframe(cols=c('name', 'hasOffspring', "child_name", 'child_value', 'class'))
		} else {
			if( is.null(names(ll)) ) out_names <- rep("", length(ll)) else out_names <- names(ll)
			out <- data.frame(name=seq_len(length(ll)),
				hasOffspring=!sapply(ll, function(x) is.atomic(x) || is.function(x)),
				childname = out_names,
				childvalue=sapply(ll, function(i) ifelse(is.atomic(i), display_func(i), "")),
				class=sapply(ll, function(x) paste(class(x), collapse=", ")),
				stringsAsFactors=FALSE)
		}
		out
	}

	if( missing(l.name) ) l.name <- deparse(substitute(l))
	w <- gwindow(l.name)
	tr <- gtree(offspring=offspring, container=w)
	
	addHandlerDoubleclick(tr, handler=function(h,...) {
# 		print(svalue(h$obj))          # the key
# 		print(h$obj[])                # vector of keys
		obj <- offspring.val(h$obj[])
		if( is.data.frame(obj) || is.matrix(obj) ) {
			sz <- dim(obj)
			if( sz[1] > 300 | sz[2] > 300 ) {
				warnmsg("Object is too large to display! Displaying part only...")
				return(gtable(obj[1:min(sz[1], 300), 1:min(sz[2], 300)], container = TRUE))
			} else {
				return(gtable(obj, container=TRUE))
			}
		}
		if( is.list(obj) ) return( glistView(obj, svalue(h$obj)) )
		if( is.environment(obj) ) return(gls(obj))
		txt <- vector("character")
		con <- textConnection("txt", 'wr', local = TRUE)
		on.exit(close(con))
		sink(con)
		if( is.factor(obj) ) {
			print(summary(obj))
		} else {
			print(obj)
		}
		sink()
		w <- gtext(container = gwindow(svalue(h$obj)) )
		svalue(w) <- txt
	})
}

gls <- function(Env, Env.name, display.size=300, container=NULL){

# 	GUI for browsing environments
#	Double-click on a chosen object will attempt to print it;
#	Right-click on a function (! function only) will try to bring up help for this function.
#	If calling from within a function to see vars within the function env, use 'gls(-1)'

#	Private members
	stopifnot(loadGuiLibs())
	envs <- NULL
	refresh.env <- NULL
	if( !missing(Env)) {
		if( missing( Env.name ) ) Env.name <- deparse(substitute(Env))
		refresh.env <- envs <- Env.name
		Env <- as.environment(Env)
		chosenEnv <- Env				# Environment to display
	} else {
		envs <- search()				# list of known environments
		chosenEnv <- envs[1]
		refresh.env <- NULL
	}
	envir.descr <- NULL					# data.frame, describing objects in a chosen env
	alltypes <- NULL				# "types" of objects available in a selected env
	selectedType <- "All"			# "All" or any of available "alltypes" to filter

#	Table display related members
	typeF <- nameF <- NULL	
	pages <- 1				 # (For performance) Number of displays needed to show an entire "envir.descr"
	displaypage <- 1
	
# 	Private methods
	envdescr <- function(Env){
		objects <- lsobj(Env, order.by="Type", screen = FALSE)
		if(is.null(objects)){
			logmessage("Environment", paste("'", Env.name, "'", sep=""), "is empty")
			return(NULL)
		}
		objects[, "Type"] <- as.character(objects[, "Type"])
		data.frame(Name=rownames(objects), objects, stringsAsFactors=FALSE)
	}
	objectTypes <- function() {
		return( c("All", unique(as.character(envir.descr[, "Type"]))) )
	}
	filterType <- function( by ) {
		if( by == "All" ) {
			typeF <<- rep(TRUE, nrow(envir.descr))
		} else {
			typeF <<- envir.descr[, "Type"] == by
		}
		return(typeF)
	}
	filterName <- function(pattern){
		rv <- rep(TRUE, nrow(envir.descr))
		if( pattern == "" ) return(rv)
		rv[grep(pattern=pattern, envir.descr[, "Name", drop=TRUE])] <- FALSE
		rv <- !rv
		rv
	}
	resetNumberOfPages <- function(){
		pages <<- ceiling(sum(typeF & typeN) / display.size)
	}
	toDisplay <- function(pn){
		F <- typeF & typeN
		if( !any(F) ) return(NULL)
		displayF <- seq_len(nrow(envir.descr))[F]
		pagestart <- display.size * (pn-1) + 1
		pageend <- min( display.size * pn, length(displayF))
		return(displayF[pagestart : pageend])
	}
	changeEnv <- function(en){
		envir.descr <<- envdescr(en)
		if (is.null(envir.descr)) return(FALSE)
		typeF <<- rep(TRUE, nrow(envir.descr))
		typeN <<- typeF
		alltypes <<- objectTypes()
		selectedType <<- "All"
		resetNumberOfPages()
		displaypage <<- 1
		return(TRUE)
	}
	redisplay <- function(){
		F <- toDisplay(displaypage)
		if( is.null(F) ) {
			widgets[["VarDislplay"]][, ] <- envir.descr[FALSE, ]
		} else {
			widgets[["VarDislplay"]][, ] <- envir.descr[F, ]
		}
	}
	resetEnvSelector <- function(){
		blockHandler(widgets[["Env"]]$widget, ID=widgets[["Env"]]$changeHandler)
		widgets[["Env"]]$widget[] <- envs
		svalue(widgets[["Env"]]$widget) <- envs[1]
		unblockHandler(widgets[["Env"]]$widget, ID=widgets[["Env"]]$changeHandler)
	}
	resetTypeFilter <- function(){
		blockHandler(widgets[["TypeFlt"]]$widget, ID=widgets[["TypeFlt"]]$changeHandler)
		widgets[["TypeFlt"]]$widget[] <- alltypes
		svalue(widgets[["TypeFlt"]]$widget) <- alltypes[1]
		unblockHandler(widgets[["TypeFlt"]]$widget, ID=widgets[["TypeFlt"]]$changeHandler)
	}
	resetNameFilter <- function(){
		blockHandler(widgets[["Search"]]$widget, ID=widgets[["Search"]]$changeHandler)
		svalue(widgets[["Search"]]$widget) <- ""
		unblockHandler(widgets[["Search"]]$widget, ID=widgets[["Search"]]$changeHandler)
	}
	resetTextDisplay <- function(){
		svalue(widgets[["txtDisplay"]]) <- ""
	}
	resetPage <- function(){
		resetNumberOfPages()
		blockHandler(widgets[["Page"]]$widget, ID=widgets[["Page"]]$changeHandler)
		widgets[["Page"]]$widget[] <- 1:pages
		svalue(widgets[["Page"]]$widget) <- 1
		unblockHandler(widgets[["Page"]]$widget, ID=widgets[["Page"]]$changeHandler)
	}
	resetDependentWidgets <- function(){
		resetTextDisplay()
		resetTypeFilter()
		resetNameFilter()
		resetPage()
	}
	refreshEnvs <- function(){
		if( is.null( refresh.env ) ) {
			envs <<- search() 
			chosenEnv <<- envs[1]
		} else {
			envs <<- refresh.env
		}
		resetEnvSelector()		
		changeEnv(chosenEnv)
		resetDependentWidgets()
		redisplay()
	}
	refreshEnvironment <- function(){
		changeEnv(chosenEnv)
		resetDependentWidgets()
		redisplay()
	}

# 	Set initial state:
	if (changeEnv(chosenEnv) == FALSE) return(NULL)

# 	List of widgets
	widgets <- list()
	if( is.null(container) ) 
		widgets[["main"]] <- gwindow(title="EnvBrowser", visible=FALSE)
	else
		widgets[["main"]] <- container
	mainGrp <- ggroup(container=widgets[["main"]], horizontal=FALSE)
	selectGrp <- ggroup(horizontal=TRUE)
	screenGrp <- gpanedgroup(horizontal=TRUE)
	buttonsGrp <- ggroup(horizontal=TRUE)
	
	# selectGrp
	widgets[["Env"]] <- list()
	widgets[["Env"]]$widget <- gdroplist(items=envs, selected=1)
	widgets[["Env"]]$changeHandler <- addHandlerChanged(widgets[["Env"]]$widget, function(h, ...){
		chosenEnv <<- svalue(h$obj)
		refreshEnvironment()
		})
	tmp <- gframe("Select Env:", container=selectGrp)
	gWidgets::add(tmp, widgets[["Env"]]$widget)
	
	widgets[["TypeFlt"]] <- list()
	widgets[["TypeFlt"]]$widget <- gdroplist(items=alltypes)
	widgets[["TypeFlt"]]$changeHandler <- addHandlerChanged(widgets[["TypeFlt"]]$widget, 
		function(h, ...){
			selectedType <<- svalue(h$obj)
			typeF <<- filterType(by=selectedType)
			resetPage()
			redisplay()
		})
	tmp <- gframe("Select Type:", container=selectGrp)
	gWidgets::add(tmp, widgets[["TypeFlt"]]$widget)

	widgets[["Search"]] <- list()
	widgets[["Search"]]$widget <- gedit()
	widgets[["Search"]]$changeHandler <- addHandlerChanged(widgets[["Search"]]$widget, 
		function(h, ...) {
			nmpattern <- svalue(h$obj)
			typeN <<- filterName(nmpattern)
			resetPage()
			redisplay()
		})
	tmp <- gframe("Name Search Pattern:", container=selectGrp)
	gWidgets::add(tmp, widgets[["Search"]]$widget)

	widgets[["Page"]] <- list()
	widgets[["Page"]]$widget <- gdroplist(items=1:pages)
	widgets[["Page"]]$changeHandler <- addHandlerChanged(widgets[["Page"]]$widget,
		function(h, ...){
			displaypage <<- svalue(h$obj)
			redisplay()
		})
	tmp <- gframe("Display Page:", container=selectGrp)
	gWidgets::add(tmp, widgets[["Page"]]$widget)

	#screenGrp
	displayVar <- function(h, ...){
# 		browser() 
		svalue(widgets[["txtDisplay"]]) <- ""
		var.ln <- svalue(h$obj, drop=FALSE)
		var.nm <- var.ln[, 1]
		var.type <- var.ln[, 2]
		var.obj <- get(var.nm, envir=as.environment(chosenEnv))
		if( var.type %in% c("matrix", "data.frame") ){
			sz <- dim(var.obj)
			if( sz[1] > 300 | sz[2] > 300 ) {
				warnmsg("Object is too large to display! Displaying part only...")
				return(gtable(var.obj[1:min(sz[1], 300), 1:min(sz[2], 300)], container = TRUE))
			} else {
				return(gtable(var.obj, container=TRUE))
			}
		} else if (var.type == "environment") {
			if(length(var.obj) > 0){
				rv <- gls(Env=var.obj, Env.name=var.nm)
			} else {
				svalue(widgets[["txtDisplay"]]) <- paste("Environment", p("'", var.nm, "'"), "is empty")
			}
		} else if (var.type == "list") {
			rv <- glistView(l=var.obj, l.name=var.nm)
		} else {
			txt <- vector("character")
			con <- textConnection("txt", 'wr', local = TRUE)
			on.exit(close(con))
			sink(con)
			if( is.factor(var.obj) ) {
				print(summary(var.obj))
			} else {
				print(var.obj)
			}
			sink()
			svalue(widgets[["txtDisplay"]]) <- txt
		} 
	}
	widgets[["VarDislplay"]] <- gtable(envir.descr, handler=displayVar)
	size(widgets[["VarDislplay"]]) <- c(300, 500)
	popupmenu <- list()
	popupmenu$Help$handler <- function(h, ...){
		var.ln <- svalue(widgets[["VarDislplay"]], drop=FALSE)
		var.nm <- var.ln[, 1]
		var.type <- var.ln[, 2]
		if( (var.type %in% c("function", "standardGeneric")) ){
			print(help(var.nm, help_type="html"))
		}
	}
	add3rdmousepopupmenu(widgets[["VarDislplay"]], popupmenu)
		
	gWidgets::add(screenGrp, widgets[["VarDislplay"]], expand=FALSE)
	widgets[["txtDisplay"]] <- gtext(font.attr=c(size=12))
	gWidgets::add(screenGrp, widgets[["txtDisplay"]], expand=TRUE)

	refreshBtn <- gbutton(text="Refresh", handler=function(h, ...) refreshEnvs(), container=buttonsGrp, expand=FALSE)

	gWidgets::add(mainGrp, selectGrp, expand=FALSE)
	gWidgets::add(mainGrp, screenGrp, expand=TRUE)
	gWidgets::add(mainGrp, buttonsGrp, expand=FALSE)

	visible(widgets[["main"]]) <- TRUE
}
