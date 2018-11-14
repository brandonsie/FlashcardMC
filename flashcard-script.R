
libcallFlashcards <- function(){
	library(data.table)
	library(magrittr)
	options(stringsAsFactors = FALSE)
}

setupFlashcards <- function(input){
	#takes df with two columns (prompt, answer) and adds three new columns:
	#nattempted, correctstreak, timestamp
	
	input$NAttempts <- input$CorrectStreak <- 0
	input$NRight <- input$NWrong <- 0
	input$TimeStamp <- ""
	return(input)
}

quizFlashcards <- function(input, reps = 0, focusrows = 0,
													 choices = 4, invert = FALSE, requiz = TRUE,
													 waitAnswer =TRUE){
	
	#if invert, flip prompt and answer
	if(invert){input <- flipQA(input)}
	
	#randomize prompts, prioritizing (1) previously incorrect (2) new prompts
	# (3) shortest CorrectStreak
	
	#set random order of prompt
	if(focusrows[1] == 0){
		
		wrongprompts <- (1:nrow(input))[
			input$NAttempts>0 & input$CorrectStreak==0] %>% randomizeSet 
		
		newprompts <- (1:nrow(input))[input$NAttempts==0] %>% randomizeSet
		
		otherprompts <- vector(mode = "integer")
		for(i in 1:max(input$CorrectStreak)){
			otherprompts %<>% c((1:nrow(input))[
				input$CorrectStreak==i] %>% randomizeSet)
		}
		
		lineup <- c(wrongprompts, newprompts, otherprompts)
		# lineup <- (1:nrow(input))[order(rnorm(1:nrow(input)))]
	} else if(length(focusrows) == 1){
		lineup <- focusrows
	} else {
		lineup <- focusrows[order(rnorm(focusrows))]
	}
	
	
	#loop number of repetitions
	if(reps == 0 | (reps > nrow(input))){reps <- nrow(input)}
	nright <- 0; nwrong <- 0; wrongreps <- vector(mode = "numeric")
	
	
	for(i in 1:reps){
		cat("\n\nQuestion",i,"of",reps)
		index <- lineup[i]
		
		#choose correct answer and 3 wrong answers
		rightanswer <- input$Answer[index]
		wronganswer <- sample(input$Answer[input$Answer != rightanswer],(choices-1))

		#randomize order, keep track of correct position, and prompt user
		answers <- sample(c(rightanswer,wronganswer)) %>% 
			c("mark as incorrect") %>% data.frame %>% setnames("") 
		rightpos <- (1:nrow(answers))[answers==rightanswer]
		answers %<>% format(justify = "left") #caution, left align adds white space
		
		cat(paste0("\n[",input$Prompt[index],"]"))
		if(waitAnswer){readline(prompt="Press Enter to view answer choices")}
		print(answers)
		userchoice <- readline(
			prompt="Choose the number of the right answer or enter x to quit: ")
		
		#update nattempts correctstreak, timestamp based on correct or not
		if(userchoice == "x"){
			#break
			return(input)
		} else if(userchoice == rightpos){
			print("Correct!")
			input$CorrectStreak[index] %<>% +1
			input$NRight[index] %<>% +1
			nright %<>% +1
		} else{
			print(paste("Incorrect. The correct choice was",rightpos,":",rightanswer))
			input$CorrectStreak[index] <- 0
			input$NWrong[index] %<>% +1
			nwrong %<>% +1
			wrongreps %<>% c(index)
		}
		
		input$NAttempts[index] %<>% +1
		input$TimeStamp[index] <- format(Sys.time())
		
	}
	
	print(paste("Yout got",nright,"correct and",nwrong,"incorrect."))
	
	if(min(input$CorrectStreak)>0){
		print(paste("You are on a correct streak of at least",
								min(input$CorrectStreak),"for all terms."))
	}
	
	
	
	#(!) if requiz, re-test wrong answers
	if(requiz & length(wrongreps) > 0){
		input %<>% quizFlashcards(reps = length(wrongreps), focusrows = wrongreps)
	}
	
	
	
	#if invert, flip back
	if(invert){input <- flipQA(input)}
	
	return(input)
}

flipQA <- function(input, cols = c(1:2)){
	output <- data.frame(input)
	output[,cols] <- output[,rev(cols)]
	return(output)
}


randomizeSet <- function(x){
	if(length(x)==0){return(x)}
	x[order(rnorm(1:length(x)))]
}



if(FALSE){
	#Example run
	libcallFlashcards()
	v1 <- fread("vocab2.csv",data.table=FALSE)
	v1 %<>% setupFlashcards()
	v1 %<>% quizFlashcards(10)
	v1 %<>% quizFlashcards(10,invert = TRUE)
}
