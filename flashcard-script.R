
libcallFlashcards <- function(){
	library(data.table)
	library(magrittr)
	options(stringsAsFactors = FALSE)
}

setupFlashcards <- function(input){
	#takes df with two columns (prompt, answer) and adds three new columns:
	#nattempted, correctstreak, timestamp
	
	input$NAttempts <- input$CorrectStreak <- 0
	input$TimeStamp <- ""
	return(input)
}

quizFlashcards <- function(input, reps = 0, choices = 4, invert = FALSE){
	
	#if invert, flip prompt and answer
	if(invert){input <- flipQA(input)}
	
	#set random order of prompt
	lineup <- (1:nrow(input))[order(rnorm(1:nrow(input)))]
	
	#loop number of repetitions
	if(reps == 0 | (reps > nrow(input))){reps <- nrow(input)}
	nright <- 0; nwrong <- 0
	
	for(i in 1:reps){
		cat("\n\nQuestion",i,"of",reps)
		index <- lineup[i]
		
		#choose correct answer and 3 wrong answers
		rightanswer <- input$Answer[index]
		wronganswer <- sample(input$Answer[input$Answer != rightanswer],(choices-1))

		#randomize order, keep track of correct position, and prompt user
		answers <- sample(c(rightanswer,wronganswer)) %>% data.table %>% setnames("")
		rightpos <- (1:nrow(answers))[answers==rightanswer]
		
		
		cat(paste0("\n[",input$Prompt[index],"]"))
		print(answers)
		userchoice <- readline(prompt="Choose the number of the right answer: ")
		
		#update nattempts correctstreak, timestamp based on correct or not
		if(userchoice == "x"){
			#break
			return(input)
		} else if(userchoice == rightpos){
			print("Correct!")
			input$CorrectStreak[index] %<>% +1
			nright %<>% +1
		} else{
			print(paste("Incorrect. The correct choice was",rightpos,":",rightanswer))
			input$CorrectStreak[index] <- 0
			nwrong %<>% +1
		}
		
		input$NAttempts[index] %<>% +1
		input$TimeStamp[index] <- Sys.time()
		
	}
	
	print(paste("Yout got",nright,"correct and",nwrong,"incorrect."))
	
	#if invert, flip back
	if(invert){input <- flipQA(input)}
	
	return(input)
}

flipQA <- function(input, cols = c(1:2)){
	output <- data.frame(input)
	output[,cols] <- output[,rev(cols)]
	return(data.table(output))
}



if(FALSE){
	#Example run
	libcallFlashcards()
	v1 <- fread("vocab1.csv")
	v1 %<>% setupFlashcards()
	v1 %<>% quizFlashcards(10)
}