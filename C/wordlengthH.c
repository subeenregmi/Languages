#include <stdio.h>
#include <string.h>

#define MAX_WORD_LENGTH 20

int main(void){

	int c = 0, currentWordLength = 0;
	int wordLengths[MAX_WORD_LENGTH];
	for(int i=0; i<MAX_WORD_LENGTH; i++){
		wordLengths[i] = 0;
	}
	
	while((c=getchar()) != EOF){
		if(c == ' ' || c == '\n' || c == '\t'){
			if(currentWordLength == 0){
				continue;
			}
			else{
				wordLengths[currentWordLength-1]++;
				currentWordLength=0;
			}
		}
		else{
			if(currentWordLength == MAX_WORD_LENGTH){
				continue;
			}
			currentWordLength++;
		}
	}

	for(int i=0; i<MAX_WORD_LENGTH; i++){
		printf("%2i Letter Word(s) | ", i+1);
		for(int j=0; j<wordLengths[i]; j++){
			printf("-");
		}
		printf("\n");
	}




	return 0;
}
