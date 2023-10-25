#include <stdio.h>
#define MAXLINE 100


void CopyStringArray(char to[], char from[]){
	int i=0;
	while(from[i] != '\n'){
		to[i] = from[i];
		i++;
	}
}

int main(void){
	
	char longestLine[MAXLINE];
	char currentLine[MAXLINE];

	for(int i=0; i<MAXLINE; i++){
		longestLine[i] = 0;
		currentLine[i] = 0;
	}

	int c; // current character in the stream
	int currentLength = 0;
	int currentLongestLength = 0;

	while((c=getchar()) != EOF){
		if(c == '\n'){
			if(currentLength > currentLongestLength){
				currentLine[currentLength] = '\n';
				currentLongestLength = currentLength;
				currentLength = 0;
				CopyStringArray(longestLine, currentLine);
			}
			currentLength=0;
		}
		else{
			currentLine[currentLength] = c;	
			++currentLength;
		}
	}

	printf("Longest line is %i words long: \n", currentLongestLength);
	printf("%s\n", longestLine);

	return 0;
}

