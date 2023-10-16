#include <stdio.h>

int main(void){
	int charF[26];
	for(int i=0; i<26; i++){
		charF[i] = 0;
	}	
	int c;
	while((c=getchar()) != EOF){
		if(c >= 'a' || c<='z'){
			charF[c-'a']++;
		}
		if(c >= 'A' || c<='Z'){
			charF[c-'A']++;
		}
	}

	for(int i=0; i<26; i++){
		printf("Character %c | ", 'a'+i);
		for(int j=0; j<charF[i]; j++){
			printf("-");
		}
		printf("\n");
	}

	return 0;
}
