#include <stdio.h>
#include <stdbool.h>

int main(void){
	bool oneLine = false;
	int c;

	while((c=getchar()) != EOF){
		if(c == '\n' || c=='\t' || c==' '){
			if(oneLine){
				continue;
			}
			printf("\n");
			oneLine = true;
		}
		else{
			putchar(c);
			oneLine = false;
		}
	}

	return 0;
}
