#include <stdio.h>

int main(void){

	int c;
	while((c=getchar()) != EOF){
		if(c == '\t'){
			putchar('\\');
			putchar('t');

		}
		if(c == '\b'){
			putchar('\\');
			putchar('b');

		}
		if(c == '\n'){
			putchar('\\');
			putchar('n');

		}
		putchar(c);
	}
	return 0;
}
