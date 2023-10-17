#include <stdio.h>

int power(int x, int y){
	int a=1;
	for(int i=0; i<y; i++){
		a=a*x;
	}
	return a;
}

int main(void){
	printf("%i", power(3, 2));
	return 0;
}
