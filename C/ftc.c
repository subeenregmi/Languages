#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv){
	if(argc == 1){
		printf("-c: for celsius to farenheit.\n-f: for farenheit to celsius.\n");
		return 0;
	}

	if(argc == 3){
		if(strcmp(argv[1], "-c") == 0){
			float farenheit = (atof(argv[2]) * (9.0/5.0)) + 32;
			printf("%f Celsius is equal to %f farenheit.\n", atof(argv[2]), farenheit);
		}
		else if(strcmp(argv[1], "-f") == 0){
			float celsius = (atof(argv[2]) -32) * (5.0/9.0);
			printf("%f Farenheit is equal to %f celsius.\n", atof(argv[2]), celsius);
		}
		else{
			printf("Invalid flag.\n");
		}
	}
	else{
		printf("Invalid amount of flags.\n");
		return 1;
	}
	return 0;
}
