#include <iostream>

class f{
	int* x; 
	f(int y){
		int* x = new y;
	}
}

class nn{
	public: 
		f x;
		nn(f ex){
			x = ex;
		}
}


int main()
{
	f gs(5);
	nn t(gs);
	std::cout << t.x;


  return 0;
}
