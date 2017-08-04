#include "newton.h"
#include <math.h>
#include <iostream>

const double Newton::EPS = 0.001; 

const double Newton::squareRoot(const double x)
{
   double y = x; // Initial guess
   while (fabs(pow(y,2) - x)  > EPS)
   {
      std::cout << " new val" << y << std::endl;
      y = (y + x/y)/2.0;
   }
   return y; 
}
