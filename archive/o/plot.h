

/************************************************************************
 * 
 *  Code is modified from the code for Gnuplot by Zou Maorong
 */
/*
 *  G N U P L O T  --  plot.h
 *  Copyright (C) 1986, 1987  Thomas Williams, Colin Kelley
 *  You may use this code as you wish if credit is given and this message
 *  is retained.
 */
/****************************************************************************/



#define memcpy(d,s,l)    bcopy(s,d,l)
#define top_of_stack     stack[s_p]


typedef int              BOOLEAN;
typedef int              (*FUNC_PTR)();


#define is_jump(operator) ((operator) >=(int)JUMP && (operator) <(int)SF_START)

enum DATA_TYPES
{
  INT, CMPLX, REAL, UNDEFINED
};

struct cmplx 
{
  double real, imag;
};

struct value 
{
  enum DATA_TYPES type;
  union
    {
      int int_val;
      struct cmplx cmplx_val;
    } v;
};


/****************************************************************************/



