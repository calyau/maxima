#include "plot.h"
#include "emulate.h"
#include "const.h"                    
struct value_function
{ char * body ;
  struct value *constants;
};
extern int s_p;
extern struct value stack[];

enum m_ops {
m_real,
m_imag,
m_conj,
m_abs,
m_acsc,
m_acos,
m_tan,
m_sin,
m_tanh,
m_cos,
m_asin,
m_sinh,
m_cosh,
m_log,
m_factorial,
m_and,
m_set,
m_plus,
m_gt,
m_lt,
m_div,
m_ne,
m_mult,
m_ge,
m_eq,
m_minus,
m_power,
m_le,
m_or,
m_push,
m_push_0,
m_push_1,
m_push_2,
m_push_3,
m_push_i,
m_pushl_0,
m_pushl_1,
m_pushl_2,
m_pushl_3,
m_pushl_i,
m_pushc_0,
m_pushc_1,
m_pushc_2,
m_pushc_3,
m_pushc_4,
m_pushc_5,
m_pushc_6,
m_pushc_7,
m_pushc_8,
m_pushc_9,
m_pushc_i,
m_end,
m_reserve,
m_set_sp,
m_set_sp_frame_pointer,
m_return,
};
execute_fun(f,n)
     struct value_function *f;
     int n;  
{ unsigned char *body;
/* frame_pointer points just beyond  last frames valid storage */
  struct value *frame_pointer = &stack[s_p -(n -1)];
  struct value *constants = f->constants;
  body = f->body;
  switch (*body)
    {
case m_real: f_real(); break;
case m_imag: f_imag(); break;
case m_conj: f_conj(); break;
case m_abs: f_abs(); break;
case m_acsc: f_acsc(); break;
case m_acos: f_acos(); break;
case m_tan: f_tan(); break;
case m_sin: f_sin(); break;
case m_tanh: f_tanh(); break;
case m_cos: f_cos(); break;
case m_asin: f_asin(); break;
case m_sinh: f_sinh(); break;
case m_cosh: f_cosh(); break;
case m_log: f_log(); break;
case m_factorial: f_factorial(); break;
case m_and: f_and(); break;
case m_set: f_set(); break;
case m_plus: f_plus(); break;
case m_gt: f_gt(); break;
case m_lt: f_lt(); break;
case m_div: f_div(); break;
case m_ne: f_ne(); break;
case m_mult: f_mult(); break;
case m_ge: f_ge(); break;
case m_eq: f_eq(); break;
case m_minus: f_minus(); break;
case m_power: f_power(); break;
case m_le: f_le(); break;
case m_or: f_or(); break;
case m_push: f_push(); break;
case m_push_0: f_push_0(); break;
case m_push_1: f_push_1(); break;
case m_push_2: f_push_2(); break;
case m_push_3: f_push_3(); break;
case m_push_i: f_push_i(); break;
case m_pushl_0: f_pushl_0(); break;
case m_pushl_1: f_pushl_1(); break;
case m_pushl_2: f_pushl_2(); break;
case m_pushl_3: f_pushl_3(); break;
case m_pushl_i: f_pushl_i(); break;
case m_pushc_0: f_pushc_0(); break;
case m_pushc_1: f_pushc_1(); break;
case m_pushc_2: f_pushc_2(); break;
case m_pushc_3: f_pushc_3(); break;
case m_pushc_4: f_pushc_4(); break;
case m_pushc_5: f_pushc_5(); break;
case m_pushc_6: f_pushc_6(); break;
case m_pushc_7: f_pushc_7(); break;
case m_pushc_8: f_pushc_8(); break;
case m_pushc_9: f_pushc_9(); break;
case m_pushc_i: f_pushc_i(); break;
case m_end: f_end(); break;
case m_reserve: f_reserve(); break;
case m_set_sp: f_set_sp(); break;
case m_set_sp_frame_pointer: f_set_sp_frame_pointer(); break;
case m_return: f_return(); break;
 default:
      abort();
    }
 END: s_p = frame_pointer -stack -n;
 pop(frame_pointer);
 s_p = frame_pointer -stack -1;
 return 1;
}