

/* caller:
   push(x);
   push(y);
   res = execute(f,2);
   res = number of return values at top of stack.
   ;; it returns the stack to where it was before the
   ;; arguments were pushed then pushes the results.
*/

#define LOC(i) frame_pointer[i+n]
struct value *frame_pointer;
#define ARG(i) frame_pointer[i]
#define f_push_0() push(ARG(0))
#define f_push_1() push(ARG(1))
#define f_push_2() push(ARG(2))
#define f_push_3() push(ARG(3))
#define f_push_i() push(ARG(body[1])); body++
#define f_pushl_0() push(LOC(0))
#define f_pushl_1() push(LOC(1))
#define f_pushl_2() push(LOC(2))
#define f_pushl_3() push(LOC(3))
#define f_pushl_i() push(LOC(body[1])); body++
#define CONSTANT(i) &(constants[i])
#define f_pushc_0() push(CONSTANT(0))
#define f_pushc_1() push(CONSTANT(1))
#define f_pushc_2() push(CONSTANT(2))
#define f_pushc_3() push(CONSTANT(3))
#define f_pushc_4() push(CONSTANT(4))
#define f_pushc_5() push(CONSTANT(5))
#define f_pushc_6() push(CONSTANT(6))
#define f_pushc_7() push(CONSTANT(7))
#define f_pushc_8() push(CONSTANT(8))
#define f_pushc_9() push(CONSTANT(9))
#define f_pushc_i() push(CONSTANT(body++[1]))
#define f_return() goto END



#define f_reserve() s_p += body[1]; if (s_p >= STACK_DEPTH-1) int_error("stack_overflow",NO_CARET); body++;
#define f_end() goto END;




