
/* Begin for cmpinclude */
#ifndef __GNUC__
#define HAVE_ALLOCA
#include <alloca.h>
#endif
 /* If can mprotect pages and so selective gc will work */
#define SGC  

/* End for cmpinclude */


/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/
#include <stdio.h>
#include <setjmp.h>
#include <varargs.h>
#define	TRUE	1
#define	FALSE	0
#ifdef SGC
#define FIRSTWORD     short t; char s,m
#define SGC_TOUCH(x) x->d.m=0
#else
#define FIRSTWORD     short t; short m
#define SGC_TOUCH(x)
#endif  
#define STSET(type,x,i,val)  do{SGC_TOUCH(x);STREF(type,x,i) = (val);} while(0)
#ifndef VOL
#define VOL
#endif
#ifndef COM_LENG
#define COM_LENG 
#endif
#ifndef CHAR_SIZE
#define CHAR_SIZE        8     
#endif
typedef int bool;
typedef int fixnum;
typedef float shortfloat;
typedef double longfloat;
typedef  unsigned short fatchar;
#define SIGNED_CHAR(x) (((char ) -1) < (char )0 ? (char) x \
		  : (x >= (1<<(CHAR_SIZE-1)) ? \
		     x - (((int)(1<<(CHAR_SIZE-1))) << 1) \
		     : (char ) x))
typedef union lispunion *object;
typedef union int_object iobject;
union int_object {int i; object o;};

#define	OBJNULL	((object)NULL)
struct fixnum_struct {
		FIRSTWORD;
	fixnum	FIXVAL;
};
#define	fix(x)	(x)->FIX.FIXVAL
#define	SMALL_FIXNUM_LIMIT	1024
extern struct fixnum_struct small_fixnum_table[COM_LENG];
#define	small_fixnum(i)	(object)(small_fixnum_table+SMALL_FIXNUM_LIMIT+(i))

struct bignum {
			FIRSTWORD;
	long             *big_self;	/*  bignum body  */
	int		big_length;	/*  bignum length  */
};
#define MP(x) ((GEN)(x)->big.big_self)
struct shortfloat_struct {
			FIRSTWORD;
	shortfloat	SFVAL;
};
#define	sf(x)	(x)->SF.SFVAL
struct longfloat_struct {
			FIRSTWORD;
	longfloat	LFVAL;
};
#define	lf(x)	(x)->LF.LFVAL
struct character {
			FIRSTWORD;
	unsigned short	ch_code;
	unsigned char	ch_font;
	unsigned char	ch_bits;
};
struct character character_table1[256+128];
#define character_table (character_table1+128)
#define	code_char(c)	(object)(character_table+(c))
#define	char_code(x)	(x)->ch.ch_code
#define	char_font(x)	(x)->ch.ch_font
#define	char_bits(x)	(x)->ch.ch_bits
enum stype {
	stp_ordinary,
	stp_constant,
        stp_special
};
struct symbol {
		FIRSTWORD;
	object	s_dbind;
	int	(*s_sfdef)();
#define	s_fillp		st_fillp
#define	s_self		st_self
	int	s_fillp;
	char	*s_self;
	object	s_gfdef;
	object	s_plist;
	object	s_hpack;
	short	s_stype;
	short	s_mflag;
};
struct cons {
		FIRSTWORD;
	object	c_cdr;
	object	c_car;
};
struct array {
		FIRSTWORD;
	short	a_rank;
	short	a_adjustable;
	int	a_dim;
	int	*a_dims;
	object	*a_self;
	object	a_displaced;
	short	a_elttype;
	short	a_offset;
};



struct fat_string {			/*  vector header  */
		FIRSTWORD;
        unsigned fs_raw : 24;     /* tells if the things in leader are raw */
	unsigned char fs_leader_length;	 /* leader_Length  */
	int	fs_dim;		/*  dimension  */
	int	fs_fillp;	/*  fill pointer  */
				/*  For simple vectors,  */
				/*  fs_fillp is equal to fs_dim.  */
	fatchar 	*fs_self;	/*  pointer to the vector Note the leader starts at (int *) *fs_self - fs_leader_length */
};


struct vector {
		FIRSTWORD;
	short	v_hasfillp;
	short	v_adjustable;
	int	v_dim;
	int	v_fillp;
	object	*v_self;
	object	v_displaced;
	short	v_elttype;
	short	v_offset;
};
struct string {
		FIRSTWORD;
	short	st_hasfillp;
	short	st_adjustable;
	int	st_dim;
	int	st_fillp;
	char	*st_self;
	object	st_displaced;
};
struct ustring {
		FIRSTWORD;
	short	ust_hasfillp;
	short	ust_adjustable;
	int	ust_dim;
	int	ust_fillp;
	unsigned char
		*ust_self;
	object	ust_displaced;
};
#define USHORT(x,i) (((unsigned short *)(x)->ust.ust_self)[i])

struct bitvector {
		FIRSTWORD;
	short	bv_hasfillp;
	short	bv_adjustable;
	int	bv_dim;
	int	bv_fillp;
	char	*bv_self;
	object	bv_displaced;
	short	bv_elttype;
	short	bv_offset;
};
struct fixarray {
		FIRSTWORD;
	short	fixa_rank;
	short	fixa_adjustable;
	int	fixa_dim;
	int	*fixa_dims;
	fixnum	*fixa_self;
	object	fixa_displaced;
	short	fixa_elttype;
	short	fixa_offset;
};
struct sfarray {
		FIRSTWORD;
	short	sfa_rank;
	short	sfa_adjustable;
	int	sfa_dim;
	int	*sfa_dims;
	shortfloat
		*sfa_self;
	object	sfa_displaced;
	short	sfa_elttype;
	short	sfa_offset;
};
struct lfarray {
		FIRSTWORD;
	short	lfa_rank;
	short	lfa_adjustable;
	int	lfa_dim;
	int	*lfa_dims;
	longfloat
		*lfa_self;
	object	lfa_displaced;
	short	lfa_elttype;
	short	lfa_offset;
};

struct structure {		/*  structure header  */
		FIRSTWORD;
	object	str_def;	/*  structure definition (a structure)  */
	object	*str_self;	/*  structure self  */
};

#define STREF(type,x,i) (*((type *)(((char *)((x)->str.str_self))+(i))))

struct cfun {
		FIRSTWORD;
	object	cf_name;
	int	(*cf_self)();
	object	cf_data;
};

  struct dclosure {		/*  compiled closure header  */
		FIRSTWORD;
	int	(*dc_self)();	/*  entry address  */
	object	*dc_env;	/*  environment  */
};

  struct cclosure {
		FIRSTWORD;

	object	cc_name;
	int	(*cc_self)();
	object	cc_env;
	object	cc_data;
	object	*cc_turbo;
};

struct sfun {
	FIRSTWORD;
	object	sfn_name;
	int	(*sfn_self)();
	object	sfn_data;
	int sfn_argd;

	      };
struct vfun {
		FIRSTWORD; 
	object	vfn_name;
	int	(*vfn_self)();
	object	vfn_data;
	unsigned short vfn_minargs;
	unsigned short vfn_maxargs;
	      };

struct dummy {
		FIRSTWORD;
};
struct stream {
		FIRSTWORD;
	FILE	*sm_fp;		/*  file pointer  */
	object	sm_object0;	/*  some object  */
	object	sm_object1;	/*  some object */
	int	sm_int0;	/*  some int  */
	int	sm_int1;	/*  some int  */
	char  	*sm_buffer;     /*  ptr to BUFSIZE block of storage */
	short	sm_mode;	/*  stream mode  */
				/*  of enum smmode  */
};
union lispunion {
	struct fixnum_struct
			FIX;
	struct shortfloat_struct
			SF;
	struct stream sm;
	struct longfloat_struct
			LF;
	struct character
			ch;
	struct symbol	s;
	struct cons	c;
	struct array	a;
	struct vector	v;
	struct string	st;
	struct ustring	ust;
	struct bignum   big;
	struct bitvector
			bv;
	struct structure
			str;
	struct cfun	cf;
	struct cclosure	cc;
	struct sfun     sfn;
	struct vfun     vfn;
	struct dummy	d;
        struct fat_string fs;
        struct dclosure dc;
	struct fixarray	fixa;
	struct sfarray	sfa;
	struct lfarray	lfa;
};
enum type {
	t_cons,
	t_start = 0 , /* t_cons, */
	t_fixnum,
	t_bignum,
	t_ratio,
	t_shortfloat,
	t_longfloat,
	t_complex,
	t_character,
	t_symbol,
	t_package,
	t_hashtable,
	t_array,
	t_vector,
	t_string,
	t_bitvector,
	t_structure,
	t_stream,
	t_random,
	t_readtable,
	t_pathname,
	t_cfun,
	t_cclosure,
	t_sfun,
        t_gfun,
	t_vfun,
	t_cfdata,
	t_spice,
	t_fat_string,
        t_dclosure,
	t_end,
	t_contiguous,
	t_relocatable,
	t_other
};
#define	type_of(obje)	((enum type)(((object)(obje))->d.t))
#define	endp(obje)	endp1(obje)
extern object value_stack[COM_LENG];
#define	vs_org		value_stack
object *vs_limit;
object *vs_base;
object *vs_top;
#define	vs_push(obje)	(*vs_top++ = (obje))
#define	vs_pop		(*--vs_top)
#define	vs_head		vs_top[-1]
#define	vs_mark		object *old_vs_top = vs_top
#define	vs_reset	vs_top = old_vs_top
#define	vs_check	if (vs_top >= vs_limit)  \
				vs_overflow();
#define	vs_check_push(obje)  \
			(vs_top >= vs_limit ?  \
			 (object)vs_overflow() : (*vs_top++ = (obje)))
#define	check_arg(n)  \
			if (vs_top - vs_base != (n))  \
				check_arg_failed(n)
#define	MMcheck_arg(n)  \
			if (vs_top - vs_base < (n))  \
				too_few_arguments();  \
			else if (vs_top - vs_base > (n))  \
				too_many_arguments()
#define vs_reserve(x)	if(vs_base+(x) >= vs_limit)  \
				vs_overflow();
struct bds_bd {
	object	bds_sym;
	object	bds_val;
};
extern struct bds_bd bind_stack[COM_LENG];
typedef struct bds_bd *bds_ptr;
bds_ptr bds_org;
bds_ptr bds_limit;
bds_ptr bds_top;
#define	bds_check  \
	if (bds_top >= bds_limit)  \
		bds_overflow()
#define	bds_bind(sym, val)  \
	((++bds_top)->bds_sym = (sym),  \
	bds_top->bds_val = (sym)->s.s_dbind,  \
	(sym)->s.s_dbind = (val))
#define	bds_unwind1  \
	((bds_top->bds_sym)->s.s_dbind = bds_top->bds_val, --bds_top)
typedef struct invocation_history {
	object	ihs_function;
	object	*ihs_base;
} *ihs_ptr;
extern struct invocation_history ihs_stack[COM_LENG];
ihs_ptr ihs_org;
ihs_ptr ihs_limit;
ihs_ptr ihs_top;
#define	ihs_check  \
	if (ihs_top >= ihs_limit)  \
		ihs_overflow()
#define ihs_push(function)  \
	(++ihs_top)->ihs_function = (function);  \
	ihs_top->ihs_base = vs_base
#define ihs_pop() 	(ihs_top--)
enum fr_class {
	FRS_CATCH,
	FRS_CATCHALL,
	FRS_PROTECT
};
struct frame {
	jmp_buf		frs_jmpbuf;
	object		*frs_lex;
	bds_ptr		frs_bds_top;
	enum fr_class	frs_class;
	object		frs_val;
	ihs_ptr		frs_ihs;
};
typedef struct frame *frame_ptr;
#define	alloc_frame_id()	alloc_object(t_spice)
extern struct frame frame_stack[COM_LENG];

frame_ptr frs_org;
frame_ptr frs_limit;
frame_ptr frs_top;
#define frs_push(class, val)  \
	if (++frs_top >= frs_limit)  \
		frs_overflow();  \
	frs_top->frs_lex = lex_env;\
	frs_top->frs_bds_top = bds_top;  \
	frs_top->frs_class = (class);  \
	frs_top->frs_val = (val);  \
	frs_top->frs_ihs = ihs_top;  \
        setjmp(frs_top->frs_jmpbuf)
#define frs_pop()	frs_top--
bool nlj_active;
frame_ptr nlj_fr;
object nlj_tag;
object *lex_env;
object caar();
object cadr();
object cdar();
object cddr();
object caaar();
object caadr();
object cadar();
object caddr();
object cdaar();
object cdadr();
object cddar();
object cdddr();
object caaaar();
object caaadr();
object caadar();
object caaddr();
object cadaar();
object cadadr();
object caddar();
object cadddr();
object cdaaar();
object cdaadr();
object cdadar();
object cdaddr();
object cddaar();
object cddadr();
object cdddar();
object cddddr();
#define MMcons(a,d)	make_cons((a),(d))
#define MMcar(x)	(x)->c.c_car
#define MMcdr(x)	(x)->c.c_cdr
#define CMPcar(x)	(x)->c.c_car
#define CMPcdr(x)	(x)->c.c_cdr
#define CMPcaar(x)	(x)->c.c_car->c.c_car
#define CMPcadr(x)	(x)->c.c_cdr->c.c_car
#define CMPcdar(x)	(x)->c.c_car->c.c_cdr
#define CMPcddr(x)	(x)->c.c_cdr->c.c_cdr
#define CMPcaaar(x)	(x)->c.c_car->c.c_car->c.c_car
#define CMPcaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car
#define CMPcadar(x)	(x)->c.c_car->c.c_cdr->c.c_car
#define CMPcaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaar(x)	(x)->c.c_car->c.c_car->c.c_cdr
#define CMPcdadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcaaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_car
#define CMPcaaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_car
#define CMPcaadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_car
#define CMPcaaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_car
#define CMPcadaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_car
#define CMPcadadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_car
#define CMPcaddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_car
#define CMPcadddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_cdr
#define CMPcdaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_cdr
#define CMPcdadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_cdr
#define CMPcdaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_cdr
#define CMPcddadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcddddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPfuncall	funcall
#define	cclosure_call	funcall
object simple_lispcall();
object simple_lispcall_no_event();
object simple_symlispcall();
object simple_symlispcall_no_event();
object CMPtemp;
object CMPtemp1;
object CMPtemp2;
object CMPtemp3;
#define	Cnil	((object)&Cnil_body)
#define	Ct	((object)&Ct_body)
struct symbol Cnil_body, Ct_body;
object MF();
object MFnew();
object MM();
object Scons;
object siSfunction_documentation;
object siSvariable_documentation;
object siSpretty_print_format;
object Slist;
object keyword_package;
object alloc_object();
object car();
object cdr();
object list();
object listA();
object coerce_to_string();
object elt();
object elt_set();
frame_ptr frs_sch();
frame_ptr frs_sch_catch();
object make_cclosure();
object make_cclosure_new();
object nth();
object nthcdr();
object make_cons();
object append();
object nconc();
object reverse();
object nreverse();
object number_expt();
object number_minus();
object number_negate();
object number_plus();
object number_times();
object one_minus();
object one_plus();
object get();
object getf();
object putprop();
object sputprop();
object remprop();
object string_to_object();
object symbol_function();
object symbol_value();
object make_fixnum();
object make_shortfloat();
object make_longfloat();
object structure_ref();
object structure_set();
object princ();
object prin1();
object print();
object terpri();
object aref();
object aset();
object aref1();
object aset1();
void call_or_link();
void call_or_link_closure();
object call_proc();
object call_proc0();
object call_proc1();
object call_proc2();
object ifuncall();   
object ifuncall1();
object ifuncall2();
object symbol_name();
#define T101 Z101
#define T102 Z102
#define T103 Z103
#define T104 Z104
#define T105 Z105
#define T106 Z106
#define T107 Z107
#define T108 Z108
#define T109 Z109
#define T110 Z110
#define T111 Z111
#define T112 Z112
#define T113 Z113
#define T114 Z114
#define T115 Z115
#define T116 Z116
#define T117 Z117
#define T118 Z118
#define T119 Z119
#define T120 Z120
#define T121 Z121
#define T122 Z122
#define T123 Z123
#define T124 Z124
#define T125 Z125
#define T126 Z126
#define T127 Z127
#define T128 Z128
#define T129 Z129
#define T130 Z130
#define T131 Z131
#define T132 Z132
#define T133 Z133
#define T134 Z134
#define T135 Z135
#define T136 Z136
#define T137 Z137
#define T138 Z138
#define T139 Z139
#define T140 Z140
#define T141 Z141
#define T142 Z142
#define T143 Z143
#define T144 Z144
#define T145 Z145
#define T146 Z146
#define T147 Z147
#define T148 Z148
#define T149 Z149
#define T150 Z150
#define T151 Z151
#define T152 Z152
#define T153 Z153
#define T154 Z154
#define T155 Z155
#define T156 Z156
#define T157 Z157
#define T158 Z158
#define T159 Z159
#define T160 Z160
#define T161 Z161
#define T162 Z162
#define T163 Z163
#define T164 Z164
#define T165 Z165
#define T166 Z166
#define T167 Z167
#define T168 Z168
#define T169 Z169
#define T170 Z170
#define T171 Z171
#define T172 Z172
#define T173 Z173
#define T174 Z174
#define T175 Z175
#define T176 Z176
#define T177 Z177
#define T178 Z178
#define T179 Z179
#define T180 Z180
#define T181 Z181
#define T182 Z182
#define T183 Z183
#define T184 Z184
#define T185 Z185
#define T186 Z186
#define T187 Z187
#define T188 Z188
#define T189 Z189
#define T190 Z190
#define T191 Z191
#define T192 Z192
#define T193 Z193
#define T194 Z194
#define T195 Z195
#define T196 Z196
#define T197 Z197
#define T198 Z198
#define T199 Z199
#define T200 Z200
#define T201 Z201
#define T202 Z202
#define T203 Z203
#define T204 Z204
#define T205 Z205
#define T206 Z206
#define T207 Z207
#define T208 Z208
#define T209 Z209
#define T210 Z210
#define T211 Z211
#define T212 Z212
#define T213 Z213
#define T214 Z214
#define T215 Z215
#define T216 Z216
#define T217 Z217
#define T218 Z218
#define T219 Z219
#define T220 Z220
#define T221 Z221
#define T222 Z222
#define T223 Z223
#define T224 Z224
#define T225 Z225
#define T226 Z226
#define T227 Z227
#define T228 Z228
#define T229 Z229
#define T230 Z230
#define T231 Z231
#define T232 Z232
#define T233 Z233
#define T234 Z234
#define T235 Z235
#define T236 Z236
#define T237 Z237
#define T238 Z238
#define T239 Z239
#define T240 Z240
#define T241 Z241
#define T242 Z242
#define T243 Z243
#define T244 Z244
#define T245 Z245
#define T246 Z246
#define T247 Z247
#define T248 Z248
#define T249 Z249
#define T250 Z250
#define T251 Z251
#define T252 Z252
#define T253 Z253
#define T254 Z254
#define T255 Z255
#define T256 Z256
#define T257 Z257
#define T258 Z258
#define T259 Z259
#define T260 Z260
#define T261 Z261
#define T262 Z262
#define T263 Z263
#define T264 Z264
#define T265 Z265
#define T266 Z266
#define T267 Z267
#define T268 Z268
#define T269 Z269
#define T270 Z270
#define T271 Z271
#define T272 Z272
#define T273 Z273
#define T274 Z274
#define T275 Z275
#define T276 Z276
#define T277 Z277
#define T278 Z278
#define T279 Z279
#define T280 Z280
#define T281 Z281
#define T282 Z282
#define T283 Z283
#define T284 Z284
#define T285 Z285
#define T286 Z286
#define T287 Z287
#define T288 Z288
#define T289 Z289
#define T290 Z290
#define T291 Z291
#define T292 Z292
#define T293 Z293
#define T294 Z294
#define T295 Z295
#define T296 Z296
#define T297 Z297
#define T298 Z298
#define T299 Z299
#define T300 Z300
#define T301 Z301
#define T302 Z302
#define T303 Z303
#define T304 Z304
#define T305 Z305
#define T306 Z306
#define T307 Z307
#define T308 Z308
#define T309 Z309
#define T310 Z310
#define T311 Z311
#define T312 Z312
#define T313 Z313
#define T314 Z314
#define T315 Z315
#define T316 Z316
#define T317 Z317
#define T318 Z318
#define T319 Z319
#define T320 Z320
#define T321 Z321
#define T322 Z322
#define T323 Z323
#define T324 Z324
#define T325 Z325
#define T326 Z326
#define T327 Z327
#define T328 Z328
#define T329 Z329
#define T330 Z330
#define T331 Z331
#define T332 Z332
#define T333 Z333
#define T334 Z334
#define T335 Z335
#define T336 Z336
#define T337 Z337
#define T338 Z338
#define T339 Z339
#define T340 Z340
#define T341 Z341
#define T342 Z342
#define T343 Z343
#define T344 Z344
#define T345 Z345
#define T346 Z346
#define T347 Z347
#define T348 Z348
#define T349 Z349
#define T350 Z350
#define T351 Z351
#define T352 Z352
#define T353 Z353
#define T354 Z354
#define T355 Z355
#define T356 Z356
#define T357 Z357
#define T358 Z358
#define T359 Z359
#define T360 Z360
#define T361 Z361
#define T362 Z362
#define T363 Z363
#define T364 Z364
#define T365 Z365
#define T366 Z366
#define T367 Z367
#define T368 Z368
#define T369 Z369
#define T370 Z370
#define T371 Z371
#define T372 Z372
#define T373 Z373
#define T374 Z374
#define T375 Z375
#define T376 Z376
#define T377 Z377
#define T378 Z378
#define T379 Z379
#define T380 Z380
#define T381 Z381
#define T382 Z382
#define T383 Z383
#define T384 Z384
#define T385 Z385
#define T386 Z386
#define T387 Z387
#define T388 Z388
#define T389 Z389
#define T390 Z390
#define T391 Z391
#define T392 Z392
#define T393 Z393
#define T394 Z394
#define T395 Z395
#define T396 Z396
#define T397 Z397
#define T398 Z398
#define T399 Z399
#define T400 Z400
#define T401 Z401
#define T402 Z402
#define T403 Z403
#define T404 Z404
#define T405 Z405
#define T406 Z406
#define T407 Z407
#define T408 Z408
#define T409 Z409
#define T410 Z410
#define T411 Z411
#define T412 Z412
#define T413 Z413
#define T414 Z414
#define T415 Z415
#define T416 Z416
#define T417 Z417
#define T418 Z418
#define T419 Z419
#define T420 Z420
#define T421 Z421
#define T422 Z422
#define T423 Z423
#define T424 Z424
#define T425 Z425
#define T426 Z426
#define T427 Z427
#define T428 Z428
#define T429 Z429
#define T430 Z430
#define T431 Z431
#define T432 Z432
#define T433 Z433
#define T434 Z434
#define T435 Z435
#define T436 Z436
#define T437 Z437
#define T438 Z438
#define T439 Z439
#define T440 Z440
#define T441 Z441
#define T442 Z442
#define T443 Z443
#define T444 Z444
#define T445 Z445
#define T446 Z446
#define T447 Z447
#define T448 Z448
#define T449 Z449
#define T450 Z450
#define T451 Z451
#define T452 Z452
#define T453 Z453
#define T454 Z454
#define T455 Z455
#define T456 Z456
#define T457 Z457
#define T458 Z458
#define T459 Z459
#define T460 Z460
#define T461 Z461
#define T462 Z462
#define T463 Z463
#define T464 Z464
#define T465 Z465
#define T466 Z466
#define T467 Z467
#define T468 Z468
#define T469 Z469
#define T470 Z470
#define T471 Z471
#define T472 Z472
#define T473 Z473
#define T474 Z474
#define T475 Z475
#define T476 Z476
#define T477 Z477
#define T478 Z478
#define T479 Z479
#define T480 Z480
#define T481 Z481
#define T482 Z482
#define T483 Z483
#define T484 Z484
#define T485 Z485
#define T486 Z486
#define T487 Z487
#define T488 Z488
#define T489 Z489
#define T490 Z490
#define T491 Z491
#define T492 Z492
#define T493 Z493
#define T494 Z494
#define T495 Z495
#define T496 Z496
#define T497 Z497
#define T498 Z498
#define T499 Z499
#define T500 Z500
#define T501 Z501
#define T502 Z502
#define T503 Z503
#define T504 Z504
#define T505 Z505
#define T506 Z506
#define T507 Z507
#define T508 Z508
#define T509 Z509
#define T510 Z510
#define T511 Z511
#define T512 Z512
#define T513 Z513
#define T514 Z514
#define T515 Z515
#define T516 Z516
#define T517 Z517
#define T518 Z518
#define T519 Z519
#define T520 Z520
#define T521 Z521
#define T522 Z522
#define T523 Z523
#define T524 Z524
#define T525 Z525
#define T526 Z526
#define T527 Z527
#define T528 Z528
#define T529 Z529
#define T530 Z530
#define T531 Z531
#define T532 Z532
#define T533 Z533
#define T534 Z534
#define T535 Z535
#define T536 Z536
#define T537 Z537
#define T538 Z538
#define T539 Z539
#define T540 Z540
#define T541 Z541
#define T542 Z542
#define T543 Z543
#define T544 Z544
#define T545 Z545
#define T546 Z546
#define T547 Z547
#define T548 Z548
#define T549 Z549
#define T550 Z550
#define T551 Z551
#define T552 Z552
#define T553 Z553
#define T554 Z554
#define T555 Z555
#define T556 Z556
#define T557 Z557
#define T558 Z558
#define T559 Z559
#define T560 Z560
#define T561 Z561
#define T562 Z562
#define T563 Z563
#define T564 Z564
#define T565 Z565
#define T566 Z566
#define T567 Z567
#define T568 Z568
#define T569 Z569
#define T570 Z570
#define T571 Z571
#define T572 Z572
#define T573 Z573
#define T574 Z574
#define T575 Z575
#define T576 Z576
#define T577 Z577
#define T578 Z578
#define T579 Z579
#define T580 Z580
#define T581 Z581
#define T582 Z582
#define T583 Z583
#define T584 Z584
#define T585 Z585
#define T586 Z586
#define T587 Z587
#define T588 Z588
#define T589 Z589
#define T590 Z590
#define T591 Z591
#define T592 Z592
#define T593 Z593
#define T594 Z594
#define T595 Z595
#define T596 Z596
#define T597 Z597
#define T598 Z598
#define T599 Z599
#define T600 Z600
#define T601 Z601
#define T602 Z602
#define T603 Z603
#define T604 Z604
#define T605 Z605
#define T606 Z606
#define T607 Z607
#define T608 Z608
#define T609 Z609
#define T610 Z610
#define T611 Z611
#define T612 Z612
#define T613 Z613
#define T614 Z614
#define T615 Z615
#define T616 Z616
#define T617 Z617
#define T618 Z618
#define T619 Z619
#define T620 Z620
#define T621 Z621
#define T622 Z622
#define T623 Z623
#define T624 Z624
#define T625 Z625
#define T626 Z626
#define T627 Z627
#define T628 Z628
#define T629 Z629
#define T630 Z630
#define T631 Z631
#define T632 Z632
#define T633 Z633
#define T634 Z634
#define T635 Z635
#define T636 Z636
#define T637 Z637
#define T638 Z638
#define T639 Z639
#define T640 Z640
#define T641 Z641
#define T642 Z642
#define T643 Z643
#define T644 Z644
#define T645 Z645
#define T646 Z646
#define T647 Z647
#define T648 Z648
#define T649 Z649
#define T650 Z650
#define T651 Z651
#define T652 Z652
#define T653 Z653
#define T654 Z654
#define T655 Z655
#define T656 Z656
#define T657 Z657
#define T658 Z658
#define T659 Z659
#define T660 Z660
#define T661 Z661
#define T662 Z662
#define T663 Z663
#define T664 Z664
#define T665 Z665
#define T666 Z666
#define T667 Z667
#define T668 Z668
#define T669 Z669
#define T670 Z670
#define T671 Z671
#define T672 Z672
#define T673 Z673
#define T674 Z674
#define T675 Z675
#define T676 Z676
#define T677 Z677
#define T678 Z678
#define T679 Z679
#define T680 Z680
#define T681 Z681
#define T682 Z682
#define T683 Z683
#define T684 Z684
#define T685 Z685
#define T686 Z686
#define T687 Z687
#define T688 Z688
#define T689 Z689
#define T690 Z690
#define T691 Z691
#define T692 Z692
#define T693 Z693
#define T694 Z694
#define T695 Z695
#define T696 Z696
#define T697 Z697
#define T698 Z698
#define T699 Z699
#define T700 Z700
#define T701 Z701
char object_to_char();
int object_to_int();
float object_to_float();
double object_to_double();
char *object_to_string();
int FIXtemp;
#define	CMPmake_fixnum(x) \
((((FIXtemp=(x))+1024)&-2048)==0?small_fixnum(FIXtemp):make_fixnum(FIXtemp))
#define Creturn(v) return((vs_top=vs,(v)))
#define Cexit return((vs_top=vs,0))
double sin(), cos(), tan();
object read_byte1(),read_char1();

#define fs_leader(ar,i) (((object *)((ar)->fs.fs_self))[-(i+1)])
#define RPAREN )
object make_list();
#ifdef HAVE_ALLOCA
#ifndef alloca
char *alloca();
#endif
char *alloca_val;
#define ALLOCA_CONS(n) (alloca_val=alloca((n)*sizeof(struct cons))) 
#define ON_STACK_CONS(x,y) (alloca_val=alloca(sizeof(struct cons)), on_stack_cons(x,y)) 
#define ON_STACK_LIST on_stack_list
#define ON_STACK_LIST_VECTOR on_stack_list_vector
#define ON_STACK_MAKE_LIST on_stack_make_list
object on_stack_cons();
object on_stack_list();
object on_stack_list_vector();
object on_stack_make_list();
#else
#define ALLOCA_CONS(n) 0
#define ON_STACK_CONS(x,y) MMcons(x,y)
#define ON_STACK_LIST list
#define ON_STACK_LIST_VECTOR list_vector
#define ON_STACK_MAKE_LIST make_list
#endif


struct call_data { object fun;
		   int argd;};
struct call_data fcall;
object  fcalln();
object list_vector();
object MVloc[10];
#define VARG(min,max) ((min) | (max << 8))
#define  VFUN_NARGS fcall.argd
extern object Cstd_key_defaults[];
int vfun_wrong_number_of_args();
int eql(),equal(),eq();
object sublis1();
object LVformat(),LVerror();
#define EQ(x,y) ((x)==(y))



/* #include "../h/genpari.h"*/
typedef  long *GEN;
GEN addii(),mulii(),mulsi(),powerii(),shifti(),stoi(),dvmdii(),subii();
int cmpii();
#define signe(x)          (((GEN)(x))[1]>>24)
#define lg(x)             (((GEN)(x))[0]&0xffff)
#define setlg(x,s)        (((GEN)(x))[0]=(((GEN)(x))[0]&0xffff0000)+s)
#define lgef(x)           (((GEN)(x))[1]&0xffff)
#define setlgef(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&0xffff0000)+s)

int in_saved_avma ;
#define ulong unsigned long
/* #define DEBUG_AVMA */

#ifdef DEBUG_AVMA
#define save_avma long lvma = (in_saved_avma = 1, avma)
#define restore_avma avma = (in_saved_avma = 0, lvma)
#else
#define save_avma long lvma = avma
#define restore_avma avma = lvma
#endif
unsigned long avma;
GEN gzero;
GEN icopy_x;

object make_integer();
  /* copy x to y, increasing space by factor of 2  */


GEN otoi();
/*
object integ_temp;
#define otoi(x) (integ_temp = (x) , (type_of(integ_temp) == t_bignum \
   ? MP(integ_temp) :stoi(fix(integ_temp))))
*/
#define ISETQ_FIX(a,b,c) isetq_fix(a,c)
void isetq_fix();
#ifdef HAVE_ALLOCA
#define SETQ_II(var,alloc,val) \
  do{GEN _xx =(val) ; \
  int _n = replace_copy1(_xx,var); \
  if(_n) var = replace_copy2(_xx,alloca(_n));}while(0)

#define SETQ_IO(var,alloc,val) {object _xx =(val) ; \
			      int _n = obj_replace_copy1(_xx,var); \
			    if(_n) var = obj_replace_copy2(_xx,alloca(_n));}
#define IDECL(a,b,c) ulong b[4];a =(b[0]=0x1010000 +4,b) ; object c
#else
GEN setq_io(),setq_ii();
#define SETQ_IO(x,alloc,val)   (x)=setq_io(x,&alloc,val)
#define SETQ_II(x,alloc,val)   (x)=setq_ii(x,&alloc,val)
#define IDECL(a,b,c) ulong b[4];a =(b[0]=0x1010000 +4,b);object c
#endif

object cmod(), cplus(), cdifference(), ctimes();

#ifdef __GNUC__
#define alloca __builtin_alloca
#endif



#include "system-init.h"
init_code(){do_init(VV);}
#define init_or_load(fn,file) do {extern int fn(); init_or_load1(fn,file);}  while(0)
static object our_init() {
init_or_load(init_lmdcls,"/d19/staff/wfs/maxima/src/lmdcls.o");
init_or_load(init_letmac,"/d19/staff/wfs/maxima/src/letmac.o");
init_or_load(init_serror,"/d19/staff/wfs/maxima/src/serror.o");
init_or_load(init_kclmac,"/d19/staff/wfs/maxima/src/kclmac.o");
init_or_load(init_clmacs,"/d19/staff/wfs/maxima/src/clmacs.o");
init_or_load(init_commac,"/d19/staff/wfs/maxima/src/commac.o");
init_or_load(init_mormac,"/d19/staff/wfs/maxima/src/mormac.o");
init_or_load(init_compat,"/d19/staff/wfs/maxima/src/compat.o");
init_or_load(init_defopt,"/d19/staff/wfs/maxima/src/defopt.o");
init_or_load(init_defcal,"/d19/staff/wfs/maxima/src/defcal.o");
init_or_load(init_maxmac,"/d19/staff/wfs/maxima/src/maxmac.o");
init_or_load(init_mopers,"/d19/staff/wfs/maxima/src/mopers.o");
init_or_load(init_mforma,"/d19/staff/wfs/maxima/src/mforma.o");
init_or_load(init_mrgmac,"/d19/staff/wfs/maxima/src/mrgmac.o");
init_or_load(init_procs,"/d19/staff/wfs/maxima/src/procs.o");
init_or_load(init_rzmac,"/d19/staff/wfs/maxima/src/rzmac.o");
init_or_load(init_strmac,"/d19/staff/wfs/maxima/src/strmac.o");
init_or_load(init_displm,"/d19/staff/wfs/maxima/src/displm.o");
init_or_load(init_ratmac,"/d19/staff/wfs/maxima/src/ratmac.o");
init_or_load(init_mhayat,"/d19/staff/wfs/maxima/src/mhayat.o");
init_or_load(init_numerm,"/d19/staff/wfs/maxima/src/numerm.o");
init_or_load(init_optimize,"/d19/staff/wfs/maxima/src/optimize.o");
init_or_load(init_opers,"/d19/staff/wfs/maxima/src/opers.o");
init_or_load(init_utils,"/d19/staff/wfs/maxima/src/utils.o");
init_or_load(init_sumcon,"/d19/staff/wfs/maxima/src/sumcon.o");
init_or_load(init_sublis,"/d19/staff/wfs/maxima/src/sublis.o");
init_or_load(init_runtim,"/d19/staff/wfs/maxima/src/runtim.o");
init_or_load(init_merror,"/d19/staff/wfs/maxima/src/merror.o");
init_or_load(init_mformt,"/d19/staff/wfs/maxima/src/mformt.o");
init_or_load(init_mutils,"/d19/staff/wfs/maxima/src/mutils.o");
init_or_load(init_outmis,"/d19/staff/wfs/maxima/src/outmis.o");
init_or_load(init_ar,"/d19/staff/wfs/maxima/src/ar.o");
init_or_load(init_misc,"/d19/staff/wfs/maxima/src/misc.o");
init_or_load(init_comm,"/d19/staff/wfs/maxima/src/comm.o");
init_or_load(init_comm2,"/d19/staff/wfs/maxima/src/comm2.o");
init_or_load(init_mlisp,"/d19/staff/wfs/maxima/src/mlisp.o");
init_or_load(init_mmacro,"/d19/staff/wfs/maxima/src/mmacro.o");
init_or_load(init_buildq,"/d19/staff/wfs/maxima/src/buildq.o");
init_or_load(init_simp,"/d19/staff/wfs/maxima/src/simp.o");
init_or_load(init_float,"/d19/staff/wfs/maxima/src/float.o");
init_or_load(init_csimp,"/d19/staff/wfs/maxima/src/csimp.o");
init_or_load(init_csimp2,"/d19/staff/wfs/maxima/src/csimp2.o");
init_or_load(init_zero,"/d19/staff/wfs/maxima/src/zero.o");
init_or_load(init_logarc,"/d19/staff/wfs/maxima/src/logarc.o");
init_or_load(init_rpart,"/d19/staff/wfs/maxima/src/rpart.o");
init_or_load(init_macsys,"/d19/staff/wfs/maxima/src/macsys.o");
init_or_load(init_mload,"/d19/staff/wfs/maxima/src/mload.o");
init_or_load(init_suprv1,"/d19/staff/wfs/maxima/src/suprv1.o");
init_or_load(init_dskfn,"/d19/staff/wfs/maxima/src/dskfn.o");
init_or_load(init_lesfac,"/d19/staff/wfs/maxima/src/lesfac.o");
init_or_load(init_factor,"/d19/staff/wfs/maxima/src/factor.o");
init_or_load(init_algfac,"/d19/staff/wfs/maxima/src/algfac.o");
init_or_load(init_nalgfa,"/d19/staff/wfs/maxima/src/nalgfa.o");
init_or_load(init_ufact,"/d19/staff/wfs/maxima/src/ufact.o");
init_or_load(init_result,"/d19/staff/wfs/maxima/src/result.o");
init_or_load(init_rat3a,"/d19/staff/wfs/maxima/src/rat3a.o");
init_or_load(init_rat3b,"/d19/staff/wfs/maxima/src/rat3b.o");
init_or_load(init_rat3d,"/d19/staff/wfs/maxima/src/rat3d.o");
init_or_load(init_rat3c,"/d19/staff/wfs/maxima/src/rat3c.o");
init_or_load(init_rat3e,"/d19/staff/wfs/maxima/src/rat3e.o");
init_or_load(init_nrat4,"/d19/staff/wfs/maxima/src/nrat4.o");
init_or_load(init_ratout,"/d19/staff/wfs/maxima/src/ratout.o");
init_or_load(init_transm,"/d19/staff/wfs/maxima/src/transm.o");
init_or_load(init_transl,"/d19/staff/wfs/maxima/src/transl.o");
init_or_load(init_transs,"/d19/staff/wfs/maxima/src/transs.o");
init_or_load(init_trans1,"/d19/staff/wfs/maxima/src/trans1.o");
init_or_load(init_trans2,"/d19/staff/wfs/maxima/src/trans2.o");
init_or_load(init_trans3,"/d19/staff/wfs/maxima/src/trans3.o");
init_or_load(init_trans4,"/d19/staff/wfs/maxima/src/trans4.o");
init_or_load(init_trans5,"/d19/staff/wfs/maxima/src/trans5.o");
init_or_load(init_transf,"/d19/staff/wfs/maxima/src/transf.o");
init_or_load(init_troper,"/d19/staff/wfs/maxima/src/troper.o");
init_or_load(init_trutil,"/d19/staff/wfs/maxima/src/trutil.o");
init_or_load(init_trmode,"/d19/staff/wfs/maxima/src/trmode.o");
init_or_load(init_trdata,"/d19/staff/wfs/maxima/src/trdata.o");
init_or_load(init_trpred,"/d19/staff/wfs/maxima/src/trpred.o");
init_or_load(init_transq,"/d19/staff/wfs/maxima/src/transq.o");
init_or_load(init_acall,"/d19/staff/wfs/maxima/src/acall.o");
init_or_load(init_fcall,"/d19/staff/wfs/maxima/src/fcall.o");
init_or_load(init_evalw,"/d19/staff/wfs/maxima/src/evalw.o");
init_or_load(init_trprop,"/d19/staff/wfs/maxima/src/trprop.o");
init_or_load(init_mdefun,"/d19/staff/wfs/maxima/src/mdefun.o");
init_or_load(init_bessel,"/d19/staff/wfs/maxima/src/bessel.o");
init_or_load(init_ellipt,"/d19/staff/wfs/maxima/src/ellipt.o");
init_or_load(init_numer,"/d19/staff/wfs/maxima/src/numer.o");
init_or_load(init_intpol,"/d19/staff/wfs/maxima/src/intpol.o");
init_or_load(init_rombrg,"/d19/staff/wfs/maxima/src/rombrg.o");
init_or_load(init_nparse,"/d19/staff/wfs/maxima/src/nparse.o");
init_or_load(init_displa,"/d19/staff/wfs/maxima/src/displa.o");
init_or_load(init_nforma,"/d19/staff/wfs/maxima/src/nforma.o");
init_or_load(init_ldisp,"/d19/staff/wfs/maxima/src/ldisp.o");
init_or_load(init_grind,"/d19/staff/wfs/maxima/src/grind.o");
init_or_load(init_spgcd,"/d19/staff/wfs/maxima/src/spgcd.o");
init_or_load(init_ezgcd,"/d19/staff/wfs/maxima/src/ezgcd.o");
init_or_load(init_option,"/d19/staff/wfs/maxima/src/option.o");
init_or_load(init_macdes,"/d19/staff/wfs/maxima/src/macdes.o");
init_or_load(init_inmis,"/d19/staff/wfs/maxima/src/inmis.o");
init_or_load(init_db,"/d19/staff/wfs/maxima/src/db.o");
init_or_load(init_compar,"/d19/staff/wfs/maxima/src/compar.o");
init_or_load(init_askp,"/d19/staff/wfs/maxima/src/askp.o");
init_or_load(init_sinint,"/d19/staff/wfs/maxima/src/sinint.o");
init_or_load(init_sin,"/d19/staff/wfs/maxima/src/sin.o");
init_or_load(init_risch,"/d19/staff/wfs/maxima/src/risch.o");
init_or_load(init_hayat,"/d19/staff/wfs/maxima/src/hayat.o");
init_or_load(init_defint,"/d19/staff/wfs/maxima/src/defint.o");
init_or_load(init_residu,"/d19/staff/wfs/maxima/src/residu.o");
init_or_load(init_trigi,"/d19/staff/wfs/maxima/src/trigi.o");
init_or_load(init_trigo,"/d19/staff/wfs/maxima/src/trigo.o");
init_or_load(init_trgred,"/d19/staff/wfs/maxima/src/trgred.o");
init_or_load(init_specfn,"/d19/staff/wfs/maxima/src/specfn.o");
init_or_load(init_mat,"/d19/staff/wfs/maxima/src/mat.o");
init_or_load(init_matrix,"/d19/staff/wfs/maxima/src/matrix.o");
init_or_load(init_sprdet,"/d19/staff/wfs/maxima/src/sprdet.o");
init_or_load(init_newinv,"/d19/staff/wfs/maxima/src/newinv.o");
init_or_load(init_linnew,"/d19/staff/wfs/maxima/src/linnew.o");
init_or_load(init_newdet,"/d19/staff/wfs/maxima/src/newdet.o");
init_or_load(init_schatc,"/d19/staff/wfs/maxima/src/schatc.o");
init_or_load(init_matcom,"/d19/staff/wfs/maxima/src/matcom.o");
init_or_load(init_matrun,"/d19/staff/wfs/maxima/src/matrun.o");
init_or_load(init_nisimp,"/d19/staff/wfs/maxima/src/nisimp.o");
init_or_load(init_tlimit,"/d19/staff/wfs/maxima/src/tlimit.o");
init_or_load(init_limit,"/d19/staff/wfs/maxima/src/limit.o");
init_or_load(init_solve,"/d19/staff/wfs/maxima/src/solve.o");
init_or_load(init_psolve,"/d19/staff/wfs/maxima/src/psolve.o");
init_or_load(init_algsys,"/d19/staff/wfs/maxima/src/algsys.o");
init_or_load(init_polyrz,"/d19/staff/wfs/maxima/src/polyrz.o");
init_or_load(init_cpoly,"/d19/staff/wfs/maxima/src/cpoly.o");
init_or_load(init_mtrace,"/d19/staff/wfs/maxima/src/mtrace.o");
init_or_load(init_scs,"/d19/staff/wfs/maxima/src/scs.o");
init_or_load(init_asum,"/d19/staff/wfs/maxima/src/asum.o");
init_or_load(init_fortra,"/d19/staff/wfs/maxima/src/fortra.o");
init_or_load(init_optim,"/d19/staff/wfs/maxima/src/optim.o");
init_or_load(init_marray,"/d19/staff/wfs/maxima/src/marray.o");
init_or_load(init_mdot,"/d19/staff/wfs/maxima/src/mdot.o");
init_or_load(init_irinte,"/d19/staff/wfs/maxima/src/irinte.o");
init_or_load(init_series,"/d19/staff/wfs/maxima/src/series.o");
init_or_load(init_numth,"/d19/staff/wfs/maxima/src/numth.o");
init_or_load(init_laplac,"/d19/staff/wfs/maxima/src/laplac.o");
init_or_load(init_pade,"/d19/staff/wfs/maxima/src/pade.o");
init_or_load(init_homog,"/d19/staff/wfs/maxima/src/homog.o");
init_or_load(init_combin,"/d19/staff/wfs/maxima/src/combin.o");
init_or_load(init_mstuff,"/d19/staff/wfs/maxima/src/mstuff.o");
init_or_load(init_ratpoi,"/d19/staff/wfs/maxima/src/ratpoi.o");
init_or_load(init_pois2,"/d19/staff/wfs/maxima/src/pois2.o");
init_or_load(init_pois3,"/d19/staff/wfs/maxima/src/pois3.o");
return Cnil;}
/*	function definition for INITIALIZE-MAXIMA	*/

static L1()
{	object *old_base=vs_base;
	object x;
	x=
	our_init();
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=x;
}
