.comm _vs_limit,4
.comm _vs_base,4
.comm _vs_top,4
.comm _bds_limit,4
.comm _bds_top,4
.comm _ihs_limit,4
.comm _ihs_top,4
.comm _frs_limit,4
.comm _frs_top,4
.comm _nlj_active,4
.comm _nlj_fr,4
.comm _nlj_tag,4
.comm _lex_env,4
.comm _CMPtemp,4
.comm _CMPtemp1,4
.comm _CMPtemp2,4
.comm _CMPtemp3,4
.comm _Cnil_body,36
.comm _Ct_body,36
.comm _Scons,4
.comm _siSfunction_documentation,4
.comm _siSvariable_documentation,4
.comm _siSpretty_print_format,4
.comm _Slist,4
.comm _keyword_package,4
.comm _FIXtemp,4
.globl _dblrem
.text
	.even
_dblrem:
	link a6,#0
	movl a6@(8),d1
	mulsl a6@(12),d0:d1
	divsl a6@(16),d0:d1
	unlk a6
	rts
.comm _TOPhalf,4
.globl _ftimes1
.text
	.even
_ftimes1:
	link a6,#0
	movl a6@(8),d1
	mulsl a6@(12),d0:d1
	movl d1, _TOPhalf
	unlk a6
	rts
.globl _ftimes
.text
	.even
_ftimes:
	link a6,#0
	moveml #0x2030,sp@-
	movl a6@(8),a3
	movl a6@(12),a2
	movl a2@(4),sp@-
	movl a3@(4),sp@-
	jbsr _ftimes1
	movl d0,d1
	addql #8,sp
	jge L5
	moveq #-1,d2
	cmpl _TOPhalf,d2
	jne L11
	movl d1,_FIXtemp
	movl _FIXtemp,d0
	addl #1024,d0
	andw #-2048,d0
	jne L8
	movl d1,d0
	asll #3,d0
	addl #_small_fixnum_table,d0
	addl #8192,d0
	jra L9
L8:
	movl _FIXtemp,sp@-
	jbsr _make_fixnum
	addql #4,sp
L9:
	jra L3
L5:
	tstl _TOPhalf
	jne L11
	movl d1,_FIXtemp
	movl _FIXtemp,d0
	addl #1024,d0
	andw #-2048,d0
	jne L12
	movl d1,d0
	asll #3,d0
	addl #_small_fixnum_table,d0
	addl #8192,d0
	jra L13
L12:
	movl _FIXtemp,sp@-
	jbsr _make_fixnum
	addql #4,sp
L13:
	jra L3
L11:
	movl a2,sp@-
	movl a3,sp@-
	jbsr _number_times
L3:
	moveml a6@(-12),#0xc04
	unlk a6
	rts
.globl _ctimes
.text
	.even
_ctimes:
	link a6,#0
	moveml #0x3020,sp@-
	movl a6@(8),a2
	movl a6@(12),a1
	movl a6@(16),a0
	bfextu a2@{#0:#24},d0
	moveq #1,d3
	cmpl d0,d3
	jne L15
	bfextu a1@{#0:#24},d0
	cmpl d0,d3
	jne L15
	cmpl #_Cnil_body,a0
	jne L17
	movl a1,sp@-
	movl a2,sp@-
	jbsr _ftimes
	jra L24
L17:
	bfextu a0@{#0:#24},d0
	moveq #1,d3
	cmpl d0,d3
	jne L18
	movl a0@(4),d2
	movl d2,sp@-
	movl a1@(4),sp@-
	movl a2@(4),sp@-
	jbsr _dblrem
	movl d0,d1
	movl d2,d0
	asrl #1,d0
	addw #12,sp
	cmpl d1,d0
	jge L20
	subl d2,d1
	jra L19
L20:
	negl d0
	cmpl d1,d0
	jle L21
	addl d2,d1
L21:
L19:
	movl d1,_FIXtemp
	movl _FIXtemp,d0
	addl #1024,d0
	andw #-2048,d0
	jne L22
	movl d1,d0
	asll #3,d0
	addl #_small_fixnum_table,d0
	addl #8192,d0
	jra L23
L22:
	movl _FIXtemp,sp@-
	jbsr _make_fixnum
	addql #4,sp
L23:
	jra L14
L18:
L15:
	movl a0,sp@-
	movl a1,sp@-
	movl a2,sp@-
	jbsr _number_times
	addql #8,sp
	movl d0,sp@-
	jbsr _mcmod
L24:
L14:
	moveml a6@(-12),#0x40c
	unlk a6
	rts
.globl _mcmod
.text
	.even
_mcmod:
	link a6,#-8
	moveml #0x3030,sp@-
	movl a6@(8),a3
	movl a6@(12),a2
	cmpl #_Cnil_body,a2
	jeq L39
	bfextu a2@{#0:#24},d0
	moveq #1,d3
	cmpl d0,d3
	jne L29
	bfextu a3@{#0:#24},d0
	cmpl d0,d3
	jne L29
	movl a2@(4),d0
	jge L30
	negl d0
L30:
	movl d0,d2
	asrl #1,d2
	movl a3@(4),d1
	divsll d0,d0:d1
	movl d0,d1
	cmpl d1,d2
	jgt L32
	subl d2,d1
	jra L31
L32:
	movl d2,d0
	negl d0
	cmpl d1,d0
	jle L33
	addl d2,d1
L33:
L31:
	movl d1,_FIXtemp
	movl _FIXtemp,d0
	addl #1024,d0
	andw #-2048,d0
	jne L34
	movl d1,d0
	asll #3,d0
	addl #_small_fixnum_table,d0
	addl #8192,d0
	jra L35
L34:
	movl _FIXtemp,sp@-
	jbsr _make_fixnum
	addql #4,sp
L35:
	jra L25
L29:
	clrl _FIXtemp
	movl _FIXtemp,d0
	addl #1024,d0
	andw #-2048,d0
	jne L37
	movl #_small_fixnum_table,d0
	addl #8192,d0
	jra L38
L37:
	movl _FIXtemp,sp@-
	jbsr _make_fixnum
	addql #4,sp
L38:
	movl d0,sp@-
	movl a2,sp@-
	jbsr _number_compare
	addql #8,sp
	tstl d0
	jge L36
	movl a2,sp@-
	jbsr _number_negate
	movl d0,a2
	addql #4,sp
L36:
	pea a6@(-8)
	pea a6@(-4)
	movl a2,sp@-
	movl a3,sp@-
	jbsr _integer_quotient_remainder_1
	pea 2:w
	movl a2,sp@-
	jbsr _shift_integer
	movl d0,sp@-
	movl a3,sp@-
	jbsr _number_compare
	addw #32,sp
	tstl d0
	jlt L39
	movl a2,sp@-
	movl a3,sp@-
	jbsr _number_minus
	movl d0,a3
	addql #8,sp
L39:
	movl a3,d0
L25:
	moveml a6@(-24),#0xc0c
	unlk a6
	rts
.globl _fplus
.text
	.even
_fplus:
	link a6,#0
	movl d2,sp@-
	movl a6@(0x8),d0
	addl a6@(0xc),d0
	bvs overflow_case
	movl d0,a6@(0x8)
	jra rest
	overflow_case:
	movl d0,a6@(0x8)
	tstl a6@(12)
	jle L41
	movl a6@(8),d2
	andl #2147483647,d2
	movl d2,sp@-
	pea 1:w
	jra L45
L41:
	movl a6@(8),d2
	andl #2147483647,d2
	movl d2,sp@-
	pea -2:w
L45:
	jbsr _bignum2
	addql #8,sp
	jra end
	rest:
	movl a6@(8),d1
	movl d1,d0
	addl #1024,d0
	andw #-2048,d0
	jne L43
	movl d1,d0
	asll #3,d0
	addl #_small_fixnum_table,d0
	addl #8192,d0
	jra L44
L43:
	movl d1,sp@-
	jbsr _make_fixnum
	addql #4,sp
L44:
	end:
	movl a6@(-4),d2
	unlk a6
	rts
