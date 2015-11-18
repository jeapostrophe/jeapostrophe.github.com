	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 10
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	leaq	L_.str(%rip), %rdi
	leaq	-20(%rbp), %rsi
	movb	$0, %al
	callq	_scanf
	leaq	L_.str(%rip), %rdi
	leaq	-20(%rbp), %rsi
	movl	%eax, -24(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_scanf
	movl	-20(%rbp), %ecx
	shll	$1, %ecx
	movl	%ecx, -20(%rbp)
	cmpl	$60, -20(%rbp)
	movl	%eax, -28(%rbp)         ## 4-byte Spill
	jle	LBB0_3
## BB#2:                                ##   in Loop: Header=BB0_1 Depth=1
	jmp	LBB0_1
LBB0_3:
	movl	-20(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%d"


.subsections_via_symbols
