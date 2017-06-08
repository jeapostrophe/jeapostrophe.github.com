
notes/18-stupid.o:     file format mach-o-x86-64


Disassembly of section .text:

0000000000000000 <_main>:
   0:	55                   	push   %rbp
   1:	48 89 e5             	mov    %rsp,%rbp
   4:	48 83 ec 10          	sub    $0x10,%rsp
   8:	48 8d 3d 1e 00 00 00 	lea    0x1e(%rip),%rdi        # 2d <_main+0x2d>
   f:	c7 45 fc 00 00 00 00 	movl   $0x0,-0x4(%rbp)
  16:	b0 00                	mov    $0x0,%al
  18:	e8 00 00 00 00       	callq  1d <_main+0x1d>
  1d:	b9 04 00 00 00       	mov    $0x4,%ecx
  22:	89 45 f8             	mov    %eax,-0x8(%rbp)
  25:	89 c8                	mov    %ecx,%eax
  27:	48 83 c4 10          	add    $0x10,%rsp
  2b:	5d                   	pop    %rbp
  2c:	c3                   	retq   
