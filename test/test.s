  .text
.globl _main
_main:
  pushq %rbp
  movq %rsp, %rbp
  subq $240, %rsp

  movl $80, -4(%rbp)
  movl -4(%rbp), %edi
  call _putc

  incl -4(%rbp)
  movl -4(%rbp), %edi
  call _putc

  incl -4(%rbp)
  movl -4(%rbp), %edi
  call _putc

  movq $4, %rax
  negq %rax
  incl (%rbp,%rax,1)
  movl (%rbp,%rax,1), %edi
  call _putc

  call _getc
  movl %eax, %edi
  call _putc

  movl $0x0A, %edi
  call _putc
  leave
  ret

_putc:
  pushq %rbp
  movq %rsp, %rbp
  movl %edi, -4(%rbp)
  movq $0x2000004, %rax  # System call write = 4
  movq $1, %rdi          # Write to standard out = 1
  leaq -4(%rbp), %rsi    # The address of string
  movq $1, %rdx          # The size to write
  syscall                # Invoke the kernel
  leave
  ret

_getc:
  pushq %rbp
  movq %rsp, %rbp
  movl %edi, -4(%rbp)
  movq $0x2000003, %rax  # System call write = 3
  movq $1, %rdi          # Read from standard in = 1
  leaq -4(%rbp), %rsi    # The address of string
  movq $1, %rdx          # The size to read
  syscall                # Invoke the kernel
  movl -4(%rbp), %eax
  leave
  ret
