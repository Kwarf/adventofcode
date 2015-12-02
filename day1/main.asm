extern printf

section .data
helptext: db "This program requires one command line argument, your puzzle input.", 10
helplen: equ $-helptext
end_floor_fmt: db "The instructions took Santa to floor %i", 10, 0
basement_fmt: db "Santa first entered the basement at position %i", 10, 0

section .text
global main
main:
	mov rcx, rdi ; argc
	cmp rcx, 2
	je input_ok
	; input not ok, print help and jump to end of main
	mov rax, 1
	mov rdi, 1
	mov	rsi, helptext
	mov	rdx, helplen
	syscall
	jmp main_end
input_ok:

	mov rax, 0 ; final elevator level
	mov rbx, 0 ; current character position as we loop
	mov rcx, 0 ; the position where we first enter the basement
	mov rdx, [rsi+8] ; argv[1], i.e. a pointer to the first letter in the puzzle input
loop:
	cmp byte [rdx], 0
	je loop_end ; stop when we reach the null byte
	inc rbx

	cmp byte [rdx], 40 ; 40 is (, go up one floor
	jne dont_increase
	inc rax
dont_increase:
	cmp byte [rdx], 41 ; 41 is ), go down one floor
	jne dont_decrease
	dec rax
dont_decrease:
	cmp rcx, 0
	jne after_basement_check ; if rcx != 0, i.e. if we have visited the basement, stop checking if we have
	cmp rax, -1
	jg after_basement_check ; if rax > -1 we're not yet in the basement
	mov rcx, rbx
after_basement_check:
	inc rdx ; increase string pointer
	jmp loop
loop_end:

	; printf clobbers rcx
	push rcx

	; print the end floor
	mov rsi, rax
	mov rdi, end_floor_fmt
	mov rax, 0
	call printf

	; print where we entered the basement
	pop rsi
	mov rdi, basement_fmt
	mov rax, 0
	call printf

main_end:
	mov rax, 0 ; return code
	ret
