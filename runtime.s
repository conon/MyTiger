	.arch armv7-a
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfpv3-d16
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 18, 4
	.file	"runtime.c"
	.text
	.align	2
	.global	initArrayT
	.type	initArrayT, %function
initArrayT:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #16
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asl #2
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-12]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L2
.L3:
	ldr	r3, [fp, #-8]
	mov	r3, r3, asl #2
	ldr	r2, [fp, #-12]
	add	r3, r2, r3
	ldr	r2, [fp, #-20]
	str	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L2:
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	cmp	r2, r3
	blt	.L3
	ldr	r3, [fp, #-12]
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	initArrayT, .-initArrayT
	.align	2
	.global	allocRecordT
	.type	allocRecordT, %function
allocRecordT:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #24
	str	r0, [fp, #-24]
	ldr	r3, [fp, #-24]
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	str	r3, [fp, #-12]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L5
.L6:
	ldr	r3, [fp, #-12]
	mov	r2, #0
	str	r2, [r3, #0]
	ldr	r3, [fp, #-12]
	add	r3, r3, #4
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-8]
	add	r3, r3, #4
	str	r3, [fp, #-8]
.L5:
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	blt	.L6
	ldr	r3, [fp, #-16]
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	allocRecordT, .-allocRecordT
	.comm	consts,2048,4
	.global	empty
	.bss
	.align	2
	.type	empty, %object
	.size	empty, 8
empty:
	.space	8
	.text
	.align	2
	.global	stringEqualT
	.type	stringEqualT, %function
stringEqualT:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #20
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	cmp	r2, r3
	bne	.L8
	mov	r3, #1
	b	.L9
.L8:
	ldr	r3, [fp, #-16]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-20]
	ldr	r3, [r3, #0]
	cmp	r2, r3
	beq	.L10
	mov	r3, #0
	b	.L9
.L10:
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L11
.L13:
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	ldrb	r2, [r3, #4]	@ zero_extendqisi2
	ldr	r1, [fp, #-20]
	ldr	r3, [fp, #-8]
	add	r3, r1, r3
	ldrb	r3, [r3, #4]	@ zero_extendqisi2
	cmp	r2, r3
	beq	.L12
	mov	r3, #0
	b	.L9
.L12:
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L11:
	ldr	r3, [fp, #-16]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L13
	mov	r3, #1
.L9:
	mov	r0, r3
	add	sp, fp, #0
	ldmfd	sp!, {fp}
	bx	lr
	.size	stringEqualT, .-stringEqualT
	.align	2
	.global	printT
	.type	printT, %function
printT:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #16
	str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	add	r3, r3, #4
	str	r3, [fp, #-12]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L15
.L16:
	ldr	r3, [fp, #-12]
	ldrb	r3, [r3, #0]	@ zero_extendqisi2
	mov	r0, r3
	bl	putchar
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-12]
	add	r3, r3, #1
	str	r3, [fp, #-12]
.L15:
	ldr	r3, [fp, #-16]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L16
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	printT, .-printT
	.align	2
	.global	flushT
	.type	flushT, %function
flushT:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	movw	r3, #:lower16:stdout
	movt	r3, #:upper16:stdout
	ldr	r3, [r3, #0]
	mov	r0, r3
	bl	fflush
	ldmfd	sp!, {fp, pc}
	.size	flushT, .-flushT
	.align	2
	.global	main
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L19
.L20:
	movw	r3, #:lower16:consts
	movt	r3, #:upper16:consts
	ldr	r2, [fp, #-8]
	mov	r1, #1
	str	r1, [r3, r2, asl #3]
	ldr	r3, [fp, #-8]
	uxtb	r2, r3
	movw	r3, #:lower16:consts
	movt	r3, #:upper16:consts
	ldr	r0, [fp, #-8]
	mov	r1, #4
	mov	r0, r0, asl #3
	add	r3, r3, r0
	add	r3, r3, r1
	strb	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L19:
	ldr	r3, [fp, #-8]
	cmp	r3, #255
	ble	.L20
	mov	r0, #0
	bl	tigermain
	mov	r3, r0
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	main, .-main
	.align	2
	.global	ordT
	.type	ordT, %function
ordT:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	ldr	r3, [r3, #0]
	cmp	r3, #0
	bne	.L22
	mvn	r3, #0
	b	.L23
.L22:
	ldr	r3, [fp, #-8]
	ldrb	r3, [r3, #4]	@ zero_extendqisi2
.L23:
	mov	r0, r3
	add	sp, fp, #0
	ldmfd	sp!, {fp}
	bx	lr
	.size	ordT, .-ordT
	.section	.rodata
	.align	2
.LC0:
	.ascii	"chr(%d) out of range\012\000"
	.text
	.align	2
	.global	chrT
	.type	chrT, %function
chrT:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	cmp	r3, #0
	blt	.L25
	ldr	r3, [fp, #-8]
	cmp	r3, #255
	ble	.L26
.L25:
	movw	r3, #:lower16:.LC0
	movt	r3, #:upper16:.LC0
	mov	r0, r3
	ldr	r1, [fp, #-8]
	bl	printf
	mov	r0, #1
	bl	exit
.L26:
	ldr	r3, [fp, #-8]
	mov	r2, r3, asl #3
	movw	r3, #:lower16:consts
	movt	r3, #:upper16:consts
	add	r3, r2, r3
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	chrT, .-chrT
	.align	2
	.global	sizeT
	.type	sizeT, %function
sizeT:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	ldr	r3, [r3, #0]
	mov	r0, r3
	add	sp, fp, #0
	ldmfd	sp!, {fp}
	bx	lr
	.size	sizeT, .-sizeT
	.section	.rodata
	.align	2
.LC1:
	.ascii	"substring([%d],%d,%d) out of range\012\000"
	.text
	.align	2
	.global	substringT
	.type	substringT, %function
substringT:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #24
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r3, [fp, #-20]
	cmp	r3, #0
	blt	.L29
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-24]
	add	r2, r2, r3
	ldr	r3, [fp, #-16]
	ldr	r3, [r3, #0]
	cmp	r2, r3
	ble	.L30
.L29:
	movw	r3, #:lower16:.LC1
	movt	r3, #:upper16:.LC1
	ldr	r2, [fp, #-16]
	ldr	r2, [r2, #0]
	mov	r0, r3
	mov	r1, r2
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-24]
	bl	printf
	mov	r0, #1
	bl	exit
.L30:
	ldr	r3, [fp, #-24]
	cmp	r3, #1
	bne	.L31
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	ldrb	r3, [r3, #4]	@ zero_extendqisi2
	mov	r2, r3, asl #3
	movw	r3, #:lower16:consts
	movt	r3, #:upper16:consts
	add	r3, r2, r3
	b	.L32
.L31:
	ldr	r3, [fp, #-24]
	add	r3, r3, #4
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-12]
	ldr	r2, [fp, #-24]
	str	r2, [r3, #0]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L33
.L34:
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-8]
	add	r2, r2, r3
	ldr	r1, [fp, #-16]
	mov	r3, #4
	add	r2, r1, r2
	add	r3, r2, r3
	ldrb	r2, [r3, #0]	@ zero_extendqisi2
	ldr	r1, [fp, #-12]
	ldr	r3, [fp, #-8]
	add	r3, r1, r3
	strb	r2, [r3, #4]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L33:
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-24]
	cmp	r2, r3
	blt	.L34
	ldr	r3, [fp, #-12]
.L32:
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	substringT, .-substringT
	.align	2
	.global	concatT
	.type	concatT, %function
concatT:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #24
	str	r0, [fp, #-24]
	str	r1, [fp, #-28]
	ldr	r3, [fp, #-24]
	ldr	r3, [r3, #0]
	cmp	r3, #0
	bne	.L36
	ldr	r3, [fp, #-28]
	b	.L37
.L36:
	ldr	r3, [fp, #-28]
	ldr	r3, [r3, #0]
	cmp	r3, #0
	bne	.L38
	ldr	r3, [fp, #-24]
	b	.L37
.L38:
	ldr	r3, [fp, #-24]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-28]
	ldr	r3, [r3, #0]
	add	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-12]
	add	r3, r3, #4
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	ldr	r2, [fp, #-12]
	str	r2, [r3, #0]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L39
.L40:
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	ldrb	r2, [r3, #4]	@ zero_extendqisi2
	ldr	r1, [fp, #-16]
	ldr	r3, [fp, #-8]
	add	r3, r1, r3
	strb	r2, [r3, #4]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L39:
	ldr	r3, [fp, #-24]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L40
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L41
.L42:
	ldr	r3, [fp, #-24]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	add	r1, r2, r3
	ldr	r2, [fp, #-28]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	ldrb	r2, [r3, #4]	@ zero_extendqisi2
	ldr	r0, [fp, #-16]
	mov	r3, #4
	add	r1, r0, r1
	add	r3, r1, r3
	strb	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L41:
	ldr	r3, [fp, #-28]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L42
	ldr	r3, [fp, #-16]
.L37:
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	concatT, .-concatT
	.align	2
	.global	notT
	.type	notT, %function
notT:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	cmp	r3, #0
	movne	r3, #0
	moveq	r3, #1
	mov	r0, r3
	add	sp, fp, #0
	ldmfd	sp!, {fp}
	bx	lr
	.size	notT, .-notT
	.align	2
	.global	getcharT
	.type	getcharT, %function
getcharT:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	movw	r3, #:lower16:stdin
	movt	r3, #:upper16:stdin
	ldr	r3, [r3, #0]
	mov	r0, r3
	bl	_IO_getc
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	cmn	r3, #1
	bne	.L45
	movw	r3, #:lower16:empty
	movt	r3, #:upper16:empty
	b	.L46
.L45:
	ldr	r3, [fp, #-8]
	mov	r2, r3, asl #3
	movw	r3, #:lower16:consts
	movt	r3, #:upper16:consts
	add	r3, r2, r3
.L46:
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	getcharT, .-getcharT
	.section	.rodata
	.align	2
.LC2:
	.ascii	"%d\000"
	.text
	.align	2
	.global	itoaT
	.type	itoaT, %function
itoaT:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #24
	str	r0, [fp, #-24]
	mov	r0, #32
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-12]
	movw	r3, #:lower16:.LC2
	movt	r3, #:upper16:.LC2
	ldr	r0, [fp, #-12]
	mov	r1, r3
	ldr	r2, [fp, #-24]
	bl	sprintf
	ldr	r0, [fp, #-12]
	bl	strlen
	mov	r3, r0
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-20]
	ldr	r3, [fp, #-20]
	ldr	r2, [fp, #-16]
	str	r2, [r3, #0]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L48
.L49:
	ldr	r3, [fp, #-8]
	ldr	r2, [fp, #-12]
	add	r3, r2, r3
	ldrb	r2, [r3, #0]	@ zero_extendqisi2
	ldr	r1, [fp, #-20]
	ldr	r3, [fp, #-8]
	add	r3, r1, r3
	strb	r2, [r3, #4]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L48:
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	cmp	r2, r3
	blt	.L49
	ldr	r3, [fp, #-20]
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	itoaT, .-itoaT
	.align	2
	.global	getStringT
	.type	getStringT, %function
getStringT:
	@ args = 0, pretend = 0, frame = 96
	@ frame_needed = 1, uses_anonymous_args = 0
	stmfd	sp!, {fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #96
	movw	r3, #:lower16:stdin
	movt	r3, #:upper16:stdin
	ldr	r3, [r3, #0]
	sub	r2, fp, #96
	mov	r0, r2
	mov	r1, #80
	mov	r2, r3
	bl	fgets
	sub	r3, fp, #96
	mov	r0, r3
	bl	strlen
	mov	r3, r0
	sub	r3, r3, #1
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-12]
	add	r3, r3, #4
	mov	r0, r3
	bl	malloc
	mov	r3, r0
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	ldr	r2, [fp, #-12]
	str	r2, [r3, #0]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L51
.L52:
	mvn	r3, #91
	ldr	r2, [fp, #-8]
	sub	r1, fp, #4
	add	r2, r1, r2
	add	r3, r2, r3
	ldrb	r2, [r3, #0]	@ zero_extendqisi2
	ldr	r1, [fp, #-16]
	ldr	r3, [fp, #-8]
	add	r3, r1, r3
	strb	r2, [r3, #4]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L51:
	ldr	r3, [fp, #-16]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L52
	ldr	r3, [fp, #-16]
	mov	r0, r3
	sub	sp, fp, #4
	ldmfd	sp!, {fp, pc}
	.size	getStringT, .-getStringT
	.align	2
	.global	strToIntT
	.type	strToIntT, %function
strToIntT:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #28
	str	r0, [fp, #-24]
	ldr	r3, [fp, #-24]
	ldr	r3, [r3, #0]
	str	r3, [fp, #-16]
	mov	r3, #0
	str	r3, [fp, #-12]
	mov	r3, #0
	str	r3, [fp, #-8]
	b	.L54
.L55:
	ldr	r2, [fp, #-12]
	mov	r3, r2
	mov	r3, r3, asl #2
	add	r3, r3, r2
	mov	r3, r3, asl #1
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	ldrb	r3, [r3, #4]	@ zero_extendqisi2
	sub	r3, r3, #48
	ldr	r2, [fp, #-12]
	add	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
.L54:
	ldr	r3, [fp, #-24]
	ldr	r2, [r3, #0]
	ldr	r3, [fp, #-8]
	cmp	r2, r3
	bgt	.L55
	ldr	r3, [fp, #-12]
	mov	r0, r3
	add	sp, fp, #0
	ldmfd	sp!, {fp}
	bx	lr
	.size	strToIntT, .-strToIntT
	.ident	"GCC: (Debian 4.6.3-14) 4.6.3"
	.section	.note.GNU-stack,"",%progbits
