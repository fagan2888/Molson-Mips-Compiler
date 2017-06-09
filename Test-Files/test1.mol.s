.text
addi $r26, $0, 10
add $r25, $r26, $0
la $r26, L36
add $r26, $r26, $0
addi $r26, $0, 16
sub $r26, $sp, $r26
add $sp, $r26, $0
addi $r26, $0, 1
sw $r26, 0($sp)
la $r26, L37
sw $r26, 4($sp)
addi $r26, $0, 2
sw $r26, 8($sp)
addi $r26, $0, 1
sw $r26, 12($sp)
add $r26, $sp, $0
move $r26, $r25
jal test2
move $r26, $rv
add $r25, $r26, $0
addi $r26, $0, 5
move $r24, $r26
jal test3
move $r26, $rv
add $r26, $r26, $0
add $r26, $r25, $r26
add $r26, $r26, $0
j main
test:
sw $fp, -4($sp)
sw $ra, -8($sp)
addi $fp, $sp, 0
addi $sp, $sp, -8
lw $r26, 12($r26)
add $rv, $r26, $0
addi $r26, $sp, 0
add $sp, $r26, $0
addi $sp, $sp, 8
lw $fp, -4($sp)
lw $ra, -8($sp)
jr $ra
test2:
sw $fp, -4($sp)
sw $ra, -8($sp)
addi $fp, $sp, 0
addi $sp, $sp, -8
addi $r26, $0, 16
sub $r26, $sp, $r26
add $sp, $r26, $0
addi $r26, $0, 1
sw $r26, 0($sp)
la $r26, L37
sw $r26, 4($sp)
addi $r26, $0, 2
sw $r26, 8($sp)
addi $r26, $0, 1
sw $r26, 12($sp)
addi $r26, $0, 0
add $rv, $r26, $0
addi $r26, $sp, 16
add $sp, $r26, $0
addi $sp, $sp, 8
lw $fp, -4($sp)
lw $ra, -8($sp)
jr $ra
test3:
sw $fp, -4($sp)
sw $ra, -8($sp)
addi $fp, $sp, 0
addi $sp, $sp, -8
add $rv, $r24, $0
addi $r26, $sp, 0
add $sp, $r26, $0
addi $sp, $sp, 8
lw $fp, -4($sp)
lw $ra, -8($sp)
jr $ra
.data
L36:
.asciiz "hello"
L37:
.asciiz "test"
