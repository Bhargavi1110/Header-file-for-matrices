include 'emu8086.inc'
org 100h

;=====================================================================
;-----------------------MACROS----------------------------------- 
;Macro for determinant
FIND_DETERMINANT MACRO MATRIX, M, N, DIM
    LOCAL EXIT, EXIT1, EXIT2, EXIT3
    MOV CX,DIM            
    CMP CX,9
    JE 3X3
    JNE 2X2
    
    2X2:
        CMP CX, 4
        JNE EXIT3
        XOR AX, AX
        XOR BX, BX
        XOR CX, CX 
        MOV SI,0
        MOV AX,MATRIX[SI]
        MOV BX,MATRIX[6]
        IMUL BX
        MOV CX,AX
        
        MOV AX,MATRIX[2]
        MOV BX,MATRIX[4]
        IMUL BX
        SUB CX,AX
        MOV DET_ANS,CX
        MOV AX,CX
        
        CMP AX, 0
        JE EXIT1
        
        PRINTN "DETERMINANT OF THE MATRIX IS : "
        CALL PRINT_NUM
        JMP EXIT
        EXIT1:
            PRINTN 'The determiannt is 0, hence singular'
        JMP EXIT 
        
    3X3:
   
        XOR CX,CX
        MOV AX,MATRIX[8]
        MOV BX,MATRIX[16]
        IMUL BX
        MOV CX,AX
        MOV AX,MATRIX[10]
        MOV BX,MATRIX[14]
        IMUL BX
        SUB CX,AX                                                          
        MOV AX,MATRIX[0]                                                     
        MOV BX,CX                                                         
        MUL BX 
        MOV TEMP[0],AX
        
        XOR CX,CX
        MOV AX,MATRIX[6]
        MOV BX,MATRIX[16]
        IMUL BX
        MOV CX,AX
        MOV AX,MATRIX[10]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        MOV AX,MATRIX[2]
        NEG AX
        MOV BX,CX
        MUL BX 
        MOV TEMP[2],AX
        
        XOR CX,CX
        MOV AX,MATRIX[6]
        MOV BX,MATRIX[14]
        IMUL BX
        MOV CX,BX
        MOV AX,MATRIX[8]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        MOV AX,MATRIX[4]
        MOV BX,CX
        MUL BX 
        MOV TEMP[4],AX
        
        MOV AX,TEMP[0]
        MOV BX,TEMP[2]
        ADD AX,BX
        ADD AX,TEMP[4]
        MOV DET_ANS,AX
                   
        CMP AX, 0
        JE EXIT2
        
        PRINTN 'THE MATRIX IS NON-SINGULAR'
        PRINTN "DETERMINANT OF THE MATRIX IS : "
        CALL PRINT_NUM
        JMP EXIT
        EXIT2:
            PRINTN 'The determiannt is 0, hence singular'
        JMP EXIT
        
    EXIT3:
        PRINTN 'DETERMINANT CANNOT BE COMPUTED!'
    EXIT:
ENDM

;---------------------------------------------------------------------------------
;macro to check type
check_type macro matrix, m, n, dim
    local exit, exit1, exit2, exit3, exit4, check_col 
    xor ax, ax
    xor bx, bx
    
    mov ax, m
    mov bx, n
    
    cmp ax, 1
    je exit1
    jmp exit3
    check_col:
        cmp bx, 1
        je exit2
        jmp exit4
    
    exit1:
        printn 'THIS IS A ROW MATRIX'
        jmp exit
    exit2:
        printn 'THIS IS A COLUMN MATRIX'
        jmp exit
    exit3:
        printn 'THIS IS NOT A ROW MATRIX'
        jmp check_col
    exit4:
        printn 'THIS IS NOT A COLUMN MATRIX'
        jmp exit
    exit:
endm

;-------------------------------------------------------------------
;Macro for addition
PERFORM_ADDITION MACRO A, B, C1, DIM1, DIM2
    LOCAL EXIT, EXIT1, NEW_LINE
    XOR AX, AX
    XOR BX, BX
    MOV AX, DIM1
    MOV BX, DIM2
    CMP AX, BX
    JNE EXIT1
    
    MOV SI,0    
    ADDITION:
        MOV BX,A[SI]
        MOV CX,B[SI]
        ADD BX,CX
        MOV ADD_ANS[SI],BX
        ADD SI,2
        DEC AX
        CMP AX,0
        JNZ ADDITION
    
    MOV CX,DIM1
    MOV SI,0
    MOV BX,0
    PRINTN ""
    PRINT_ADD:
        PRINT "   "
        MOV AX,ADD_ANS[SI]
        CALL PRINT_NUM
        INC BX
        CMP BX, C1
        JE NEW_LINE
        ADD SI,2
        LOOP PRINT_ADD
        NEW_LINE:
            PRINTN ""
            MOV BX, 0
            ADD SI, 2
        LOOP PRINT_ADD
    JMP EXIT    
    EXIT1:
        PRINTN 'ADDITION NOT POSSIBLE !'
    EXIT:
ENDM

;--------------------------------------------------------------------
;Macro for subtraction
PERFORM_SUBTRACTION MACRO A, B,C1, DIM1, DIM2, R1, R2, C2
    LOCAL EXIT, EXIT1, NEW_LINE
    XOR AX, AX
    XOR BX, BX
    MOV AX, R1
    MOV BX, R2
    CMP AX, BX
    JNE EXIT1
    
    XOR AX, AX
    XOR BX, BX
    MOV AX, R1
    MOV BX, R2
    CMP AX, BX
    JNE EXIT1
    
    
    MOV SI,0   
    SUBTRACTION:
        MOV BX,A[SI]
        MOV CX,B[SI]
        SUB BX,CX
        MOV SUB_ANS[SI],BX
        ADD SI,2
        DEC AX
        CMP AX,0
        JNZ SUBTRACTION
       
    MOV CX,DIM1
    MOV SI,0
    MOV BX,0
    PRINTN ""
    PRINT_SUB:
        PRINT "   "
        MOV AX,SUB_ANS[SI]
        CALL PRINT_NUM
        INC BX
        CMP BX, C1
        JE NEW_LINE
        ADD SI,2
        LOOP PRINT_SUB
        NEW_LINE:
            PRINTN ""
            MOV BX,0
            ADD SI, 2
        LOOP PRINT_SUB
    JMP EXIT     
    EXIT1:
        PRINTN 'SUBTRACTION NOT POSSIBLE !'
    EXIT:
ENDM

;--------------------------------------------------------------------
;Macro for transpose
FIND_TRANSPOSE MACRO MATRIX, M, N, DIM
    LOCAL LAB, LAB1, NEW_LINE, TRANSPOSE, PRINT_TRANSPOSE
    MOV SI,0
    MOV BP,0
    MOV DI,0               ;points to transpose matrix            
    MOV AX,N
    MOV BX,2
    MUL BX
    MOV SP,AX
    MOV BX,N
    ADD BX,1                ;bx no of times lab runs 
                             
    LAB:
        MOV CX,M          ;cx times transpose will run each time
        MOV SI,BP
        ADD BP,2
        DEC BX   
        CMP BX,0
        JNZ TRANSPOSE
        JZ LAB1
        TRANSPOSE:
            MOV AX,MATRIX[SI]
            MOV TRANSPOSE_ANS[DI],AX
            ADD DI,2
            ADD SI,SP
            DEC CX 
            CMP CX,0
            JNZ TRANSPOSE
            JZ LAB
             
    LAB1:        
        MOV SI,0
        MOV CX,DIM        
          
    MOV CX,DIM
    MOV SI,0
    MOV BX,0
    PRINT_TRANSPOSE:
        PRINT "   "    
        MOV AX,TRANSPOSE_ANS[SI]
        CALL PRINT_NUM
        INC BX
        CMP BX,M
        JE NEW_LINE
        ADD SI,2
        LOOP PRINT_TRANSPOSE
        NEW_LINE:
            PRINTN ""
            MOV BX, 0
            ADD SI, 2
            LOOP PRINT_TRANSPOSE
ENDM

;--------------------------------------------------------------------
;Macro for trace
FIND_TRACE MACRO MATRIX, M, N, DIM
    LOCAL EXIT_TRACE, EXIT, TRACE 
    MOV SI,0     ;WHICH POINTS TO THE MATRIX
    MOV AX, M
    MOV BX, N
    CMP AX, BX
    JNE EXIT_TRACE
    MOV CX,N
    MOV BX,CX    ; ADD WITH SI
    ADD BX,CX
    ADD BX,2
    MOV DX,0
   
    TRACE:
        MOV AX,MATRIX[SI]
        ADD AX,DX
        MOV DX,AX
        ADD SI,BX
        LOOP TRACE
        
    MOV TRACE_ANS, AX   
    CALL PRINT_NUM
    JMP EXIT
    
    EXIT_TRACE:
        PRINTN 'Trace cannot be computed! '
    EXIT:
ENDM

;-------------------------------------------------------------------
;Macro for adjoint
FIND_ADJOINT MACRO MATRIX, M, N, DIM
    LOCAL 2X2, 3X3, PRINTADJ2, PRINTADJ3, EXIT_INVERSE, EXIT1_INVERSE, DIVIDE_BY_DET2, DIVIDE_BY_DET3
    XOR AX, AX
    XOR BX, BX
    XOR CX, CX
    MOV AX, M
    MOV BX, N
    MOV CX, DIM
    MOV M_NEW, AX
    MOV N_NEW, BX
    MOV DIM_NEW, CX
    XOR AX, AX
    XOR BX, BX
    XOR CX, CX
     
    MOV CX,DIM            
    CMP CX,9
    JE 3X3
    JNE 2X2
    
    2X2:
        CMP CX, 4
        JNE EXIT_INVERSE
             
        MOV AX,MATRIX[0]
        MOV BX,MATRIX[6]
        MOV ADJOINT_ANS[0],BX
        MOV ADJOINT_ANS[6],AX
        MOV AX,MATRIX[2]
        NEG AX
        MOV BX,MATRIX[4]
        NEG BX
        MOV ADJOINT_ANS[2],AX
        MOV ADJOINT_ANS[4],BX
        
        ;FIND_TRANSPOSE ADJOINT_ANS, M_NEW, N_NEW, DIM_NEW 
        MOV BX, DET_ANS
        MOV CX, 4
        MOV SI, 0
        DIVIDE_BY_DET2:
            MOV AX, TRANSPOSE_ANS[SI]
            DIV BX
            MOV INVERSE_ANS[SI], AX
            ADD SI, 2
            LOOP DIVIDE_BY_DET2
         
    MOV CX,DIM
    MOV SI,0
    PRINT "INVERSE OF THE  MATRIX IS : "
    PRINTADJ2:
        PRINT "   "
        MOV AX,INVERSE_ANS[SI]
        CALL PRINT_NUM
        ADD SI,2
        LOOP PRINTADJ2 
    mov det_ans, 0
    jmp exit_inverse
        JMP EXIT1_INVERSE
    
    3X3:
        XOR CX,CX
        MOV AX,MATRIX[8]
        MOV BX,MATRIX[16]
        IMUL BX
        MOV CX,AX
        MOV AX,MATRIX[10]        ;0
        MOV BX,MATRIX[14]
        IMUL BX
        SUB CX,AX                                                           
        MOV ADJOINT_ANS[0],CX
        
        XOR CX,CX
        MOV AX,MATRIX[6]
        MOV BX,MATRIX[16]
        IMUL BX
        MOV CX,AX              ;2
        MOV AX,MATRIX[10]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        NEG CX
        MOV ADJOINT_ANS[2],CX
        
        XOR CX,CX
        MOV AX,MATRIX[6]
        MOV BX,MATRIX[14]
        IMUL BX                 ;4
        MOV CX,BX
        MOV AX,MATRIX[8]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        MOV ADJOINT_ANS[4],CX
        
        XOR CX,CX
        MOV AX,MATRIX[2]
        MOV BX,MATRIX[16]
        IMUL BX
        MOV CX,AX              ;6
        MOV AX,MATRIX[4]
        MOV BX,MATRIX[14]
        IMUL BX
        SUB CX,AX
        NEG CX
        MOV ADJOINT_ANS[6],CX
        
        XOR CX,CX
        MOV AX,MATRIX[0]
        MOV BX,MATRIX[16]
        IMUL BX                 ;8
        MOV CX,BX
        MOV AX,MATRIX[4]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        MOV ADJOINT_ANS[8],CX
        
        
        XOR CX,CX
        MOV AX,MATRIX[0]
        MOV BX,MATRIX[14]
        IMUL BX
        MOV CX,AX              ;10
        MOV AX,MATRIX[2]
        MOV BX,MATRIX[12]
        IMUL BX
        SUB CX,AX
        NEG CX
        MOV ADJOINT_ANS[10],CX
        
        
        XOR CX,CX
        MOV AX,MATRIX[2]
        MOV BX,MATRIX[10]
        IMUL BX                 ;12
        MOV CX,BX
        MOV AX,MATRIX[4]
        MOV BX,MATRIX[8]
        IMUL BX
        SUB CX,AX
        MOV ADJOINT_ANS[12],CX
        
        XOR CX,CX
        MOV AX,MATRIX[0]
        MOV BX,MATRIX[10]
        IMUL BX
        MOV CX,AX              ;14
        MOV AX,MATRIX[4]
        MOV BX,MATRIX[6]
        IMUL BX
        SUB CX,AX
        NEG CX
        MOV ADJOINT_ANS[14],CX
        
        
        XOR CX,CX
        MOV AX,MATRIX[0]
        MOV BX,MATRIX[8]
        IMUL BX                 ;16
        MOV CX,BX
        MOV AX,MATRIX[2]
        MOV BX,MATRIX[6]
        IMUL BX
        SUB CX,AX
        MOV ADJOINT_ANS[16],CX

        FIND_TRANSPOSE ADJOINT_ANS, M_NEW, N_NEW, DIM_NEW
        MOV BX, DET_ANS
        MOV CX, 9
        MOV SI, 0
        DIVIDE_BY_DET3:
            MOV AX, TRANSPOSE_ANS[SI]
            DIV BX
            MOV INVERSE_ANS[SI], AX
            ADD SI, 2
            LOOP DIVIDE_BY_DET3 
           
        MOV CX,DIM
        MOV SI,0
        PRINT "ADJOINT OF THE  MATRIX IS : "
        PRINTADJ3:
            PRINT "   "
            MOV AX,INVERSE_ANS[SI]
            CALL PRINT_NUM
            ADD SI,2
            LOOP PRINTADJ3
       JMP EXIT1_INVERSE
       EXIT_INVERSE:
            PRINTN 'INVERSE CANNOT BE COMPUTED'
            JMP EXIT1_INVERSE
       EXIT1_INVERSE:
            ENDM
 
       
;=====================================================================
;-----------------------DRIVER CODE-----------------------------------
;-------TO IMPLEMENT OPERATIONS DEFINED IN THE HEADER FILE--------------

    printn 'IMPLEMENTATION OF MATRIX OPERATIONS'
    printn '-----------------------------------'
    printn ''
    printn 'LIST OF OPERATIONS AVAILABLE'
    printn '1. Addition'
    printn '2. Subtraction'
    printn '3. Multiplication'
    printn '4. Inverse'
    printn '5. Transpose'
    printn '6. Determinant'
    printn '7. Trace'
    printn ''
    
    ;clearing all previously stored data in registers
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    xor si, si
    xor di, di
            
   
;-----------------getting matrix as input-----------------------------   
   operate:
        ;dimensions of matrixA
        call clear_screen
        printn 'Enter number of rows of matrixA: '
        call scan_num
        mov r1, cx
        printn 'Enter number of columns of matrixA: '
        call scan_num
        mov c1, cx
        mov ax, r1
        mov bx, c1
        mul bx
        mov dim1, ax
        ;getting elements of matrixA
        printn 'Enter the elements of matrixA: '
        mov ax, dim1
        mov si, 0
        get_input1:
            call scan_num
            mov a[si], cx
            add si, 2
            dec ax
            cmp ax, 0
            jg get_input1
        
        ;dimensions of matrixB
        call clear_screen
        printn 'Enter number of rows of matrixB: '
        call scan_num
        mov r2, cx
        printn 'Enter number of columns of matrixB: '
        call scan_num
        mov c2, cx
        mov ax, r2
        mov bx, c2
        mul bx
        mov dim2, ax
        ;getting elements of matrixB
        printn 'Enter the elements of matrixB: '
        mov ax, dim2
        mov si, 0
        get_input2:
            call scan_num
            mov b[si], cx
            add si, 2
            dec ax
            cmp ax, 0
            jg get_input2 

;--------------------types of matrix-------------------------------    
    call clear_screen
    printn 'Type of Matrix A: '
    check_type a, r1, c1, dim1
    printn ''
    printn 'Type of Matrix B: '   
    check_type b, r2, c2, dim2

;--------------------determinant-----------------------------------       
    call clear_screen
    printn 'DETERMINANT AND SINGULAR MATRIX FOR MATRIX A:' 
    find_determinant a, r1, c1, dim1
    printn ''
    printn 'DETERMINANT AND SINGULAR MATRIX FOR MATRIX B:' 
    find_determinant b, r2, c2, dim2

;------------------------transpose---------------------------------
    call clear_screen
    printn 'TRANSPOSE FOR MATRIX A:' 
    find_transpose a, r1, c1, dim1
    printn ''
    printn 'TRANSPOSE FOR MATRIX B:' 
    find_transpose b, r2, c2, dim1
        
;------------------------addition-----------------------------------
    call clear_screen
    print 'ADDITION OF THE TWO MATRICES: '
    perform_addition a, b, c1, dim1, dim2, r1, r2, c2  
    
;------------------------subtraction-----------------------------------
    call clear_screen
    print 'SUBTRACTION OF THE TWO MATRICES: '
    perform_subtraction a, b, c1, dim1, dim2, r1, r2, c2   
    
;--------------------trace-----------------------------------       
    call clear_screen
    printn 'TRACE OF MATRIX A:' 
    find_trace a, r1, c1, dim1
    printn ''
    printn 'TRACE OF MATRIX B:' 
    find_trace b, r2, c2, dim2    

    
;--------------------inverse-----------------------------------
    call clear_screen
    printn 'INVERSE FOR MATRICES: '
    printn ''
    printn 'INVERSE FOR MATRIX A:'
    find_determinant a, r1, c1, dim1
    find_adjoint a, r1, c1, dim1
    call clear_screen
    printn 'INVERSE FOR MATRIX B:'
    find_determinant b, r2, c2, dim2
    find_adjoint b, r2, c2, dim2
    
;--------------------multiplication---------------------------
     
;-----------------thank you--------------------------------------    
    call clear_screen
    printn 'THANK YOU !'
    printn ''
    printn ''
    xor dx, dx
    xor cx, cx
    printn 'Do you want to continue? '
    printn '1. Yes        2.No'
    call scan_num
    mov dx, cx
    cmp dx, 2
    je exit_final
    jmp operate
    
    exit_final:
        ret   
ret


           
;===============================================================

define_scan_num
define_clear_screen 
define_print_num 
define_print_num_uns
;VARIABLES TAKEN AS INPUT
r1 dw ?
c1 dw ?
dim1 dw ?
r2 dw ?
c2 dw ?  
dim2 dw ?
m_new dw ?
n_new dw ?
dim_new dw ?
;MATRICES TAKEN AS INPUT
a dw 20 dup(?)
b dw 20 dup(?)
;VARIABLES FOR OUTPUT
det_ans dw ?
trace_ans dw ?
;MATRICES FOR STORING RESULT
add_ans dw 20 dup(?)
sub_ans dw 20 dup(?)
transpose_ans dw 20 dup(?)
adjoint_ans dw 20 dup(?)
inverse_ans dw 20 dup(?)
temp dw 3 dup(?)

end