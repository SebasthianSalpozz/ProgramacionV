#make_bin#
 

name "thermo"


TEMP_LOW     db ?
TEMP_HIGH    db ?
PORT_THERMO  equ 125
PORT_HEATER  equ 127
ERROR_FLAG   equ 1;
 

start:

    ; Read temperature from thermometer
    in al, PORT_THERMO
    
    ; Check for I/O error (assuming failed read sets Carry flag)
    jc error_handler
    
    ; Check for valid temperature range (0-127 is a common assumption)
    cmp al, 0
    jl error_handler
    cmp al, 127
    jg error_handler
    
    ; Compare temperature
    cmp al, TEMP_LOW
    jl  turnHeaterOn

    cmp al, TEMP_HIGH
    jle turnHeaterOff
    jmp turnHeaterOn  ; If temperature exceeds 80°, turn on heater

turnHeaterOn:
    mov al, 1
    out PORT_HEATER, al   ; Turn heater on
    jmp continue

 
turnHeaterOff:
    mov al, 0
    out PORT_HEATER, al   ; Turn heater off
 

continue:
    jmp start   ; Endless loop


error_handler:
    ; (Implement error handling logic here)
    mov al, ERROR_FLAG  ; Set error flag
    ; Add your specific error handling actions here (e.g., blinking LED, logging error)
    jmp continue  ; Continue (potentially with a visual indicator or logging the error)