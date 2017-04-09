.equ JTAG, 0xff201000		#Address of the JTAG 
.equ ADDR_JP1, 0xff200060   # Address GPIO JP1
.equ TIMER, 0xff202000
.equ PERIOD, 65
.equ COUNTER, 1000000

# User input instructions
.equ PLAY,   0x073 				# "s" hex value in ASCII code used for Play instruction
.equ PAUSE,  0x070 				# "p" hex value in ASCII code used for Pause instruction
.equ REWIND, 0x072				# "r" hex value in ASCII code used for Rewind instruction

.section .text

.global _start
_start:

#*************Initialize JTAG to accept interrupts**************
    movia r8,JTAG
	
    movui r9,0b01   #enable interrupt instruction
    stwio r9,4(r8)	#activate interrupt write on the JTAG
    
#********************Enable interrupts*************************    
    wrctl ctl0,r9       #enable PEI

    movui r9,0x100	#enable IQR line 8 (JTAG)
    wrctl ctl3,r9   #enable ienable 
    
#*******Initialize Lego controller and motor-light timer************    
	movia  r10, ADDR_JP1
	movia  r9, 0x07f557ff 	# direction register for Lego controller      
	stwio  r9, 4(r10)		# set direction for motors to all output

	movi r9, 0xffffffff		#motor and light OFF at initial state
	stwio  r9, 0(r10)
	
	call INIT_TIMER
	
	movi r4, 0 				# state
	movi r5, 0				# true boolean
	movia r14, COUNTER

#*****endless continuous loop********
Loop:
	beq r4, r5, Loop     #nothing happens state
	
	#***** Pulse Width Modulation ******
	call CHECK_TIMER
	
	movi r13, 1					# timer returns r16 with time-out bit
	bne r13, r16, Loop			# if bit != 1 br Loop
	
	movia r9,TIMER				# else if bit ==1 set base address
	stwio r0,0(r9) 				# clear â€œtime-outâ€? bit
	
	subi r14, r14, 1
	ldw r9, 0(r10)				# load Lego Bits
	beq r14, r0, LIGHT_TIMER
	beq r4, r5, Loop     #nothing happens state
	xori	r9, r9, 0b0100		# turn on/off motor and light
	stwio  r9, 0(r10)			# send instruction
	br Loop	
	
LIGHT_TIMER:
	movia r14, COUNTER
	beq r4, r5, Loop     #nothing happens state
	xori	r9, r9, 0b0001		# turn on/off motor and light
	stwio  r9, 0(r10)			# send instruction
	br Loop	

#********Interrupt handling section**********
.section .exceptions, "ax"

myISR:
#*********Save registers that could be overwritten on the stack************
    addi sp, sp ,-20
    stwio et, 0(sp)		#save et     

    rdctl et, ctl1	
    stwio et, 4(sp)		#save ctl1

    stwio ea, 8(sp)		#save ea to return after the interrupt is done

	stwio ra, 12(sp)	#save ra in the case interrupt is called inside a call

	stwio r9, 16(sp)	#save ea to return after the interrupt is done
	
    call READ_POLL  	#check if JTAG input is valid

#*********Check user input instruction************
	movia r12,PLAY
    beq r11, r12, PLAY_HANDLING 		# Play instruction
    movia r12,PAUSE
    beq r11, r12, PAUSE_HANDLING 		# Pause instruction
    movia r12, REWIND				
    beq r11, r12, PAUSE_HANDLING		# Rewind instruction
    
    #if user press something else
	br INTERRUPT_EXIT

READ_POLL:

  ldwio r16, 0(r8)					#Load from the JTAG address saved in R8
  andi  r17, r16, 0x00008000 		# Mask other bits 
  beq   r17, r0, READ_POLL 			# If this is 0 (branch true), data is not valid
  andi  r11, r16, 0x00ff      		# Data read is now in r11

  ret

#*********** If PLAY ***********
PLAY_HANDLING:
	#Motor ON and motor timer restarted
	movia r9,TIMER			#set base address
	#stwio r0,0(r9) 				#clear â€œtime-outâ€? bit, just in case
	movi r11,0b0110 			#start timer motor, and continue
	stwio r11,4(r9) 			
	
	movi r12, 0b11110100       	# motor0 (lights on) (bit0=0) and motor1 off (bit2=1) , direction set to forward (bit1=0, bit3 = 0) 
	stwio	 r12, 0(r10)
	
	movi r4, 1					#set state 1("play")
	
	br INTERRUPT_EXIT
 
 #*********** If PAUSE ***********
PAUSE_HANDLING:
	#Motor OFF and motor timer stopped
	movia r9,TIMER			#set base address
	#stwio r0,0(r9) 				#clear â€œtime-outâ€? bit, just in case
	movi r11,0b1000 			#stop timer motor
	stwio r11,4(r9)
	
	movi	 r12, 0b11110101       # motor0 (bit0=1) and motor1 (bit2=1) off , don't care about direction 
	stwio	 r12, 0(r10)	
	
	movi r4, 0					#set state 0("pause")
	
	br INTERRUPT_EXIT

#*********** If REVERSE ***********
REWIND_HANDLING:
	#Motor ON and motor timer restarted
	movia r9,TIMER			#set base address
	#stwio r0,0(r9) 		#clear time-out bit, just in case
	movi r11,0b0110 		#start timer motor, and continue
	stwio r11,4(r9) 			
	
	movi r12, 0b11111000       	# motor0 (lights on bit 0 = 0) and motor1 off (bit2=1) , direction set to forward (bit1=0, bit3 = 1) 
	stwio	 r12, 0(r10)
	
	movi r4, 1			#set state ("reverse") same as play
	
	br PAUSE_HANDLING
	
INTERRUPT_EXIT:

# Recover saved registed before exiting interrupt
    wrctl ctl0, r0			#Disable PIE
    ldwio ea, 8(sp)         #Restore return address of the interrupt  
    
    ldwio et,4(sp)			    
    wrctl ctl1,et           #Restore status
    
    ldwio et,0(sp)			#Restore return address for calls in interrupt
    
	ldwio ra, 12(sp)		#Restore ra

	ldwio r9, 16(sp)		#Restore r9
	
    addi sp, sp, 16			#Restore stack pointer to normal

    #movia ea, Loop
		
    subi ea, ea, 4			#Modify return address to return to line above
    	
    eret					#Return to program

	
# ************** Timer Initialization **************

INIT_TIMER:
	movia r9,TIMER		#set base address
	movi r11,%hi(PERIOD) 		#set timer period
	stwio r11,12(r9)
	movi r11,%lo(PERIOD)
	stwio r11,8(r9)
	stwio r0,0(r9) 				#clear â€œtime-outâ€? bit, just in case
	movi r11,0b1000 			#timer initialized but not ON
	stwio r11,4(r9) 			
	ret


# ************** Check for time-out and reset timers **************
CHECK_TIMER:
	movia r9,TIMER				#set base address
    ldwio r16, 0(r9) 			#read status
    andi r16, r16, 0b01 		#get time-out bit
    # beq r0, r16, CHECK_TIMER_M 	#check for time out
    # stwio r0, 0(r9) 			#reset time-out
    ret

	
