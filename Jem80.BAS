#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf

screen 12

Dim Shared Parar as Long    ' lo empleo para depuracion, para que se pare 
Dim Shared DirParar as Long ' aqui va la direccion a atrapar ("TRAP")
Dim Shared Contador as long    ' lo empleo como reloj temporal, por ahora

Declare Sub leeteclado()
Declare Sub LeeRaton()
Dim Shared As Long xraton, xraton2, yraton, yraton2, xratond, yratond, braton
Dim Shared As UInteger TeclaPulsada

Dim Shared deb as integer =0 ' 1 o 2 para debugar (1 normal, 2 con peek-poke) 

DirParar=&H25fd '&h1ab8 ' direccion donde empezar a depurar
Parar=0     ' un contador para la depuracion
' rutinas a ejecutar en cada interrupcion del Z80
declare Sub DoEvents()

' rutinas de intercambio de bancos de ROM y RAM
declare sub copiarom (zona_ram as long, banco as long) 
declare sub copiaram (zona_ram as long, banco as long, puerta as long)
Dim Shared rom(&h100000) as long ' espacio de 1 mega de ROM (64 páginas de 16k)
Dim Shared mem(&h2000*4) as long ' espacio para 4 páginas de 2k cada una
Dim Shared ram(65535   ) as long ' RAM completa de un Z80 (64k)

' direccion de RAM a visualizar en la depuracion
Dim shared dir_ram as long
dir_ram=&hFF20

' Puertos Z80 (los ponemos a cero) (creo, que el valor correcto es FF, equivalente a "0")
Dim shared puerto(65536) as Long
Dim shared var_a as Long 
for var_a=0 to 65535:puerto(var_a)=&hff: next
var_a=0

' solo debug, para guardar los puertos leidos/escritos y luego sacarlos a un fichero
Dim shared inbut(65536) as Long
Dim shared outbut(65536) as Long

'Private Function interruptTriggered(tstates As Long) As Long
'    interruptTriggered = (tstates >= 0)
'End Function

' pruebas, para generar una irq "falsa"
Dim Shared irq_counter As Long =0
Dim Shared sig_ins As Long =0

' ///// incluimos el Emulador Z80 principal /////
#include "Z80.bas"

' *************************************************************
'                    lectura de puertos 
' *************************************************************
Function inb(port As Long) As Long
   
    port=port and &hff
    

    inb = puerto(port) 'and &HFF&
    'inb = puerto(port)
    inbut(port)+=1

End Function

' *************************************************************
'                    escritura de puertos
' *************************************************************
Sub outb(port As Long, outbyte As Long)
  
  port = port And &hff
  outbyte = outbyte and &hff

  select case port
  	
  	' escritura RATON
  	'Case &H04
  	'    outbyte = &h80 	    
  	'Case &H05
  	'    outbyte = &h80
  	    
  	' escritura teclado
  	'Case &H40
  	    'OutByte=&hFF
  	'    'puerto(port) = outbyte	

  	case &h50
  		' zona ROM 0: 0x0000-0x3FFF
       copiarom &h0000,outbyte And &h3f
       'puerto(port) = outbyte
  	case &h51
  		' zona ROM 1: 0x4000-0x7FFF
       copiarom &h4000,outbyte And &h3f
       'puerto(port) = outbyte
  	case &h52
  		' zona ROM 2: 0x8000-0xBFFF
       copiarom &h8000,outbyte And &h3f
       'puerto(port) = outbyte
  	case &h53
  		' zona RAM-A: 0xC000-0xDFFF
       copiaram &hC000,outbyte And &h03 , port and &hff
       'puerto(port) = outbyte
  	case &h54
  		' zona RAM-B: 0xE000-0xFFFF
       copiaram &hE000,outbyte And &h03 , port and &hff
       'puerto(port) = outbyte
  	case &h55
       'DirParar=&h4F45 '&h4F45 TRAMPEAMOS CON &H3C (62=LD A,xx) PARA SEGUIR
       'copiaram &h4000,2, &hff
       'puerto(port) = outbyte
  	case &h56
       'DirParar=&h4F45 '&h4F45 TRAMPEAMOS CON &H3C (62=LD A,xx) PARA SEGUIR
       'copiaram &h4000,2, &hff
       'puerto(port) = outbyte
       
  end select

    puerto(port)=outbyte
    outbut(port)+=1

End Sub

' *************************************************************
'      lee el raton
' *************************************************************
Sub LeeRaton()
    ' cogemos el estado del raton
    GetMouse (xraton, yraton, , braton)
    ' sacamos la distancia entre dos lecturas
    If xraton2<>xraton Then xratond=xraton2-xraton:xraton2=xraton Else xratond=0
    If yraton2<>yraton Then yratond=yraton2-yraton:yraton2=yraton Else yratond=0
    'Locate 27,40:Print xratond;yratond;"   "
    
         puerto(&h04)=&h80-xratond
     		puerto(&h05)=&h80-yratond
     		
End Sub

' *************************************************************
'      miramos el teclado, y actuamos en consecuencia
' *************************************************************
Sub leeteclado()
	 
	        Dim tecla As UInteger = 0

    				' Fila de teclas: LINE0
					'If Multikey ( SC_BUTTON1 ) Then Tecla = &h01 ' BOTON 1 DEL RATON
					If Multikey ( SC_1 ) Then Tecla = &h02
					If Multikey ( SC_9 ) Then Tecla = &h04
					If Multikey ( SC_E ) Then Tecla = &h08
					If Multikey ( SC_LEFTBRACKET ) Then Tecla = &h10
					If Multikey ( SC_G ) Then Tecla = &h20
					If Multikey ( SC_LSHIFT ) Then Tecla = &h40
					If Multikey ( SC_COMMA ) Then Tecla = &h80
					
					' Fila de teclas: LINE1
					'If Multikey ( SC_BUTTON2 ) Then Tecla = &h01 ' BOTON 2 DEL RATON
					If Multikey ( SC_2 ) Then Tecla = &h02
					If Multikey ( SC_0 ) Then Tecla = &h04
					If Multikey ( SC_R ) Then Tecla = &h08
					If Multikey ( SC_PLUS ) Then Tecla = &h10
					If Multikey ( SC_H ) Then Tecla = &h20
					If Multikey ( SC_Z ) Then Tecla = &h40
					If Multikey ( SC_PERIOD ) Then Tecla = &h80
					
					' Fila de teclas: LINE2
					'If Multikey ( SC_8_PAD ) Then Tecla = &h01
					If Multikey ( SC_3 ) Then Tecla = &h02
					If Multikey ( SC_QUOTE ) Then Tecla = &h04
					If Multikey ( SC_T ) Then Tecla = &h08
					If Multikey ( SC_HOME ) Then Tecla = &h10
					If Multikey ( SC_J ) Then Tecla = &h20
					If Multikey ( SC_X ) Then Tecla = &h40
					If Multikey ( SC_MINUS ) Then Tecla = &h80
					
					' Fila de teclas: LINE3
					'If Multikey ( SC_4_PAD ) Then Tecla = &h01
					If Multikey ( SC_4 ) Then Tecla = &h02
					If Multikey ( SC_RIGHTBRACKET ) Then Tecla = &h04
					If Multikey ( SC_Y ) Then Tecla = &h08
					If Multikey ( SC_CAPSLOCK ) Then Tecla = &h10
					If Multikey ( SC_K ) Then Tecla = &h20
					If Multikey ( SC_C ) Then Tecla = &h40
					If Multikey ( SC_UP ) Then Tecla = &h80  ' cursor arriba
					
					' Fila de teclas: LINE4
					'If Multikey ( SC_6_PAD ) Then Tecla = &h01
					If Multikey ( SC_5 ) Then Tecla = &h02
					If Multikey ( SC_BACKSPACE ) Then Tecla = &h04
					If Multikey ( SC_U ) Then Tecla = &h08
					If Multikey ( SC_A ) Then Tecla = &h10
					If Multikey ( SC_L ) Then Tecla = &h20
					If Multikey ( SC_V ) Then Tecla = &h40
					If Multikey ( SC_RSHIFT ) Then Tecla = &h80
					
					' Fila de teclas: LINE5
					'If Multikey ( SC_2_PAD ) Then Tecla = &h01
					If Multikey ( SC_6 ) Then Tecla = &h02
					If Multikey ( SC_ESCAPE ) Then Tecla = &h04
					If Multikey ( SC_I ) Then Tecla = &h08
					If Multikey ( SC_S ) Then Tecla = &h10
					If Multikey ( SC_BACKSLASH ) Then Tecla = &h20
					If Multikey ( SC_B ) Then Tecla = &h40
					If Multikey ( SC_PAGEUP ) Then Tecla = &h80
					
					' Fila de teclas: LINE6
					If Multikey ( SC_F10 ) Then Tecla = &h01
					If Multikey ( SC_7 ) Then Tecla = &h02
					If Multikey ( SC_Q ) Then Tecla = &h04
					If Multikey ( SC_O ) Then Tecla = &h08
					If Multikey ( SC_D ) Then Tecla = &h10
					If Multikey ( SC_SLASH ) Then Tecla = &h20
					If Multikey ( SC_N ) Then Tecla = &h40
					If Multikey ( SC_PAGEDOWN ) Then Tecla = &h80
					
					' Fila de teclas: LINE7
					If Multikey ( SC_F9 )    Then Tecla = &h01
					If Multikey ( SC_8 )     Then Tecla = &h02
					If Multikey ( SC_W )     Then Tecla = &h04
					If Multikey ( SC_P )     Then Tecla = &h08
					If Multikey ( SC_F )     Then Tecla = &h10
					If Multikey ( SC_ENTER ) Then Tecla = &h20
					If Multikey ( SC_M )     Then Tecla = &h40
					If Multikey ( SC_END )   Then Tecla = &h80
					
					' Fila de teclas: LINE8
					If Multikey ( SC_SPACE ) Then Tecla = &h0100
					If Multikey ( SC_F1 )    Then Tecla = &h0200
					If Multikey ( SC_F2 )    Then Tecla = &h0400
					
					' Fila de teclas: LINE9
					If Multikey ( SC_ALT ) Then Tecla = &h0100' ALT IZQUIERDO
					If Multikey ( SC_F3 )  Then Tecla = &h0200
					If Multikey ( SC_F4 )  Then Tecla = &h0400
					
					' Fila de teclas: LINEA
					If Multikey ( SC_ALT ) Then Tecla = &h0100 ' ALT DERECHO
					If Multikey ( SC_F5 )  Then Tecla = &h0200
					If Multikey ( SC_F6 )  Then Tecla = &h0400
					
					' Fila de teclas: LINEB
					If Multikey ( SC_LEFT ) Then Tecla = &h0100 ' cursor izquierda
					If Multikey ( SC_F7 )   Then Tecla = &h0200
					If Multikey ( SC_F8 )   Then Tecla = &h0400
					
					' Fila de teclas: LINEC
					If Multikey ( SC_DOWN ) Then Tecla = &h0100 ' cursor abajo
					
					' Fila de teclas: LINED
					If Multikey ( SC_RIGHT ) Then Tecla = &h0100 ' cursor derecha
					
					' Fila de teclas: LINEE
					'If Multikey ( SC_0_PAD ) Then Tecla = &h01
					
					' Fila de teclas: LINEF
					'If Multikey ( SC_DEL_PAD ) Then Tecla = &h01
					
					' prueba (borrar si no vale)
					TeclaPulsada = &hffff Xor Tecla

         puerto(&h41)=HiByte (TeclaPulsada) 

         puerto(&h42)=LoByte (TeclaPulsada) 

End Sub

' *************************************************************
'      aqui se ejecutan todos los temas "hardware"
' *************************************************************
Sub DoEvents ()
    Dim ini_ram as long
    Dim f as Long
    Dim g as Long
    Dim x as Long
    Dim y as Long
    Dim h as string
    dim a as string
    dim m as Long 
    Dim hs As string
   
    leeteclado()
    LeeRaton()
 
If DEB Then 
	GoTo NODEBUG
EndIf

       ' ponemos 16 direcciones hex
        ini_ram=dir_ram
        for f=1 to 14
                locate f,1
                hs=Right("0000"+Hex(ini_ram),4)
                print hs
                ini_ram+=16
        next
        ' ponemos 16x16 datos hex
        ini_ram=dir_ram
        for f=1 to 14
            for g=1+6 to 48+6 step 3
                locate f,g
                hs=Right("00"+Hex(RAM(ini_ram)),2)
                print hs
                ini_ram+=1
            next
        next
        ' ponemos los mismo 16x16 datos, pero ascii
        ini_ram=dir_ram
        for f=1 to 14
            for g=65 to 80
                locate f,g
                m=RAM(ini_ram) and &hff:if m<32 then hs="." else hs=chr(m)
                print hs
                ini_ram+=1
            next
         next
         
        ' ponemos los 256 puertos
        ini_ram=0
        m=0:for f=16 to 16+(31*2) step 2:locate 15,f:print ltrim(RTrim(str(m)));:m=m+1:next
        for f=1 to 8
            for g=16 to 16+(31*2) step 2
                locate f+15,g
                m=puerto(ini_ram)
                hs=Right("00"+Hex(m),2)
                if ini_ram/2=ini_ram\2 then color 12,0 else color 7,0
                print hs;
                ini_ram+=1
            next
         next
         ' cuadricula alrededor de los puertos
         for f=119 to 640 step 16:line (f,222)-step(0,144):next
         for g=222 to 372 step 16:line (119,g)-step(512,0):next
         
         ' ahora registros del Z80
         locate 15,1:print "PC:";Right("0000"+Hex(regPC),4)
         locate 16,1:print "SP:";Right("0000"+Hex(regSP),4)
         locate 17,1:print "A :";Right("00"+Hex(regA),2)
         locate 18,1:print "BC:";right("00"+hex(regB),2)+Right("00"+Hex(regC),2) 
         locate 19,1:print "HL:";Right("0000"+Hex(regHL),4)  
         locate 20,1:print "DE:";Right("0000"+Hex(regDE),4)
         locate 21,1:print "IX:";Right("0000"+Hex(regIX),4)
         locate 22,1:print "IY:";Right("0000"+Hex(regIY),4)
         exx
         Locate 17,9:print "'";Right("00"+Hex(regA),2)
         Locate 18,9:print "'";right("00"+hex(regB),2)+Right("00"+Hex(regC),2)               
         Locate 19,9:print "'";Right("0000"+Hex(regHL),4)  
         Locate 20,9:print "'";right("0000"+Hex(regDE),4)
         exx

        if MultiKey(SC_CONTROL) And MultiKey(SC_PAGEUP)   Then dir_ram-=16
        if MultiKey(SC_CONTROL) And MultiKey(SC_PAGEDOWN) Then dir_ram+=16
        If MultiKey(SC_CONTROL) And MultiKey(SC_HOME)     Then dir_ram-=256
        if MultiKey(SC_CONTROL) And MultiKey(SC_END)      Then dir_ram+=256
        
        if dir_ram>&hfff0 then dir_ram=&hfff0
        if dir_ram<0 then dir_ram=0
        
        if MultiKey(SC_CONTROL) And MultiKey(SC_SPACE) then sleep ' pausa con el espacio

        ' tecla F2 de pruebas, que parece el boton del raton
        if MultiKey(sc_f2) then puerto(&h21)+=1

        ' tecla para que salte una pagina 
        if MultiKey(SC_F1) then 
        	   While MultiKey(SC_F1):Wend
            puerto(&h52)=11
            copiarom &h8000,11 
            regPC=&h8000+var_a
            locate 10,55:print "ROM:";Hex(&h8000+var_a)
            var_a=var_a+3
        end if

        ' salir y grabar datos
        if multikey(SC_ESCAPE) then 
          open "puertos.txt" for output as 2
            dim f as long
            print #2,"puerto","out","in","valor"
            for f=0 to 65535
              if outbut(f)+inbut(f)>0 then print #2,hex(f),outbut(f),inbut(f),hex(puerto(f))
            next
          close 1   
          
          Open "RAMDUMP.BIN" For Binary Access write As 1
            For f=0 To 65535
            	Put #1,f+1,RAM(f)
            Next
          Close 1
          stop
        end if


NODEBUG:
        
 ' ***************************************************************
 '              emulacion grafica en tiempo real       
 ' ***************************************************************
        ' ponemos la pantalla primaria (240x100 en la E000)
        x=0:y=380
        for f=&he000 to &he000+&hbb7
            m=RAM(f)
            for g=7 to 0 step -1
                if bit(m,g) then pset(x+(7-g),y),8 else pset(x+(7-g),y),10
            next
            x=x+8:if x>239 then x=0:y=y+1
        Next
   
   ' por ahora, no la dibujamos, ya que funciona bin ahora....
        ' lo mismo pero la secundaria en EBB8 (seguido de la primaria)
        x=250:y=380
        for f=&hebb8 to &hebb8+&hbb7
            m=RAM(f)
            for g=7 to 0 step -1
                if bit(m,g) then pset(x+(7-g),y),8 else pset(x+(7-g),y),10
            next
            x=x+8:if x>250+239 then x=250:y=y+1
        next
         
 end sub           
' *******************************************************************************
 
' ****************************************************************
'  rutinas de movimiento entre bancos, se activan con OUTB del Z80
' **************************************************************** 
Sub copiarom (zona_ram as long, banco as long)
    dim f as long
    'if banco>63 then locate 40,60:print "banco>64": banco = banco and 63
    for f=banco*&h4000 to (banco*&h4000)+&h3fff
        RAM(zona_ram And &hffff)=rom(f) And &hff
        zona_ram+=1
    next
end sub

Sub copiaram (zona_ram as long, banco as long, puerta as long)
    dim f as long
    dim zona2 as long
    dim banco2 as long

      banco2 = puerto(puerta And &hff) And &h03
      zona2 = zona_ram And &hffff

    ' primero dejamos en su sitio la actual
     for f=banco2*&h2000 to (banco2*&h2000)+&h1fff
         mem(f)=RAM(zona2) And &hff
         zona2+=1
     Next
     
    ' ahora ponemos la nueva
     for f=banco*&h2000 to (banco*&h2000)+&h1fff
         RAM(zona_ram And &hffff)=mem(f) And &hff
         zona_ram+=1
     next
   
end sub

'=============================================================
'************************* INICIO ****************************
'=============================================================
INICIO:
    Dim b as Long
    Dim f as Long
    Dim g as long
    Dim a as string
    Dim reg as long

Z80reset()

 Dim As String sa="                "
 reg=1
 Cls
 Print "Leyendo 1 mega de ROM"
 Open "PRESTIGE.ROM" for binary access read as 1
  while not eof(1)
   get #1,reg,sa
   for f=1 to len(sa)
      rom(reg-1+(f-1))=asc(mid(sa,f,1))
   next
   reg=reg+len(sa)
  wend
 close 1
 Print "Ejecutando Emulacion...."
 
    ' nos aseguramos que la ram es puesta a cero (los cuatro bancos)
     For f=0 to &HFFFF:RAM(f)=0:Next
     for f=0 to &h2000*4:mem(f)=0:Next

    ' copiamos el banco 0 de la rom de 1 mega, en la posicion 0 de la ram del Z80
     copiarom(&h0000,&h0)
     
     ' ponemos a "0" los puertos de bancos de memoria 
     For f=&h50 To &h56:puerto(f)=0:next
    
     ' contador de milisegundos que tarda entre instrucciones
     glInterruptTimer = Timer*1000
     ' maximos milisengundos antes de "frenar" al PC. bajar si va lento.
     glInterruptDelay =5
     ' velocidad real del dispositivo a emular. Aqui, uno de 4mhz aprox. (4000000mhz/50hz=80000)
     glTstatesPerInterrupt = 139888 '69888
     
     ' 17 seg. hasta dejar de girar el logo centra, 30seg. hasta que sale el idioma
     ' 28 y 45 en el mio: 15seg mas lento
     ' el real tarda 14 y 18

   cls
   Execute
   
End
