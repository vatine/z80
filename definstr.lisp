(in-package "Z80-ASM")
;;; awk -F" '()" '/defmethod emit-instruction/ { print "(definstr", $NF }'
(definstr adc nil)
(definstr add nil)
(definstr and nil)
(definstr bit nil)
(definstr call t)
(definstr ccf nil)
(definstr cp nil)
(definstr cpd nil)
(definstr cpdr nil)
(definstr cpi nil)
(definstr cpir nil)
(definstr cpl nil)
(definstr daa nil)
(definstr dec nil)
(definstr di nil)
(definstr djnz nil)
(definstr ei nil)
(definstr ex nil)
(definstr exx nil)
(definstr halt nil)
(definstr im nil)
(definstr in nil)
(definstr ind nil)
(definstr indr nil)
(definstr ini nil)
(definstr inir nil)
(definstr inc nil)
(definstr jp t)
(definstr jr t)
(definstr ld nil)
(definstr ldd nil)
(definstr lddr nil)
(definstr ldi nil)
(definstr ldir nil)
(definstr neg nil)
(definstr nop nil)
(definstr or nil)
(definstr otdr nil)
(definstr otir nil)
(definstr out nil)
(definstr outd nil)
(definstr outi nil)
(definstr pop nil)
(definstr push nil)
(definstr res nil)
(definstr ret t)
(definstr reti nil)
(definstr retn nil)
(definstr rl nil)
(definstr rla nil)
(definstr rlc nil)
(definstr rlca nil)
(definstr rld nil)
(definstr rr nil)
(definstr rra nil)
(definstr rrc nil)
(definstr rrca nil)
(definstr rrd nil)
(definstr rst nil)
(definstr sdc nil)
(definstr scf nil)
(definstr set nil)
(definstr sla nil)
(definstr sra nil)
(definstr srl nil)
(definstr sub nil)
(definstr xor nil)