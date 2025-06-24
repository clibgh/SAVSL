
library ieee;
use     ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

library WORK;
use WORK.preprocessor_pkg.all; 
use WORK.precompiler_pkg.all;

--use WORK.SAVSL_IOmoduleComponent.all;

entity SAVSL is
  generic (
		 nbIO   : integer:=64;   -- Nb Of IOS
		 nbVIO  : integer := 32; -- Nb of Virtual IOs  (not presented to module interface )
		 scriptFile   : string := "test.asm";
		 DataStackSize: integer := 64; -- Nb 32-bit words in direct access data stack.
		 LogFile      : string := "SAVSL.log";
		 DebugFile    : string := "SAVSLDebug.log";
		 MinResolution: time    := 1 fs		
    ); 
 
  port (
   -- IO's
    IO : inout std_logic_vector(nbIO-1 downto 0);
    IO_driven : out std_logic_vector(nbIO-1 downto 0)
  );
end SAVSL;

architecture SimulationModel of SAVSL is 

constant SAVSLVersion : std_logic_vector(31 downto 0) := X"00010000";

-- This variable memorises the total number of strings 
shared variable nb_strings: natural := 0;
shared variable nb_progsteps: natural := 0;
shared variable nb_labels : natural := 0;

type LineArrayElement is array(natural range <>) of line;
-- Définir le type d'accès (pointeur) vers ce tableau de LINE
type LineArrayPointer is access LineArrayElement;
shared variable AllLines : LineArrayPointer := null;
shared variable AllLabels: LineArrayPointer := null;  
shared variable lastlabelidx : natural:= 1; -- Consider "main" is mandatory.

procedure add_label(
	name: in string;
	idx: out natural
	) is 
		variable i : natural := AllLabels'low;
		variable finished : boolean := false;
	begin
		while not finished loop
			if AllLabels(i)=null then
				AllLabels(i) := new String'(name);
				idx := i;
				finished := true;
			else
				if AllLabels(i)'length = name'length then
					if AllLabels(i).all = name then
						idx := i;
						finished := true;
					end if;
				end if;
			end if;
			i := i+1; -- Note: by construction impossible to overflow.
		end loop;
	end procedure add_label;

type inst_type is (
    i_none,
	i_NOP,
	i_LABEL,
	i_FGOTO,
	i_BGOTO,
	i_GOTO,
	i_CALL,
	i_BGCALL,
	i_FCALL,
	i_FBGCALL,
	i_BCALL,
	i_BBGCALL,
	i_WTTIME,
	i_WTTIMEOUT,
	i_RET,
	i_LDBIT,
	i_LD,
	i_KILL,
	i_SWAP,
	i_ADD,
	i_SUB,
	i_ADDC,
	i_SUBB,
	i_CMP,
	i_CMPSTATE,
	i_SCMPSTATE,
	i_JEQ,
	i_JGT,
	i_JLT,
    i_FJEQ,
    i_FJGT,
    i_FJLT,
    i_BJEQ,
    i_BJGT,
    i_BJLT,
    i_JNE,
    i_FJNE,
    i_BJNE,
    i_JNGT,
    i_JNLT,
    i_FJNGT,
    i_FJNLT,
    i_BJNGT,
    i_BJNLT,
    i_AJNGT,
    i_AJNLT,
    i_FMUL,
    i_FDIV,
    i_FADD,
    i_FSUB,
    i_FNOT,
    i_FCOMP,
    i_I2F,
    i_F2I,
    i_F2F,
    i_FINT,
    i_FFRAC,
    i_FABS,
    i_FSIGN,
    i_FROLL,
    i_FROLLD,
    i_FDUP,
    i_FDROP,
    i_FSTACK,
    i_IADD,
    i_ISUB,
    i_IMUL,
    i_IDIV,
    i_IMOD,
    i_SADD,
    i_SSUB,
    i_SMUL,
    i_SDIV,
    i_SABS,
    i_INOT,
    i_ICOMP,
    i_SCOMP,
    i_ISHR,            -- Long Integer Shift Right by b bits.
    i_ISHL,            -- Long Integer Shift Left  by b bits.
    i_IDROP,           -- = DROP b long integers from stack
    i_IROLL,           -- = Roll of b long integers
    i_IDUP,    	     -- = DUP of b long integers
    i_IROLLD,          -- = RollD of b long integers
    i_ISTACK,          -- = STACK b long integers (value:0 -> 2 words: 0 + 1 )
    i_IEXT,            -- = Extend last integer to b bits (unsigned) (fill 0 on the left)
    i_SEXT,            -- = Extend last integer to b bits (sign extension)
    i_DUP,             -- DUP b : push again the b first elements of the data stack
    i_ROLL,            -- Roll b element. = moves  T(b-1) at T0 and shift up the intermediate T(x)
    i_ROLLD,           -- Invert of ROLL: POP T0 and insert it at as b'th element
    i_CLR,             -- Reg a = 0
    i_CLRLINE,         -- resets LINE
    i_LINE2M,          -- copy maximum b chars from LINE to DPTR pointed zone. If b is 0, it means no limit. Note: DPTR unchanged after this. 
    i_M2LINE,          -- copy from DPTR pointed zone a string to the line. ( strings are Null terminated). Note:DPTR unchanged after this.  
    i_AGOTO,           -- ; Goto Absolute 
    i_ACALL,           -- Call at label
    i_ABGCALL,         -- Call at label in separate proc.
    i_AJEQ,            -- jump if a=b
    i_AJGT,            -- jump if a>b
    i_AJLT,            -- jump if a<b
    i_AJNE,            -- jump if a /= b
    i_APUSH,           -- PUSH address of Label
    i_AFPUSH,          -- PUSH Forward label
    i_ABPUSH,          -- PUSH Backward label
    i_PUSHAT,          -- PUSH Indirect address to given param. (Ok for FIFO and FIFO control, V,W )
    i_SHR,             --    a=a SHR b  Carry = a(0)
    i_SHL,             --    a=a SHL b  Carry = a(31)
    i_SHRC,            --    a=a SHR 1  a(31)=Carry; Carry = a(0)
    i_SHLC,            --    a=a SHL 1  a(0)=Carry;  Carry = a(31)
    i_AND,             --    a=a AND b
    i_OR,              --    a=a OR b
    i_XOR,             --    a=a XOR b
    i_NOT,             --   a=not a
    i_MOD,             --   a=a MOD b
    i_MUL,             --   a=a * b
    i_DIV,             --   a=a / b
    i_MIN,             --   a= min(a,b)
    i_MAX,             --   a= max(a,b)
    i_PUSH,            --   Push in data stack
    i_LPUSH,           --   Push in Local data stack ( where only push and pops are possible)
    i_RPUSH,           --   Push in Response data stack ( where only push and pops are possible)
    i_POP,             --   Pop from data stack
    i_LPOP,            --   Pop from local data stack ( where only push and pops are possible)
    i_RPOP,            --   Pop from Response data stack ( where only push and pops are possible)
    i_DROP,            --   Removes b elts from data stack
    i_STACK,           --   Add b elts to data stack ( values=0 )
    i_DEBUGON,	       --   Enables 1fs pause after each instruction for this processor
    i_DEBUGOFF,        --   Disables 1fs pause after each instruction
    i_DEBUGONLOG,      --  Enables writing to debug log file from now. 
    i_DEBUGOFFLOG,     --   Disables writing to debug log file.
    i_WTRISE,          --   wait for rise on selected (one bit) 				     --   -- WTRISE b             : translated by asm to : PUSH START;PUSH SIZE;LD START,b;LD SIZE,#1;WTRISE P;POP SIZE;POP START
    i_WTFALL,          --   wait for fall on selected (one bit)                    --   -- WTFALL b             : translated by asm to : PUSH START;PUSH SIZE;LD START,b;LD SIZE,#1;WTFALL P;POP SIZE;POP START
    i_WTSTATE,         --   wait until a=b (b captured, wait on a)
                     --  for a: No Fifo Pop possible....
			         --    possible value for a:
				     --        Wxx,Vxx,P,Z,Sxx,Axx,Lxx,Cxx,Txx,DPTR, M(mem,W,V,S,A) , FWMxx,FRMxx
                     --   -- WTSTATE #ionumber,b   : translated by asm to : PUSH START;PUSH SIZE;LD START,#ionumber;LD SIZE,#1;WTSTATE P,b;POP SIZE;POP START
    i_WTGT,            --    wait until a>b (b captured, wait on a)
                     --  for a: No Fifo Pop possible....
			         --    possible value for a:
				     --     Wxx,Vxx,P,Z,Sxx,Axx,Lxx,Cxx,Txx,DPTR, M(mem,W,V,S,A) , FWMxx,FRMxx
    i_WTLT,            --    wait until a<b (b captured, wait on a)
                     --  for a: No Fifo Pop possible....
			         --     possible value for a:
				     --                                       Wxx,Vxx,P,Z,Sxx,Axx,Lxx,Cxx,Txx,DPTR, M(mem,W,V,S,A) , FWMxx,FRMxx
    i_WTEVENT,         --  wait any event on selected reg/Port
			         --  possible values for b:
			         --   Wxx,Vxx,P,Z,Sxx,Axx,Lxx,Cxx,Txx,DPTR, M(mem,W,V,S,A) , FWMxx, FRMxx
                     --  -- WTEVENT #ionumber   : translated by asm to : PUSH START;PUSH SIZE;LD START,#ionumber;LD SIZE,#1;WTEVENT P;POP SIZE;POP START
					 -- 
    i_STOP,            --  --:   2F 00 00 00  00 00 00 00
    i_BREAK,           --  --:   2F 00 00 00  00 00 00 01
					 -- 
                     --  for files: note X00 and XFF are  not to be opened or closed
                     --  X00 is the stdout 
                     --  XFF targets the tempory line buffer.
                     --  The content of the line is written out if you use FPRINT Xxx,""
                     --  (asm will change LINE to XFF for first arg and to "" for second arg)
    i_FOPEN,           --  Open File: m=mode: R,W,A for  0 = READ 1=WRITE 2=APPEND
    i_FCLOSE,          --  
                     -- -- if XFF -> write to the current line
                     -- -- if XFE -> write to the current selected file (via FILESELECTOR) . Synonym: XSELECTED
    i_FPRINT,          --  ss = size of string following instruction bb= FILE id
    i_LOG,             --  ss = size of string following instruction bb= FILE id
    i_HFPRINT,         -- Print in Hex the value of argument.
    i_DFPRINT,         -- Print in DEC the value of argument.
    i_BFPRINT,         -- Print in Binary the value of argument.
    i_H8FPRINT,        -- Print in Hex the value of argument(7..0).
    i_H16FPRINT,       -- Print in Hex the value of argument(15..0).
    i_B8FPRINT,        -- Print in Binary the value of argument(7..0).
    i_B16FPRINT,       -- Print in Binary the value of argument(15..0).
    i_TFPRINT,         -- Print the 'now' in the output file and/or LINE
    i_CFPRINT,         -- Print the asci character which value is in b. (only 8 lsbs of b are taken into account )
    i_HFREAD,          -- Read in HEX the value from file. Store to selected argument( like LD )
    i_DFREAD,          -- Read in decimal the value from file
    i_BFREAD,          -- Read in binary the value from file
    i_CFREAD,          -- Read ONE ASCI char and put it as LSB of destination reg.
    i_SWITCHIOSEL,     -- Switch between current and saved IO selection.
    i_EQ,		         -- Bypass next instr if flag not set.
    i_NE,		         -- Bypass next instr if flag not set.
    i_GT,		         -- Bypass next instr if flag not set.
    i_LT,		         -- Bypass next instr if flag not set.
    i_NGT,	         -- Bypass next instr if flag not set.
    i_NLT,	         -- Bypass next instr if flag not set.
    i_IOSEL2M,         -- Push SIZE+START+START....+0xFFFFFFFF to M, if not fifo, inc DPTR after push
    i_M2IOSEL,         -- Get from M an IO selection. Stops when read 0xFFFFFFFF
    i_FIFOLD           -- Duplicate Fifo index @BITSRC to @BITDST
	);
type param_type is (
    p_none,
	p_Wxx,
	p_Vxx,
	p_P,     -- Port 
	p_Z,     -- Tristate 
	p_Fxx,   -- Fifo push/pop 
	p_Sxx,   -- Fifo Used Space
	p_Axx,   -- Fifo Available 
	p_Cst, 
	p_Lxx,   -- Local variable 
	p_Cxx,   -- Caller Local variable 
	p_Txx,   -- Stack elts
	p_Start,
	p_Size, 
	p_X,     -- Force X
	p_PU,    -- PullUp
	p_PD,    -- PullDown
	p_R,     -- Random  ( Write = Set Seed )
	p_CDPTR,
	p_ID,
	p_UNIT,
	p_LASTEVENT,
	p_FIO,
	p_CKControl, -- Value:0=T0,1=T1,2=T2, 3=IdleState ,4=TrigSource, 5=TrigEn, 6=Enable 
	p_All_X,
	p_All_U,
	p_U,
	p_CID,
	p_BGID,
	p_DEBUGMODE,   -- values 0=OFF, 1:Inst=1fs instead of deltas, 2=Log to debug, 3 = Log to debug, including subprocesses
	p_PARAM, --  regarding value, select a RO parameter: -- 0=NBIOS 1=NBVIOS 2=STACKSIZE 3=DSTACKSIZE 4=SWPROCVERSION 
	p_MANTISSA,
	p_EXPONENT,
	p_FNBWORDS,  -- RO: Nb Words needed to code a float.
	p_FROUNDMODE, -- 0=truncate  1=Round(default) 
	p_DPTR2,
	p_M2,
	p_C,
	p_M,			-- Note: indirect access : 
                -- M(0xF0000000..F00000FF)  = Wxx  ( to allow indirect access )
                -- M(0xF0000100..F00001FF)  = Vxx
                -- M(0xF0000400..F00004FF)  = Fxx ( Push/Pop )
                -- M(0xF0000500..F00005FF)  = Sxx 
                -- M(0xF0000D00..F0000DFF)  = Axx 
                -- M(0xF0000900..F00009FF)  = FWMxx 
                -- M(0xF0000A00..F0000AFF)  = FRMxx 
                -- M(0xF0000B00..F0000BFF)  = FWCxx 
                -- M(0xF0000C00..F0000CFF)  = FRCxx 
    p_ATBITSRC,
	p_ATBITDST,
	p_FILESELECTOR,
	p_FIFOSELECTOR,
	p_ALL_Z,
	p_REGISTRY,
	p_ALL_W,
	p_ALL_L,
	p_ALL_H,
	p_ALL_DONTCARE,
	p_SYNCEN,
	p_Gxxxx,   -- Global
	p_Dxxxx,   -- Local 
	p_HASH,
	p_NOW,
	p_DPTR,
	p_Oxx,
	p_Exx,
	p_FWMxx,        -- Fifo write mode.
	p_FRMxx,
	p_FWCxx,
	p_FRCxx,
	p_WSS,
	p_VSS,
	p_STR            --- When STR => value is the string index in string table...			
	);

type instruction_rec is record
	inst : inst_type;
	a_type: param_type;
	a_value : std_logic_vector(31 downto 0);
	b_type: param_type;
	b_value : std_logic_vector(31 downto 0);
    end record;			
type ProgArrayPointerElement is array(natural range <>) of instruction_rec;
type ProgPointer is access ProgArrayPointerElement;
shared variable Prog : ProgPointer := null;


-- Do line reduction
    procedure linereduce_file(
        input_filename  : in string;
        output_filename : in string
    ) is
        file input_file     : TEXT;
        file output_file    : TEXT;
		
		variable lr_byte : character := ' ';
		variable end_of_line_flag : boolean; -- Pour vérifier si on a atteint la fin de la ligne
		
		
		variable last_lr_byte : character := '0';
		
		variable inside_quote: boolean := false;
		variable insert_lf : boolean;
--		variable line_buffer : line;         -- Pour lire une ligne entière
		
        variable status_in  : FILE_OPEN_STATUS;
        variable status_out : FILE_OPEN_STATUS;
        variable input_line : LINE;
        variable output_line: LINE;
--        variable temp_line  : LINE;
--        variable new_line  : LINE;
		variable bypass_spaces : boolean;
		variable remove_space: boolean;
		variable i: natural;
		variable seen_instr : inst_type;
    begin
        -- Ouvrir les fichiers
        FILE_OPEN(status_in, input_file, input_filename, READ_MODE);
        assert status_in = OPEN_OK
            report "Cannot open input file: " & input_filename
            severity failure;
        
        FILE_OPEN(status_out, output_file, output_filename, WRITE_MODE);
        assert status_out = OPEN_OK
            report "Cannot open output file: " & output_filename
            severity failure;

        -- Traiter chaque ligne du fichier d'entrée
        while not endfile(input_file) loop
		
			-- Lire une ligne du fichier dans le buffer
			readline(input_file, input_line);
			bypass_spaces := true;
			seen_instr := i_none;
			remove_space := false;
			i:=input_line'low;
			while i<= input_line'high loop
				-- Traitement du caractère....
				insert_lf := false;
				-- replace any tab with space ...
				if input_line(i)=HT then 
					input_line(i) := ' '; 
				end if;
				last_lr_byte := lr_byte;
				lr_byte := input_line(i);
				if not inside_quote then
				
					if i > input_line'low + 2 then 
						if input_line(i-3 to i)= "PUSH" then
							seen_instr := i_PUSH;
						end if;
					end if;
					if i > input_line'low + 1 then 
						if input_line(i-2 to i)= "POP" then
							seen_instr := i_POP;
						end if;
					end if;
					if i > input_line'low + 3 then 
						if input_line(i-4 to i)= "LPUSH" then
							seen_instr := i_LPUSH;
						end if;
					end if;
					if i > input_line'low + 2 then 
						if input_line(i-3 to i)= "LPOP" then
							seen_instr := i_LPOP;
						end if;
					end if;
					if i > input_line'low + 3 then 
						if input_line(i-4 to i)= "RPUSH" then
							seen_instr := i_RPUSH;
						end if;
					end if;
					if i > input_line'low + 2 then 
						if input_line(i-3 to i)= "RPOP" then
							seen_instr := i_RPOP;
						end if;
					end if;
				
				
					if i > input_line'low + 4 then
						if input_line(i-5) = 'L' and
						   input_line(i-4) = 'A' and
						   input_line(i-3) = 'B' and
						   input_line(i-2) = 'E' and
						   input_line(i-1) = 'L' then  
							if input_line(i)=' ' then
								nb_labels := nb_labels+1;
							end if;
--							if input_line(i)=HT then-
--								nb_labels := nb_labels+1;
--							end if;
						end if;
					end if;
				
					if lr_byte=',' then
						-- each time a , is seen in PUSH or POP we add one progstep
						case seen_instr is
							when i_PUSH =>
								insert_lf := true;
								input_line(i-5 to i) := " PUSH ";
								i:=i-5;
							when i_LPUSH => 
								insert_lf := true;
								input_line(i-6 to i) := " LPUSH ";
								i:=i-6;
							when i_RPUSH => 
								insert_lf := true;
								input_line(i-6 to i) := " RPUSH ";
								i:=i-6;
							when i_POP =>
								insert_lf := true;
								input_line(i-4 to i) := " POP ";
								i:=i-4;
							when i_LPOP => 
								insert_lf := true;
								input_line(i-5 to i) := " LPOP ";
								i:=i-5;
							when i_RPOP => 
								insert_lf := true;
								input_line(i-5 to i) := " RPOP ";
								i:=i-5;
							when others =>
								null;
						end case;
								
--						if seen_instr = i_PUSH OR
--						   seen_instr = i_POP then
--								nb_progsteps := nb_progsteps + 1; 		
--						end if;
					end if;
				
					if lr_byte=' ' then 
						if last_lr_byte =' ' or 
						   last_lr_byte =',' then
					     remove_space := true;
						end if;
					end if;
					-- remplace ; et : par un saut de ligne .. 
					if last_lr_byte /= 'c' then
							if lr_byte = '"' then
								inside_quote := true;
							elsif lr_byte = ';' or lr_byte = ':' then
								-- Must generate a line feed on output
								insert_lf := true;
							end if;
					end if;
				elsif lr_byte='"' then
					inside_quote := false;
					nb_strings := nb_strings + 1;
				end if;
				
				if not insert_lf then
					if lr_byte/=' ' then
						bypass_spaces := false;
					end if;
					if not bypass_spaces then
						if not remove_space then 
							write(output_line, lr_byte);
							remove_space := false;
						end if;
						remove_space := false;
					end if;
				else
					writeline(output_file, output_line);
					nb_progsteps := nb_progsteps + 1; 
					bypass_spaces:=true;
					seen_instr := i_none;
					last_lr_byte := '0';
				end if;
			i:=i+1;	
			end loop;
			writeline(output_file, output_line);
			nb_progsteps := nb_progsteps + 1; 
		end loop;
		
		file_close(input_file);
		file_close(output_file);
		
 end procedure linereduce_file;



-- Do compilation 
    procedure compile_file(
        input_filename  : in string;
        output_filename : in string    -- Used as "listing" for debug 
    ) is
        file input_file     : TEXT;
        file output_file    : TEXT;
		
		variable end_of_line_flag : boolean; -- Pour vérifier si on a atteint la fin de la ligne
		
		variable inside_quote: boolean := false;
		
        variable status_in  : FILE_OPEN_STATUS;
        variable status_out : FILE_OPEN_STATUS;
        variable input_line : LINE;
        variable output_line: LINE;
		
		variable cur_instr : instruction_rec;
		variable i : natural;
		variable p0: string(1 to 20);  -- Store the instruction 
		variable p1: string(1 to 80);  -- Store param1, (remove any preceding spaces and folowing "," ) 
		variable p2: string(1 to 80);  -- Store param2.
		variable p3: string(1 to 80);  -- Store param3.
		variable p0l: natural;
		variable p0r: natural;
		variable p1l: natural;
		variable p1r: natural;
		variable p2l: natural;
		variable p2r: natural;
		variable p3l: natural;
		variable p3r: natural;
		
    begin
        -- Ouvrir les fichiers
        FILE_OPEN(status_in, input_file, input_filename, READ_MODE);
        assert status_in = OPEN_OK
            report "Cannot open input file: " & input_filename
            severity failure;
        
        FILE_OPEN(status_out, output_file, output_filename, WRITE_MODE);
        assert status_out = OPEN_OK
            report "Cannot open output file: " & output_filename
            severity failure;

        -- Traiter chaque ligne du fichier d'entrée
        while not endfile(input_file) loop
		
			-- Lire une ligne du fichier dans le buffer
			readline(input_file, input_line);
						
			if input_line'length>0 then 
			
-- 				end_of_line_flag := false;
-- 				p1l:= input_file'low;
-- 				p1r:= p1l;
-- 				p2l:= 0;
-- 				p2r:= 0;
-- 				p3l:= 0;
-- 				p3r:= 0;
--  
-- 				while not end_of_line_flag loop 
-- 					-- get param1
-- 					p1r := p1l;
-- 					while p1r < input_file'high and is_identifier_char(input_line(p1r+1)) loop
-- 						p1r := p1r+1;
-- 					end loop;
-- 					p2l:=p1r+1;
-- 					while p2l < input_file'high and is_separator(input_line(p2l+1)) loop
-- 					   p2l := p2l + 1;
-- 					end loop;
-- 					p2r := p2l;
-- 					
-- 					while
-- 				cur_instr.inst: i_none;
-- 				cur_inst.a_type :=	p_none ;
-- 				cur_inst.a_value:= (others => '0');
-- 				cur_inst.b_type :=	p_none ;
-- 				cur_inst.b_value:= (others => '0');
-- 			
-- 
			
--			i:=input_line'low;
--			
--			while i <= input_line'high loop
--				-- extraction parametres
--				p2idx 
--			
--			
--				if not inside_quote then
--					-- remplace ; et : par un saut de ligne .. 
--					if last_lr_byte /= 'c' then
--							if lr_byte = '"' then
--								inside_quote := true;
--							elsif lr_byte = ';' or lr_byte = ':' then
--								-- Must generate a line feed on output
--								insert_lf := true;
--							end if;
--					end if;
--				elsif lr_byte='"' then
--					inside_quote := false;
--					nb_strings := nb_strings + 1;
--				end if;
--				
--				if not insert_lf then
--					write(output_line, lr_byte);
--				else
--					writeline(output_file, output_line);
--					nb_progsteps := nb_progsteps+1;					
--				end if;
--			end loop;
			end if;
			writeline(output_file, output_line);
		end loop;
		
		file_close(input_file);
		file_close(output_file);
		
 end procedure compile_file;


begin

	-- Insert here VIos instances ...
	

	-- Main process.
	process
	
		-- Treat the whole startup of simulation. Including code compilation etc....
		procedure DoInit is
			variable v_line: line;
			variable idx : natural;
			begin
			-- Copyright information
			write(v_line,string'("************************"));WriteLine(output,v_line);
			Write(v_line,string'("*   Stand Alone Vhdl Simulation Library ( SAVSL ) "));WriteLine(output,v_line);
	        Write(v_line,string'("*     by:               "));WriteLine(output,v_line);
	        Write(v_line,string'("*      Claudy Libois    "));WriteLine(output,v_line);
	        Write(v_line,string'("*                       "));WriteLine(output,v_line);
	        Write(v_line,string'("*  Copyright (c) 2025   "));WriteLine(output,v_line);
	        Write(v_line,string'("*    All rights reserved.           "));WriteLine(output,v_line);
	        Write(v_line,string'("*                       "));WriteLine(output,v_line);
           	Write(v_line,string'("*    VERSION: ")); hwrite(v_line, SAVSLVersion ); WriteLine(output,v_line);
	        Write(v_line,string'("*                       "));WriteLine(output,v_line);
	        Write(v_line,string'("*  Parameters: "));WriteLine(output,v_line);
            Write(v_line,string'("*        nbIO       : ")); Write(v_line,nbIO); WriteLine(output,v_line);
            Write(v_line,string'("*        nbVirtualIO: ")); Write(v_line,nbVIO); WriteLine(output,v_line);
            Write(v_line,string'("*        ScriptFile      : ")); Write(v_line,scriptFile); WriteLine(output,v_line);
            Write(v_line,string'("*        DataStackSize   : ")); Write(v_line,DataStackSize);WriteLine(output,v_line);
	        Write(v_line,string'("*        LogFile         : ")); Write(v_line,LogFile);WriteLine(output,v_line);
	        Write(v_line,string'("*        DebugFile       : ")); Write(v_line,DebugFile);WriteLine(output,v_line);
	        Write(v_line,string'("************************"));WriteLine(output,v_line);WriteLine(output,v_line);
			-- coverage off
				if nbVIO < 32 then
					Write(v_line,string'("ERROR : nbVIO parameter must be at least 32"));WriteLine(output,v_line);WriteLine(output,v_line);
					assert (false) report "ERROR: nbVIO parameter must be at least 32" severity failure;
				end if;
			-- coverage on			
			
		-- preprocess source....
		preprocess_file(scriptFile, "prep1.asm", "INC1;INC2");
		precompile_file("prep1.asm","prep2.asm");
		-- Second pass of preprocessor pass...
		preprocess_file("prep2.asm", "prep3.asm", "INC1;INC2");
        linereduce_file("prep3.asm", "prep4.asm");
		-- Here, nb_strings have the number of strings 
		--     and nb_progsteps contains the number of instructions
		--     and nb_labels    contains the number of labels
		AllLines := new LineArrayElement(nb_strings-1 downto 0 );
		Prog     := new ProgArrayPointerElement( nb_progsteps-1 downto 0 );
		nb_labels := nb_labels+2; -- For main and main_irq
		AllLabels:= new LineArrayElement(nb_labels-1 downto 0);

		add_label("main",idx);	   -- Force IDX of main = 0 
        add_label("main_irq",idx); 			
		
		-- Do Compile 
		-- compile_file("prep4.asm","LISTING.asm");
			
		wait for 1 ns;
		assert(false) report "SAVSL: STOP simulation request by program." severity failure;		
			
		end DoInit;
		
 --deallocate(AllLines(line_index));
 --              AllLines(line_index) := current_line_buffer;	
	
	begin
	

	DoInit;
	
	
	end process;
	
	

end SimulationModel;

library ieee;
use     ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

-- library WORK;
-- use WORK.SAVSL_IOmoduleComponent.all;

package SAVSLComponent is 

    component SAVSL
	generic (
		 nbIO   : integer:=64;   -- Nb Of IOS
		 nbVIO  : integer := 32; -- Nb of Virtual IOs  (not presented to module interface )
		 scriptFile   : string := "test.asm";
		 DataStackSize: integer := 64; -- Nb 32-bit words in direct access data stack.
		 LogFile      : string := "SAVSL.log";
		 DebugFile    : string := "SAVSLDebug.log";
		 MinResolution: time    := 1 fs		
		 );
	port (IO	   : inout std_logic_vector(nbIO-1 downto 0);
	      IO_driven: out   std_logic_vector(nbIO-1 downto 0)
	      );
    end component;

end SAVSLComponent;
