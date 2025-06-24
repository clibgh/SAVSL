library IEEE;
use IEEE.STD_LOGIC_1164.all;
use STD.TEXTIO.all;

library WORK;
use WORK.preprocessor_pkg.all;

package precompiler_pkg is
    procedure precompile_file(
        input_filename  : in string;
        output_filename : in string
    );
end package precompiler_pkg;

package body precompiler_pkg is

	constant MAX_IDENTIFIER_SIZE : natural := 40;

    procedure precompile_file(
        input_filename  : in string;
        output_filename : in string
    ) is
        file input_file     : TEXT;
        file output_file    : TEXT;
        variable status_in  : FILE_OPEN_STATUS;
        variable status_out : FILE_OPEN_STATUS;
        variable input_line : LINE;
        variable output_line: LINE;
        variable temp_line  : LINE;
        variable new_line  : LINE;
        variable i, j,k     : natural;
        variable a_str, b_str : string(1 to MAX_IDENTIFIER_SIZE);
        variable a_val, b_val : integer;
		variable variable_name : LINE;
        variable num_str    : string(1 to MAX_IDENTIFIER_SIZE);
        variable range_str  : string(1 to 4000);
        variable range_len  : natural;
        variable new_str    : string(1 to 4000);
        variable new_len    : natural;
        variable char_count : natural;
        variable found      : boolean;
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
            READLINE(input_file, input_line);
            temp_line := new string'(input_line.all);
            
            -- Supprimer les commentaires (lignes commençant par "--")
            -- INUTILE deja fait dans preproc.  if temp_line'length > 0 then
            -- INUTILE deja fait dans preproc.      i := 1;
            -- INUTILE deja fait dans preproc.      while i <= temp_line'length and temp_line(i) = ' ' loop
            -- INUTILE deja fait dans preproc.          i := i + 1;
            -- INUTILE deja fait dans preproc.      end loop;
            -- INUTILE deja fait dans preproc.      
            -- INUTILE deja fait dans preproc.      if i <= temp_line'length and i+1 <= temp_line'length then
            -- INUTILE deja fait dans preproc.          if temp_line(i to i+1) = "--" then
            -- INUTILE deja fait dans preproc.              deallocate(temp_line);
            -- INUTILE deja fait dans preproc.              temp_line := new string'("");
            -- INUTILE deja fait dans preproc.          end if;
            -- INUTILE deja fait dans preproc.      end if;
            -- INUTILE deja fait dans preproc.  end if;



            
            -- Appliquer les transformations uniquement sur les lignes non vides
            if temp_line'length > 0 then
                -- Remplacer $postdefine par #define
                for i in 1 to temp_line'length-11 loop
                    if temp_line(i to i+10) = "$postdefine" then
                        WRITE(output_line, temp_line(1 to i-1) & "#define" & temp_line(i+11 to temp_line'length));
                        deallocate(temp_line);
                        temp_line := output_line;
                        exit;
                    end if;
                end loop;
                
                -- Traiter les intervalles numériques [a:b]
                i := 1;
                while i <= temp_line'length loop
                    if temp_line(i) = '[' then
                        j := i + 1;
                        while j <= temp_line'length and temp_line(j) /= ':' loop
                            j := j + 1;
                        end loop;
                        
                        if j <= temp_line'length then
                            a_str := (others => ' ');
                            a_str(1 to j-i-1) := temp_line(i+1 to j-1);
                            
                            k := j + 1;
                            while k <= temp_line'length and temp_line(k) /= ']' loop
                                k := k + 1;
                            end loop;
                            
                            if k <= temp_line'length then
                                b_str := (others => ' ');
                                b_str(1 to k-j-1) := temp_line(j+1 to k-1);
                                
                                -- Convertir en entiers
                                a_val := integer'value(a_str(1 to j-i-1));
                                b_val := integer'value(b_str(1 to k-j-1));
                                
                                -- Générer la séquence numérique
                                range_str := (others => ' ');
                                range_len := 0;
                                
                                if a_val <= b_val then
                                    for num in a_val to b_val loop
										new_line := new string'(integer'image(num));
                                        if range_len > 0 then
                                            range_str(range_len+1) := ',';
                                            range_len := range_len + 1;
                                        end if;
                                        range_str(range_len+1 to range_len+new_line'length) := trim_string(new_line.all);
                                        range_len := range_len + string_length(trim_string(new_line.all));
										deallocate(new_line);
                                    end loop;
                                else
                                    for num in a_val downto b_val loop
										new_line := new string'(integer'image(num));
                                        if range_len > 0 then
                                            range_str(range_len+1) := ',';
                                            range_len := range_len + 1;
                                        end if;
                                        range_str(range_len+1 to range_len+new_line'length) := trim_string(new_line.all);
                                        range_len := range_len + string_length(trim_string(new_line.all));
										deallocate(new_line);
                                    end loop;
                                end if;
                                
                                -- Construire la nouvelle chaîne
                                new_str := (others => ' ');
                                new_len := 0;
                                new_str(1 to i-1) := temp_line(1 to i-1);
                                new_len := i-1;
                                new_str(new_len+1 to new_len+range_len) := range_str(1 to range_len);
                                new_len := new_len + range_len;
                                new_str(new_len+1 to new_len+temp_line'length-k) := temp_line(k+1 to temp_line'length);
                                new_len := new_len + temp_line'length - k;
                                
                                -- Mettre à jour la ligne temporaire
                                deallocate(temp_line);
                                temp_line := new string'(new_str(1 to new_len));
                            end if;
                        end if;
                    end if;
                    i := i + 1;
                end loop;
                
				-- TODO : Remplacer expressions math entre  [] ...
				
				-- TODO  # test simple comparisons
                -- #
                -- #   ?( variable [<,<=,==,/=,>,>=] value )
                -- #      ==>  CMP variable,value ;[LT,NGT,EQ,NE,GT,NLT]
                -- #
                -- #       while ( /^(.*)\?\(([^\)]*)\)(.*)$/ ) {
                -- 
                --         while ( /^(.*)\?\(([^<=>\/]*)([<=>\/]*)([^\)^\"]*)\)(.*)$/ ) {
                --            $_= "$1 ;CMP $2 , $4 ; !!$3!! ;$5";
                --         }
                --        s/!!<=!!/NGT/g;
                --        s/!!<!!/LT/g;
                --        s/!!==!!/EQ/g;
                --        s/!!\/=!!/NE/g;
                --        s/!!>=!!/NLT/g;
                --        s/!!>!!/GT/g;				
                -- 
                -- TODO				
                -- # Special case of tests with constant std_logic_vector
                --         while ( /^(.*)\?\(([^<=>\/]*)([<=>\/]*)\s*\"([^\)^\"]*)\"\s*\)(.*)$/ ) {
                --           my $p1 = $1;
                --           my $p2 = $2;
                --           my $p3 = $3;
                --           my $p4 = $4;
                --           my $p5 = $5;
                -- 
                --           $_= "$p1\nRPUSH VSS\nLD VSS,0\n";
                --           for my $c (split //,$p4) { 
                --               if ( $c =~ m/0/ ) { $_ .= "SHL VSS,1\n" }
                --               if ( $c =~ m/1/ ) { $_ .= "SHL VSS,1;LDBIT VSS.0,1\n" }
                --               if ( $c =~ m/z/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_Z.0\n" }
                --               if ( $c =~ m/u/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_U.0\n" }
                --               if ( $c =~ m/h/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_H.0\n" }
                --               if ( $c =~ m/l/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_L.0\n" }
                --               if ( $c =~ m/x/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_X.0\n" }
                --               if ( $c =~ m/-/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_DONTCARE.0\n" }
                --               if ( $c =~ m/w/i ) { $_ .= "SHL VSS,1;LDBIT VSS.0,ALL_W.0\n" }
                --           } 
                --           if ( $p3 =~ m/===/ ) { $_ .= "S" }
                --           if ( $p3 =~ m/\/==/ ) { $_ .= "S" }
                -- 
                --            $_ .= "CMPSTATE $p2 , VSS ;RPOP VSS; !!$p3!! ;$p5";
                --         }
                --        s/!!==!!/EQ/g;
                --        s/!!\/=!!/NE/g;
                --        s/!!===!!/EQ/g;
                --        s/!!\/==!!/NE/g;
                -- 
                -- TODO
                -- # Assignations with special chars
                -- #       ... Variable<="value" ...
                -- #       ==>   prepare value and then load Variable with it
                --        while ( /(\w+)\s*<=\s*\"([^\)^\"]*)\"/ ) {
                --            my $p1= $1;
                --            my $p2= $2;
                --            my $r = "RPUSH VSS;LD VSS,0";
                --            for my $c (split //,$p2) { 
                --               if ( $c =~ m/0/ ) { $r .= ";SHL VSS,1" }
                --               if ( $c =~ m/1/ ) { $r .= ";SHL VSS,1;LDBIT VSS.0,1" }
                --               if ( $c =~ m/z/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_Z.0" }
                --               if ( $c =~ m/u/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_U.0" }
                --               if ( $c =~ m/h/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_H.0" }
                --               if ( $c =~ m/l/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_L.0" }
                --               if ( $c =~ m/x/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_X.0" }
                --               if ( $c =~ m/-/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_DONTCARE.0" }
                --               if ( $c =~ m/w/i ) { $r .= ";SHL VSS,1;LDBIT VSS.0,ALL_W.0" }
                --            } 
                --            $r .= ";LPUSH VSS;RPOP VSS;LPOP $p1";
                --            s/(\w+)\s*<=\s*\"([^\)^\"]*)\"/$r/g;
                --        }				
				
                -- Remplacer les assignations
                i := 1;
                while i <= temp_line'length-2 loop
                    if temp_line(i) = '<' and temp_line(i+1) = '=' then
                        -- Trouver le début du nom de variable
                        j := i - 1;
                        while j > 1 and temp_line(j) = ' ' loop
                            j := j - 1;
                        end loop;
                        
                        k := j;
                        while k > 1 and temp_line(k) /= ' ' and temp_line(k) /= ';' loop
                            k := k - 1;
                        end loop;
                        
                        if k = 1 or temp_line(k) = ' ' or temp_line(k) = ';' then
                            if temp_line(k) = ' ' or temp_line(k) = ';' then
                                k := k + 1;
                            end if;
                            
                            variable_name := new string'( temp_line(k to j));
                            new_line := new string'(
                                temp_line(1 to k-1) & "LD " & variable_name.all & "," & temp_line(i+2 to temp_line'length)
                            );
                            
                            deallocate(temp_line);
                            temp_line := new_line;
                            i := k + 3 + variable_name'length; -- Ajuster la position
							deallocate(variable_name);
                        end if;
                    end if;
                    i := i + 1;
                end loop;
				
				-- TODO
                -- # Ops alternates: += -= *= /= <<= >>= &= |= ^= %=
                --        while ( /(\w+)\s*\+=/ ) {
                --            s/(\w+)\s*\+=/ADD $1,/g;
                --        }
                --         while ( /(\w+)\s*-=/ ) {
                --             s/(\w+)\s*-=/SUB $1,/g;
                --         }
                --         while ( /(\w+)\s*\*=/ ) {
                --             s/(\w+)\s*\*=/MUL $1,/g;
                --         }
                --         while ( /(\w+)\s*\/=/ ) {
                --             s/(\w+)\s*\/=/DIV $1,/g;
                --         }
                --         while ( /(\w+)\s*<<=/ ) {
                --             s/(\w+)\s*<<=/SHL $1,/g;
                --         }
                --         while ( /(\w+)\s*>>=/ ) {
                --             s/(\w+)\s*>>=/SHR $1,/g;
                --         }
                --         while ( /(\w+)\s*&=/ ) {
                --             s/(\w+)\s*&=/AND $1,/g;
                --         }
                --         while ( /(\w+)\s*\|=/ ) {
                --             s/(\w+)\s*\|=/OR $1,/g;
                --         }
                --         while ( /(\w+)\s*\^=/ ) {
                --             s/(\w+)\s*\^=/XOR $1,/g;
                --         }
                --         while ( /(\w+)\s*\%=/ ) {
                --             s/(\w+)\s*\%=/MOD $1,/g;
                --         }				
				
				-- TODO 
                -- # Fix $var and $lvar definitions: remove any space in definition
                --        if ( /^\s*\$lvar\s+(.*)/ ) {
                --            s/\s*,\s*/,/g;
                --        }
                --        if ( /^\s*\$var\s+(.*)/ ) {
                --            s/\s*,\s*/,/g;
                --        }
                -- 
                -- 
                -- # Functions declarations
                -- #       $declare FunctionName(p1,p2,...)
                -- #         ==>  #define FunctionName(p1,p2,...) LPUSH p1,p2,...;CALL fdef_FunctionName
                -- #         ==>  #define FunctionName_bg(p1,p2,...) LPUSH p1,p2,...;BGCALL fdef_FunctionName; LPOP @BITSRC; LPOP @BITSRC,.... [ NOTE:@BITSRC used as dummy variable to clean LStack ]
                -- #
                --        if ( /^\s*\$declare_function\s+(\w+)\s*\(([^\)]*)\)(.*)/ ) {
                --            my $fname   = $1;
                --            my $arglist = $2;
                --            my $striparglist = $arglist;
                --            my $started = 0;
                --            my $first = "";
                --            my $parglist = "";
                --            my $endline = $3;
                -- 
                --            $striparglist =~s/\@//g;           
                -- 
                --            foreach my $v  (split(/[,]/,$arglist)) {
                --                if ( $v =~ m/@(.*)/ ) {
                --                   # Current argument is a variable "pointer"
                --                       $parglist=$parglist . $first . "PUSH@ $1;LPUSH T0;DROP 1";
                --                       $first = ";";
                --                       $started = 0;
                --                } else {
                --                    # Current argument is a normal parameter
                --                    if ($started eq 1) {
                --                        $parglist=$parglist . ",$v";
                --                    } else {
                --                        $parglist =$parglist . $first . "LPUSH $v";
                --                        $first = ";";
                --                        $started = 1;
                --                    }
                --                }
                --            }
                --            $_="#define $fname($striparglist) $parglist;CALL fdef_$fname\n#define ${fname}_bg($striparglist) $parglist;BGCALL fdef_$fname;RPUSH \@BITSRC";
                --            foreach my $v  (split(/[,]/,$striparglist)) {
                --                $_ .= ";LPOP \@BITSRC"
                --                }
                --            $_ .= ";RPOP \@BITSRC\n$endline"
                --        }
                -- #       $declare FunctionName
                -- #         ==>  #define FunctionName CALL fdef_FunctionName
                -- #         ==>  #define FunctionName_bg BGCALL fdef_FunctionName
                -- #
                --        if ( /^\s*\$declare_function\s+(\w+)(\s*[^(]*.*$)/ ) {
                --             $_="#define $1 CALL fdef_$1\n#define $1_bg BGCALL fdef_$1\n$2";
                --        }
                -- 
                -- #      $define_function FunctionName(p1,p2,....)
                -- #         ==> LABEL FunctionName
                -- #             $lvar p1,p2,...
                -- #             LPOP ...,p2,p1
                -- #
                --        if ( /^\s*\$define_function\s+(\w+)\s*\(([^\)]*)\)(.*)/ ) {
                -- 
                --            my $fname   = $1;
                --            my $arglist = $2;
                --            my $striparglist = $arglist;
                --            my $started = 0;
                --            my $first = "";
                --            my $parglist = "";
                --            my $endline = $3;
                -- 
                --            $striparglist =~s/\@//g;           
                --            $striparglist =~s/\s*//g;           
                --            $_="#ifndef $fname\n";
                --            foreach my $v  (split(/[,]/,$arglist)) {
                --                if ( $v =~ m/@(.*)/ ) {
                --                   # Current argument is a variable "pointer"
                --                       $parglist=$parglist . $first . "PUSH@ $1;LPUSH T0;DROP 1";
                --                       $first = ";";
                --                       $started = 0;
                --                } else {
                --                    # Current argument is a normal parameter
                --                    if ($started eq 1) {
                --                        $parglist=$parglist . ",$v";
                --                    } else {
                --                        $parglist =$parglist . $first . "LPUSH $v";
                --                        $first = ";";
                --                        $started = 1;
                --                    }
                --                }
                --            }
                --            $_.="#define $fname($striparglist) $parglist;CALL fdef_$fname\n#define ${fname}_bg($striparglist) $parglist;BGCALL fdef_$fname;RPUSH \@BITSRC";
                --            foreach my $v  (split(/[,]/,$striparglist)) {
                --                $_ .= ";LPOP \@BITSRC"
                --                }
                --            $_ .= ";RPOP \@BITSRC\n";
                --            $_ .= "\n#endif\n";               
                -- 
                --            my $rp = join ',',reverse split(/[,]/,$striparglist);
                --            $_.="LABEL fdef_$fname\n\$lvar $striparglist\nLPOP $rp\n$endline"
                --        }
                -- #      $define_function FunctionName
                -- #         ==> LABEL FunctionName
                -- #
                --        if ( /^\s*\$define_function\s+(\w+)(\s*[^\(]*.*$)/ ) {
                --            $_="#ifndef $1\n";
                --            $_.="#define $1 CALL fdef_$1\n#define $1_bg BGCALL fdef_$1\n";
                --            $_.="#endif\n";
                --            $_.="LABEL fdef_$1\n$2"
                --        }
                -- 
                -- #      $end_function
                -- #         ==> RET
                -- #             $clrlvars
                -- 
                --        s/\$end_function/RET\n\$clrlvars/g;
                --        
                -- #      $case variable
                -- #         ==>  [ Simply store the variable parameter for all next detected $when clauses ]
                -- #         ==>  FGOTO lbl_case_[index]
                --        if ( /^\s*\$case\s+(\w+)(.*)/ ) {
                --            $case_level++;
                --            $case_variables{$case_level} = $1;
                --            $_="FGOTO lbl_case_$case_level\n";
                --        }
                -- 
                -- 
                -- 
                -- #      $when (Value1[,Value2[,...]]) 
                -- #        ==>   FGOTO lbl_endcase_[index]
                -- #        ==>   LABEL lbl_case_[index]
                -- #        ==>   CMP variable, Value1
                -- #        ==>   FJEQ lbl_case_cont_[index]
                -- #        ==>   CMP variable, Value2
                -- #        ==>   FJEQ lbl_case_cont_[index]
                -- #        ==>   FGOTO lbl_case_[index]
                -- #        ==>   LABEL lbl_case_cont_[index]
                -- 
                --        if ( /^\s*\$when\s+\(([^\)]*)\)(.*)/ ) {
                -- 
                --            my $arglist = $1;
                --            my $striparglist = $arglist;
                --            my $started = 0;
                --            my $first = "";
                --            my $parglist = "";
                --            my $endline = $2;
                -- 
                --            $striparglist =~s/\s*//g;
                --            $_="FGOTO lbl_endcase_$case_level\n";
                --            $_.="LABEL lbl_case_$case_level\n";
                --            foreach my $v  (split(/[,]/,$striparglist)) {
                --                $_.="CMP $case_variables{$case_level},$v \n";
                --                $_.="FJEQ lbl_case_cont_$case_level\n";
                --            }
                --            $_.=" FGOTO lbl_case_$case_level\n";
                --            $_.=" LABEL lbl_case_cont_$case_level\n";
                --            $_.="$endline\n";
                --        }
                -- 
                -- #      $when Value
                -- #        ==>   FGOTO lbl_endcase_[index]
                -- #        ==>   LABEL lbl_case_[index]
                -- #        ==>   CMP variable, Value
                -- #        ==>   FJNE lbl_case_[index]
                --        if ( /^\s*\$when\s+(\w+)(.*)/ ) {
                --            $_="FGOTO lbl_endcase_$case_level\n";
                --            $_.="LABEL lbl_case_$case_level\n";
                --            $_.="CMP $case_variables{$case_level},$1 \n";
                --            $_.="FJNE lbl_case_$case_level\n";
                --            $_.="$2\n";
                --        }
                -- #      $whenothers
                -- #        ==>   FGOTO lbl_endcase_[index]
                -- #        ==>   LABEL lbl_case_[index]
                --        if ( /^\s*\$whenothers(.*)/ ) {
                --            $_="FGOTO lbl_endcase_$case_level\n";
                --            $_.="LABEL lbl_case_$case_level\n";
                --            $_.="$1\n";
                --        }
                -- #      $endcase
                -- #        ==>   FGOTO lbl_endcase_[index]
                -- #        ==>   LABEL lbl_case_[index]
                -- #        ==>   LABEL lbl_endcase_[index]
                --        if ( /^\s*\$endcase/ ) {
                --            $_="FGOTO lbl_endcase_$case_level\n";
                --            $_.="LABEL lbl_case_$case_level\n";
                --            $_.="LABEL lbl_endcase_$case_level\n";
                --            $case_level--;
                --        }
                --         
                -- #      $if CONDITION_INSTRUCTIONS FINISHING WITH either EQ,NE,GT,LT,NLT,NGT, possibly on several lines] $then
                --        if ( /\$if/ ) {
                --            s/\$if//g;
                --            $if_level++;
                --        }
                -- 
                --        if ( /(.*)\$elsif(.*)/s ) {
                --            $_ = "$1\n";
                --            $_.= "FGOTO lbl_if_endif_$if_level\n";
                --            $_.= "LABEL lbl_if_next_$if_level\n";
                --            $_.= "$2\n";
                --        }
                -- 
                --        if ( /(.*)\$then(.*)/s ) {
                --            $_ ="$1\n";
                --            $_.="FGOTO lbl_if_ok_$if_level\n";
                --            $_.="FGOTO lbl_if_next_$if_level\n";
                --            $_.="LABEL lbl_if_ok_$if_level\n";
                --            $_.="$2\n";
                --        }
                --        if ( /(.*)\$else(.*)/s ) {
                --            $_ = "$1\n";
                --            $_.= "FGOTO lbl_if_endif_$if_level\n";
                --            $_.= "LABEL lbl_if_next_$if_level\n";
                --            $_.="$2\n";
                --        }
                --        if ( /(.*)\$endif(.*)/s) {
                --            $_ = "$1\n";
                --            $_.= "LABEL lbl_if_endif_$if_level\n";
                --            $_.= "LABEL lbl_if_next_$if_level\n";
                --            $_.="$2\n";
                --            $if_level--;
                --        }
                -- 
                --        # Forks
                --        if ( /(.*)\$fork_begin(.*)/s ) {
                --            $fork_level++;
                --            $_ = "$1\n";
                --            $_.= "FGOTO lbl_fork_nxt_$fork_level\nLABEL lbl_fork_routine_$fork_level\n";
                --            $_.="$2\n";
                --        }
                --        if ( /(.*)\$fork_end(.*)/s ) {
                --            $_ = "$1\n";
                --            $_ = "RET;LABEL lbl_fork_nxt_$fork_level;BBGCALL lbl_fork_routine_$fork_level\n";
                --            $_.="$2\n";
                --            $fork_level--;
                --        }
                --        # While Loops
                --        if ( /(.*)\$while(.*)/s ) {
                --            $loop_level++;
                --            $_ = "$1\n";
                --            $_.= "LABEL lbl_while_$loop_level \n";
                --            $_.= "$2\n";
                --            push @breaklist,"FGOTO lbl_while_end_loop$loop_level";
                --            push @continuelist,"BGOTO lbl_while_$loop_level";
                --        }
                --        if ( /(.*)\$loop(.*)/s ) {
                --            $_  = "$1\n";
                --            $_ .= "FGOTO lbl_while_continue_$loop_level\n";
                --            $_ .= "FGOTO lbl_while_end_loop$loop_level\n";
                --            $_ .= "LABEL lbl_while_continue_$loop_level\n";
                --            $_ .= "$2\n";
                --        }
                --        if ( /(.*)\$endloop(.*)/s ) {
                --            $_  = "$1\n";
                --            $_ .= "BGOTO lbl_while_$loop_level\n";
                --            $_ .= "LABEL lbl_while_end_loop$loop_level\n";
                --            $_ .= "$2\n";
                --            pop @breaklist;
                --            pop @continuelist;
                --            $loop_level--;
                --        }
                --        # Repeat until
                --        if ( /(.*)\$repeat(.*)/s ) {
                --            $loop_level++;
                --            $_ = "$1\n";
                --            $_.= "LABEL lbl_repeat_$loop_level \n";
                --            $_.= "$2\n";
                --            push @breaklist,"FGOTO lbl_endrepeat_$loop_level";
                --            push @continuelist,"FGOTO lbl_until_$loop_level";
                --        }
                --        if ( /(.*)\$until(.*)/s ) {
                --            $_  = "$1\nLABEL lbl_until_$loop_level\n$2\n";
                --        }
                -- 
                --        if ( /(.*)\$endrepeat(.*)/s ) {
                --            $_  = "$1\n";
                --            $_ .= "FGOTO lbl_endrepeat_$loop_level\n";
                --            $_ .= "BGOTO lbl_repeat_$loop_level\n";
                --            $_ .= "LABEL lbl_endrepeat_$loop_level\n";
                --            $_ .= "$2\n";
                --            pop @breaklist;
                --            pop @continuelist;
                --            $loop_level--;
                --        }
                --        
                --        if ( /(.*)\$break(.*)/s ) {
                --            $_  = "$1\n";
                --            $_ .= $breaklist[-1] . "\n";
                --            $_ .= "$2\n";
                --        }
                --        if ( /(.*)\$continue(.*)/s ) {
                --            $_  = "$1\n";
                --            $_ .= $continuelist[-1] . "\n";
                --            $_ .= "$2\n";
                --        }
                -- # $or 
                --        while ( /(.*)\$or(.*)/s ) {
                --            $_ = "$1\n";
                --            $_.= "FGOTO end_exp_true_lbl\n";
                --            $_.="$2\n";
                --        }
                -- # $and
                --        while ( /(.*)\$and(.*)/s ) {
                --            $_ = "$1\n";
                --            $_.="FGOTO and_nxt_lbl\n";
                --            $_.="FGOTO end_exp_false_lbl\n";
                --            $_.="LABEL and_nxt_lbl\n";
                --            $_.="$2\n";
                --        }
                -- # $endexp
                --        if ( /(.*)\$endexp(.*)/s ) {
                --            $_ = "$1\n";
                --    	   $_.="FGOTO end_exp_true_lbl\n";
                -- 	   $_.="LABEL end_exp_false_lbl\n";
                -- 	   $_.="LD \@BITSRC,1\n";
                -- 	   $_.="CMP \@BITSRC,0\n";
                -- 	   $_.="FGOTO end_exp_lbl\n";
                -- 	   $_.="LABEL end_exp_true_lbl\n";
                -- 	   $_.="LD \@BITSRC,1\n";
                -- 	   $_.="CMP \@BITSRC,1\n";
                -- 	   $_.="LABEL end_exp_lbl\n";
                -- 	   $_.="EQ\n";
                --            $_.="$2\n";
                --        }
                -- 				
				
            end if;
            
			
			-- Écrire la ligne transformée dans le fichier de sortie
			WRITELINE(output_file, temp_line);
            deallocate(temp_line);
        end loop;
        
        -- Fermer les fichiers
        FILE_CLOSE(input_file);
        FILE_CLOSE(output_file);
    end procedure precompile_file;

end package body precompiler_pkg;