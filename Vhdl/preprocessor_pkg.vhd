library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;
use ieee.std_logic_textio.all;

package preprocessor_pkg is

    -- Constantes de configuration
    constant MAX_INCLUDE_DEPTH : natural := 16;
    constant MAX_CONDITIONAL_DEPTH : natural := 32;
    constant MAX_MACROS : natural := 512;
    constant MAX_STRING_LEN : natural := 1024;
    constant MAX_LINE_LEN : natural := 4096;
    constant MAX_SEARCH_PATHS : natural := 16;
    constant MAX_MACRO_ARGS : natural := 16;
    
    -- Type pour les chemins de recherche
    type t_string_array is array (0 to MAX_SEARCH_PATHS-1) of string(1 to MAX_STRING_LEN);
    type t_args_array is array (0 to MAX_MACRO_ARGS-1) of string(1 to MAX_STRING_LEN);

    -- Types pour l'état du préprocesseur
    type t_macro is record
        name       : string(1 to MAX_STRING_LEN);
        value      : string(1 to MAX_STRING_LEN);
        has_args   : boolean;
        is_variadic: boolean;
        arg_names  : string(1 to MAX_STRING_LEN);
        arg_count  : natural;
        defined    : boolean;
    end record;

    type t_macro_table is array (0 to MAX_MACROS-1) of t_macro;
    
    type t_conditional is record
        active      : boolean;
        if_active   : boolean;
        else_found  : boolean;
    end record;
    
    type t_conditional_stack is array (0 to MAX_CONDITIONAL_DEPTH-1) of t_conditional;
    
    type t_file_state is record
        filename   : string(1 to MAX_STRING_LEN);
        dir        : string(1 to MAX_STRING_LEN);
        line_num   : natural;
        in_comment : boolean;  -- État pour les commentaires multi-lignes
    end record;
    
    type t_file_stack is array (0 to MAX_INCLUDE_DEPTH-1) of t_file_state;
    
    type t_preprocessor_state is record
        macro_table     : t_macro_table;
        macro_count     : natural;
        cond_stack      : t_conditional_stack;
        cond_top        : natural;
        file_stack      : t_file_stack;
        file_top        : integer range -1 to 4096;
        search_paths    : t_string_array;
        search_path_cnt : natural;
    end record;

    -- Fonctions utilitaires
    function is_space(c: character) return boolean;
    function to_lower(s: string) return string;
	function is_identifier_char(c : character) return boolean;
    function extend_string(s: string;size: natural) return string;
    function trim_string(s: string) return string;
    function remove_double_dash(s: string) return string;
    function replace_arg(
        source        : string;
        identifier    : string;
        replace_value : string) return string;
    function string_length(s: string) return integer;
    function index_char(s: string; c: character) return integer;
    function index_substring(s: string; substr: string) return integer;
    function rindex(s: string; c: character) return integer;
    function min(a, b: natural) return natural;




    procedure preprocess_file(
        input_filename  : in string;
        output_filename : in string;
        search_paths    : in string := ""
    );
    procedure process_file(
        state        : inout t_preprocessor_state;
        file output_file  : text;
        search_paths : in t_string_array;
        search_cnt   : in natural
    );
	
end package;

package body preprocessor_pkg is


    -- Fonctions utilitaires
    function is_space(c: character) return boolean is
    begin
        return c = ' ' or c = HT or c = CR or c = LF;
    end function;

    function to_lower(s: string) return string is
        variable res: string(s'range);
    begin
        for i in s'range loop
            if s(i) >= 'A' and s(i) <= 'Z' then
                res(i) := character'val(character'pos(s(i)) + 32);
            else
                res(i) := s(i);
            end if;
        end loop;
        return res;
    end function;

	function is_identifier_char(c : character) return boolean is
	begin
		return (c >= 'a' and c <= 'z') or 
			   (c >= 'A' and c <= 'Z') or 
               (c >= '0' and c <= '9') or 
                c = '_';
    end function;
	
    function extend_string(s: string;size: natural) return string is
        variable start, fin: integer;
		variable outs : string(1 to size) := (others => ' ');
		variable idxs: integer := 1;
		variable idxd: integer := 1;
    begin
        start := s'low;
        while start <= s'high and is_space(s(start)) loop
            start := start + 1;
        end loop;
        idxs := start;
        fin := s'high;
        --while fin >= s'low and is_space(s(fin)) loop
		while fin >= s'low  loop
			outs(idxd) := s(idxs);
			idxd := idxd + 1;
			idxs := idxs + 1;
            fin := fin - 1;
        end loop;
        
		return outs ;
    end function;

    function trim_string(s: string) return string is
        variable start, fin: integer;
    begin
        start := s'low;
        while start <= s'high and is_space(s(start)) loop
            start := start + 1;
        end loop;
        
        fin := s'high;
        while fin >= s'low and is_space(s(fin)) loop
            fin := fin - 1;
        end loop;
        
        if start > fin then
            return "";
        else
            return s(start to fin);
        end if;
    end function;
    function remove_double_dash(s: string) return string is
        variable start, fin: integer;
		variable tmp_str:string(1 to MAX_STRING_LEN); 
		variable i:natural:=1;
		variable insidequote : boolean := false;
    begin
        start := s'low;
		i:=1;
        while start <= s'high loop
			tmp_str(i) := s(start);
			if s(start)='"' then
				insidequote := not insidequote;
			end if;
            start := start + 1;
			i:=i+1;
			if i>2 and not insidequote then
				if tmp_str(i-2 to i-1) = "##" then
				   tmp_str(i-2 to i-1):= "  ";
				   i:=i-2;
				end if;
			end if;
        end loop;
        i:=i-1;
		if i>0 then 
			return tmp_str(1 to i);
		else
			return "";
		end if;
    end function;

    -- Attention a considere que identifier et replace_value ont un start index=1 
    function replace_arg(
        source        : string;
        identifier    : string;
        replace_value : string) return string is
    
        variable result : string(1 to MAX_STRING_LEN):=(others => ' ');
		variable result_len : integer := 0;
		variable i : integer := source'low;
		variable match_found : boolean;
		variable j : integer;
		variable insertidx: integer;
		variable insidequote: boolean:=false;
	begin
		-- Parcourir la chaîne source
		while i <= source'high loop
			match_found := false;
			if source(i)='"' then
				insidequote:= not insidequote;
			end if;
			
			if not insidequote then
				-- Vérifier si on a une correspondance potentielle
				if i + identifier'length - 1 <= source'high then
					-- Vérifier que le caractère précédent n'est pas un caractère d'identifiant
					if i > source'low then
						if is_identifier_char(source(i-1)) then
							match_found := false;
						else
							-- Vérifier la correspondance de l'identifiant
							match_found := true;
							for k in identifier'range loop
								if source(i + k - identifier'low) /= identifier(k) then
									match_found := false;
									exit;
								end if;
							end loop;
							
							-- Si correspondance trouvée, vérifier le caractère suivant
							if match_found then
								if i + identifier'length <= source'high then
									if is_identifier_char(source(i + identifier'length)) then
										match_found := false;
									end if;
								end if;
							end if;
						end if;
					else
						-- Premier caractère de la chaîne
						match_found := true;
						for k in identifier'range loop
							if source(i + k - identifier'low) /= identifier(k) then
								match_found := false;
								exit;
							end if;
						end loop;
					
						-- Vérifier le caractère suivant
						if match_found then
							if i + identifier'length <= source'high then
								if is_identifier_char(source(i + identifier'length)) then
									match_found := false;
								end if;
							end if;
						end if;
					end if;
				end if;
				if match_found then
					-- Remplacer par la nouvelle valeur
					insertidx:=result_len;
					for k in replace_value'range loop
						result_len := result_len + 1;
						if result_len <= MAX_STRING_LEN then 
						result(result_len) := replace_value(k);
						end if;
					end loop;
					-- If identifier preceded by '#', stringify it   (simple... no char escape ) 
					if i > source'low then
						if source(i-1)='#' then 
							if i > source'low+1 then
								if source(i-2)/='#' then 
									-- STRINGIFY only if not concatenation operator
									result(insertidx) := '"';
									result_len := result_len + 1;
									if result_len <= MAX_STRING_LEN then 
										result(result_len) := '"';
									end if;
								end if;
							else
								-- STRINGIFY 
								result(insertidx) := '"';
								result_len := result_len + 1;
								if result_len <= MAX_STRING_LEN then 
									result(result_len) := '"';
								end if;
							end if;
						end if;
					end if;
					i := i + identifier'length;
				else
					-- Copier le caractère actuel
					result_len := result_len + 1;
					if result_len <= MAX_STRING_LEN then 
						result(result_len) := source(i);
					end if;
					i := i + 1;
				end if;
			else 
				-- if inside quote
				-- Copier le caractère actuel
				result_len := result_len + 1;
				if result_len <= MAX_STRING_LEN then 
					result(result_len) := source(i);
				end if;
				i := i + 1;
			end if;
		end loop;
    
		return result;
		
	end function;

    function string_length(s: string) return integer is
    begin
        for i in s'high downto s'low loop
            if s(i) /= ' ' then
                return i;
            end if;
        end loop;
        return 0;
    end function;

    function index_char(s: string; c: character) return integer is
    begin
        for i in s'range loop
            if s(i) = c then
                return i;
            end if;
        end loop;
        return 0;
    end function;

    function index_substring(s: string; substr: string) return integer is
        variable i, j: integer;
        variable match: boolean;
    begin
        if substr'length = 0 or s'length < substr'length then
            return 0;
        end if;
        for i in s'low to s'high - substr'length + 1 loop
            match := true;
            for j in 0 to substr'length-1 loop
                if s(i+j) /= substr(substr'low+j) then
                    match := false;
                    exit;
                end if;
            end loop;
            if match then
                return i;
            end if;
        end loop;
        return 0;
    end function;

    function rindex(s: string; c: character) return integer is
    begin
        for i in s'high downto s'low loop
            if s(i) = c then
                return i;
            end if;
        end loop;
        return 0;
    end function;

    function min(a, b: natural) return natural is
    begin
        if a < b then return a; else return b; end if;
    end function;

    function extract_dir(filepath: string) return string is
        variable pos: integer := filepath'high;
    begin
        while pos >= filepath'low loop
            if filepath(pos) = '/' or filepath(pos) = '\' then
                return filepath(filepath'low to pos);
            end if;
            pos := pos - 1;
        end loop;
        return "./";
    end function;

    procedure extract_paths(
        paths_in  : in  string;
        paths_out : out t_string_array;
        count     : out natural
    ) is
        variable start : integer := paths_in'low;
        variable pos   : integer := paths_in'low;
        variable idx   : natural := 0;
        variable len   : natural;
    begin
        count := 0;
        while pos <= paths_in'high and idx < MAX_SEARCH_PATHS loop
            if paths_in(pos) = ';' then
                len := pos - start;
                if len > 0 then
                    len := min(len, MAX_STRING_LEN);
                    paths_out(idx) := (others => ' ');
                    paths_out(idx)(1 to len) := paths_in(start to start + len - 1);
                    idx := idx + 1;
                end if;
                start := pos + 1;
            end if;
            pos := pos + 1;
        end loop;
        
        -- Dernier chemin
        len := pos - start;
        if len > 0 and idx < MAX_SEARCH_PATHS then
            len := min(len, MAX_STRING_LEN);
            paths_out(idx) := (others => ' ');
            paths_out(idx)(1 to len) := paths_in(start to start + len - 1);
            idx := idx + 1;
        end if;
        
        count := idx;
    end procedure;

    impure function file_exists(filename: string) return boolean is
        file f: text;
        variable status: file_open_status;
    begin
        file_open(status, f, filename, read_mode);
        if status = open_ok then
            file_close(f);
            return true;
        end if;
        return false;
    end function;

    impure function find_file(
        filename    : string;
        current_dir : string;
        search_paths: t_string_array;
        search_cnt  : natural
    ) return string is
        variable full_path : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable len       : natural;
        variable copy_len  : natural;
    begin
        -- Vérifier dans le répertoire courant
        if current_dir'length > 0 then
            len := min(current_dir'length, MAX_STRING_LEN - 1);
            if len > 0 then
                full_path(1 to len) := current_dir(1 to len);
                --  full_path(len + 1) := '/';    -- Path already has a final "/"
                copy_len := min(filename'length, MAX_STRING_LEN - len - 1);
                if copy_len > 0 then
                    full_path(len + 1 to len + copy_len) := filename(1 to copy_len);
                end if;
                if file_exists(trim_string(full_path)) then
                    return full_path;
                end if;
            end if;
        end if;

        -- Vérifier dans les chemins de recherche
        for i in 0 to search_cnt - 1 loop
            if string_length(search_paths(i)) > 0 then
                len := min(string_length(search_paths(i)), MAX_STRING_LEN - 1);
                if len > 0 then
                    full_path(1 to len) := search_paths(i)(1 to len);
                    full_path(len + 1) := '/';
                    copy_len := min(string_length(filename), MAX_STRING_LEN - len - 1);
                    if copy_len > 0 then
                        full_path(len + 2 to len + 1 + copy_len) := filename(1 to copy_len);
                    end if;
                    if file_exists(trim_string(full_path)) then
                        return full_path;
                    end if;
                end if;
            end if;
        end loop;
		full_path := (others => ' ');
        return full_path;  -- Non trouvé
    end function;

    -- Fonction pour supprimer les commentaires
    procedure remove_comments(
        line_in     : in string;
        in_comment  : inout boolean;
		line_out    : out string 
    )  is 
        variable result    : string(1 to MAX_LINE_LEN) := (others => ' ');
        variable pos       : natural := 1;
        variable i         : natural := line_in'low;
        variable in_string : boolean := false;
    begin
        -- Si on est déjà dans un commentaire multi-ligne
        if in_comment then
            i := index_substring(line_in, "*/");
            if i > 0 then
                i := i + 2; -- Passer après le */
				in_comment := false;
            else
                -- Commentaire non fermé, ignorer toute la ligne
				result := ( others => ' ' );
				
            end if;
        else 

            while i <= line_in'high loop
                -- Gestion des chaînes de caractères (les commentaires dans les chaînes sont ignorés)
                if line_in(i) = '"' then
                    in_string := not in_string;
                    if pos <= MAX_LINE_LEN then
                        result(pos) := line_in(i);
                        pos := pos + 1;
                    end if;
                    i := i + 1;
                elsif in_string then
                    if pos <= MAX_LINE_LEN then
                        result(pos) := line_in(i);
                        pos := pos + 1;
                    end if;
                    i := i + 1;
                -- Commentaire en ligne (//)
                elsif i < line_in'high and line_in(i) = '/' and line_in(i+1) = '/' then
                    -- Ignorer le reste de la ligne
                    exit;
                -- Commentaire en ligne (--)
                elsif i < line_in'high and line_in(i) = '-' and line_in(i+1) = '-' then
                    -- Ignorer le reste de la ligne
                    exit;
                -- Commentaire multi-ligne (/*)
                elsif i < line_in'high and line_in(i) = '/' and line_in(i+1) = '*' then
                    i := i + 2; -- Passer après le /*
					in_comment := true;
                    while i <= line_in'high loop
                        if i < line_in'high and line_in(i) = '*' and line_in(i+1) = '/' then
                            i := i + 2; -- Passer après le */
							in_comment := false;
                            exit;
                        end if;
                        i := i + 1;
                    end loop;
                else
                    if pos <= MAX_LINE_LEN then
                        result(pos) := line_in(i);
                        pos := pos + 1;
                    end if;
                    i := i + 1;
                end if;
            end loop;
		end if;
        -- if pos = 1 then
        --    return "";
        -- else
		line_out := result;
        -- end if;
    end remove_comments;

    -- Fonctions de gestion des macros
    procedure add_macro(
        state: inout t_preprocessor_state;
        name : in string;
        value: in string;
        has_args: in boolean := false;
        is_variadic: in boolean := false;
        arg_names: in string := "";
        arg_count: in natural := 0
    ) is
        variable name_len : natural := min(name'length, MAX_STRING_LEN);
        variable value_len : natural := min(value'length, MAX_STRING_LEN);
    begin
        if state.macro_count < MAX_MACROS then
            state.macro_table(state.macro_count).name := (others => ' ');
            if name_len > 0 then
                state.macro_table(state.macro_count).name(1 to name_len) := name(1 to name_len);
            end if;
            
            state.macro_table(state.macro_count).value := (others => ' ');
            if value_len > 0 then
                state.macro_table(state.macro_count).value(1 to value_len) := value; -- (1 to value_len);
            end if;
            
            state.macro_table(state.macro_count).has_args := has_args;
            state.macro_table(state.macro_count).is_variadic := is_variadic;
            state.macro_table(state.macro_count).arg_names := (others => ' ');
            if has_args and arg_names'length > 0 then
                state.macro_table(state.macro_count).arg_names(1 to min(arg_names'length, MAX_STRING_LEN)) := 
                    arg_names(1 to min(arg_names'length, MAX_STRING_LEN));
            end if;
            state.macro_table(state.macro_count).arg_count := arg_count;
			if is_variadic then 
				state.macro_table(state.macro_count).arg_count := arg_count - 1;
			end if;
            state.macro_table(state.macro_count).defined := true;
            state.macro_count := state.macro_count + 1;
        end if;
    end procedure;

    function find_macro(
        state: t_preprocessor_state;
        name : string
    ) return natural is
        variable name_len : natural := name'length;
    begin
        for i in 0 to state.macro_count - 1 loop
            if state.macro_table(i).defined and 
               state.macro_table(i).name(1 to min(name_len, MAX_STRING_LEN)) = name(1 to min(name_len, MAX_STRING_LEN)) and 
			   state.macro_table(i).name(min(name_len+1, MAX_STRING_LEN)) = ' ' then
                return i;
            end if;
        end loop;
        return MAX_MACROS;  -- Not found
    end function;

    procedure remove_macro(
        state: inout t_preprocessor_state;
        name : in string
    ) is
        variable idx: natural;
    begin
        idx := find_macro(state, name);
        if idx < MAX_MACROS then
            state.macro_table(idx).defined := false;
        end if;
    end procedure;

    function expand_macro_call(
        state: t_preprocessor_state;
        macro_idx: natural;
        call_args: string;
        file_dir: string
    ) return string is
        variable value : string(1 to MAX_STRING_LEN) := state.macro_table(macro_idx).value;
        variable arg_names : string(1 to MAX_STRING_LEN) := state.macro_table(macro_idx).arg_names;
		variable arg_names_array: t_args_array := (others => (others => ' '));
        variable args : t_args_array := (others => (others => ' '));
        variable arg_cnt : natural := 0;
        variable start, pos, paren_depth : natural;
        variable in_quotes : boolean := false;
        variable result : string(1 to MAX_STRING_LEN) := value;
        variable tmp : string(1 to MAX_STRING_LEN);
        variable va_args : string(1 to MAX_STRING_LEN) := (others => ' ');
		variable va_args_index: natural := 1;  -- Nouvel index de position
        variable fixed_arg_count : natural := state.macro_table(macro_idx).arg_count;
        variable copy_len : natural;
		variable va_args_trimmed: line;
    begin
        -- Parser les arguments
        start := call_args'low;
        pos := call_args'low;
        paren_depth := 0;
        
        while pos <= call_args'high loop
            if call_args(pos) = '"' then
                in_quotes := not in_quotes;
            elsif not in_quotes then
                if call_args(pos) = '(' then
                    paren_depth := paren_depth + 1;
                elsif call_args(pos) = ')' then
                    paren_depth := paren_depth - 1;
                elsif call_args(pos) = ',' and paren_depth = 0 then
                    if arg_cnt < MAX_MACRO_ARGS - 1 then
                        copy_len := min(pos - start, MAX_STRING_LEN);
                        if copy_len > 0 then
                            args(arg_cnt)(1 to copy_len) := call_args(start to start + copy_len - 1);
                        end if;
                        arg_cnt := arg_cnt + 1;
                        start := pos + 1;
                    end if;
                end if;
            end if;
            pos := pos + 1;
        end loop;
        
        -- Dernier argument
        if start <= call_args'high then
            if arg_cnt < MAX_MACRO_ARGS then
                copy_len := min(call_args'high - start + 1, MAX_STRING_LEN);
                if copy_len > 0 then
                    args(arg_cnt)(1 to copy_len) := call_args(start to start + copy_len - 1);
                end if;
                arg_cnt := arg_cnt + 1;
            end if;
        end if;
        
        -- Pour les macros variadiques, le dernier argument est __VA_ARGS__

		if state.macro_table(macro_idx).is_variadic and arg_cnt > fixed_arg_count then
			va_args := (others => ' ');  -- Réinitialiser
			for i in fixed_arg_count to arg_cnt - 1 loop
				if i > fixed_arg_count then
					if va_args_index <= MAX_STRING_LEN then
						va_args(va_args_index) := ',';
						va_args_index := va_args_index + 1;
					end if;
				end if;
				if va_args_index <= MAX_STRING_LEN then
					va_args_trimmed := new string'(trim_string(args(i)));				
					copy_len := min(va_args_trimmed'length, MAX_STRING_LEN - va_args_index + 1);
					if copy_len > 0 then
						va_args(va_args_index to va_args_index + copy_len - 1) := va_args_trimmed(1 to copy_len);
						va_args_index := va_args_index + copy_len;
					end if;
					deallocate(va_args_trimmed);				
				end if;
			end loop;
		end if;		
        
		-- Extraire les noms de chaque argument
		start := 1;
		pos := 1;
		for i in 0 to fixed_arg_count-2 loop
			while arg_names(start) /= ',' loop
				arg_names_array(i)(pos) := arg_names(start);
				start := start+1;
				pos := pos+1;
			end loop;
			start:=start+1; -- jump over the ','
			pos:=1; -- First char of next arg wil be at offset 0
		end loop;
		-- last arg is the remaining of the string
		while start<= MAX_STRING_LEN loop 
			arg_names_array(fixed_arg_count-1)(pos) := arg_names(start);
			start := start+1;
			pos := pos+1;
		end loop;
		
		
        for i in 0 to fixed_arg_count - 1 loop
            tmp := result;
            result := replace_arg( tmp , trim_string(arg_names_array(i)), trim_string(args(i)));
		end loop;
--         -- Remplacer les arguments dans la valeur de la macro
--         for i in 0 to fixed_arg_count - 1 loop
--             tmp := result;
--             result := (others => ' ');
--             start := 1;
--             pos := 1;
--             
--             while pos <= tmp'high and pos <= MAX_STRING_LEN loop
--                 if tmp(pos) = arg_names(i * MAX_STRING_LEN + 1) then
--                     -- Vérifier si c'est bien le nom complet de l'argument
--                     copy_len := min(args(i)'length, MAX_STRING_LEN - start + 1);
--                     if tmp(pos to min(pos + args(i)'length - 1, MAX_STRING_LEN)) = args(i)(1 to copy_len) then
--                         result(start to start + copy_len - 1) := args(i)(1 to copy_len);
--                         start := start + copy_len;
--                         pos := pos + copy_len;
--                     else
--                         result(start) := tmp(pos);
--                         start := start + 1;
--                         pos := pos + 1;
--                     end if;
--                 else
--                     result(start) := tmp(pos);
--                     start := start + 1;
--                     pos := pos + 1;
--                 end if;
--             end loop;
--         end loop;
        
        -- Remplacer __VA_ARGS__
        if state.macro_table(macro_idx).is_variadic then
            tmp := result;
            result := (others => ' ');
            start := 1;
            pos := 1;
			va_args_trimmed := new string'( trim_string(va_args) );            
            while pos <= tmp'high and pos <= MAX_STRING_LEN and start<= MAX_STRING_LEN loop
                if pos <= tmp'high - 10 and tmp(pos to pos + 10) = "__VA_ARGS__" then
                    copy_len := min(va_args_trimmed'length, MAX_STRING_LEN - start + 1);
                    if copy_len > 0 then
                        result(start to start + copy_len - 1) := va_args_trimmed(1 to copy_len);
                    end if;
                    start := start + copy_len;
                    pos := pos + 11;
                else
                    result(start) := tmp(pos);
                    start := start + 1;
                    pos := pos + 1;
                end if;
            end loop;
			deallocate(va_args_trimmed);
        end if;
        
        return result(1 to MAX_STRING_LEN);
    end function;

    function expand_macros(
        state : t_preprocessor_state;
        iline  : string;
        file_dir: string
    ) return string is
        variable result    : string(1 to MAX_LINE_LEN) := (others => ' ');
        variable pos      : natural := 1;
        variable i        : natural := iline'low;
        variable start    : natural;
        variable in_word  : boolean := false;
        variable ident    : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable ident_len: natural;
        variable macro_idx: natural;
        variable expanded : line;
        variable exp_len  : natural;
        variable has_args : boolean;
        variable call_start, call_end : natural;
        variable paren_depth : natural;
        variable call_args  : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable copy_len   : natural;
    begin
        while i <= iline'high loop
            if not in_word and (iline(i) = '_' or (iline(i) >= 'a' and iline(i) <= 'z') or (iline(i) >= 'A' and iline(i) <= 'Z')) then
                in_word := true;
                start := i;
                i := i + 1;
            elsif in_word and not (iline(i) = '_' or (iline(i) >= 'a' and iline(i) <= 'z') or (iline(i) >= 'A' and iline(i) <= 'Z') or (iline(i) >= '0' and iline(i) <= '9')) then
                in_word := false;
                ident := (others => ' ');
                ident_len := min(i - start, MAX_STRING_LEN);
                if ident_len > 0 then
                    ident(1 to ident_len) := iline(start to start + ident_len - 1);
                end if;
                
                -- Macro spéciale __FILE__
                if ident_len = 8 and ident(1 to 8) = "__FILE__" then
                    expanded := new string'( '"'& trim_string( state.file_stack(state.file_top).filename ) & '"' );
                    exp_len := expanded'length;
                -- Macro spéciale __LINE__
                elsif ident_len = 8 and ident(1 to 8) = "__LINE__" then
                    expanded := new string'( integer'image(state.file_stack(state.file_top).line_num) );
                    exp_len := expanded'length;
                -- Macro normale
                else
                    macro_idx := find_macro(state, ident(1 to ident_len));
                    if macro_idx < MAX_MACROS then
                        has_args := state.macro_table(macro_idx).has_args;
                        
                        -- Vérifier si c'est un appel de macro avec arguments
                        if has_args and i <= iline'high and iline(i) = '(' then
                            -- Trouver les arguments
                            paren_depth := 1;
                            call_start := i + 1;
                            call_end := call_start;
                            
                            while call_end <= iline'high and paren_depth > 0 loop
                                if iline(call_end) = '(' then
                                    paren_depth := paren_depth + 1;
                                elsif iline(call_end) = ')' then
                                    paren_depth := paren_depth - 1;
                                end if;
                                call_end := call_end + 1;
                            end loop;
                            call_args := (others => ' '); -- needed if multiple macro calls on one line
                            if paren_depth = 0 then
                                copy_len := min((call_end - 2) - (i + 1) + 1, MAX_STRING_LEN);
                                if copy_len > 0 then
                                    call_args(1 to copy_len) := iline(i + 1 to i + 1 + copy_len - 1);
                                end if;
                                expanded := new string'( remove_double_dash(trim_string( expand_macro_call(state, macro_idx, trim_string(call_args), file_dir) ) ) );
                                exp_len := expanded'length;
                                i := call_end ; -- ESSAI -- - 1;  -- Avancer l'index principal
                            else
                                expanded := new string'( ident(1 to ident_len) );
                                exp_len := ident_len;
                            end if;
                        else
                            expanded := new string'( trim_string(state.macro_table(macro_idx).value ) );
                            exp_len := expanded'length;
                        end if;
                    else
                        expanded := new string'( ident(1 to ident_len) );
                        exp_len := expanded'length;
                    end if;
                end if;
                
                -- Ajouter l'expansion au résultat
                if exp_len > 0 then
                    exp_len := min(exp_len, MAX_LINE_LEN - pos + 1);
                    if exp_len > 0 then
                        result(pos to pos + exp_len - 1) := expanded(1 to exp_len);
                        pos := pos + exp_len;
                    end if;
                end if;
                if i <= iline'high then
                    if pos <= MAX_LINE_LEN then
                        result(pos) := iline(i);
                        pos := pos + 1;
                    end if;
                end if;
				deallocate( expanded );
                i := i + 1;
            elsif in_word then
                i := i + 1;  -- Continue à accumuler
            else
                if pos <= MAX_LINE_LEN then
                    result(pos) := iline(i);
                    pos := pos + 1;
                end if;
                i := i + 1;
            end if;
        end loop;
        if pos = 1 then
            return "";
        else
            return result(1 to pos - 1);
        end if;
    end function;

    -- Fonctions principales de traitement
    procedure handle_define(
        state: inout t_preprocessor_state;
        line : string
    ) is
        variable name_start: natural;
        variable name_end  : natural;
        variable name      : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable value     : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable args_start: natural;
        variable args_end  : natural;
        variable args_str  : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable is_variadic: boolean := false;
        variable has_args  : boolean := false;
        variable arg_count : natural := 0;
        variable paren_depth: natural;
        variable j: natural;
        variable len: natural;
        variable copy_len  : natural;
    begin
        -- Trouver le début du nom
        name_start := 8;  -- Après "#define "
        while name_start <= line'high and is_space(line(name_start)) loop
            name_start := name_start + 1;
        end loop;
        
        -- Trouver la fin du nom
        name_end := name_start;
        while name_end <= line'high and not is_space(line(name_end)) and 
              line(name_end) /= '(' loop
            name_end := name_end + 1;
        end loop;
        
        -- Extraire le nom
        len := min(name_end - name_start, MAX_STRING_LEN);
        if len > 0 then
            name(1 to len) := line(name_start to name_start + len - 1);
        end if;
        
        -- Vérifier les arguments
        if name_end <= line'high and line(name_end) = '(' then
            has_args := true;
            args_start := name_end + 1;
            args_end := args_start;
            paren_depth := 1;
            
            -- Trouver la fin des arguments
            while args_end <= line'high and paren_depth > 0 loop
                if line(args_end) = '(' then
                    paren_depth := paren_depth + 1;
                elsif line(args_end) = ')' then
                    paren_depth := paren_depth - 1;
                end if;
                args_end := args_end + 1;
            end loop;
            
            if paren_depth = 0 then
                len := min(args_end - 2 - args_start + 1, MAX_STRING_LEN);
                if len > 0 then
                    args_str(1 to len) := line(args_start to args_start + len - 1);
                end if;
                
                len := min(line'high - args_end + 1, MAX_STRING_LEN);
                if len > 0 then
                    value(1 to len) := line(args_end to args_end + len - 1);
                end if;
                
                -- Vérifier si macro variadique
                if index_substring(args_str, "...") > 0 then
                    is_variadic := true;
                end if;
                
                -- Compter les arguments
                arg_count := 1;
                for j in args_str'low to min(args_str'high, MAX_STRING_LEN) loop
                    if j <= args_str'high and args_str(j) = ',' then
                        arg_count := arg_count + 1;
                    end if;
                end loop;
            else
                len := min(line'high - name_end + 1, MAX_STRING_LEN);
                if len > 0 then
                    value(1 to len) := line(name_end to name_end + len - 1);
                end if;
            end if;
        else
            len := min(line'high - name_end + 1, MAX_STRING_LEN);
            if len > 0 then
                value(1 to len) := line(name_end to name_end + len - 1);
            end if;
        end if;
        
        add_macro(
            state, 
            name, 
            trim_string(value),
            has_args,
            is_variadic,
            args_str,
            arg_count
        );
    end procedure;

    procedure handle_append(
        state: inout t_preprocessor_state;
        iline : string
    ) is
        variable name_start: natural;
        variable name_end  : natural;
        variable name      : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable value     : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable args_start: natural;
        variable args_end  : natural;
        variable args_str  : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable is_variadic: boolean := false;
        variable has_args  : boolean := false;
        variable arg_count : natural := 0;
        variable paren_depth: natural;
        variable j: natural;
        variable len: natural;
        variable copy_len  : natural;
		
		variable idx: natural;
		variable expanded: line;
    begin
        -- Trouver le début du nom
        name_start := 8;  -- Après "#append "
        while name_start <= iline'high and is_space(iline(name_start)) loop
            name_start := name_start + 1;
        end loop;
        
        -- Trouver la fin du nom
        name_end := name_start;
        while name_end <= iline'high and not is_space(iline(name_end)) and 
              iline(name_end) /= '(' loop
            name_end := name_end + 1;
        end loop;
        
        -- Extraire le nom
        len := min(name_end - name_start, MAX_STRING_LEN);
        if len > 0 then
            name(1 to len) := iline(name_start to name_start + len - 1);
        end if;
        
		-- On considere d'office que ce n'est pas une macro mais un simple define... 
		
        len := min(iline'high - name_end + 1, MAX_STRING_LEN);
        if len > 0 then
           value(1 to len) := iline(name_end to name_end + len - 1);
        end if;
        
		idx := find_macro(state, name);
		if idx < MAX_MACROS then
		    -- Ici la macro existe, on va ajouter  value au contenu.
			expanded := new string'( trim_string(state.macro_table(idx).value ) & trim_string(value));
			state.macro_table(idx).value := extend_string(trim_string(expanded.all),MAX_STRING_LEN);
			deallocate(expanded);
		end if;
    end procedure;

    procedure handle_prepend(
        state: inout t_preprocessor_state;
        iline : string
    ) is
        variable name_start: natural;
        variable name_end  : natural;
        variable name      : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable value     : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable args_start: natural;
        variable args_end  : natural;
        variable args_str  : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable is_variadic: boolean := false;
        variable has_args  : boolean := false;
        variable arg_count : natural := 0;
        variable paren_depth: natural;
        variable j: natural;
        variable len: natural;
        variable copy_len  : natural;
		
		variable idx: natural;
		variable expanded: line;
    begin
        -- Trouver le début du nom
        name_start := 9;  -- Après "#prepend "
        while name_start <= iline'high and is_space(iline(name_start)) loop
            name_start := name_start + 1;
        end loop;
        
        -- Trouver la fin du nom
        name_end := name_start;
        while name_end <= iline'high and not is_space(iline(name_end)) and 
              iline(name_end) /= '(' loop
            name_end := name_end + 1;
        end loop;
        
        -- Extraire le nom
        len := min(name_end - name_start, MAX_STRING_LEN);
        if len > 0 then
            name(1 to len) := iline(name_start to name_start + len - 1);
        end if;
        
		-- On considere d'office que ce n'est pas une macro mais un simple define... 
		
        len := min(iline'high - name_end + 1, MAX_STRING_LEN);
        if len > 0 then
           value(1 to len) := iline(name_end to name_end + len - 1);
        end if;
        
		idx := find_macro(state, name);
		if idx < MAX_MACROS then
		    -- Ici la macro existe, on va ajouter  value au contenu.
			expanded := new string'( trim_string(value) & " " & trim_string(state.macro_table(idx).value ) );
			state.macro_table(idx).value := extend_string(trim_string(expanded.all),MAX_STRING_LEN);
			deallocate(expanded);
		end if;
    end procedure;

    procedure handle_include(
        state       : inout t_preprocessor_state;
        line        : in string;
        search_paths: in t_string_array;
        search_cnt  : in natural;
		file output_file : text 
    ) is
        variable filename : string(1 to MAX_STRING_LEN) := (others => ' ');
        variable full_path: string(1 to MAX_STRING_LEN);
        variable start, fin: integer;
        variable quote_char: character;
        variable len: natural;
    begin
        -- Trouver le premier caractère après "#include"
        start := 9; -- Après "#include"
        while start <= line'high and is_space(line(start)) loop
            start := start + 1;
        end loop;
        
        if start > line'high then
            return; -- Format invalide
        end if;
        
        quote_char := line(start);
        if quote_char = '"' or quote_char = '<' then
            fin := start + 1;
            while fin <= line'high and line(fin) /= '"' and line(fin) /= '>' loop
                fin := fin + 1;
            end loop;
            
            if fin > line'high then
                return; -- Format invalide
            end if;
            
            len := min(fin - start - 1, MAX_STRING_LEN);
            if len > 0 then
                filename(1 to len) := line(start + 1 to start + len);
            end if;
        else
            return; -- Format invalide
        end if;

        -- Trouver le fichier
        if quote_char = '"' then
            full_path := find_file(
                filename => trim_string(filename),
                current_dir => trim_string(state.file_stack(state.file_top).dir),
                search_paths => search_paths,
                search_cnt => search_cnt
            );
        else -- '<'
            full_path := find_file(
                filename => trim_string(filename),
                current_dir => "",
                search_paths => search_paths,
                search_cnt => search_cnt
            );
        end if;

        -- Vérifier si le chemin est non vide
        if string_length(full_path) > 0 and full_path(1) /= ' ' then
            -- Empiler le nouveau fichier
            if state.file_top < MAX_INCLUDE_DEPTH - 1 then
                state.file_top := state.file_top + 1;
                state.file_stack(state.file_top).filename := full_path;
                state.file_stack(state.file_top).dir := extend_string(extract_dir(full_path),MAX_STRING_LEN);
                state.file_stack(state.file_top).line_num := 0;
                state.file_stack(state.file_top).in_comment := false;
				
				-- recursively call process_file 
				process_file(state, output_file , state.search_paths, state.search_path_cnt);
	
				
            end if;
        end if;
    end procedure;

    procedure handle_ifdef(
        state: inout t_preprocessor_state;
        line : string;
        is_ifndef: boolean := false
    ) is
        variable macro_name: string(1 to MAX_STRING_LEN) := (others => ' ');
        variable start, fin: natural;
        variable defined  : boolean;
        variable len: natural;
    begin
        -- Trouver le nom de la macro
        if is_ifndef then
            start := 8; -- Après "#ifndef "
        else
            start := 7; -- Après "#ifdef "
        end if;
        
        while start <= line'high and is_space(line(start)) loop
            start := start + 1;
        end loop;
        
        fin := start;
        while fin <= line'high and not is_space(line(fin)) loop
            fin := fin + 1;
        end loop;
        
        len := min(fin - start, MAX_STRING_LEN);
        if len > 0 then
            macro_name(1 to len) := line(start to start + len - 1);
        end if;
        
        -- Vérifier si la macro est définie
        defined := (find_macro(state, macro_name(1 to len)) < MAX_MACROS);
        if is_ifndef then
            defined := not defined;
        end if;
        
        -- Empiler l'état conditionnel
        if state.cond_top < MAX_CONDITIONAL_DEPTH - 1 then
            state.cond_top := state.cond_top + 1;
            state.cond_stack(state.cond_top).active := true;
            state.cond_stack(state.cond_top).if_active := defined;
            state.cond_stack(state.cond_top).else_found := false;
        end if;
    end procedure;
	
	-- This one is called when in a section that is anyway deactivated...
    procedure handle_ifdef_forced_off(
        state: inout t_preprocessor_state;
        line : string;
        is_ifndef: boolean := false
    ) is
        variable defined  : boolean;
    begin
        -- Forcer de maniere inconditionelle l'etat OFF 
        defined := false; 
        -- Empiler l'état conditionnel
        if state.cond_top < MAX_CONDITIONAL_DEPTH - 1 then
            state.cond_top := state.cond_top + 1;
            state.cond_stack(state.cond_top).active := false;
            state.cond_stack(state.cond_top).if_active := false;
            state.cond_stack(state.cond_top).else_found := false;
        end if;
    end procedure;

    procedure handle_else(
        state: inout t_preprocessor_state
    ) is
    begin
        if state.cond_top > 0 then
            state.cond_stack(state.cond_top).else_found := true;
            state.cond_stack(state.cond_top).if_active := 
                not state.cond_stack(state.cond_top).if_active;
        end if;
    end procedure;

    procedure handle_endif(
        state: inout t_preprocessor_state
    ) is
    begin
        if state.cond_top > 0 then
            state.cond_top := state.cond_top - 1;
        end if;
    end procedure;

    procedure handle_undef(
        state: inout t_preprocessor_state;
        line : string
    ) is
        variable macro_name: string(1 to MAX_STRING_LEN) := (others => ' ');
        variable start, fin: natural;
        variable len: natural;
    begin
        -- Trouver le nom de la macro
        start := 7; -- Après "#undef "
        while start <= line'high and is_space(line(start)) loop
            start := start + 1;
        end loop;
        
        fin := start;
        while fin <= line'high and not is_space(line(fin)) loop
            fin := fin + 1;
        end loop;
        
        len := min(fin - start, MAX_STRING_LEN);
        if len > 0 then
            macro_name(1 to len) := line(start to start + len - 1);
            remove_macro(state, macro_name(1 to len));
        end if;
    end procedure;

    function is_condition_active(state: t_preprocessor_state) return boolean is
    begin
        if state.cond_top = 0 then
            return true; -- Pas de conditionnel actif
        end if;
        
        for i in 1 to state.cond_top loop
            if not state.cond_stack(i).active then
                return false;
            end if;
        end loop;
        
        return state.cond_stack(state.cond_top).if_active;
    end function;

    function starts_with(s: string; prefix: string) return boolean is
    begin
        if s'length < prefix'length then
            return false;
        else
            return s(1 to prefix'length) = prefix;
        end if;
    end function;

    -- Procédure principale de traitement de fichier
    procedure process_file(
        state        : inout t_preprocessor_state;
        file output_file  : text;
        search_paths : in t_string_array;
        search_cnt   : in natural
    ) is
        file input_file     : text;
        variable input_line : line;
        variable output_line: line;
        constant filename   : string := trim_string( state.file_stack(state.file_top).filename );
        variable line_str   : string(1 to MAX_LINE_LEN) := (others => ' ');
        variable cleaned_line: string(1 to MAX_LINE_LEN) := (others => ' ');
		variable cleaned_line_trimed : line;
        variable status     : file_open_status;
        variable len: natural;
    begin
        -- filename := state.file_stack(state.file_top).filename;
        file_open(status, input_file, filename, read_mode);
        if status /= open_ok then
            report "Cannot open file: " & filename severity error;
            return;
        end if;

        while not endfile(input_file) loop
            readline(input_file, input_line);
            state.file_stack(state.file_top).line_num := state.file_stack(state.file_top).line_num + 1;
            
            -- Lire la ligne et la tronquer
            if input_line /= null and input_line'length > 0 then
                len := min(input_line'length, MAX_LINE_LEN);
                line_str := (others => ' ');
                line_str(1 to len) := input_line(1 to len);
                
                -- Supprimer les commentaires
                remove_comments(
                    line_in => line_str,
                    in_comment => state.file_stack(state.file_top).in_comment,
					line_out => cleaned_line
                );
				
--                cleaned_line := remove_comments(
--                    line => line_str,
--                    in_comment => state.file_stack(state.file_top).in_comment
--                );

                
                cleaned_line_trimed := new string'( trim_string(cleaned_line) );

                -- Traitement des directives uniquement si condition active
                if is_condition_active(state) then
                    if cleaned_line_trimed'length > 0 and cleaned_line(1) = '#' then
                        if starts_with(cleaned_line, "#define ") then
                            handle_define(state, cleaned_line);
                        elsif starts_with(cleaned_line, "#append ") then
                            handle_append(state, cleaned_line);
                        elsif starts_with(cleaned_line, "#prepend ") then
                            handle_prepend(state, cleaned_line);
                        elsif starts_with(cleaned_line, "#include") then
                            handle_include(state, cleaned_line, search_paths, search_cnt, output_file );
                        elsif starts_with(cleaned_line, "#ifdef ") then
                            handle_ifdef(state, cleaned_line);
                        elsif starts_with(cleaned_line, "#ifndef ") then
                            handle_ifdef(state, cleaned_line, true);
                        elsif starts_with(cleaned_line, "#else") then
                            handle_else(state);
                        elsif starts_with(cleaned_line, "#endif") then
                            handle_endif(state);
                        elsif starts_with(cleaned_line, "#undef ") then
                            handle_undef(state, cleaned_line);
                        end if;
                    else
                        -- Expansion des macros et écriture
                        output_line := new string'(trim_string(expand_macros(state, cleaned_line, state.file_stack(state.file_top).filename)));
                        writeline(output_file, output_line);
                        deallocate(output_line);
                    end if;
                else
                    -- Traitement des directives conditionnelles même si inactif
                    if cleaned_line_trimed'length > 0 and cleaned_line(1) = '#' then
                        if starts_with(cleaned_line, "#ifdef ") then
                            handle_ifdef_forced_off(state, cleaned_line);
                        elsif starts_with(cleaned_line, "#ifndef ") then
                            handle_ifdef_forced_off(state, cleaned_line, true);
                        elsif starts_with(cleaned_line, "#else") then
                            handle_else(state);
                        elsif starts_with(cleaned_line, "#endif") then
                            handle_endif(state);
                        end if;
                    end if;
                end if;
				deallocate(cleaned_line_trimed);
            end if;
        end loop;

        file_close(input_file);
        state.file_top := state.file_top - 1;  -- Dépiler
    end procedure;

    -- Procédure principale d'interface
    procedure preprocess_file(
        input_filename  : in string;
        output_filename : in string;
        search_paths    : in string := ""
    ) is
        variable state         : t_preprocessor_state;
        file output_file       : text;
        variable search_paths_arr: t_string_array;
        variable search_cnt   : natural;
        variable status       : file_open_status;
        variable len: natural;
		
--        file zf: text;
--        variable zfstatus: file_open_status;
--		variable zfn : string( := "./inc1.asm";
    begin
--        file_open(zfstatus, zf, zfn, read_mode);
--        if status = open_ok then
--			file_close(zf);
--		end if;
		
        -- Initialisation de l'état
        state.macro_count := 0;
        state.cond_top := 0;
        state.file_top := 0;
        state.search_path_cnt := 0;

        -- Initialiser la pile de fichiers
        state.file_stack(0).filename := (others => ' ');
        len := min(input_filename'length, MAX_STRING_LEN);
        if len > 0 then
            state.file_stack(0).filename(1 to len) := input_filename(1 to len);
        end if;
        state.file_stack(0).dir := extend_string(extract_dir(input_filename),MAX_STRING_LEN );
        state.file_stack(0).line_num := 0;
        state.file_stack(0).in_comment := false;  -- Initialiser l'état des commentaires
        state.file_top := 0;

        -- Configurer les chemins de recherche
        extract_paths(search_paths, search_paths_arr, search_cnt);
        state.search_paths := search_paths_arr;
        state.search_path_cnt := search_cnt;

        -- Ajouter les macros prédéfinies
        add_macro(state, "__FILE__", "", false);
        add_macro(state, "__LINE__", "", false);

        -- Ouvrir le fichier de sortie
        file_open(status, output_file, output_filename, write_mode);
        if status /= open_ok then
            report "Cannot open output file: " & output_filename severity error;
            return;
        end if;

        -- Traiter tous les fichiers (pile)
        while state.file_top >= 0 loop
            process_file(state, output_file, state.search_paths, state.search_path_cnt);
        end loop;

        file_close(output_file);
    end procedure;

end package body;