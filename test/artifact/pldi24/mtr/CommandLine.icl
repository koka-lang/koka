implementation module CommandLine

import StdInt, StdList, StdEnum, StdMisc, StdEnv
from StdArray import class Array

// Change if necessary!
IF_POSIX_OR_WINDOWS posix windows   :== posix

:: Pointer	:== Int
:: Offset	:== Int

readInt4Z :: !Pointer !Offset -> Int
readInt4Z pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

derefInt :: !Pointer -> Int
derefInt ptr = code {
		load_i 0
}

readInt :: !Pointer !Offset -> Int
readInt pointer offset = IF_INT_64_OR_32 (readInt_64 pointer offset) (readInt_32 pointer offset)

readInt_64 :: !Pointer !Offset -> Int
readInt_64 pointer offset = code {
		pop_b 1
|		mov    (%rbx,%rax,1),%rax
		instruction 72
	 	instruction 139
	 	instruction 4
	 	instruction 3
}

readInt_32 :: !Pointer !Offset -> Int
readInt_32 pointer offset = code {
		pop_b 1
|		mov    (%ebx,%eax,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 3
}

unsafeCreateArray :: !.Int -> u:(a v:b) | Array a b, [u<=v]
unsafeCreateArray size = code {
	updatepop_a 0 7
	.d 1 1 i
	jmp_i 1
}

memcpyPointerToString :: !{#Char} !Pointer !Int -> Pointer
memcpyPointerToString s p n = code {
    ccall memcpy "spI:p"
}

derefCharArray :: !Pointer !Int -> {#Char}
derefCharArray ptr len
	# res = unsafeCreateArray len
	| memcpyPointerToString res ptr len == 0 = undef
	= res

load_char :: !Pointer -> Char
load_char ptr = code inline {
		load_ui8 0
	}

derefString :: !Pointer -> String
derefString ptr = derefCharArray ptr len
where
	len = skip_to_zero ptr - ptr

	skip_to_zero :: !Pointer -> Pointer
	skip_to_zero ptr
		| load_char ptr <> '\0'	= skip_to_zero (ptr+1)
								= ptr


getCommandLine :: !*World -> (![String],!*World)
getCommandLine env
	# argc = readInt4Z global_argc 0
	# argv = derefInt global_argv
	= ([derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\ i <- [0..argc - 1]], env)
where
	//The pushLc ABC instruction should work on all platforms / architectures
	//Since it does not always work properly we use a fallback to pushL in some cases
	//Fallback currently neccessary on:	
	// - 64 bit windows, 

	//Global argc pointer
	global_argc :: Pointer
	global_argc = IF_POSIX_OR_WINDOWS global_argclc (IF_INT_64_OR_32 global_argcl global_argclc)
	
	global_argclc :: Pointer
	global_argclc = code {
		pushLc global_argc
	}
	global_argcl :: Pointer
	global_argcl = code {
		pushL global_argc
	}
	//Global argv pointer
	global_argv :: Pointer
	global_argv = IF_POSIX_OR_WINDOWS global_argvlc (IF_INT_64_OR_32 global_argvl global_argvlc)
	
	global_argvlc :: Pointer
	global_argvlc = code {
		pushLc global_argv
	}
	
	global_argvl :: Pointer
	global_argvl = code {
		pushL global_argv
	}
