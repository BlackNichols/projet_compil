{
type compteur =
  { mutable line : int;
    mutable word : int;
    mutable char : int;
  }
let cpt = {line = 0;word = 0; char = 0}
}

rule count = parse
    | "\n"
	{
	  cpt.line <- cpt.line + 1;
	  cpt.word <- cpt.word + 1;
	  cpt.char <- cpt.char + 1;
	  count lexbuf
	}
    | ['\t' ' ']+
	{
	  cpt.word <- cpt.word + 1;
	  cpt.char <- cpt.char + 1;
	  count lexbuf
	}
    | _
	{
	  cpt.char <- cpt.char + 1;
	  count lexbuf
	}
    | eof
	{
	  ()
	}
    
{
  let () =
    count (Lexing.from_channel (open_in Sys.argv.(1)));
    Printf.printf "Lines : %d\nWords : %d\nCharacters : %d\n"
      cpt.line cpt.word cpt.char
}
