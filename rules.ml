let (english : rule list) = [

  ({person = UnifyBlock(Any);
  		number = UnifyBlock(Any);
	  	gender = UnifyBlock(Any);
	  	case = UnifyBlock(Any);
	  	subordinate = UnifyBlock(Any);
	  	entry = "S"; lex = ""},
    [{no_ft with entry = "NP"; case = Only Nom}; only_entry "VP"]);

  (only_entry "NP", [only_entry "Det"; only_entry "N"]);

  (only_entry "VP", [only_entry "IV"]);

  (only_entry "VP",
    [only_entry "TV";
    {person = UnifyBlock(Any);
    	number = UnifyBlock(Any);
    	gender = UnifyBlock(Any);
    	subordinate = UnifyBlock(Any);
    	lex= ""; entry = "NP";
    	case = UnifyBlock(Only Acc)}]);

  (only_entry "VP", 
    [only_entry "CV"; only_entry "C";
    {person = UnifyBlock(Any);
      number = UnifyBlock(Any);
      gender = UnifyBlock(Any);
      case = UnifyBlock(Any);
      subordinate = Only(Yes);
      entry = "S"; lex = ""}]);

  (only_entry "NP",
    [{no_ft with entry = "John";
    	number = Unifier Sing;
    	person = Unifier Third;
    	lex="John"}]);

  (only_entry "NP",
    [{no_ft with entry = "Mary"; 
    	number = Unifier Sing;
    	person = Unifier Third;
    	lex="Mary"}]);

  (only_entry "Det",
  	[{no_ft with entry = "the";
  		person = Unifier Third;
  		lex="the"}]);

  (only_entry "Det",
    [{no_ft with entry = "a";
    	number = Unifier Sing;
	    person = Unifier Third;
	    lex="a"}]);

  (only_entry "N",
    [{no_ft with entry = "dog";
    	number = Unifier Sing;
    	person = Unifier Third;
    	lex="dog"}]);

  (only_entry "IV",
    [{no_ft with entry = "walk";
    	lex="walk"}]);

  (only_entry "TV",
    [{no_ft with entry = "see";
    	lex="see"}]);


  (only_entry "TV",
    [{no_ft with entry = "walk";
    	lex="walk"}]);

  (only_entry "CV",
    [{no_ft with entry = "say";
	    lex="say"}]);

  (only_entry "CV",
    [{no_ft with entry = "think";
    	lex="think"}]);

  (only_entry "C",
    [{no_ft with entry = "that";lex="that"}])
]

let (italian : rule list) = [

  ({person = UnifyBlock(Any);
  		number = UnifyBlock(Any);
	  	gender = UnifyBlock(Any);
	  	case = UnifyBlock(Any);
	  	subordinate = UnifyBlock(Any);
	  	entry = "S"; lex = ""},
    [{no_ft with entry = "NP"; case = Only Nom}; only_entry "VP"]);

  (only_entry "NP", [only_entry "Det"; only_entry "N"]);

  (only_entry "VP", [only_entry "IV"]);

  (only_entry "VP",
    [only_entry "TV";
    {person = UnifyBlock(Any);
    	number = UnifyBlock(Any);
    	gender = UnifyBlock(Any);
    	subordinate = UnifyBlock(Any);
    	lex= ""; entry = "NP";
    	case = UnifyBlock(Only Acc)}]);

    (only_entry "VP", 
    [only_entry "CV"; only_entry "C";
    {person = UnifyBlock(Any);
      number = UnifyBlock(Any);
      gender = UnifyBlock(Any);
      case = UnifyBlock(Any);
      subordinate = Only(Yes);
      entry = "S"; lex = ""}]);

  (only_entry "NP",
    [{no_ft with entry = "Giovanni";
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Masc;
    	lex="John"}]);

  (only_entry "NP",
    [{no_ft with entry = "Maria"; 
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Fem;
    	lex="Mary"}]);

  (only_entry "Det",
  	[{no_ft with entry = "il";
  		person = Unifier Third;
  		gender = Unifier Masc;
  		number = Unifier Sing;
  		lex="the"}]);

  (only_entry "Det",
    [{no_ft with entry = "i";
      person = Unifier Third;
      gender = Unifier Masc;
      number = Unifier Pl;
      lex="the"}]);

  (only_entry "Det",
    [{no_ft with entry = "la";
    	number = Unifier Sing;
	    person = Unifier Third;
	    gender = Unifier Fem;
	    lex="the"}]);

  (only_entry "N",
    [{no_ft with entry = "cane";
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Masc;
    	lex="dog"}]);

  (only_entry "IV",
    [{no_ft with entry = "camminare";
    	lex="walk"}]);

  (only_entry "TV",
    [{no_ft with entry = "vedere";
    	lex="see"}]);


  (only_entry "TV",
    [{no_ft with entry = "camminare";
    	lex="walk"}]);

  (only_entry "CV",
    [{no_ft with entry = "dire";
	    lex="say"}]);

  (only_entry "CV",
    [{no_ft with entry = "pensare";
    	lex="think"}]);

  (only_entry "C",
    [{no_ft with entry = "che";lex="that"}])
]

let (german : rule list) = [

  ({person = UnifyBlock(Any);
  		number = UnifyBlock(Any);
	  	gender = UnifyBlock(Any);
	  	case = UnifyBlock(Any);
	  	subordinate = UnifyBlock(Any);
	  	entry = "S"; lex = ""},
    [{no_ft with entry = "NP"; case = Only Nom}; only_entry "VP"]);

  (only_entry "NP", [only_entry "Det"; only_entry "N"]);

  ({no_ft with entry = "VP"; subordinate = UnifyBlock(Any)}, [only_entry "IV"]);

  ({no_ft with entry = "VP"; subordinate = UnifyBlock(Only Yes)},
    [{person = UnifyBlock(Any);
    	number = UnifyBlock(Any);
    	gender = UnifyBlock(Any);
    	subordinate = Unifier(Yes);
    	lex= ""; entry = "NP";
    	case = UnifyBlock(Only Acc)};
    only_entry "TV"]);

  ({no_ft with entry = "VP"; subordinate = UnifyBlock(Only No)},
    [only_entry "TV"; {person = UnifyBlock(Any);
    	number = UnifyBlock(Any);
    	gender = UnifyBlock(Any);
    	subordinate = Unifier(No);
    	lex= ""; entry = "NP";
    	case = UnifyBlock(Only Acc)};]);

    (only_entry "VP", 
    [only_entry "CV"; only_entry "C";
    {person = UnifyBlock(Any);
      number = UnifyBlock(Any);
      gender = UnifyBlock(Any);
      case = UnifyBlock(Any);
      subordinate = Only(Yes);
      entry = "S"; lex = ""}]);

  (only_entry "NP",
    [{no_ft with entry = "Johannes";
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Masc;
    	lex="John"}]);

  (only_entry "NP",
    [{no_ft with entry = "Maria"; 
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Fem;
    	lex="Mary"}]);

  (only_entry "Det",
  	[{no_ft with entry = "der";
  		person = Unifier Third;
  		gender = Unifier Masc;
  		number = Unifier Sing;
  		case = Unifier Nom;
  		lex="the"}]);

  (only_entry "Det",
    [{no_ft with entry = "den";
    	number = Unifier Sing;
	    person = Unifier Third;
	    gender = Unifier Masc;
	    case = Unifier Acc;
	    lex="the"}]);

  (only_entry "Det",
    [{no_ft with entry = "die";
      person = Unifier Third;
      gender = Unifier Masc;
      number = Unifier Pl;
      case = Any;
      lex="the"}]);

  (only_entry "N",
    [{no_ft with entry = "Hund";
    	number = Unifier Sing;
    	person = Unifier Third;
    	gender = Unifier Masc;
    	lex="dog"}]);

  (only_entry "IV",
    [{no_ft with entry = "gehen";
    	lex="walk"}]);

  (only_entry "TV",
    [{no_ft with entry = "sehen";
    	lex="see"}]);


  (only_entry "TV",
    [{no_ft with entry = "gehen";
    	lex="walk"}]);

  (only_entry "CV",
    [{no_ft with entry = "sagen";
	    lex="say"}]);

  (only_entry "CV",
    [{no_ft with entry = "denken";
    	lex="think"}]);

  (only_entry "C",
    [{no_ft with entry = "dass";lex="that"}])
]