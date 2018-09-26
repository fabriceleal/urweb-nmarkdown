open Nregexmk

datatype mkTree =
	 H1 of string
       | H2 of string
       | H3 of string
       | Paragraph of list mkTree
       | Frag of string
       | Bold of string
       | Italic of string
       | Parts of list mkTree
       | Bullets of list mkTree
       | Numbered of list mkTree
       | Link of string * url
       | Hr
       | Br
       | Blank


	 
(* TODO this would be fancier if there was a typeclass showMk *)
(* TODO tables *)
(* TODO embed posts / boards / etc *)
(* TODO parse raw text *)
(* TODO editor *)
(* TODO find a way of serialize / deserialize the parsed structure from db. urls, specially *)
	 
fun renderMk markdown =
    case markdown of
	H1 title => <xml><h1>{[title]}</h1></xml>
      | H2 title => <xml><h2>{[title]}</h2></xml>
      | H3 title => <xml><h3>{[title]}</h3></xml>
      | Paragraph ls =>
	<xml>
	  <div> { List.foldr (fn e acc => <xml>{renderMk e} {acc}</xml>) <xml></xml> ls }</div></xml>
      | Frag text => <xml>{[text]}</xml>
      | Bold text => <xml><strong>{[text]}</strong></xml>
      | Italic text => <xml><em>{[text]}</em></xml>
      | Hr => <xml><hr /></xml>
      | Br => <xml><br /></xml>
      | Blank => <xml></xml>
      | Bullets items =>
	<xml>
	  <ul>
	    { List.foldr (fn e acc => <xml><li>{renderMk e}</li> {acc}</xml>) <xml></xml> items }
	  </ul>
	</xml>
      | Numbered items =>
	<xml>
	  <ol>
	    { List.foldr (fn e acc => <xml><li>{renderMk e}</li> {acc}</xml>) <xml></xml> items }
	  </ol>
	</xml>
      | Parts p =>
	List.foldr (fn e acc => <xml>{renderMk e} {acc}</xml>) <xml></xml> p
      | Link (text, target) =>
	<xml><a href={target}>{[text]}</a></xml>

fun testPageWithPars blah =
    return <xml>
      <body>
	test with param {[blah]}
      </body>
      </xml>

fun otherPage () =
    return <xml>
      <body>
	other page
      </body>
    </xml>

val testMk = Parts ((H1 "title") ::
				 (Paragraph ((Frag "this is some text") :: (Bold "test bold") :: (Frag "bla bla ") ::
									(Italic "test italic") :: (Frag "bla bla bla ") ::
									[]))
				 ::
				 (H2 "title2")
				 ::
				 (H3 "title3")
				 ::
				 Hr
				 ::
				 (Bullets ((Frag "item 1") :: (Parts ((Frag "item 2") :: (Bullets ((Frag "item 3.1") :: (Frag "item 3.2") :: [])) :: [])) :: []))
				 ::
				 (Numbered ((Frag "item 1") :: (Frag "item 2") ::
							    (Numbered ((Frag "item 3.1") :: (Frag "item 3.2") :: [])) :: []))
				 ::
				 (Link ("link from blessed", (bless "/Mk/otherPage")))
				 ::
				 (Link ("link from urled", (url (otherPage ()))))
				 ::
				 (Link ("link from urled with pars", (url (testPageWithPars "blah"))))
				 ::
				 [])
	     
fun test () =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/mk.css"/>
      </head>
      <body>
	{ renderMk testMk }
      </body>
    </xml>

style left
style right
style fit

type mkGroup = string * mkTag
      
fun renderMk' s =
    v <- signal s;
    return (renderMk v)

fun compileAsString (e : mkGroup) : string =
    case e of
	(raw, tag) =>
	case tag of
	    Whitespace => " "
	  | _ => raw

fun compileAsStringL (t: list mkGroup) : string =
    List.foldr strcat "" (List.mp compileAsString t)

fun compileAsMkTreeL (e : list mkGroup) : list mkTree =
    let
	fun eatItalic ls acc =
	    case ls of
		[] =>
		(Frag "_") :: compileAsMkTreeL (List.rev acc)
	      | h :: t =>
		case h of
		    (raw, tag) =>
		    case tag of
			ItalicDel => Italic (compileAsStringL (List.rev acc)) :: (compileAsMkTreeL t)
		      | _ => eatItalic t (h :: acc)
	
	and eatBold ls acc =
	    case ls of
		[] =>
		(Frag "*") ::  compileAsMkTreeL (List.rev acc)
	      | h :: t =>
		case h of
		    (raw, tag) =>
		    case tag of
			BoldDel => Bold (compileAsStringL (List.rev acc)) :: (compileAsMkTreeL t)
		      | _ => eatBold t (h :: acc)
    in
	case e of
	    [] => []
	  | h :: t =>
	    case h of
		(raw, tag) =>
		case tag of
		    BoldDel => eatBold t []
		  | ItalicDel => eatItalic t []
		  | _ => (Frag raw) :: compileAsMkTreeL t
    end
			  
fun compileParag (e : list mkGroup) : mkTree =
    Paragraph (compileAsMkTreeL e)

fun compileL (e : list mkGroup) : mkTree =
    case e of
	[] => Blank
      | h :: t =>
	case h of
	    (raw, tag) =>
	    case tag of
		Hash =>
		(case (strlen raw) of
		    0 => H1 (compileAsStringL t)
		  | 1 => H1 (compileAsStringL t)
		  | 2 => H2 (compileAsStringL t)
		  | _ => H3 (compileAsStringL t))
	      | _ => compileParag e
		     
					  
fun compile (txt : string) : mkTree =
    let
	val tokens = decomposeMk txt
    in
	Parts (List.mp compileL tokens)
    end

fun compileM txt =
    return (compile txt)
      
fun test2 () =
    data <- source "";
    compiled <- source (Parts []);
    
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/mk.css"/>
      </head>
      <body>
	<div class="left">
	  
	    <ctextarea source={data} class="fit" />
	    <button value="compile" onclick={fn _ => s' <- get data; d' <- rpc (compileM s'); set compiled d' } />
	</div>
	<div class="left">
	  <dyn signal={renderMk' compiled} />
	</div>
      </body>
    </xml>
(**)
