
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
	   
(* TODO this would be fancier if there was a typeclass showMk *)
(* TODO tables *)
(* TODO embed posts / boards / etc *)
(* TODO parse raw text *)
(* TODO editor *)
	 
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
