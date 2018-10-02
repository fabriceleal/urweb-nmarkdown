val otherPage : unit -> transaction page
val test : unit -> transaction page
val test2 : unit -> transaction page

datatype mkTree =
	 H1 of string
       | H2 of string
       | H3 of string
       | Paragraph of list mkTree
       | Frag of string
       | Bold of list mkTree
       | Italic of list mkTree
       | Parts of list mkTree
       | Bullets of list mkTree
       | Numbered of list mkTree
       | Link of string * url
       | Hr
       | Br
       | Blank

val renderMk : mkTree -> xbody
val compile : string -> mkTree

