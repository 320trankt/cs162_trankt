fun maxHelper with list =
  if isnil list then Nil
  else if isnil #list then !list
  else if !list > !(#list) then maxHelper ((!list) @ (#(#list)))
  else maxHelper (#list)
in
fun largest_elems with l =
  if isnil l then Nil
  else if isnil !l then largest_elems (#l)
  else (maxHelper (!l)) @ (largest_elems (#l))
in
largest_elems