fun maxHelper with list =
  if isnil list then Nil
  else if (#list = Nil) then !list
  else if (!list > !(#list)) then maxHelper (!list @ #(#list))
  else if (!list < !(#list)) then maxHelper (#list)
in
fun largest_elems with l =
  if isnil l then Nil
  else maxHelper (!l) @ (largest_elems #l)
in
largest_elems