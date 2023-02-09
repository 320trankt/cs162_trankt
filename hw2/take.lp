fun take with n, l =
  if isnil l
    then Nil
    else if (n = 0) then (Nil)
    else (!l) @ (take (n-1), (#l))
in
take